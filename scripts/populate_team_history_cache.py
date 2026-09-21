#!/usr/bin/env python3
"""Populate Torvik team-history cache from all_trank_team_tables.csv only."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from src.pipelines.madness import (
    _get_cfg_path,
    build_team_history_all,
    load_all_trank_base,
    load_config,
)


def _parse_exclude_years(raw: str | None, default: tuple[int, ...]) -> tuple[int, ...]:
    if raw is None:
        return default
    vals: list[int] = []
    for part in raw.split(","):
        p = part.strip()
        if not p:
            continue
        vals.append(int(p))
    return tuple(vals)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Populate team_history_cache.csv from all_trank_team_tables.csv."
    )
    parser.add_argument(
        "--all-tables",
        type=str,
        default=None,
        help="Path to all_trank_team_tables.csv (default: configs/default.yaml).",
    )
    parser.add_argument(
        "--cache",
        type=str,
        default=None,
        help="Output cache path (default: configs/default.yaml paths.team_history_cache).",
    )
    parser.add_argument(
        "--year-start",
        type=int,
        default=None,
        help="Override config ingestion year start.",
    )
    parser.add_argument(
        "--year-end",
        type=int,
        default=None,
        help="Override config ingestion year end.",
    )
    parser.add_argument(
        "--exclude-years",
        type=str,
        default=None,
        help="Comma-separated years to exclude (default from config, e.g. 2020).",
    )
    parser.add_argument(
        "--delay-sec",
        type=float,
        default=None,
        help="Delay between team requests (default from config ingestion.team_history_delay_sec).",
    )
    parser.add_argument(
        "--playwright",
        action="store_true",
        help="Start with Playwright instead of HTTP first.",
    )
    parser.add_argument(
        "--refresh",
        action="store_true",
        help="Refresh all teams even if they are already in cache.",
    )
    parser.add_argument(
        "--max-teams",
        type=int,
        default=None,
        help="Limit to N teams (for testing).",
    )
    parser.add_argument(
        "--team",
        type=str,
        default=None,
        help="Fetch only this team (e.g. 'Texas A&M'). Writes only that team to --cache.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    cfg = load_config()

    year_range = cfg.get("ingestion", {}).get("year_range", [2008, 2025])
    year_start = int(args.year_start if args.year_start is not None else year_range[0])
    year_end = int(args.year_end if args.year_end is not None else year_range[1])
    exclude_years = _parse_exclude_years(
        args.exclude_years,
        tuple(cfg.get("ingestion", {}).get("exclude_years", [2020])),
    )
    delay_sec = float(
        args.delay_sec
        if args.delay_sec is not None
        else cfg.get("ingestion", {}).get("team_history_delay_sec", 0.8)
    )
    force_playwright = bool(
        args.playwright
        if args.playwright
        else cfg.get("ingestion", {}).get("team_history_force_playwright", False)
    )

    all_tables_path = Path(
        args.all_tables or _get_cfg_path(cfg, "all_trank_team_tables", "data/raw/assembled/all_trank_team_tables.csv")
    )
    cache_path = Path(
        args.cache or _get_cfg_path(cfg, "team_history_cache", "data/raw/team_history_cache.csv")
    )
    if not all_tables_path.is_absolute():
        all_tables_path = project_root / all_tables_path
    if not cache_path.is_absolute():
        cache_path = project_root / cache_path

    base = load_all_trank_base(all_tables_path, year_start, year_end, exclude_years)
    if base.empty:
        print(f"No base rows found in {all_tables_path}", file=sys.stderr)
        return 1

    teams = sorted(base["Team"].dropna().astype(str).str.strip().unique().tolist())
    if args.team is not None:
        team_str = args.team.strip()
        if team_str not in teams:
            print(f"Team '{team_str}' not in base; will try fetch anyway.", file=sys.stderr)
        teams = [team_str]
    elif args.max_teams is not None and args.max_teams > 0:
        teams = teams[: args.max_teams]

    print(
        f"Populating team history cache for {len(teams)} teams "
        f"(years {year_start}-{year_end}, excluded={list(exclude_years)})..."
    )
    history, failed = build_team_history_all(
        teams=teams,
        cache_path=cache_path,
        refresh=args.refresh,
        delay_sec=delay_sec,
        force_playwright=force_playwright,
    )

    print(f"Cache path: {cache_path}")
    print(f"Rows returned: {len(history)}")
    print(f"Unique teams returned: {history['team'].nunique() if not history.empty else 0}")
    print(f"Failed teams: {len(failed)}")
    if failed:
        preview = ", ".join(sorted(failed)[:15])
        print(f"Failed sample: {preview}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

