#!/usr/bin/env python3
"""
Data-prep step 2: Enrich team_history_cache.csv with coach tournament experience.

Adds four columns derived from the cache's own coach/finish data:
  - coach_tourn_appearances_5y
  - coach_tourn_wins_5y / coach_tourn_losses_5y
  - coach_years_with_program

Input:  data/raw/team_history_cache.csv       (from populate_team_history_cache.py)
Output: data/raw/team_history_cache_enriched.csv

Run order:
  1. populate_team_history_cache.py  → team_history_cache.csv
  2. enrich_team_history_cache.py    → team_history_cache_enriched.csv   (this script)
  3. recompile_all_trank_team_tables.py → all_trank_team_tables.csv + tournament filter
"""
from __future__ import annotations

import re
from pathlib import Path

import pandas as pd

project_root = Path(__file__).resolve().parent.parent
RAW_DIR = project_root / "data" / "raw"
CACHE_PATH = RAW_DIR / "team_history_cache.csv"
OUT_PATH = RAW_DIR / "team_history_cache_enriched.csv"

EXCLUDE_YEAR = 2020

FINISH_TO_WL: dict[str, tuple[int, int]] = {
    "r64": (0, 1), "64": (0, 1), "first four": (0, 1),
    "r32": (1, 1), "32": (1, 1),
    "s16": (2, 1), "sweet 16": (2, 1), "16": (2, 1),
    "e8": (3, 1), "elite eight": (3, 1), "8": (3, 1),
    "f4": (4, 1), "final four": (4, 1), "4": (4, 1),
    "2nd": (5, 1), "runner": (5, 1), "finals": (5, 1),
    "champion": (6, 0), "champ": (6, 0), "1": (6, 0), "1st": (6, 0),
}


def _finish_to_wl(finish: str) -> tuple[int, int]:
    if not finish or not isinstance(finish, str):
        return (0, 0)
    s = str(finish).strip().lower()
    for key, wl in FINISH_TO_WL.items():
        if key in s:
            return wl
    if re.search(r"^\d+-\d+$", s):
        parts = s.split("-")
        return (int(parts[0]), int(parts[1]))
    return (0, 0)


def _years_prior_5(season_year: int) -> list[int]:
    """Five years prior to season_year, excluding the current year and 2020."""
    out = []
    for y in range(season_year - 5, season_year):
        if y != EXCLUDE_YEAR:
            out.append(y)
    return out


def main() -> int:
    cache = pd.read_csv(CACHE_PATH)
    if "Team" in cache.columns and "team" not in cache.columns:
        cache = cache.rename(columns={"Team": "team", "Year": "year"})
    if "year" not in cache.columns:
        print("team_history_cache must have 'year' (or 'Year') column.")
        return 1
    cache["year"] = pd.to_numeric(cache["year"], errors="coerce")
    cache = cache.dropna(subset=["year"])
    cache["year"] = cache["year"].astype(int)

    coach_col = "coach" if "coach" in cache.columns else "coach_raw"
    if coach_col not in cache.columns:
        print("team_history_cache must have 'coach' column.")
        return 1

    cache["tourn_w"] = cache["finish"].apply(lambda x: _finish_to_wl(x)[0])
    cache["tourn_l"] = cache["finish"].apply(lambda x: _finish_to_wl(x)[1])
    cache["_coach"] = cache[coach_col].astype(str).str.strip()

    # Precompute (coach, season_year) -> (appearances_5y, wins_5y, losses_5y): row (coach, year)
    # contributes to every season_year where year is in prior_5(season_year), i.e. sy in [year+1, year+5], sy != 2020
    # Only tournament rows (w + l > 0) count as appearances; non-tournament seasons are skipped.
    from collections import defaultdict
    coach_year_stats: dict[tuple[str, int], list[int]] = defaultdict(lambda: [0, 0, 0])
    for _, row in cache.iterrows():
        coach_name, year, w, l = row["_coach"], int(row["year"]), row["tourn_w"], row["tourn_l"]
        if not coach_name or coach_name == "nan":
            continue
        is_tournament = (w + l) > 0
        if not is_tournament:
            continue
        for sy in range(year + 1, min(year + 6, 2027)):
            if sy != EXCLUDE_YEAR:
                key = (coach_name, sy)
                coach_year_stats[key][0] += 1
                coach_year_stats[key][1] += w
                coach_year_stats[key][2] += l
    coach_year_stats = {k: tuple(v) for k, v in coach_year_stats.items()}

    # Precompute (team, year) -> years_with_program: for each team, years descending
    team_year_coach = cache.set_index(["team", "year"])["_coach"].to_dict()
    ywp_map: dict[tuple[str, int], int] = {}
    for team, grp in cache.groupby("team"):
        years_desc = sorted(grp["year"].unique(), reverse=True)
        for year in years_desc:
            coach = team_year_coach.get((team, year), "")
            ywp = 1
            for y in range(year - 1, 2006, -1):
                if team_year_coach.get((team, y)) == coach:
                    ywp += 1
                else:
                    break
            ywp_map[(team, year)] = ywp

    # Map to each row
    def lookup_5y(row):
        key = (row["_coach"], row["year"])
        return coach_year_stats.get(key, (0, 0, 0))

    out = cache.apply(lambda r: lookup_5y(r), axis=1)
    cache["coach_tourn_appearances_5y"] = [x[0] for x in out]
    cache["coach_tourn_wins_5y"] = [x[1] for x in out]
    cache["coach_tourn_losses_5y"] = [x[2] for x in out]
    cache["coach_years_with_program"] = cache.apply(
        lambda r: ywp_map.get((r["team"], r["year"]), 0), axis=1
    )

    cache = cache.drop(columns=["tourn_w", "tourn_l", "_coach"], errors="ignore")
    cache.to_csv(OUT_PATH, index=False)
    print(f"Wrote {OUT_PATH}: {len(cache)} rows, +4 experience columns.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
