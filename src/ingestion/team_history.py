"""Torvik team-history.php: fetch, parse and cache each program's season history (coach, seed, finish)."""

from __future__ import annotations

import logging
import re
import time
from io import StringIO
from pathlib import Path
from typing import Any
from urllib.parse import quote_plus

import numpy as np
import pandas as pd
import requests

from src.ingestion.team_tables import _normalize_header
from src.ingestion.torvik import USER_AGENT, _strip_rank_suffix

LOGGER = logging.getLogger(__name__)


TEAM_HISTORY_URL = "https://barttorvik.com/team-history.php"

TEAM_HISTORY_CANONICAL_COLS = [
    "team",
    "year",
    "t_rk",
    "coach",
    "seed",
    "finish",
    "conf",
    "ov_rec",
    "conf_rec",
    "adj_o",
    "adj_d",
    "efg_o",
    "efg_d",
    "to_percent",
    "tod_percent",
    "or_percent",
    "dr_percent",
    "ftr",
    "ftrd",
    "ft_percent",
    "two_pct",
    "two_pct_d",
    "three_pct",
    "adj_t",
]

TEAM_HISTORY_NUMERIC_COLS = [
    "year",
    "t_rk",
    "seed",
    "adj_o",
    "adj_d",
    "efg_o",
    "efg_d",
    "to_percent",
    "tod_percent",
    "or_percent",
    "dr_percent",
    "ftr",
    "ftrd",
    "ft_percent",
    "two_pct",
    "two_pct_d",
    "three_pct",
    "adj_t",
]

TEAM_HISTORY_HEADER_MAP = {
    "year": "year",
    "t-rk": "t_rk",
    "coach": "coach_raw",
    "conf": "conf",
    "rec.": "rec",
    "rec": "rec",
    "adjo": "adj_o",
    "adjd": "adj_d",
    "efgo": "efg_o",
    "efgd": "efg_d",
    "to%": "to_percent",
    "tod%": "tod_percent",
    "or%": "or_percent",
    "dr%": "dr_percent",
    "ftr": "ftr",
    "ftrd": "ftrd",
    "ft%": "ft_percent",
    "2p%": "two_pct",
    "2p%d": "two_pct_d",
    "3p%": "three_pct",
    "adj t": "adj_t",
    "adjt": "adj_t",
}


def _team_history_empty() -> pd.DataFrame:
    return pd.DataFrame(columns=TEAM_HISTORY_CANONICAL_COLS)


def _split_record_cell(rec: Any) -> tuple[str, str]:
    """Split Torvik record cell into overall and conference records."""
    s = str(rec).strip()
    if not s or s.lower() == "nan":
        return "", ""
    m = re.match(r"^\s*([0-9]+-[0-9]+)\s*(?:\(([^)]*)\))?\s*$", s)
    if m:
        ov = (m.group(1) or "").strip()
        conf = (m.group(2) or "").strip()
        return ov, conf
    return s, ""


def _split_coach_seed_finish(coach_cell: Any) -> tuple[str, float, str]:
    """Parse coach cell from team history into (coach, seed, finish)."""
    s = str(coach_cell).strip()
    if not s or s.lower() == "nan":
        return "", np.nan, ""

    # Examples:
    # "Jon Scheyer 1 seed, Final Four"
    # "Mike Krzyzewski 3 seed (Proj.)"
    # "Dan Hurley8 seed, R32"
    m = re.match(r"^(.+?)\s*(\d+)\s*seed(?:\s*\(([^)]*)\))?(?:\s*,\s*(.*))?$", s, re.I)
    if m:
        coach = (m.group(1) or "").strip()
        seed = float(m.group(2)) if m.group(2) else np.nan
        proj = (m.group(3) or "").strip()
        finish = (m.group(4) or "").strip()
        if not finish and proj:
            finish = proj
        return coach, seed, finish

    m2 = re.match(r"^(.+?)\s+(\d+)[,\s]+(.+)$", s)
    if m2:
        return (m2.group(1) or "").strip(), float(m2.group(2)), (m2.group(3) or "").strip()

    return s, np.nan, ""


def _parse_team_history_numeric(col: pd.Series) -> pd.Series:
    """Parse numeric history cells that may contain rank suffixes (e.g. '128.6 5')."""
    parsed = _strip_rank_suffix(col)
    return pd.to_numeric(parsed, errors="coerce")


def parse_team_history_to_schema(html: str, team: str) -> pd.DataFrame:
    """
    Parse Torvik team-history HTML into the bart_team_history-like canonical schema.

    Returns one row per season with TEAM_HISTORY_CANONICAL_COLS.
    """
    if not html:
        return _team_history_empty()
    try:
        tables = pd.read_html(StringIO(html))
    except Exception:
        return _team_history_empty()
    if not tables:
        return _team_history_empty()

    # Prefer table that has year + something coach-like.
    def _norm(s: str) -> str:
        return _normalize_header(s).replace(" ", "").replace("–", "-")

    chosen = None
    for tbl in tables:
        cols = {_norm(c): c for c in tbl.columns}
        has_year = "year" in cols
        has_coach = "coach" in cols or any("coach" in k for k in cols)
        if has_year and has_coach:
            chosen = tbl.copy()
            break
    if chosen is None:
        chosen = tables[0].copy()

    rename: dict[Any, str] = {}
    coach_col = None
    for c in chosen.columns:
        raw = _norm(c)
        if raw in TEAM_HISTORY_HEADER_MAP:
            rename[c] = TEAM_HISTORY_HEADER_MAP[raw]
        elif "coach" in raw and coach_col is None:
            rename[c] = "coach_raw"
            coach_col = c
    df = chosen.rename(columns=rename)
    if "year" not in df.columns or "coach_raw" not in df.columns:
        return _team_history_empty()

    if "rec" not in df.columns:
        df["rec"] = ""
    if "conf" not in df.columns:
        df["conf"] = ""

    coach_split = df["coach_raw"].apply(_split_coach_seed_finish)
    df["coach"] = coach_split.apply(lambda x: x[0])
    df["seed"] = coach_split.apply(lambda x: x[1])
    df["finish"] = coach_split.apply(lambda x: x[2])

    rec_split = df["rec"].apply(_split_record_cell)
    df["ov_rec"] = rec_split.apply(lambda x: x[0])
    df["conf_rec"] = rec_split.apply(lambda x: x[1])

    for col in TEAM_HISTORY_NUMERIC_COLS:
        if col in df.columns:
            df[col] = _parse_team_history_numeric(df[col])

    out = _team_history_empty()
    for c in TEAM_HISTORY_CANONICAL_COLS:
        if c in df.columns:
            out[c] = df[c]
    out["team"] = str(team).strip()

    out = out.dropna(subset=["year"]).copy()
    out["year"] = pd.to_numeric(out["year"], errors="coerce")
    out = out.dropna(subset=["year"])
    out["year"] = out["year"].astype(int)
    out = out.loc[:, TEAM_HISTORY_CANONICAL_COLS]
    out = out.sort_values("year").reset_index(drop=True)
    return out


def fetch_team_history(
    team: str,
    session: requests.Session | None = None,
    playwright_page=None,
    force_playwright: bool = False,
    user_agent: str = USER_AGENT,
) -> pd.DataFrame:
    """Fetch and parse one team from Torvik team-history.php."""
    team_str = str(team).strip()
    if not team_str:
        return _team_history_empty()
    # URL-encode so "Texas A&M" -> Texas+A%26M (ampersand must be %26)
    team_query = quote_plus(team_str)
    url = f"{TEAM_HISTORY_URL}?team={team_query}"
    html = None

    if not force_playwright:
        sess = session or requests.Session()
        headers = {
            "User-Agent": user_agent,
            "Accept": "text/html,application/xhtml+xml",
            "Referer": "https://barttorvik.com/",
        }
        try:
            resp = sess.get(url, headers=headers, timeout=20)
            resp.raise_for_status()
            if "Verifying Browser" not in resp.text and "Verifying your browser" not in resp.text:
                html = resp.text
        except Exception:
            html = None

    if html is None and playwright_page is not None:
        try:
            playwright_page.goto(url, wait_until="load", timeout=45000)
            try:
                playwright_page.wait_for_selector("table", timeout=10000)
            except Exception:
                pass
            playwright_page.wait_for_timeout(8000)
            maybe_html = playwright_page.content()
            if "Verifying Browser" not in maybe_html and "Verifying your browser" not in maybe_html:
                html = maybe_html
        except Exception:
            html = None

    return parse_team_history_to_schema(html or "", team=team_str)


def build_team_history_all(
    teams: list[str],
    cache_path: Path,
    refresh: bool = False,
    delay_sec: float = 0.8,
    force_playwright: bool = False,
    user_agent: str = USER_AGENT,
) -> tuple[pd.DataFrame, list[str]]:
    """
    Build/refresh team history cache and return history rows for requested teams.

    Returns (history_df, failed_teams).
    """
    cache_path = Path(cache_path)
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    team_list = sorted({str(t).strip() for t in teams if str(t).strip()})
    if not team_list:
        return _team_history_empty(), []

    cached = _team_history_empty()
    if cache_path.exists() and not refresh:
        try:
            cached = pd.read_csv(cache_path)
            for c in TEAM_HISTORY_CANONICAL_COLS:
                if c not in cached.columns:
                    cached[c] = np.nan
            cached = cached[TEAM_HISTORY_CANONICAL_COLS]
            cached["team"] = cached["team"].astype(str).str.strip()
            cached["year"] = pd.to_numeric(cached["year"], errors="coerce")
            cached = cached.dropna(subset=["year"])
            cached["year"] = cached["year"].astype(int)
        except Exception:
            cached = _team_history_empty()

    cached_teams = set(cached["team"].unique()) if not cached.empty else set()
    missing_teams = team_list if refresh else [t for t in team_list if t not in cached_teams]
    if not missing_teams:
        out = cached[cached["team"].isin(team_list)].copy()
        out = out.drop_duplicates(subset=["team", "year"], keep="last")
        return out, []

    session = requests.Session()
    session.headers.update({"User-Agent": user_agent})
    failed: list[str] = []
    fetched_rows: list[pd.DataFrame] = []
    use_playwright = force_playwright
    playwright_mgr = None
    browser = None
    page = None
    try:
        for i, team in enumerate(missing_teams, start=1):
            if i == 1 or i % 25 == 0:
                LOGGER.info(
                    {
                        "event": "team_history_progress",
                        "fetched": i,
                        "total": len(missing_teams),
                        "team": team,
                    }
                )
            if use_playwright and page is None:
                try:
                    from playwright.sync_api import sync_playwright

                    playwright_mgr = sync_playwright().start()
                    browser = playwright_mgr.chromium.launch(headless=True)
                    page = browser.new_page()
                except Exception:
                    page = None

            hist = fetch_team_history(
                team,
                session=session,
                playwright_page=page,
                force_playwright=use_playwright,
                user_agent=user_agent,
            )
            if hist.empty and not use_playwright:
                # Retry this team with Playwright fallback and keep it for remaining teams.
                use_playwright = True
                if page is None:
                    try:
                        from playwright.sync_api import sync_playwright

                        playwright_mgr = sync_playwright().start()
                        browser = playwright_mgr.chromium.launch(headless=True)
                        page = browser.new_page()
                    except Exception:
                        page = None
                hist = fetch_team_history(
                    team,
                    session=session,
                    playwright_page=page,
                    force_playwright=use_playwright,
                    user_agent=user_agent,
                )
            if hist.empty:
                failed.append(team)
            else:
                fetched_rows.append(hist)
            if delay_sec > 0:
                time.sleep(delay_sec)
    finally:
        try:
            session.close()
        except Exception:
            pass
        if browser is not None:
            try:
                browser.close()
            except Exception:
                pass
        if playwright_mgr is not None:
            try:
                playwright_mgr.stop()
            except Exception:
                pass

    fetched = pd.concat(fetched_rows, ignore_index=True) if fetched_rows else _team_history_empty()
    combined = pd.concat([cached, fetched], ignore_index=True)
    if combined.empty:
        return combined, failed
    combined = combined.drop_duplicates(subset=["team", "year"], keep="last")
    combined = combined.sort_values(["team", "year"]).reset_index(drop=True)
    combined.to_csv(cache_path, index=False)
    out = combined[combined["team"].isin(team_list)].copy()
    return out, failed


def _write_failed_teams_report(failed_teams: list[str], data_raw_dir: Path, run_tag: str) -> Path | None:
    """Write unresolved team names to a report file."""
    if not failed_teams:
        return None
    data_raw_dir = Path(data_raw_dir)
    run_dir = data_raw_dir / "pipeline_runs"
    run_dir.mkdir(parents=True, exist_ok=True)
    path = run_dir / f"team_history_failed_{run_tag}.txt"
    lines = sorted({t for t in failed_teams if t})
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path
