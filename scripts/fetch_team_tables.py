#!/usr/bin/env python3
"""
Fetch Bart Torvik team-tables data from team-tables_each.php.

Attempts HTTP first, falls back to Playwright if bot protection blocks requests.
Outputs CSVs in data/external/sources/ as trank_team_table_data_YYYY.csv for pipeline use.
"""

from __future__ import annotations

import sys
import time
from io import StringIO
from pathlib import Path

# Ensure project root on path
project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

import pandas as pd
import requests

# Expected column order for pipeline (madness.py / madness.R)
COL_NAMES_EXT = [
    "TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES",
    "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%",
    "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%",
    "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR",
    "PAKE", "PASE", "TALENT", "blank", "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS",
]

# Map common HTML/API column names to COL_NAMES_EXT
COLUMN_MAP = {
    "Team": "TEAM", "team": "TEAM", "TEAM": "TEAM",
    "AdjOE": "ADJOE", "adj_o": "ADJOE", "ADJOE": "ADJOE", "Adj O": "ADJOE",
    "AdjDE": "ADJDE", "adj_d": "ADJDE", "ADJDE": "ADJDE", "Adj D": "ADJDE",
    "Barthag": "BARTHAG", "barthag": "BARTHAG", "BARTHAG": "BARTHAG",
    "Rec": "RECORD", "Record": "RECORD", "RECORD": "RECORD", "rec": "RECORD",
    "Wins": "WINS", "WINS": "WINS", "wins": "WINS",
    "G": "GAMES", "GAMES": "GAMES", "games": "GAMES", "Games": "GAMES",
    "EFG%": "EFG", "EFG": "EFG", "efg": "EFG", "EFG.": "EFG",
    "EFGD%": "EFGD.", "EFGD": "EFGD.", "EFGD.": "EFGD.", "efgd": "EFGD.",
    "FTR": "FTRATE", "FTRATE": "FTRATE", "ftr": "FTRATE", "FTRD": "FTRATED",
    "FTRATED": "FTRATED", "ftrd": "FTRATED",
    "TOR": "TOV%", "TOV%": "TOV%", "TOV": "TOV%", "tor": "TOV%",
    "TORD": "TOV%D", "TOV%D": "TOV%D", "tord": "TOV%D",
    "ORB": "OREB%", "OREB%": "OREB%", "OREB": "OREB%", "orb": "OREB%",
    "DRB": "OPOREB%", "OPOREB%": "OPOREB%", "OPOREB": "OPOREB%", "drb": "OPOREB%",
    "Adj T.": "ADJ.T", "Adj T": "ADJ.T", "ADJ.T": "ADJ.T", "adj_t": "ADJ.T",
    "2P%": "2P%", "2P%D": "2P%D", "3P%": "3P%", "3P%D": "3P%D",
    "BLK%": "BLK%", "BLKED%": "BLKED%", "AST%": "AST%", "OPAST%": "OPAST%",
    "3PRATE": "3PRATE", "3PRATED": "3PRATED",
    "AVGHGT": "AVGHGT", "EFFHGT": "EFFHGT", "EXP.": "EXP.", "TALENT": "TALENT",
    "ELITESOS": "ELITESOS", "Year": "YEAR", "year": "YEAR", "YEAR": "YEAR",
}


def load_config() -> dict:
    """Load config from YAML."""
    try:
        import yaml
        cfg_path = project_root / "configs" / "default.yaml"
        if cfg_path.exists():
            with open(cfg_path) as f:
                return yaml.safe_load(f)
    except ImportError:
        pass
    return {
        "ingestion": {
            "team_tables_url": "https://barttorvik.com/team-tables_each.php",
            "team_tables_year_range": [2008, 2025],
            "user_agent": "Mozilla/5.0",
        },
        "paths": {"data_external": "data/external"},
    }


def _fetch_http(url: str, user_agent: str) -> str | None:
    """Fetch page via HTTP. Returns HTML or None if blocked."""
    headers = {
        "User-Agent": user_agent,
        "Accept": "text/html,application/xhtml+xml",
        "Accept-Language": "en-US,en;q=0.9",
        "Referer": "https://barttorvik.com/",
    }
    try:
        resp = requests.get(url, headers=headers, timeout=30)
        resp.raise_for_status()
        text = resp.text
        if "Verifying Browser" in text or "Verifying your browser" in text:
            return None
        return text
    except Exception:
        return None


def _fetch_playwright(url: str, page=None) -> str | None:
    """Fetch page via Playwright. Use shared page to reuse session; otherwise create new browser."""
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        return None

    def _fetch_with_page(pg):
        try:
            pg.goto(url, wait_until="domcontentloaded", timeout=30000)
            pg.wait_for_timeout(3500)  # Allow JS verification / table render
            html = pg.content()
            if "Verifying Browser" in html:
                return None
            return html
        except Exception:
            return None

    if page is not None:
        return _fetch_with_page(page)

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            pg = browser.new_page()
            result = _fetch_with_page(pg)
            browser.close()
            return result
    except Exception:
        return None


def _map_columns(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Map scraped columns to COL_NAMES_EXT, add YEAR."""
    result = {}
    df_cols = {c.strip(): c for c in df.columns}

    for target in COL_NAMES_EXT:
        if target == "YEAR":
            result[target] = year
            continue
        # Find source column
        val = None
        for src, tgt in COLUMN_MAP.items():
            if tgt == target and src in df_cols:
                val = df[df_cols[src]]
                break
        if val is not None:
            result[target] = val
        else:
            result[target] = ""  # Empty for missing

    out = pd.DataFrame(result, columns=COL_NAMES_EXT)
    # Use first row as header row indicator - if Rk/header row exists, drop it
    if "TEAM" in out.columns and len(out) > 0:
        rk_vals = out.get("TEAM", pd.Series())
        if (rk_vals.astype(str).str.upper() == "TEAM").any():
            out = out[out["TEAM"].astype(str).str.upper() != "TEAM"].copy()
    return out


def _parse_table(html: str, year: int) -> pd.DataFrame | None:
    """Parse HTML into DataFrame with expected columns."""
    try:
        tables = pd.read_html(StringIO(html))
        if not tables:
            return None
        df = tables[0].copy()
        if len(df) < 2:
            return None
        # Drop header duplicate rows (e.g. Rk == 'Rk')
        for col in df.columns:
            if col in ("Rk", "RK") and (df[col].astype(str) == "Rk").any():
                df = df[df[col].astype(str) != "Rk"].copy()
                break
        mapped = _map_columns(df, year)
        return mapped
    except Exception:
        return None


def fetch_year(
    base_url: str,
    year: int,
    user_agent: str,
    use_playwright: bool = True,
    table_type: str = "R",
    playwright_page=None,
) -> pd.DataFrame | None:
    """Fetch team-tables data for one year. table_type: R=regular season, P=postseason, T=tournament.
    Pass playwright_page to reuse session (avoids repeated bot verification)."""
    params = f"year={year}&type={table_type}"
    url = f"{base_url}?{params}" if "?" not in base_url else f"{base_url}&{params}"
    html = _fetch_http(url, user_agent)
    if html is None:
        html = _fetch_http(base_url, user_agent)  # Try without year
    if html is None and use_playwright:
        html = _fetch_playwright(url, page=playwright_page)
    if html is None and use_playwright:
        html = _fetch_playwright(base_url, page=playwright_page)

    if html is None:
        return None
    return _parse_table(html, year)


def run(
    out_dir: Path | None = None,
    year_start: int | None = None,
    year_end: int | None = None,
    use_playwright: bool = True,
    table_type: str | None = None,
) -> list[Path]:
    """Fetch team-tables for year range and write CSVs. Returns list of written paths."""
    cfg = load_config()
    ingestion = cfg.get("ingestion", {})
    base_url = ingestion.get("team_tables_url", "https://barttorvik.com/team-tables_each.php")
    year_range = ingestion.get("team_tables_year_range", [2008, 2025])
    if table_type is None:
        table_type = ingestion.get("team_tables_type", "R")
    user_agent = ingestion.get("user_agent", "Mozilla/5.0")

    start = year_start if year_start is not None else year_range[0]
    end = year_end if year_end is not None else year_range[1]

    out_dir = out_dir or project_root / "data" / "external" / "sources"
    out_dir.mkdir(parents=True, exist_ok=True)

    written: list[Path] = []
    all_dfs: list[pd.DataFrame] = []
    exclude_years = set(ingestion.get("exclude_years", []))
    delay_sec = 2.5  # Longer delay to avoid rate limiting with type=R

    def _loop(playwright_page=None):
        for year in range(start, end + 1):
            if year in exclude_years:
                print(f"Fetching team-tables for {year}... skipped (excluded)")
                continue
            print(f"Fetching team-tables for {year} (type={table_type})...", end=" ", flush=True)
            df = fetch_year(
                base_url, year, user_agent, use_playwright,
                table_type=table_type, playwright_page=playwright_page,
            )
            if df is not None and len(df) > 0:
                path = out_dir / f"trank_team_table_data_{year}.csv"
                df.to_csv(path, index=False, header=False)
                written.append(path)
                all_dfs.append(df)
                print(f"OK ({len(df)} rows)")
            else:
                print("skipped (no data or blocked)")
            time.sleep(delay_sec)

    if use_playwright:
        try:
            from playwright.sync_api import sync_playwright
            with sync_playwright() as p:
                browser = p.chromium.launch(headless=True)
                page = browser.new_page()
                _loop(playwright_page=page)
                browser.close()
        except ImportError:
            _loop()
        except Exception as e:
            print(f"\nPlaywright session failed ({e}), retrying per-year...")
            _loop()
    else:
        _loop()

    if all_dfs:
        combined = pd.concat(all_dfs, ignore_index=True)
        combined_path = out_dir / "trank_team_table_data.csv"
        combined.to_csv(combined_path, index=False, header=False)
        written.append(combined_path)
        print(f"Wrote combined: {combined_path}")

    return written


def main() -> int:
    """CLI entry point."""
    import argparse
    parser = argparse.ArgumentParser(description="Fetch Bart Torvik team-tables into data/external/sources/")
    parser.add_argument("--no-playwright", action="store_true", help="Skip Playwright fallback")
    parser.add_argument("--year-start", type=int, help="Start year")
    parser.add_argument("--year-end", type=int, help="End year")
    parser.add_argument("--out-dir", type=Path, help="Output directory")
    parser.add_argument("--type", dest="table_type", choices=["R", "P", "T"], help="Table type: R=regular, P=postseason, T=tournament (default from config)")
    args = parser.parse_args()

    run_kw = dict(
        out_dir=args.out_dir,
        year_start=args.year_start,
        year_end=args.year_end,
        use_playwright=not args.no_playwright,
    )
    if getattr(args, "table_type", None) is not None:
        run_kw["table_type"] = args.table_type
    written = run(**run_kw)

    if not written:
        print(
            "\nNo data fetched. The site may use bot protection.\n"
            "Options:\n"
            "  1. Install playwright: pip install playwright && playwright install chromium\n"
            "  2. Manually export from https://barttorvik.com/team-tables_each.php\n"
            "     and save as trank_team_table_data*.csv in data/external/sources/"
        )
        return 1

    print(f"\nWrote {len(written)} file(s). Run the pipeline to use them.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
