#!/usr/bin/env python3
"""
Fetch Bart Torvik player advanced season stats for each year.

Strategy:
  - 2008-2023: Download pre-built parquet files from the toRvik-data GitHub repo
  - 2024+:     Fetch raw CSV from barttorvik.com/getadvstats.php?year=YYYY&csv=1
               and map columns to match the parquet schema

Output: Per-year CSVs and a combined CSV in data/raw/player_stats/
"""

from __future__ import annotations

import sys
import time
from io import BytesIO, StringIO
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

import pandas as pd
import requests

PARQUET_BASE = (
    "https://github.com/andreweatherman/toRvik-data/raw/main/player_season"
)
CSV_BASE = "https://barttorvik.com/getadvstats.php"

ADV_COLUMNS = [
    "player", "pos", "exp", "team", "conf", "g", "min",
    "porpag", "dporpag", "ortg", "adj_oe", "drtg", "adj_de",
    "stops", "obpm", "dbpm", "bpm", "oreb", "dreb", "ast",
    "to", "blk", "stl", "ftr", "pfr", "rec", "pick", "year", "id",
]

# R uses 1-indexed columns; Python uses 0-indexed.
# R select for adv: 1, 65, 26, 2:5, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 46, 32, 33
RAW_CSV_COL_INDICES = [
    0, 64, 25, 1, 2, 3, 4, 28, 48, 5, 29, 46, 47, 49,
    55, 56, 53, 9, 10, 11, 12, 22, 23, 24, 30, 34, 45, 31, 32,
]

LAST_PARQUET_YEAR = 2023
USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)


def load_config() -> dict:
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
            "year_range": [2008, 2025],
            "exclude_years": [2020],
            "user_agent": USER_AGENT,
        },
    }


def _fetch_parquet(year: int) -> pd.DataFrame | None:
    """Download a pre-built parquet from the toRvik-data GitHub repo."""
    url = f"{PARQUET_BASE}/{year}/advanced_{year}.parquet"
    try:
        r = requests.get(url, timeout=60)
        if r.status_code != 200:
            return None
        import pyarrow.parquet as pq
        table = pq.read_table(BytesIO(r.content))
        df = table.to_pandas()
        if "year" not in df.columns:
            df["year"] = year
        return df
    except Exception as e:
        print(f"  parquet error: {e}")
        return None


def _fetch_csv(year: int, user_agent: str) -> pd.DataFrame | None:
    """Download raw CSV from barttorvik.com and map to standard columns."""
    url = f"{CSV_BASE}?year={year}&csv=1"
    headers = {
        "User-Agent": user_agent,
        "Accept": "text/csv,text/plain,*/*",
        "Referer": "https://barttorvik.com/",
    }
    try:
        r = requests.get(url, headers=headers, timeout=30)
        if r.status_code != 200:
            return None
        if "Verifying Browser" in r.text:
            return None

        df_raw = pd.read_csv(StringIO(r.text), header=None)
        if len(df_raw) < 2:
            return None

        max_idx = max(RAW_CSV_COL_INDICES)
        if len(df_raw.columns) <= max_idx:
            print(f"  unexpected column count: {len(df_raw.columns)} (need {max_idx + 1})")
            return None

        df = df_raw.iloc[:, RAW_CSV_COL_INDICES].copy()
        df.columns = ADV_COLUMNS
        return df
    except Exception as e:
        print(f"  csv error: {e}")
        return None


def fetch_year(year: int, user_agent: str) -> pd.DataFrame | None:
    """Fetch player advanced stats for one year, choosing the best source."""
    if year <= LAST_PARQUET_YEAR:
        df = _fetch_parquet(year)
        if df is not None:
            return df
        print(f"  parquet unavailable, falling back to CSV...")

    return _fetch_csv(year, user_agent)


def run(
    out_dir: Path | None = None,
    year_start: int | None = None,
    year_end: int | None = None,
) -> list[Path]:
    """Fetch player advanced stats for each year. Returns list of written paths."""
    cfg = load_config()
    ingestion = cfg.get("ingestion", {})
    year_range = ingestion.get("year_range", [2008, 2025])
    exclude_years = set(ingestion.get("exclude_years", []))
    user_agent = ingestion.get("user_agent", USER_AGENT)

    start = year_start if year_start is not None else year_range[0]
    end = year_end if year_end is not None else year_range[1]

    out_dir = out_dir or project_root / "data" / "raw" / "player_stats"
    out_dir.mkdir(parents=True, exist_ok=True)

    written: list[Path] = []
    all_dfs: list[pd.DataFrame] = []

    for year in range(start, end + 1):
        if year in exclude_years:
            print(f"[{year}] skipped (excluded)")
            continue

        source = "parquet" if year <= LAST_PARQUET_YEAR else "CSV"
        print(f"[{year}] fetching ({source})...", end=" ", flush=True)

        df = fetch_year(year, user_agent)
        if df is not None and len(df) > 0:
            for col in ADV_COLUMNS:
                if col not in df.columns:
                    df[col] = None
            df = df[ADV_COLUMNS]

            path = out_dir / f"player_advanced_{year}.csv"
            df.to_csv(path, index=False)
            written.append(path)
            all_dfs.append(df)
            print(f"OK ({len(df)} players)")
        else:
            print("FAILED (no data or blocked)")

        time.sleep(1.5 if year <= LAST_PARQUET_YEAR else 3.0)

    if all_dfs:
        combined = pd.concat(all_dfs, ignore_index=True)
        combined_path = out_dir / "player_advanced_all.csv"
        combined.to_csv(combined_path, index=False)
        written.append(combined_path)
        print(f"\nWrote combined: {combined_path} ({len(combined)} total players)")

    return written


def main() -> int:
    import argparse
    parser = argparse.ArgumentParser(
        description="Fetch Bart Torvik player advanced stats per year"
    )
    parser.add_argument("--year-start", type=int, help="Start year (default: from config)")
    parser.add_argument("--year-end", type=int, help="End year (default: from config)")
    parser.add_argument("--out-dir", type=Path, help="Output directory (default: data/raw/player_stats/)")
    args = parser.parse_args()

    written = run(
        out_dir=args.out_dir,
        year_start=args.year_start,
        year_end=args.year_end,
    )

    if not written:
        print("\nNo data fetched. Check network or Bart Torvik availability.")
        return 1

    print(f"\nDone. Wrote {len(written)} file(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
