#!/usr/bin/env python3
"""Fetch the 8 ampersand teams missing from team_history_cache into a temp file, then merge."""
from __future__ import annotations
import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(project_root))

import pandas as pd
from src.pipelines.madness import build_team_history_all, TEAM_HISTORY_CANONICAL_COLS

MISSING_TEAMS = [
    "Alabama A&M",
    "East Texas A&M",
    "Florida A&M",
    "North Carolina A&T",
    "Prairie View A&M",
    "Texas A&M",
    "Texas A&M Corpus Chris",
    "William & Mary",
]

def main() -> int:
    root = project_root
    temp_cache = root / "data" / "raw" / "_staging" / "team_history_ampersand_temp.csv"
    temp_cache.parent.mkdir(parents=True, exist_ok=True)
    main_cache = root / "data" / "raw" / "team_history_cache.csv"

    print(f"Fetching {len(MISSING_TEAMS)} teams with ampersands...")
    history, failed = build_team_history_all(
        teams=MISSING_TEAMS,
        cache_path=temp_cache,
        refresh=True,
        delay_sec=1.0,
        force_playwright=True,
    )
    print(f"Fetched: {len(history)} rows, {history['team'].nunique()} teams")
    if failed:
        print(f"Failed: {failed}")

    # Merge into main cache
    if main_cache.exists():
        existing = pd.read_csv(main_cache)
        # Drop old rows for these teams, replace with fresh data
        mask = existing["team"].isin(MISSING_TEAMS)
        kept = existing[~mask]
        merged = pd.concat([kept, history], ignore_index=True)
        merged = merged.drop_duplicates(subset=["team", "year"], keep="last")
        merged = merged.sort_values(["team", "year"]).reset_index(drop=True)
        merged.to_csv(main_cache, index=False)
        print(f"Merged into {main_cache}: {len(merged)} total rows")
    else:
        history.to_csv(main_cache, index=False)
        print(f"Wrote new cache: {main_cache}")

    return 0

if __name__ == "__main__":
    sys.exit(main())
