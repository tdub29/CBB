#!/usr/bin/env python3
"""Fetch one team's history page and print table structure + coach column sample."""
import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(project_root))

import pandas as pd
from io import StringIO

from src.pipelines.madness import (
    TEAM_HISTORY_HEADER_MAP,
    fetch_team_history,
    _normalize_header,
    _split_coach_seed_finish,
)

def main():
    team = sys.argv[1] if len(sys.argv) > 1 else "Texas A&M"
    # Fetch via requests (no playwright) to get raw HTML
    import requests
    url = f"https://barttorvik.com/team-history.php?team={team.replace(' ', '+')}"
    r = requests.get(url, headers={"User-Agent": "Mozilla/5.0", "Referer": "https://barttorvik.com/"}, timeout=15)
    html = r.text
    if "Verifying" in html:
        print("Page returned verification; run with Playwright or try later.")
        return 1

    tables = pd.read_html(StringIO(html))
    print(f"Found {len(tables)} table(s)\n")
    for i, tbl in enumerate(tables):
        print(f"=== Table {i} === ")
        print("Columns:", list(tbl.columns))
        print("Normalized (no spaces):", {_normalize_header(c).replace(" ", ""): c for c in tbl.columns})
        # Check for coach-like column
        for c in tbl.columns:
            norm = _normalize_header(c).replace(" ", "").replace("–", "-")
            if "coach" in norm or "seed" in norm or "finish" in norm:
                print(f"  Sample values in '{c}':")
                for v in tbl[c].head(10).tolist():
                    print(f"    {repr(v)} -> _split_coach_seed_finish: {_split_coach_seed_finish(v)}")
        print()
    return 0

if __name__ == "__main__":
    sys.exit(main())
