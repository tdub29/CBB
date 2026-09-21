#!/usr/bin/env python3
"""
Merge legacy/coach_2025_temp/coach_tournament_experience.csv (2025 rows) into
data/external/legacy/coach_tournament_experience.csv.
Run after: python scripts/fetch_coach_experience.py --training-csv data/external/templates/temp_2025_teams.csv --out-dir data/external/legacy/coach_2025_temp --fetch --playwright
"""
from pathlib import Path

import pandas as pd

project = Path(__file__).resolve().parent.parent
main_path = project / "data" / "external" / "legacy" / "coach_tournament_experience.csv"
temp_path = project / "data" / "external" / "legacy" / "coach_2025_temp" / "coach_tournament_experience.csv"

if not temp_path.exists():
    print(f"Not found: {temp_path}")
    print("Run the 2025 fetch first.")
    exit(1)

main = pd.read_csv(main_path)
temp = pd.read_csv(temp_path)

# Drop 2025 from main if any (shouldn't be), then concat
main = main[main["Year"] != 2025]
merged = pd.concat([main, temp], ignore_index=True)
merged = merged.sort_values(["Team", "Year"])
merged.to_csv(main_path, index=False)
print(f"Merged 2025 ({len(temp)} rows) into {main_path}. Total rows: {len(merged)}")
