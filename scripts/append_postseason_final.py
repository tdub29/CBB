#!/usr/bin/env python3
"""
Append all CSVs in data/raw/trank_postseason/, deduplicate, ensure years
2008--2025 (excluding 2020), and overwrite data/raw/trank_postseason_final.csv.
"""

from __future__ import annotations

import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

import pandas as pd

# Match pipeline schema (madness.py) — 39 names; raw files have 40 cols
TEAM_TABLE_COL_NAMES = [
    "TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES",
    "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%",
    "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%",
    "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR",
    "PAKE", "PASE", "TALENT", "blank", "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS",
]

REQUIRED_YEARS = {y for y in range(2008, 2026) if y != 2020}  # 2008..2025 except 2020


def main() -> int:
    raw_dir = project_root / "data" / "raw"
    season_dir = raw_dir / "trank_postseason"
    out_path = raw_dir / "trank_postseason_final.csv"

    if not season_dir.is_dir():
        print(f"Directory not found: {season_dir}", file=sys.stderr)
        return 1

    files = sorted(season_dir.glob("*.csv"))
    if not files:
        print("No CSV files in data/raw/trank_postseason/", file=sys.stderr)
        return 1

    combined = []
    for f in files:
        try:
            df = pd.read_csv(f, header=None)
            if df.empty or len(df.columns) < 4:
                continue
            n = min(len(TEAM_TABLE_COL_NAMES), len(df.columns))
            extra = [f"_{i}" for i in range(n, len(df.columns))]
            df.columns = list(TEAM_TABLE_COL_NAMES[:n]) + extra
            if "YEAR" not in df.columns:
                continue
            df["YEAR"] = pd.to_numeric(df["YEAR"], errors="coerce")
            df = df.dropna(subset=["YEAR"])
            df["YEAR"] = df["YEAR"].astype(int)
            combined.append(df)
        except Exception as e:
            print(f"Skip {f.name}: {e}", file=sys.stderr)
            continue

    if not combined:
        print("No valid data to append.", file=sys.stderr)
        return 1

    out_df = pd.concat(combined, ignore_index=True)
    # Deduplicate by TEAM + YEAR (keep first)
    out_df = out_df.drop_duplicates(subset=["TEAM", "YEAR"], keep="first")
    # Restrict to 2008..2025 and exclude 2020
    out_df = out_df[out_df["YEAR"].isin(REQUIRED_YEARS)].copy()
    out_df = out_df.sort_values(["YEAR", "TEAM"]).reset_index(drop=True)

    present_years = set(out_df["YEAR"].unique())
    missing = REQUIRED_YEARS - present_years
    if missing:
        print(f"Warning: missing years in source data: {sorted(missing)}", file=sys.stderr)

    # Write only pipeline columns (no extra _39 etc.)
    out_cols = [c for c in TEAM_TABLE_COL_NAMES if c in out_df.columns]
    out_df = out_df[out_cols]
    out_df.to_csv(out_path, index=False, header=False)

    print(f"Wrote {out_path} ({len(out_df)} rows, {len(present_years)} years)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
