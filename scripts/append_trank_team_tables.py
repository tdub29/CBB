#!/usr/bin/env python3
"""
Append all trank_team_table_data*.csv files in data/raw/ into a single
all_trank_team_tables.csv (headless, same format as inputs for pipeline use).
"""

from __future__ import annotations

import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

import pandas as pd

# Match pipeline schema (TEAM_TABLE_COL_NAMES in src/ingestion/team_tables.py)
TEAM_TABLE_COL_NAMES = [
    "TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES",
    "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%",
    "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%",
    "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR",
    "PAKE", "PASE", "TALENT", "blank", "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS",
]


def main() -> int:
    raw_dir = project_root / "data" / "raw"
    out_path = raw_dir / "assembled" / "all_trank_team_tables.csv"

    # Only trank_team_table_dataNN.csv (sources/ or raw root)
    files = sorted((raw_dir / "sources").glob("trank_team_table_data[0-9]*.csv")) if (raw_dir / "sources").exists() else []
    if not files:
        files = sorted(raw_dir.glob("trank_team_table_data[0-9]*.csv"))
    if not files:
        print("No trank_team_table_data*.csv files found in data/raw/", file=sys.stderr)
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
            combined.append(df)
        except Exception as e:
            print(f"Skip {f.name}: {e}", file=sys.stderr)
            continue

    if not combined:
        print("No valid data to append.", file=sys.stderr)
        return 1

    out_df = pd.concat(combined, ignore_index=True)
    # Keep only pipeline columns so schema is consistent
    out_cols = [c for c in TEAM_TABLE_COL_NAMES if c in out_df.columns]
    out_df = out_df[out_cols]
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(out_path, index=False, header=False)
    print(f"Wrote {out_path} ({len(out_df)} rows from {len(combined)} files)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
