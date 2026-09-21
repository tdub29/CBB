#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import pandas as pd

RENAME_MAP = {
    "Year": "year",
    "conf": "Conf",
    "Record": "Rec",
    "Wins": "W",
    "Games": "G",
    "FT Rate": "FTR",
    "FT Rate D": "FTRD",
    "Op. FT%": "OPFT%",
    "Adj. T": "AdjT",
    "Exp.": "EXP.",
    "Avg Hgt.": "AVGHGT",
    "Eff. Hgt.": "EFFHGT",
}


def main() -> int:
    project_root = Path(__file__).resolve().parent.parent
    input_path = project_root / "data" / "raw" / "assembled" / "all_trank_team_tables.csv"
    output_path = project_root / "data" / "processed" / "free_throw_rate_sample.csv"

    df = pd.read_csv(input_path)
    sample = df.rename(columns={k: v for k, v in RENAME_MAP.items() if k in df.columns})

    required = ["FTR", "FTRD", "FT%", "OPFT%"]
    missing = [column for column in required if column not in sample.columns]
    if missing:
        raise ValueError(f"Missing free throw columns after rename: {missing}")

    for column in required:
        sample[column] = pd.to_numeric(sample[column], errors="coerce")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    sample.to_csv(output_path, index=False)

    print(f"Wrote Free Throw sample: {output_path} ({len(sample)} rows, {len(sample.columns)} cols)")
    print(f"Year range: {int(pd.to_numeric(sample['year'], errors='coerce').min())}-{int(pd.to_numeric(sample['year'], errors='coerce').max())}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
