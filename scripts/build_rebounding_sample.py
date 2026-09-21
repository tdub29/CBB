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
    "O Reb%": "ORB",
    "Op OReb%": "OP_ORB",
    "Adj. T": "AdjT",
    "Exp.": "EXP.",
    "Avg Hgt.": "AVGHGT",
    "Eff. Hgt.": "EFFHGT",
}


def main() -> int:
    project_root = Path(__file__).resolve().parent.parent
    input_path = project_root / "data" / "raw" / "assembled" / "all_trank_team_tables.csv"
    output_path = project_root / "data" / "processed" / "rebounding_sample.csv"

    df = pd.read_csv(input_path)
    sample = df.rename(columns={k: v for k, v in RENAME_MAP.items() if k in df.columns})
    if "ORB" not in sample.columns or "OP_ORB" not in sample.columns:
        raise ValueError("Missing rebounding columns after rename: expected ORB and OP_ORB")

    sample["ORB"] = pd.to_numeric(sample["ORB"], errors="coerce")
    sample["OP_ORB"] = pd.to_numeric(sample["OP_ORB"], errors="coerce")
    sample["DRB"] = 100.0 - sample["OP_ORB"]

    output_path.parent.mkdir(parents=True, exist_ok=True)
    sample.to_csv(output_path, index=False)

    print(f"Wrote Rebounding sample: {output_path} ({len(sample)} rows, {len(sample.columns)} cols)")
    print(f"Year range: {int(pd.to_numeric(sample['year'], errors='coerce').min())}-{int(pd.to_numeric(sample['year'], errors='coerce').max())}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
