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
    "Adj OE": "AdjOE",
    "Adj DE": "AdjDE",
    "Adj. T": "AdjT",
    "Exp.": "EXP.",
    "Avg Hgt.": "AVGHGT",
    "Eff. Hgt.": "EFFHGT",
}


def main() -> int:
    project_root = Path(__file__).resolve().parent.parent
    input_path = project_root / "data" / "raw" / "assembled" / "all_trank_team_tables.csv"
    output_path = project_root / "data" / "processed" / "tempo_sample.csv"

    df = pd.read_csv(input_path)
    sample = df.rename(columns={k: v for k, v in RENAME_MAP.items() if k in df.columns})

    required = {"AdjT", "AdjOE", "AdjDE", "Barthag"}
    missing = sorted(required - set(sample.columns))
    if missing:
        raise ValueError(f"Missing tempo columns after rename: {missing}")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    sample.to_csv(output_path, index=False)

    print(f"Wrote Tempo sample: {output_path} ({len(sample)} rows, {len(sample.columns)} cols)")
    print(
        "Year range: "
        f"{int(pd.to_numeric(sample['year'], errors='coerce').min())}-"
        f"{int(pd.to_numeric(sample['year'], errors='coerce').max())}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
