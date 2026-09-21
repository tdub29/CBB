#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import pandas as pd

CONFERENCE_RENAMES = {
    "P10": "P12",
}


def main() -> int:
    project_root = Path(__file__).resolve().parent.parent
    input_path = project_root / "data" / "raw" / "assembled" / "all_trank_team_tables.csv"
    output_path = project_root / "data" / "processed" / "sos_sample.csv"

    df = pd.read_csv(input_path)
    rename_map = {"Year": "year", "conf": "Conf"}
    sample = df.rename(columns=rename_map)
    if "Conf" in sample.columns:
        sample["Conf"] = (
            sample["Conf"]
            .astype(str)
            .str.strip()
            .replace(CONFERENCE_RENAMES)
        )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    sample.to_csv(output_path, index=False)

    print(f"Wrote SOS sample: {output_path} ({len(sample)} rows, {len(sample.columns)} cols)")
    print(f"Year range: {int(pd.to_numeric(sample['year'], errors='coerce').min())}-{int(pd.to_numeric(sample['year'], errors='coerce').max())}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
