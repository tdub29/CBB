#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import pandas as pd

from size_analysis_common import CONFERENCE_RENAMES, get_size_base_paths
from position_group_features import compute_max_porpag_feature, merge_feature_into_dataset


def main() -> int:
    paths = get_size_base_paths()
    input_path = paths["input_raw"]
    output_path = paths["input_sample"]
    player_stats_path = paths["player_stats_advanced"]

    df = pd.read_csv(input_path)
    rename_map = {
        "Year": "year",
        "conf": "Conf",
        "Avg Hgt.": "AVGHGT",
        "Eff. Hgt.": "EFFHGT",
        "Blk %": "BLK%",
        "Blked %": "BLKED%",
        "Talent": "TALENT",
    }
    applicable = {old: new for old, new in rename_map.items() if old in df.columns and new not in df.columns}
    sample = df.rename(columns=applicable)
    if "Conf" in sample.columns:
        sample["Conf"] = sample["Conf"].astype(str).str.strip().replace(CONFERENCE_RENAMES)
    center_feature = compute_max_porpag_feature(
        player_stats_path=player_stats_path,
        position_token="C",
        feature_name="max_c_porpag",
    )
    sample = merge_feature_into_dataset(
        sample,
        center_feature,
        feature_name="max_c_porpag",
        team_col="Team",
        year_col="year",
        fill_value=0.0,
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    sample.to_csv(output_path, index=False)

    year_min = int(pd.to_numeric(sample["year"], errors="coerce").min())
    year_max = int(pd.to_numeric(sample["year"], errors="coerce").max())
    print(f"Wrote size sample: {output_path} ({len(sample)} rows, {len(sample.columns)} cols)")
    print(f"Year range: {year_min}-{year_max}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
