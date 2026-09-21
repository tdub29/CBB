#!/usr/bin/env python3
from __future__ import annotations

from datetime import datetime
from pathlib import Path
import shutil

import pandas as pd

from build_size_sample import main as build_size_sample_main
from position_group_features import compute_max_porpag_feature, merge_feature_into_dataset


def main() -> int:
    project_root = Path(__file__).resolve().parent.parent
    training_path = project_root / "data" / "processed" / "cbb_training_clean.csv"
    archive_dir = project_root / "data" / "processed" / "archive"
    player_stats_path = project_root / "data" / "raw" / "player_stats" / "player_advanced_all.csv"

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir.mkdir(parents=True, exist_ok=True)
    backup_path = archive_dir / f"cbb_training_clean_before_max_c_porpag_{timestamp}.csv"
    shutil.copy2(training_path, backup_path)

    training_df = pd.read_csv(training_path)
    row_count = len(training_df)
    original_order = training_df.index.to_list()
    if "max_c_porpag" in training_df.columns:
        training_df = training_df.drop(columns=["max_c_porpag"])

    center_feature = compute_max_porpag_feature(
        player_stats_path=player_stats_path,
        position_token="C",
        feature_name="max_c_porpag",
    )
    merged_df = merge_feature_into_dataset(
        training_df,
        center_feature,
        feature_name="max_c_porpag",
        team_col="Team",
        year_col="year",
        fill_value=0.0,
    )
    if len(merged_df) != row_count:
        raise ValueError("Row count changed while merging max_c_porpag into cbb_training_clean.csv")
    if merged_df.index.to_list() != original_order:
        raise ValueError("Row order changed while merging max_c_porpag into cbb_training_clean.csv")

    merged_df.to_csv(training_path, index=False)
    build_size_sample_main()

    matched_count = int((merged_df["max_c_porpag"] > 0).sum())
    print(f"Backed up training file to: {backup_path}")
    print(f"Updated training file: {training_path}")
    print(f"Rows preserved: {len(merged_df)}")
    print(f"Non-zero max_c_porpag rows: {matched_count}/{len(merged_df)}")
    print("Rebuilt data/processed/size_sample.csv with max_c_porpag included.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
