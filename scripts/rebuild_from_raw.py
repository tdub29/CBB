#!/usr/bin/env python3
"""Rebuild processed training data from raw T-Rank CSVs + external data (no re-scrape)."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Rebuild cbbtrainingdata and cbb_training_clean from data/raw + data/external."
    )
    parser.add_argument(
        "--run-tag",
        default=None,
        help="Date tag for raw files (e.g. 20260216). If omitted, uses latest trank_regular_*.csv.",
    )
    parser.add_argument(
        "--raw-dir",
        default=None,
        help="Directory containing trank_regular_*.csv etc. (default: data/raw).",
    )
    args = parser.parse_args()

    raw_dir = Path(args.raw_dir) if args.raw_dir else project_root / "data" / "raw"
    if not raw_dir.is_absolute():
        raw_dir = project_root / raw_dir
    run_dir = raw_dir / "pipeline_runs"

    run_tag = args.run_tag
    if not run_tag:
        candidates = sorted(run_dir.glob("trank_regular_*.csv"), reverse=True) if run_dir.exists() else []
        if not candidates:
            candidates = sorted(raw_dir.glob("trank_regular_*.csv"), reverse=True)
        if not candidates:
            print(
                "No trank_regular_*.csv in data/raw or data/raw/pipeline_runs. Run the full pipeline once to save raw tables.",
                file=sys.stderr,
            )
            return 1
        run_tag = candidates[0].stem.replace("trank_regular_", "")
        print(f"Using run tag: {run_tag}")

    import pandas as pd
    from src.frozen_inputs import write_csv
    from src.pipelines.madness import build_features, build_clean_training_dataset

    master_path = run_dir / f"trank_regular_{run_tag}.csv"
    if not master_path.exists():
        master_path = raw_dir / f"trank_regular_{run_tag}.csv"
    conf_path = run_dir / f"trank_postseason_{run_tag}.csv"
    if not conf_path.exists():
        conf_path = raw_dir / f"trank_postseason_{run_tag}.csv"
    tourn_path = run_dir / f"trank_tournament_{run_tag}.csv"
    if not tourn_path.exists():
        tourn_path = raw_dir / f"trank_tournament_{run_tag}.csv"

    for p in (master_path, conf_path, tourn_path):
        if not p.exists():
            print(f"Missing {p}", file=sys.stderr)
            return 1

    master = pd.read_csv(master_path)
    conf = pd.read_csv(conf_path)
    tourn = pd.read_csv(tourn_path)
    ext_dir = project_root / "data" / "external"

    print("Building features from raw + external...")
    training_data, training_data_full = build_features(master, conf, tourn, ext_dir)

    processed_dir = project_root / "data" / "processed"
    processed_dir.mkdir(parents=True, exist_ok=True)

    full_path = processed_dir / f"cbbtrainingdata_{run_tag}.csv"
    write_csv(training_data_full, full_path)
    print(f"Wrote {full_path} ({len(training_data_full)} rows)")

    clean_df = build_clean_training_dataset(training_data_full)
    # Run-tagged: data/processed/cbb_training_clean.csv is frozen (src/frozen_inputs.py).
    clean_path = write_csv(clean_df, processed_dir / f"cbb_training_clean_{run_tag}.csv")
    print(f"Wrote {clean_path} ({len(clean_df)} rows, {len(clean_df.columns)} columns)")

    return 0


if __name__ == "__main__":
    sys.exit(main())
