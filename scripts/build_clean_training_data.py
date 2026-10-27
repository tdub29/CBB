#!/usr/bin/env python3
"""Build the single clean training dataset from a full pipeline CSV (no duplicate vars/rows, all outcomes)."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

def main() -> None:
    parser = argparse.ArgumentParser(description="Build cbb_training_clean.csv from a full training CSV.")
    parser.add_argument(
        "input_csv",
        nargs="?",
        default=None,
        help="Path to cbbtrainingdata_YYYYMMDD.csv; if omitted, uses latest in data/processed.",
    )
    parser.add_argument(
        "-o", "--output",
        default=None,
        help="Output path (default: data/processed/cbb_training_clean_YYYYMMDD.csv; the undated file is frozen).",
    )
    args = parser.parse_args()

    processed_dir = project_root / "data" / "processed"
    if args.input_csv:
        input_path = Path(args.input_csv)
        if not input_path.is_absolute():
            input_path = project_root / input_path
    else:
        candidates = sorted(processed_dir.glob("cbbtrainingdata_*.csv"), reverse=True)
        if not candidates:
            print("No cbbtrainingdata_*.csv found in data/processed. Run the pipeline first.", file=sys.stderr)
            sys.exit(1)
        input_path = candidates[0]
        print(f"Using latest: {input_path.name}")

    if not input_path.exists():
        print(f"File not found: {input_path}", file=sys.stderr)
        sys.exit(1)

    from datetime import date

    import pandas as pd
    from src.frozen_inputs import write_csv
    from src.pipelines.madness import build_clean_training_dataset

    full = pd.read_csv(input_path)
    clean_df = build_clean_training_dataset(full)

    out_path = Path(args.output) if args.output else processed_dir / f"cbb_training_clean_{date.today():%Y%m%d}.csv"
    if not out_path.is_absolute():
        out_path = project_root / out_path
    write_csv(clean_df, out_path)
    print(f"Clean training dataset written to {out_path} ({len(clean_df)} rows, {len(clean_df.columns)} columns)")


if __name__ == "__main__":
    main()
