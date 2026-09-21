#!/usr/bin/env python3
"""Run data quality gates on any CSV. Reports pass/fail for each check.

Usage:
    python quality_gates.py data.csv
    python quality_gates.py data.csv --key-cols id date --target score
    python quality_gates.py data.csv --null-threshold 0.10
"""

from __future__ import annotations

import argparse
import sys

import numpy as np
import pandas as pd


def run_gates(
    df: pd.DataFrame,
    key_cols: list[str] | None = None,
    target: str | None = None,
    null_threshold: float = 0.05,
) -> pd.DataFrame:
    results = []

    # G01: No full-row duplicates
    dupes = df.duplicated().sum()
    results.append({"gate": "G01", "check": "no_full_duplicates", "value": dupes, "pass": dupes == 0, "severity": "critical"})

    # G02: Key uniqueness
    if key_cols:
        present = [c for c in key_cols if c in df.columns]
        if present:
            key_dupes = df.duplicated(subset=present).sum()
            results.append({"gate": "G02", "check": f"key_unique({','.join(present)})", "value": key_dupes, "pass": key_dupes == 0, "severity": "critical"})

    # G03: Target not null
    if target and target in df.columns:
        target_nulls = int(df[target].isnull().sum())
        results.append({"gate": "G03", "check": "target_complete", "value": target_nulls, "pass": target_nulls == 0, "severity": "critical"})

    # G04: No all-null columns
    all_null_cols = df.columns[df.isnull().all()].tolist()
    results.append({"gate": "G04", "check": "no_all_null_columns", "value": len(all_null_cols), "pass": len(all_null_cols) == 0, "severity": "critical"})

    # G05: No duplicate column names
    dup_cols = df.columns[df.columns.duplicated()].tolist()
    results.append({"gate": "G05", "check": "no_duplicate_col_names", "value": len(dup_cols), "pass": len(dup_cols) == 0, "severity": "critical"})

    # G06: Feature nulls below threshold
    max_null = float(df.isnull().mean().max())
    worst_col = df.isnull().mean().idxmax()
    results.append({"gate": "G06", "check": f"nulls<{null_threshold*100:.0f}% (worst: {worst_col})", "value": round(max_null, 4), "pass": max_null < null_threshold, "severity": "warning"})

    # G07: No constant columns
    constants = df.columns[df.nunique() <= 1].tolist()
    results.append({"gate": "G07", "check": "no_constant_columns", "value": len(constants), "pass": len(constants) == 0, "severity": "warning"})

    # G08: Reasonable row count
    results.append({"gate": "G08", "check": "row_count", "value": len(df), "pass": len(df) > 0, "severity": "critical"})

    return pd.DataFrame(results)


def main() -> None:
    parser = argparse.ArgumentParser(description="Run data quality gates on a CSV.")
    parser.add_argument("csv_path", help="Path to CSV file")
    parser.add_argument("--key-cols", nargs="+", default=None, help="Key columns for uniqueness check")
    parser.add_argument("--target", default=None, help="Target column name")
    parser.add_argument("--null-threshold", type=float, default=0.05, help="Max allowed null fraction per column")
    args = parser.parse_args()

    try:
        df = pd.read_csv(args.csv_path)
    except Exception as e:
        print(f"Error reading {args.csv_path}: {e}", file=sys.stderr)
        sys.exit(1)

    report = run_gates(df, key_cols=args.key_cols, target=args.target, null_threshold=args.null_threshold)

    print("=" * 70)
    print("DATA QUALITY GATES")
    print("=" * 70)
    for _, row in report.iterrows():
        status = "PASS" if row["pass"] else "FAIL"
        icon = "+" if row["pass"] else "X"
        print(f"  [{icon}] {row['gate']} {row['check']}: {row['value']} ({row['severity']}) -> {status}")

    critical_fails = report[(report["severity"] == "critical") & (~report["pass"])]
    warning_fails = report[(report["severity"] == "warning") & (~report["pass"])]

    print(f"\nSummary: {len(report)} gates | {(report['pass']).sum()} passed | {(~report['pass']).sum()} failed")
    if len(critical_fails) > 0:
        print(f"  CRITICAL FAILURES: {len(critical_fails)} — must fix before modeling")
    if len(warning_fails) > 0:
        print(f"  WARNINGS: {len(warning_fails)} — investigate")
    if len(critical_fails) == 0 and len(warning_fails) == 0:
        print("  All gates passed.")

    sys.exit(1 if len(critical_fails) > 0 else 0)


if __name__ == "__main__":
    main()
