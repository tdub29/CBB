#!/usr/bin/env python3
"""Quick dataset profiling script. Run on any CSV to get a structured summary.

Usage:
    python profile_dataset.py data.csv
    python profile_dataset.py data.csv --key-cols Team year
    python profile_dataset.py data.csv --corr-threshold 0.85
"""

from __future__ import annotations

import argparse
import sys

import numpy as np
import pandas as pd


def profile(df: pd.DataFrame, key_cols: list[str] | None = None, corr_threshold: float = 0.85) -> None:
    print("=" * 60)
    print("DATASET PROFILE")
    print("=" * 60)

    print(f"\nShape: {df.shape[0]:,} rows x {df.shape[1]} columns")
    print(f"Memory: {df.memory_usage(deep=True).sum() / 1e6:.1f} MB")
    print(f"Full-row duplicates: {df.duplicated().sum():,}")
    if key_cols:
        present = [c for c in key_cols if c in df.columns]
        if present:
            print(f"Key duplicates ({', '.join(present)}): {df.duplicated(subset=present).sum():,}")

    print(f"\nDtype breakdown:")
    for dtype, count in df.dtypes.value_counts().items():
        print(f"  {dtype}: {count}")

    null_pct = (df.isnull().mean() * 100).sort_values(ascending=False)
    nulls = null_pct[null_pct > 0]
    if len(nulls) > 0:
        print(f"\nColumns with nulls ({len(nulls)}):")
        for col, pct in nulls.head(15).items():
            print(f"  {col}: {pct:.1f}%")
        if len(nulls) > 15:
            print(f"  ... and {len(nulls) - 15} more")
    else:
        print("\nNo null values.")

    numeric = df.select_dtypes(include=[np.number])
    if not numeric.empty:
        print(f"\nNumeric summary ({len(numeric.columns)} columns):")
        desc = numeric.describe().T[["count", "mean", "std", "min", "50%", "max"]]
        desc.columns = ["count", "mean", "std", "min", "median", "max"]
        print(desc.round(3).to_string())

        constant = numeric.columns[numeric.nunique() <= 1].tolist()
        if constant:
            print(f"\n  Constant columns (drop these): {constant}")

        skewed = numeric.skew().abs()
        high_skew = skewed[skewed > 2].sort_values(ascending=False)
        if not high_skew.empty:
            print(f"\n  Highly skewed (|skew| > 2):")
            for col, s in high_skew.items():
                print(f"    {col}: {s:.2f}")

    categorical = df.select_dtypes(exclude=[np.number])
    if not categorical.empty:
        print(f"\nCategorical summary ({len(categorical.columns)} columns):")
        for col in categorical.columns:
            n_unique = categorical[col].nunique()
            top = categorical[col].mode().iloc[0] if not categorical[col].mode().empty else "N/A"
            print(f"  {col}: {n_unique} unique, top='{top}'")

    if len(numeric.columns) >= 2:
        corr = numeric.corr()
        upper = corr.where(np.triu(np.ones(corr.shape), k=1).astype(bool))
        pairs = []
        for i in range(len(upper)):
            for j in range(len(upper.columns)):
                v = upper.iloc[i, j]
                if pd.notna(v) and abs(v) > corr_threshold:
                    pairs.append((upper.index[i], upper.columns[j], round(v, 4)))
        if pairs:
            pairs.sort(key=lambda x: -abs(x[2]))
            print(f"\nHighly correlated pairs (|r| > {corr_threshold}):")
            for a, b, r in pairs[:15]:
                print(f"  {a} <-> {b}: {r}")

    print("\n" + "=" * 60)


def main() -> None:
    parser = argparse.ArgumentParser(description="Profile a CSV dataset.")
    parser.add_argument("csv_path", help="Path to CSV file")
    parser.add_argument("--key-cols", nargs="+", default=None, help="Key columns to check for duplicates")
    parser.add_argument("--corr-threshold", type=float, default=0.85, help="Correlation threshold for flagging")
    args = parser.parse_args()

    try:
        df = pd.read_csv(args.csv_path)
    except Exception as e:
        print(f"Error reading {args.csv_path}: {e}", file=sys.stderr)
        sys.exit(1)

    profile(df, key_cols=args.key_cols, corr_threshold=args.corr_threshold)


if __name__ == "__main__":
    main()
