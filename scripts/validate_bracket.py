#!/usr/bin/env python3
"""Validate NCAA bracket CSV input."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import pandas as pd

# Ensure project root import path
PROJECT_ROOT = Path(__file__).resolve().parent.parent
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from src.models.bracket import REQUIRED_BRACKET_COLUMNS, validate_bracket


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Validate bracket CSV for simulation.")
    parser.add_argument("bracket_csv", type=str, help="Path to bracket CSV file.")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    path = Path(args.bracket_csv)
    if not path.exists():
        print(f"Error: file not found -> {path}")
        return 1

    df = pd.read_csv(path)
    errors, warnings = validate_bracket(df)

    print(f"Checked: {path}")
    print(f"Required columns: {', '.join(REQUIRED_BRACKET_COLUMNS)}")
    if warnings:
        print("Warnings:")
        for warning in warnings:
            print(f"- {warning}")

    if errors:
        print("Errors:")
        for err in errors:
            print(f"- {err}")
        return 1

    print("Bracket is valid for simulation.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
