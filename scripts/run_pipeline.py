#!/usr/bin/env python3
"""Entry point for the March Madness pipeline (Python)."""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path

# Ensure project root is on path
project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Change to project root
os.chdir(project_root)

from src.pipelines.madness import run_pipeline


def parse_args() -> argparse.Namespace:
    """Parse CLI args for runtime overrides."""
    parser = argparse.ArgumentParser(description="Run March Madness pipeline.")
    parser.add_argument(
        "--bracket",
        type=str,
        default=None,
        help="Optional path to bracket CSV (Team,Seed,Region) for round-probability simulation.",
    )
    parser.add_argument(
        "--n-sims",
        type=int,
        default=None,
        help="Optional Monte Carlo simulation count override.",
    )
    parser.add_argument(
        "--k",
        type=float,
        default=None,
        help="Optional Bradley-Terry logistic scale override.",
    )
    parser.add_argument(
        "--ridge-alpha",
        type=float,
        default=None,
        help="Optional Ridge alpha override (ignored if --no-alpha-tune is set and config enables tuning).",
    )
    parser.add_argument(
        "--alpha-grid",
        type=str,
        default=None,
        help="Optional comma-separated Ridge alpha grid override (e.g. '0,0.01,0.1,1,10').",
    )
    parser.add_argument(
        "--no-alpha-tune",
        action="store_true",
        help="Disable per-model Ridge alpha tuning (uses fixed --ridge-alpha/config value).",
    )
    parser.add_argument(
        "--skip-fetch",
        action="store_true",
        help="Skip optional fetch_team_tables.py pre-step.",
    )
    return parser.parse_args()


def run_optional_fetch() -> None:
    """Fetch optional external team-table files before main pipeline."""
    fetch_script = project_root / "scripts" / "fetch_team_tables.py"
    if not fetch_script.exists():
        return
    try:
        result = subprocess.run(
            [sys.executable, str(fetch_script)],
            cwd=str(project_root),
            capture_output=True,
            text=True,
            timeout=600,
            check=False,
        )
        if result.returncode == 0 and result.stdout:
            print(result.stdout)
    except (subprocess.TimeoutExpired, OSError) as exc:
        print(f"Note: Skipping team-tables fetch ({exc}). Pipeline continues with existing data.")


if __name__ == "__main__":
    args = parse_args()
    if not args.skip_fetch:
        run_optional_fetch()

    alpha_grid = None
    if args.alpha_grid:
        try:
            alpha_grid = [float(x.strip()) for x in args.alpha_grid.split(",") if x.strip() != ""]
        except ValueError as exc:
            raise SystemExit(f"Invalid --alpha-grid value: {args.alpha_grid!r}") from exc

    run_pipeline(
        bracket_csv=args.bracket,
        n_sims=args.n_sims,
        k_bradley_terry=args.k,
        ridge_alpha=args.ridge_alpha,
        optimize_ridge_alpha=False if args.no_alpha_tune else None,
        ridge_alpha_grid=alpha_grid,
    )
