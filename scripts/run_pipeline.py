#!/usr/bin/env python3
"""Entry point for the March Madness pipeline (Python)."""

from __future__ import annotations

import argparse
import os
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
        help="Deprecated no-op (kept for backward compatibility).",
    )
    parser.add_argument(
        "--refresh-team-history",
        action="store_true",
        help="Force refresh of team-history cache before building training data.",
    )
    parser.add_argument(
        "--prep-raw",
        action="store_true",
        help="Run raw-data prep pipeline (enrich cache → recompile tables) before modeling.",
    )
    parser.add_argument(
        "--data-only",
        action="store_true",
        help="Only compile and validate training data; do not run any models.",
    )
    parser.add_argument(
        "--year",
        type=int,
        default=None,
        metavar="YEAR",
        help="Single year to compile (e.g. 2025); overrides config year range.",
    )
    parser.add_argument(
        "--holdout",
        type=int,
        default=None,
        metavar="YEAR",
        help=(
            "Honest holdout evaluation: refit on seasons before YEAR from the existing "
            "data/processed/cbb_training_clean.csv (no data rebuild), score YEAR against the "
            "historical seed lookup, and write reports/model_optimization_YYYYMMDD.md."
        ),
    )
    parser.add_argument(
        "--trank-only",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    return parser.parse_args()


def run_raw_data_prep() -> None:
    """Run the raw-data preparation pipeline (enrich + recompile).

    Equivalent to running these scripts in order:
      1. python scripts/enrich_team_history_cache.py
      2. python scripts/recompile_all_trank_team_tables.py
    """
    import subprocess
    scripts_dir = project_root / "scripts"

    print("=" * 60)
    print("RAW DATA PREP: Step 1 - Enrich team-history cache")
    print("=" * 60)
    ret = subprocess.run(
        [sys.executable, str(scripts_dir / "enrich_team_history_cache.py")],
        cwd=str(project_root),
    )
    if ret.returncode != 0:
        raise SystemExit(f"enrich_team_history_cache.py failed (exit {ret.returncode})")

    print()
    print("=" * 60)
    print("RAW DATA PREP: Step 2 - Recompile all_trank_team_tables")
    print("=" * 60)
    ret = subprocess.run(
        [sys.executable, str(scripts_dir / "recompile_all_trank_team_tables.py")],
        cwd=str(project_root),
    )
    if ret.returncode != 0:
        raise SystemExit(f"recompile_all_trank_team_tables.py failed (exit {ret.returncode})")
    print()


if __name__ == "__main__":
    args = parse_args()

    if args.holdout is not None:
        from scripts.run_analysis import main as run_holdout_analysis

        raise SystemExit(run_holdout_analysis(
            ["--holdout-year", str(args.holdout), "--report-prefix", "model_optimization"]
        ))

    if args.prep_raw:
        run_raw_data_prep()

    alpha_grid = None
    if args.alpha_grid:
        try:
            alpha_grid = [float(x.strip()) for x in args.alpha_grid.split(",") if x.strip() != ""]
        except ValueError as exc:
            raise SystemExit(f"Invalid --alpha-grid value: {args.alpha_grid!r}") from exc

    year_start = year_end = None
    if args.year is not None:
        year_start = year_end = args.year

    run_pipeline(
        bracket_csv=args.bracket,
        n_sims=args.n_sims,
        k_bradley_terry=args.k,
        ridge_alpha=args.ridge_alpha,
        optimize_ridge_alpha=False if args.no_alpha_tune else None,
        ridge_alpha_grid=alpha_grid,
        data_only=args.data_only,
        year_start=year_start,
        year_end=year_end,
        refresh_team_history=args.refresh_team_history,
        use_trank_only=args.trank_only,
    )
