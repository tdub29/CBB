#!/usr/bin/env python3
"""Evaluate round probability predictions on holdout data."""

from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.metrics import mean_squared_error


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Evaluate March Madness prediction outputs.")
    parser.add_argument("--predictions", required=True, help="Path to predictions CSV.")
    parser.add_argument(
        "--actuals",
        default="",
        help="Optional path to actual outcomes CSV. If omitted, actuals are read from predictions file.",
    )
    parser.add_argument("--team-col", default="Team", help="Team key column name.")
    parser.add_argument("--year-col", default="year", help="Year key column name.")
    parser.add_argument("--actual-col", default="round", help="Actual round column name (0..6).")
    return parser.parse_args()


def _load_eval_frame(args: argparse.Namespace) -> pd.DataFrame:
    preds = pd.read_csv(args.predictions)
    if "expected_round" not in preds.columns:
        raise ValueError("Predictions file must include expected_round column.")

    if args.actuals:
        actuals = pd.read_csv(args.actuals)
        keys = [args.team_col]
        if args.year_col in preds.columns and args.year_col in actuals.columns:
            keys.append(args.year_col)
        cols = keys + [args.actual_col]
        missing = [c for c in cols if c not in actuals.columns]
        if missing:
            raise ValueError(f"Actuals file missing required columns: {', '.join(missing)}")
        merged = preds.merge(actuals[cols], on=keys, how="inner")
        return merged

    if args.actual_col not in preds.columns:
        raise ValueError(
            f"Actual column '{args.actual_col}' not found in predictions. "
            "Provide --actuals to evaluate."
        )
    return preds


def _brier_score(prob: pd.Series, truth: pd.Series) -> float:
    return float(np.mean((prob - truth) ** 2))


def main() -> int:
    args = parse_args()
    start = time.time()
    result: dict[str, object] = {
        "status": "success",
        "start_time_epoch": start,
        "predictions_path": str(Path(args.predictions)),
    }

    try:
        eval_df = _load_eval_frame(args)
        eval_df["actual_round"] = pd.to_numeric(eval_df[args.actual_col], errors="coerce")
        eval_df["expected_round"] = pd.to_numeric(eval_df["expected_round"], errors="coerce")
        eval_df = eval_df.dropna(subset=["actual_round", "expected_round"])
        if eval_df.empty:
            raise ValueError("No valid rows after joining predictions and actuals.")

        rmse = mean_squared_error(eval_df["actual_round"], eval_df["expected_round"], squared=False)
        metrics: dict[str, float] = {"rmse_expected_round": float(rmse)}

        round_targets = {
            "p_R32": 1,
            "p_S16": 2,
            "p_E8": 3,
            "p_F4": 4,
            "p_Finals": 5,
            "p_Champ": 6,
        }
        for prob_col, threshold in round_targets.items():
            if prob_col not in eval_df.columns:
                continue
            probs = pd.to_numeric(eval_df[prob_col], errors="coerce").fillna(0.0).clip(0.0, 1.0)
            truth = (eval_df["actual_round"] >= threshold).astype(float)
            metrics[f"brier_{prob_col}"] = _brier_score(probs, truth)

        result["rows_evaluated"] = int(len(eval_df))
        result["metrics"] = metrics
    except Exception as exc:
        result["status"] = "error"
        result["error"] = str(exc)

    end = time.time()
    result["end_time_epoch"] = end
    result["elapsed_seconds"] = round(end - start, 3)
    print(json.dumps(result, indent=2))
    return 0 if result["status"] == "success" else 1


if __name__ == "__main__":
    raise SystemExit(main())
