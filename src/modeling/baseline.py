"""Seed-only baseline: round given seed, estimated from the training seasons."""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd


def fit_seed_baseline(train_df: pd.DataFrame, target: str = "round", stat: str = "mean") -> dict[str, Any]:
    """
    Round given seed, estimated from the training seasons only.

    stat="mean" is the best seed-only predictor under squared error (RMSE); stat="median"
    is the best under absolute error (MAE). round is skewed and mostly 0 for seeds 9-16,
    so a mean baseline is a weak opponent on MAE and overstates a least-squares model's
    MAE gain.
    """
    if stat not in {"mean", "median"}:
        raise ValueError(f"seed baseline stat must be 'mean' or 'median' (got {stat!r})")
    seed = pd.to_numeric(train_df["seed"], errors="coerce")
    y = pd.to_numeric(train_df[target], errors="coerce")
    ok = seed.notna() & y.notna()
    if not ok.any():
        raise ValueError("seed baseline needs rows with both seed and target")
    return {
        "by_seed": y[ok].groupby(seed[ok]).agg(stat).to_dict(),
        "fallback": float(y[ok].agg(stat)),
    }


def predict_seed_baseline(baseline: dict[str, Any], df: pd.DataFrame) -> np.ndarray:
    seed = pd.to_numeric(df["seed"], errors="coerce")
    return seed.map(baseline["by_seed"]).fillna(baseline["fallback"]).to_numpy(dtype=float)
