"""General model: scaled ridge regression, with alpha and out-of-fold MSE from season-grouped CV."""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression, Ridge
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

from src.config import DEFAULT_CONFIG
from src.evaluation.leakage import assert_no_target_leakage
from src.evaluation.splits import season_cv_label, season_group_splits, season_years
from src.modeling.design import (
    GEN_FEATURES,
    GEN_POLY,
    _add_poly_features,
    _get_Xy,
    _resolve_features,
)
from src.models.win_probability import compute_strength


def season_cv_predict(
    df: pd.DataFrame,
    features: list[str],
    poly: dict | None = None,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    n_splits: int | None = None,
) -> tuple[Pipeline, float, np.ndarray, np.ndarray]:
    """
    Season-grouped out-of-fold predictions (leave-one-season-out by default).

    Returns (model fit on all rows, out-of-fold MSE, out-of-fold predictions in row
    order, actuals). Replaces the old row-level jackknife, which mixed teams from one
    bracket across train and test.
    """
    model_all = _build_regression_model(ridge_alpha)
    X, y = _get_Xy(df, features, poly=poly)
    if X.empty or len(y) == 0:
        return model_all, np.inf, np.array([]), np.array([])
    years = season_years(df)
    if len(X) < 3 or len(np.unique(years)) < 2:
        # Too few rows or seasons to hold anything out; still return a fitted model.
        model_all.fit(X, y)
        return model_all, np.inf, np.array([]), np.array([])

    preds = np.full(len(X), np.nan)
    for train_idx, test_idx in season_group_splits(years, n_splits):
        model = _build_regression_model(ridge_alpha)
        model.fit(X.iloc[train_idx], y.iloc[train_idx])
        preds[test_idx] = model.predict(X.iloc[test_idx])

    actuals = y.to_numpy(dtype=float)
    mse_total = float(mean_squared_error(actuals, preds))
    model_all.fit(X, y)
    return model_all, mse_total, preds, actuals


def _build_regression_model(ridge_alpha: float) -> Pipeline:
    """Build a scaled linear model (Ridge when alpha>0, else OLS)."""
    # fit_intercept must stay True. madness.R fits `lm(... + 0)` on raw columns, where the
    # level can still ride on the uncentred features; after StandardScaler centres them,
    # no intercept forces every prediction to average 0, which is what the port did
    # (measured in docs/modeling_plan.md, "Intercept fix").
    if ridge_alpha > 0:
        estimator = Ridge(alpha=ridge_alpha, fit_intercept=True, random_state=42)
    else:
        estimator = LinearRegression(fit_intercept=True)
    return Pipeline([("scaler", StandardScaler()), ("model", estimator)])


def tune_ridge_alpha(
    df: pd.DataFrame,
    features: list[str],
    poly: dict | None,
    alpha_grid: list[float],
    n_splits: int | None = None,
) -> dict[str, Any]:
    """
    Pick ridge alpha by season-grouped CV MSE on a grid.

    Folds keep each season whole: leave-one-season-out by default, GroupKFold(n_splits)
    on year otherwise. Returns a dict with alpha_selected and a compact CV summary.
    """
    X, y = _get_Xy(df, features, poly=poly)
    n = len(X)
    years = season_years(df)
    n_seasons = len(np.unique(years))
    if n < 3 or n_seasons < 2:
        return {
            "tuned": False,
            "reason": f"insufficient_rows({n})_or_seasons({n_seasons})",
            "alpha_selected": None,
            "cv": None,
            "results": [],
        }

    splits = season_group_splits(years, n_splits)
    results: list[dict[str, float]] = []
    for alpha in alpha_grid:
        model = _build_regression_model(float(alpha))
        scores = cross_val_score(
            model,
            X,
            y,
            cv=splits,
            scoring="neg_mean_squared_error",
            n_jobs=1,
        )
        mse_scores = (-scores).astype(float)
        results.append(
            {
                "alpha": float(alpha),
                "mean_mse": float(np.mean(mse_scores)),
                "std_mse": float(np.std(mse_scores)),
            }
        )

    best = min(results, key=lambda r: r["mean_mse"])
    # Keep the report small: only return the full grid if it's short.
    max_keep = 15
    kept = results if len(results) <= max_keep else sorted(results, key=lambda r: r["mean_mse"])[:max_keep]
    return {
        "tuned": True,
        "alpha_selected": float(best["alpha"]),
        "cv": season_cv_label(years, n_splits),
        "results": kept,
    }


def train_general_model(
    training_data: pd.DataFrame,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    optimize_ridge_alpha: bool = DEFAULT_CONFIG["model"]["optimize_ridge_alpha"],
    ridge_alpha_grid: list[float] | None = None,
    features: list[str] | None = None,
) -> tuple[Pipeline, list[str], dict]:
    """
    Train the general model; alpha and the reported MSE both come from season-grouped CV.

    features defaults to GEN_FEATURES. Passing another list (with GEN_POLY still applied
    to whichever of its terms are present) is for ablations in scripts/run_analysis.py.
    """
    features = list(GEN_FEATURES if features is None else features)
    assert_no_target_leakage(training_data, features, "round", label="general_model")
    alpha_used = float(ridge_alpha)
    tuning: dict[str, Any] = {"tuned": False, "alpha_selected": None, "cv": None, "results": []}
    grid = ridge_alpha_grid or list(DEFAULT_CONFIG["model"]["ridge_alpha_grid"])
    if optimize_ridge_alpha:
        try:
            tuning = tune_ridge_alpha(training_data, features, poly=GEN_POLY, alpha_grid=grid)
            if tuning.get("tuned") and tuning.get("alpha_selected") is not None:
                alpha_used = float(tuning["alpha_selected"])
        except Exception as exc:
            tuning = {"tuned": False, "reason": str(exc), "alpha_selected": None, "cv": None, "results": []}

    model, mse, _, _ = season_cv_predict(
        training_data,
        features,
        poly=GEN_POLY,
        ridge_alpha=alpha_used,
    )
    df = _add_poly_features(training_data.copy(), GEN_POLY)
    all_feats = _resolve_features(features, GEN_POLY, df)
    return model, all_feats, {"mse": mse, "ridge_alpha": alpha_used, "tuning": tuning}


def fit_predict_general(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
    *,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    optimize_ridge_alpha: bool = DEFAULT_CONFIG["model"]["optimize_ridge_alpha"],
    ridge_alpha_grid: list[float] | None = None,
    features: list[str] | None = None,
) -> np.ndarray:
    """Fit the general model on train_df alone (alpha tuned inside it) and predict test_df."""
    gen_model, gen_feats, _ = train_general_model(
        train_df,
        ridge_alpha=ridge_alpha,
        optimize_ridge_alpha=optimize_ridge_alpha,
        ridge_alpha_grid=ridge_alpha_grid,
        features=features,
    )
    return compute_strength(test_df, gen_model, gen_feats).to_numpy(dtype=float)
