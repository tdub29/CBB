"""Seed-stratified stack: out-of-fold predround and the 14 seed-band ridge models built on it."""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd
from sklearn.pipeline import Pipeline

from src.config import DEFAULT_CONFIG
from src.evaluation.leakage import assert_no_target_leakage
from src.modeling.design import (
    GEN_FEATURES,
    GEN_POLY,
    ROUND_MAX,
    ROUND_MIN,
    SEED_MODELS,
)
from src.modeling.ridge import season_cv_predict, train_general_model, tune_ridge_alpha
from src.models.win_probability import compute_strength

PREDROUND_OOF_COL = "predround_oof"


def oof_predround(
    training_data: pd.DataFrame,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    n_splits: int | None = None,
) -> pd.Series:
    """
    Out-of-fold general-model predictions, grouped by season.

    Each row's value comes from a general model fit without that row's season. Seed
    models used to train on the in-sample fit instead, so they learned to trust a
    predround that had already seen the answer, and their LOO scores inherited it.
    """
    _, _, preds, _ = season_cv_predict(
        training_data,
        GEN_FEATURES,
        poly=GEN_POLY,
        ridge_alpha=ridge_alpha,
        n_splits=n_splits,
    )
    if len(preds) != len(training_data) or np.isnan(preds).any():
        raise ValueError("out-of-fold predround needs at least 2 seasons and 3 rows")
    return pd.Series(preds, index=training_data.index, name=PREDROUND_OOF_COL)


def _ensure_derived_features(df: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """Add any required derived feature columns if needed."""
    df = df.copy()
    return df


def train_seed_models(
    training_data: pd.DataFrame,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    optimize_ridge_alpha: bool = DEFAULT_CONFIG["model"]["optimize_ridge_alpha"],
    ridge_alpha_grid: list[float] | None = None,
) -> tuple[dict[int, tuple[Pipeline, list[str]]], dict[int, dict[str, Any]]]:
    """
    Train all 14 seed-stratified models. Returns {model_id: (model, features)}.

    The predround feature is read from PREDROUND_OOF_COL (see oof_predround), never from
    an in-sample general-model fit; a frame without it is refused.

    season_cv_mse in the returned meta is for alpha selection, not an accuracy claim. The
    OOF column is built once over every season, so when season s is held out here, the
    training rows' predround came from general models that saw s. Walk-forward scoring
    (fit_predict_stack inside each window) rebuilds the column and does not have this.
    """
    if PREDROUND_OOF_COL not in training_data.columns:
        raise ValueError(
            f"train_seed_models needs out-of-fold predround in '{PREDROUND_OOF_COL}' "
            "(see oof_predround); the in-sample general-model fit leaks the target."
        )
    data = training_data.copy()
    data["predround"] = pd.to_numeric(data[PREDROUND_OOF_COL], errors="coerce")

    seed_models = {}
    seed_meta: dict[int, dict[str, Any]] = {}
    grid = ridge_alpha_grid or list(DEFAULT_CONFIG["model"]["ridge_alpha_grid"])
    for i, (hist_seeds, _, features) in enumerate(SEED_MODELS, 1):
        sub = data[data["seed"].isin(hist_seeds)].copy()
        sub = _ensure_derived_features(sub, features)
        if len(sub) < 3:
            continue
        avail = [f for f in features if f in sub.columns]
        if "predround" not in avail and "predround" in sub.columns:
            avail.append("predround")
        if not avail:
            continue
        assert_no_target_leakage(sub, avail, "round", label=f"seed_model_{i}")

        alpha_used = float(ridge_alpha)
        tuning: dict[str, Any] = {"tuned": False, "alpha_selected": None, "cv": None, "results": []}
        if optimize_ridge_alpha:
            try:
                tuning = tune_ridge_alpha(sub, avail, poly=None, alpha_grid=grid)
                if tuning.get("tuned") and tuning.get("alpha_selected") is not None:
                    alpha_used = float(tuning["alpha_selected"])
            except Exception as exc:
                tuning = {"tuned": False, "reason": str(exc), "alpha_selected": None, "cv": None, "results": []}

        model, cv_mse, _, _ = season_cv_predict(sub, avail, poly=None, ridge_alpha=alpha_used)
        seed_models[i] = (model, avail)
        seed_meta[i] = {
            "model_id": i,
            "hist_seeds": list(hist_seeds),
            "holdout_seeds": list(SEED_MODELS[i - 1][1]),
            "n_train": int(len(sub)),
            "ridge_alpha": alpha_used,
            "season_cv_mse": float(cv_mse),
            "tuning": tuning,
            "features": list(avail),
        }
    return seed_models, seed_meta


def _predict_with_seed_models(
    df: pd.DataFrame,
    seed_models: dict[int, tuple[Pipeline, list[str]]],
) -> pd.DataFrame:
    """Apply seed models to overwrite general predictions for configured seed groups."""
    out = df.copy()
    out["seed"] = pd.to_numeric(out.get("seed", np.nan), errors="coerce")
    for idx, (_, holdout_seeds, _) in enumerate(SEED_MODELS, 1):
        if idx not in seed_models:
            continue
        model, features = seed_models[idx]
        mask = out["seed"].isin(holdout_seeds)
        if not mask.any():
            continue
        sub = _ensure_derived_features(out.loc[mask].copy(), features)
        X_sub = sub.reindex(columns=features, fill_value=0).copy()
        for col in X_sub.columns:
            X_sub[col] = pd.to_numeric(X_sub[col], errors="coerce")
        X_sub = X_sub.fillna(0.0)
        out.loc[mask, "predround"] = model.predict(X_sub)
    out["predround"] = pd.to_numeric(out["predround"], errors="coerce").fillna(0.0).clip(ROUND_MIN, ROUND_MAX)
    return out


def fit_predict_stack(
    train_df: pd.DataFrame,
    test_df: pd.DataFrame,
    *,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    optimize_ridge_alpha: bool = DEFAULT_CONFIG["model"]["optimize_ridge_alpha"],
    ridge_alpha_grid: list[float] | None = None,
) -> np.ndarray:
    """
    Fit general + seed-stratified models on train_df alone, as run_pipeline does, and
    predict test_df.

    Training rows get out-of-fold predround (grouped by season, inside train_df); test
    rows get predround from the general model fit on all of train_df.
    """
    gen_model, gen_feats, gen_meta = train_general_model(
        train_df,
        ridge_alpha=ridge_alpha,
        optimize_ridge_alpha=optimize_ridge_alpha,
        ridge_alpha_grid=ridge_alpha_grid,
    )
    train = train_df.copy()
    train[PREDROUND_OOF_COL] = oof_predround(train, ridge_alpha=gen_meta["ridge_alpha"])
    seed_models, _ = train_seed_models(
        train,
        ridge_alpha=ridge_alpha,
        optimize_ridge_alpha=optimize_ridge_alpha,
        ridge_alpha_grid=ridge_alpha_grid,
    )
    test = test_df.copy()
    test["predround"] = compute_strength(test, gen_model, gen_feats)
    return _predict_with_seed_models(test, seed_models)["predround"].to_numpy(dtype=float)
