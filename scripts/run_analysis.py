#!/usr/bin/env python3
"""Season-grouped, walk-forward predictive analysis for NCAA tournament finish modeling.

Every model is scored against a seed-only baseline on the same folds. A season is one
bracket, so no split ever puts teams from the same tournament on both sides.
"""

from __future__ import annotations

import argparse
import math
import os
import sys
import warnings
from dataclasses import dataclass
from datetime import datetime
from functools import partial
from pathlib import Path
from typing import Any, Callable

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import yaml
from sklearn.base import clone
from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.inspection import permutation_importance
from sklearn.linear_model import ElasticNet, Lasso, Ridge
from sklearn.metrics import mean_squared_error
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.models.win_probability import compute_strength  # noqa: E402
from src.pipelines.madness import (  # noqa: E402
    DEFAULT_CONFIG,
    GEN_FEATURES,
    GEN_POLY,
    PREDROUND_OOF_COL,
    ROUND_MAX,
    ROUND_MIN,
    WALK_FORWARD_FIRST_TEST_YEAR,
    _deep_merge,
    _validate_runtime_config,
    assert_no_target_leakage,
    fit_predict_general,
    fit_predict_stack,
    fit_seed_baseline,
    oof_predround,
    predict_seed_baseline,
    season_cv_label,
    season_group_splits,
    season_years,
    tournament_model_frame,
    train_general_model,
    train_seed_models,
    walk_forward_splits,
)
from src.visualization.palette import (  # noqa: E402
    ALGORITHM_COLORS,
    CBB_DIVERGING_CMAP,
    CBB_SEQUENTIAL_CMAP,
    PROJECT_PALETTE,
    apply_report_background,
    set_project_theme,
)


DEFAULT_INPUT = "data/processed/cbb_training_clean.csv"
DEFAULT_REPORT_DIR = "reports"
DEFAULT_FIGURES_DIR = "reports/figures/predictive"
DEFAULT_MODEL_DIR = "models"
DEFAULT_HOLDOUT_YEAR = 2025
DEFAULT_CV_FOLDS = 10
DEFAULT_RANDOM_STATE = 42
DEFAULT_SEARCH_DEPTH = "balanced"
DEFAULT_BOOTSTRAP_DRAWS = 10000

RIDGE_ALPHA_GRID = [0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]

# PAKE and PASE are tournament wins MINUS expectation, so they are computed from the
# result this script predicts. round = E[wins|seed] + PASE reconstructs the target exactly
# on 100% of rows. They were filed under "Schedule" in docs/modeling_plan.md:37 and so
# never reached OUTCOME_COLUMNS, which is how they got into the feature sets.
LEAKY_COLUMNS = {"PAKE", "PASE"}
UNUSED_CANDIDATES = ["ELITESOS", "FT%", "OPFT%", "PPPOFF", "PPPDEF"]
ENGINEERED_FEATURES = ["eff_ratio", "seed_barthag", "coach_seed"]
TARGET_COL = "round"
OUTCOME_COLUMNS = {"round", "Round2", "S16", "E8", "F4", "Ship", "Champ", "result", "predround", PREDROUND_OOF_COL}
# anything derived from the outcome is excluded from every feature set, not just the target
EXCLUDE_FROM_FEATURES = OUTCOME_COLUMNS | LEAKY_COLUMNS

SEED_GROUPS = [(1, 4), (5, 8), (9, 12), (13, 16)]
BASELINE_NAME = "seed_baseline"
GENERAL_NAME = "general_ridge"
STACK_NAME = "seed_stratified_stack"

# A recipe fits on a training frame and predicts a test frame; nothing else is shared.
Recipe = Callable[[pd.DataFrame, pd.DataFrame], np.ndarray]

# For rank transforms, lower value indicates stronger performance for these columns.
LOWER_IS_BETTER = {
    "seed",
    "AdjDE",
    "EFGD%",
    "FTRD",
    "2P%D",
    "3P%D",
    "BLKED%",
    "OPAST%",
    "3PRATED",
    "OPFT%",
    "PPPDEF",
    "TOR",
    "TORD",
}


@dataclass
class FeatureSpec:
    name: str
    features: list[str]
    poly: dict[str, int] | None


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run season-grouped predictive analysis on cbb_training_clean.csv.")
    parser.add_argument("--input-csv", default=DEFAULT_INPUT, help="Path to cleaned training CSV.")
    parser.add_argument("--report-dir", default=DEFAULT_REPORT_DIR, help="Directory for markdown reports.")
    parser.add_argument("--figures-dir", default=DEFAULT_FIGURES_DIR, help="Directory for PNG outputs.")
    parser.add_argument("--holdout-year", type=int, default=DEFAULT_HOLDOUT_YEAR, help="Final blind holdout year.")
    parser.add_argument(
        "--cv-folds",
        type=int,
        default=DEFAULT_CV_FOLDS,
        help="Season-grouped CV folds (GroupKFold on year); >= number of seasons means leave-one-season-out.",
    )
    parser.add_argument(
        "--walk-forward-start",
        type=int,
        default=WALK_FORWARD_FIRST_TEST_YEAR,
        help="First walk-forward test season; each test season Y trains on every season before Y.",
    )
    parser.add_argument("--random-state", type=int, default=DEFAULT_RANDOM_STATE, help="Random seed.")
    parser.add_argument(
        "--search-depth",
        choices=["fast", "balanced", "exhaustive"],
        default=DEFAULT_SEARCH_DEPTH,
        help="Hyperparameter search depth.",
    )
    parser.add_argument(
        "--run-tag",
        default=datetime.now().strftime("%Y%m%d"),
        help="Tag for the report and model artifact file names (default: today as YYYYMMDD).",
    )
    return parser.parse_args()


def ensure_dirs(*paths: Path) -> None:
    for path in paths:
        path.mkdir(parents=True, exist_ok=True)


def unique_keep_order(items: list[str]) -> list[str]:
    return list(dict.fromkeys(items))


def add_poly_features(df: pd.DataFrame, poly: dict[str, int] | None) -> pd.DataFrame:
    out = df.copy()
    if not poly:
        return out
    for feat, degree in poly.items():
        if feat not in out.columns:
            continue
        base = pd.to_numeric(out[feat], errors="coerce")
        out[f"{feat}_2"] = base**2
        if degree >= 3:
            out[f"{feat}_3"] = base**3
    return out


def add_engineered_features(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    adj_oe = pd.to_numeric(out.get("AdjOE", np.nan), errors="coerce")
    adj_de = pd.to_numeric(out.get("AdjDE", np.nan), errors="coerce")
    seed = pd.to_numeric(out.get("seed", np.nan), errors="coerce")
    barthag = pd.to_numeric(out.get("Barthag", np.nan), errors="coerce")
    coach_pct = pd.to_numeric(out.get("coach_tourn_win_pct_5y", np.nan), errors="coerce")

    denom = adj_de.replace(0, np.nan)
    out["eff_ratio"] = adj_oe / denom
    out["seed_barthag"] = seed * barthag
    out["coach_seed"] = coach_pct * seed
    return out


def choose_rank_base_columns(df: pd.DataFrame) -> list[str]:
    candidates = unique_keep_order(
        list(GEN_FEATURES)
        + UNUSED_CANDIDATES
        + [
            "AdjOE",
            "AdjDE",
            "Barthag",
            "EFGD%",
            "FTR",
            "FTRD",
            "TOR",
            "TORD",
            "ORB",
            "DRB",
            "2P%",
            "2P%D",
            "3P%",
            "3P%D",
            "BLK%",
            "BLKED%",
            "AST%",
            "OPAST%",
            "3PRATE",
            "3PRATED",
            "PPPOFF",
            "PPPDEF",
            "seed",
            "coach_tourn_win_pct_5y",
        ]
    )
    return [c for c in candidates if c in df.columns]


def add_rank_features(df: pd.DataFrame, base_cols: list[str]) -> tuple[pd.DataFrame, list[str]]:
    out = df.copy()
    rank_cols: list[str] = []
    for col in base_cols:
        series = pd.to_numeric(out[col], errors="coerce")
        ascending = col in LOWER_IS_BETTER
        rank_col = f"{col}_rank"
        out[rank_col] = (
            out.assign(_v=series)
            .groupby("year", dropna=False)["_v"]
            .rank(method="dense", ascending=ascending)
            .astype(float)
        )
        rank_cols.append(rank_col)
    return out, rank_cols


def build_feature_specs(df: pd.DataFrame, rank_cols: list[str]) -> list[FeatureSpec]:
    baseline = [c for c in GEN_FEATURES if c in df.columns]
    baseline_poly = {k: v for k, v in GEN_POLY.items() if k in df.columns}

    drop_barthag = [c for c in baseline if c != "Barthag"]
    drop_barthag_poly = {k: v for k, v in baseline_poly.items() if k != "Barthag"}

    engineered = unique_keep_order(baseline + [c for c in ENGINEERED_FEATURES if c in df.columns])
    plus_unused = unique_keep_order(baseline + [c for c in UNUSED_CANDIDATES if c in df.columns])

    rank_lookup = {r.replace("_rank", ""): r for r in rank_cols}
    rank_baseline = [rank_lookup[c] for c in baseline if c in rank_lookup]
    rank_unused = [rank_lookup[c] for c in UNUSED_CANDIDATES if c in rank_lookup]
    rank_only = unique_keep_order(rank_baseline + rank_unused)
    raw_plus_rank = unique_keep_order(baseline + rank_only)

    exclude = EXCLUDE_FROM_FEATURES | {"year"}
    numeric_cols = [c for c in df.select_dtypes(include=[np.number]).columns if c not in exclude]
    kitchen_sink = unique_keep_order(numeric_cols)

    return [
        FeatureSpec(name="baseline_raw_poly", features=baseline, poly=baseline_poly),
        FeatureSpec(name="drop_barthag_keep_adjoe_adjde", features=drop_barthag, poly=drop_barthag_poly),
        FeatureSpec(name="engineered_interactions", features=engineered, poly=baseline_poly),
        FeatureSpec(name="baseline_plus_unused", features=plus_unused, poly=baseline_poly),
        FeatureSpec(name="rank_only", features=rank_only, poly=None),
        FeatureSpec(name="raw_plus_rank", features=raw_plus_rank, poly=baseline_poly),
        FeatureSpec(name="kitchen_sink_numeric_no_leakage", features=kitchen_sink, poly=None),
    ]


def prepare_design(
    df: pd.DataFrame,
    spec: FeatureSpec,
    target: str = TARGET_COL,
    fixed_features: list[str] | None = None,
) -> tuple[pd.DataFrame, pd.Series, list[str]]:
    work = add_poly_features(df, spec.poly)
    if fixed_features is None:
        features = list(spec.features)
        if spec.poly:
            for base, degree in spec.poly.items():
                if degree >= 2:
                    features.append(f"{base}_2")
                if degree >= 3:
                    features.append(f"{base}_3")
        features = [c for c in unique_keep_order(features) if c in work.columns]
    else:
        features = list(fixed_features)
        for col in features:
            if col not in work.columns:
                work[col] = np.nan

    if not features:
        raise ValueError(f"No usable features for spec '{spec.name}'.")

    X = work[features].apply(pd.to_numeric, errors="coerce")
    y = pd.to_numeric(work[target], errors="coerce")
    valid = y.notna()
    X = X.loc[valid]
    y = y.loc[valid]

    if fixed_features is None:
        keep = [c for c in X.columns if X[c].notna().any()]
        X = X[keep]
        features = keep

    return X, y, features


def build_estimator(algorithm: str, params: dict[str, Any], random_state: int) -> Pipeline:
    if algorithm == "ridge":
        # fit_intercept=False after StandardScaler forced every prediction to average 0
        # instead of the mean round (0.98); see _build_regression_model in madness.py.
        model = Ridge(alpha=float(params["alpha"]), random_state=random_state, fit_intercept=True)
        return Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                ("model", model),
            ]
        )
    if algorithm == "lasso":
        model = Lasso(alpha=float(params["alpha"]), random_state=random_state, max_iter=50000)
        return Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                ("model", model),
            ]
        )
    if algorithm == "elasticnet":
        model = ElasticNet(
            alpha=float(params["alpha"]),
            l1_ratio=float(params["l1_ratio"]),
            random_state=random_state,
            max_iter=50000,
        )
        return Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                ("model", model),
            ]
        )
    if algorithm == "gbr":
        model = GradientBoostingRegressor(
            n_estimators=int(params["n_estimators"]),
            max_depth=int(params["max_depth"]),
            learning_rate=float(params["learning_rate"]),
            random_state=random_state,
        )
        return Pipeline(steps=[("imputer", SimpleImputer(strategy="median")), ("model", model)])
    if algorithm == "rf":
        model = RandomForestRegressor(
            n_estimators=int(params["n_estimators"]),
            max_depth=params["max_depth"],
            random_state=random_state,
            n_jobs=1,
        )
        return Pipeline(steps=[("imputer", SimpleImputer(strategy="median")), ("model", model)])
    raise ValueError(f"Unsupported algorithm: {algorithm}")


# -----------------------------------------------------------------------------
# Scoring on season-whole splits
# -----------------------------------------------------------------------------


def clip_rounds(pred: Any) -> np.ndarray:
    # round lives in [0, 6]. Every model is clipped the same way before scoring, so the
    # seed-stratified stack's built-in clip gives it no edge over the others.
    return np.clip(np.asarray(pred, dtype=float), ROUND_MIN, ROUND_MAX)


def score_predictions(actual: Any, pred: Any, years: Any) -> dict[str, float]:
    """MAE, RMSE, and mean within-season Spearman of predicted vs actual round."""
    actual = np.asarray(actual, dtype=float)
    pred = np.asarray(pred, dtype=float)
    years = np.asarray(years)
    err = pred - actual
    rhos: list[float] = []
    for year in np.unique(years):
        mask = years == year
        if mask.sum() < 3 or np.ptp(pred[mask]) == 0:
            # a constant prediction has no rank order; Spearman is undefined
            continue
        rho = pd.Series(pred[mask]).corr(pd.Series(actual[mask]), method="spearman")
        if pd.notna(rho):
            rhos.append(float(rho))
    mse = float(np.mean(err**2))
    return {
        "mae": float(np.mean(np.abs(err))),
        "rmse": float(math.sqrt(mse)),
        "mse": mse,
        "spearman_within_season": float(np.mean(rhos)) if rhos else float("nan"),
        "n_rows": int(len(actual)),
        "n_seasons": int(len(np.unique(years))),
    }


def run_grouped_cv(
    estimator: Pipeline,
    X: pd.DataFrame,
    y: pd.Series,
    years: np.ndarray,
    splits: list[tuple[np.ndarray, np.ndarray]],
) -> dict[str, Any]:
    """Out-of-fold scores for one estimator on season-whole splits."""
    pred = np.full(len(X), np.nan)
    fold_mse: list[float] = []
    for train_idx, test_idx in splits:
        model = clone(estimator)
        model.fit(X.iloc[train_idx], y.iloc[train_idx])
        fold_pred = clip_rounds(model.predict(X.iloc[test_idx]))
        pred[test_idx] = fold_pred
        fold_mse.append(float(mean_squared_error(y.iloc[test_idx], fold_pred)))

    scores = score_predictions(y.to_numpy(dtype=float), pred, years)
    return {
        "overall_mse": scores["mse"],
        "overall_mae": scores["mae"],
        "overall_rmse": scores["rmse"],
        "spearman": scores["spearman_within_season"],
        "fold_mse_std": float(np.std(fold_mse)),
        "n_splits": int(len(splits)),
        "pred": pred,
    }


def evaluate_recipe(
    recipe: Recipe,
    df: pd.DataFrame,
    splits: list[tuple[Any, np.ndarray, np.ndarray]],
) -> dict[str, Any]:
    """Fit a recipe on each split's training seasons only and score its test seasons."""
    actual = pd.to_numeric(df[TARGET_COL], errors="coerce").to_numpy(dtype=float)
    years = season_years(df)
    pred = np.full(len(df), np.nan)
    rows: list[dict[str, Any]] = []
    for label, train_idx, test_idx in splits:
        split_pred = clip_rounds(recipe(df.iloc[train_idx], df.iloc[test_idx]))
        if len(split_pred) != len(test_idx):
            raise ValueError(f"recipe returned {len(split_pred)} predictions for {len(test_idx)} rows")
        pred[test_idx] = split_pred
        rows.append({"split": label, **score_predictions(actual[test_idx], split_pred, years[test_idx])})
    covered = ~np.isnan(pred)
    overall = score_predictions(actual[covered], pred[covered], years[covered])
    return {**overall, "per_split": pd.DataFrame(rows), "pred": pred}


def seed_baseline_recipe(train_df: pd.DataFrame, test_df: pd.DataFrame) -> np.ndarray:
    """Expected round given seed, from the training seasons only."""
    return predict_seed_baseline(fit_seed_baseline(train_df, TARGET_COL), test_df)


def ridge_recipe(spec: FeatureSpec, features: list[str], cv_folds: int, random_state: int) -> Recipe:
    """Ridge on a fixed feature list; alpha is re-tuned inside every training window."""

    def fit_predict(train_df: pd.DataFrame, test_df: pd.DataFrame) -> np.ndarray:
        X_train, y_train, _ = prepare_design(train_df, spec, fixed_features=features)
        X_test, _, _ = prepare_design(test_df, spec, fixed_features=features)
        train_years = season_years(train_df.loc[X_train.index])
        alpha, _ = tune_ridge_alpha(X_train, y_train, train_years, RIDGE_ALPHA_GRID, cv_folds, random_state)
        est = build_estimator("ridge", {"alpha": alpha}, random_state=random_state)
        est.fit(X_train, y_train)
        return est.predict(X_test)

    return fit_predict


def estimator_recipe(
    algorithm: str,
    params: dict[str, Any],
    spec: FeatureSpec,
    features: list[str],
    random_state: int,
) -> Recipe:
    """A searched algorithm with its hyperparameters held fixed across windows."""

    def fit_predict(train_df: pd.DataFrame, test_df: pd.DataFrame) -> np.ndarray:
        X_train, y_train, _ = prepare_design(train_df, spec, fixed_features=features)
        X_test, _, _ = prepare_design(test_df, spec, fixed_features=features)
        est = build_estimator(algorithm, params, random_state=random_state)
        est.fit(X_train, y_train)
        return est.predict(X_test)

    return fit_predict


def paired_season_bootstrap(
    model_per_season: pd.DataFrame,
    base_per_season: pd.DataFrame,
    n_draws: int,
    random_state: int,
) -> dict[str, float]:
    """
    Model minus baseline on per-season MAE and pooled RMSE, with a 95% interval from
    resampling seasons (the independent unit) with replacement.
    """
    merged = model_per_season[["split", "mae", "mse"]].merge(
        base_per_season[["split", "mae", "mse"]], on="split", suffixes=("_model", "_base")
    )
    n = len(merged)
    d_mae = (merged["mae_model"] - merged["mae_base"]).to_numpy(dtype=float)
    mse_model = merged["mse_model"].to_numpy(dtype=float)
    mse_base = merged["mse_base"].to_numpy(dtype=float)
    rng = np.random.default_rng(random_state)
    draws = rng.integers(0, n, size=(n_draws, n))
    boot_mae = d_mae[draws].mean(axis=1)
    boot_rmse = np.sqrt(mse_model[draws].mean(axis=1)) - np.sqrt(mse_base[draws].mean(axis=1))
    return {
        "d_mae": float(d_mae.mean()),
        "d_mae_lo": float(np.percentile(boot_mae, 2.5)),
        "d_mae_hi": float(np.percentile(boot_mae, 97.5)),
        "d_rmse": float(np.sqrt(mse_model.mean()) - np.sqrt(mse_base.mean())),
        "d_rmse_lo": float(np.percentile(boot_rmse, 2.5)),
        "d_rmse_hi": float(np.percentile(boot_rmse, 97.5)),
        "seasons_better_mae": int((d_mae < 0).sum()),
        "n_seasons": int(n),
    }


def pct_better(model_value: float, base_value: float) -> float:
    """Percent improvement over the baseline (positive = model has lower error)."""
    return float(100.0 * (base_value - model_value) / base_value)


def load_pipeline_model_config() -> dict[str, Any]:
    """Model settings from configs/default.yaml, read the way run_pipeline reads them."""
    cfg = dict(DEFAULT_CONFIG)
    cfg_path = REPO_ROOT / "configs" / "default.yaml"
    if cfg_path.exists():
        cfg = _deep_merge(cfg, yaml.safe_load(cfg_path.read_text(encoding="utf-8")) or {})
    return _validate_runtime_config(cfg)


def compare_predround_sources(train_df: pd.DataFrame, pipeline_kwargs: dict[str, Any]) -> pd.DataFrame:
    """Seed-model season-CV MSE when predround is the in-sample fit (old) vs out-of-fold (new)."""
    gen_model, gen_feats, gen_meta = train_general_model(train_df, **pipeline_kwargs)
    in_sample = train_df.copy()
    # Deliberately the old, leaky input, only to measure what it did to the seed-model scores.
    in_sample[PREDROUND_OOF_COL] = compute_strength(in_sample, gen_model, gen_feats).to_numpy()
    _, meta_in = train_seed_models(in_sample, **pipeline_kwargs)
    out_of_fold = train_df.copy()
    out_of_fold[PREDROUND_OOF_COL] = oof_predround(out_of_fold, ridge_alpha=gen_meta["ridge_alpha"])
    _, meta_oof = train_seed_models(out_of_fold, **pipeline_kwargs)

    rows = []
    for model_id in sorted(set(meta_in) & set(meta_oof)):
        rows.append(
            {
                "model_id": model_id,
                "seeds": ",".join(map(str, meta_oof[model_id]["hist_seeds"])),
                "n_train": meta_oof[model_id]["n_train"],
                "season_cv_mse_in_sample_predround": meta_in[model_id]["season_cv_mse"],
                "season_cv_mse_oof_predround": meta_oof[model_id]["season_cv_mse"],
            }
        )
    out = pd.DataFrame(rows)
    out["optimism"] = out["season_cv_mse_oof_predround"] - out["season_cv_mse_in_sample_predround"]
    return out


def tune_ridge_alpha(
    X: pd.DataFrame,
    y: pd.Series,
    years: np.ndarray,
    alpha_grid: list[float],
    cv_folds: int,
    random_state: int,
) -> tuple[float, list[dict[str, float]]]:
    splits = season_group_splits(years, cv_folds)
    rows: list[dict[str, float]] = []
    for alpha in alpha_grid:
        est = build_estimator("ridge", {"alpha": float(alpha)}, random_state=random_state)
        cv = run_grouped_cv(estimator=est, X=X, y=y, years=years, splits=splits)
        rows.append(
            {
                "alpha": float(alpha),
                "cv_mse": float(cv["overall_mse"]),
                "cv_mae": float(cv["overall_mae"]),
                "cv_mse_std": float(cv["fold_mse_std"]),
            }
        )
    best = min(rows, key=lambda r: r["cv_mse"])
    return float(best["alpha"]), rows


def compute_vif(X: pd.DataFrame) -> pd.DataFrame:
    imp = SimpleImputer(strategy="median")
    data = imp.fit_transform(X)
    cols = list(X.columns)
    vif_rows: list[dict[str, float]] = []
    for i, col in enumerate(cols):
        y_col = data[:, i]
        X_others = np.delete(data, i, axis=1)
        if X_others.shape[1] == 0:
            vif = 1.0
        else:
            # Solve linear least squares and compute R^2 for VIF = 1/(1-R^2).
            beta, *_ = np.linalg.lstsq(X_others, y_col, rcond=None)
            y_hat = X_others @ beta
            ss_res = float(np.sum((y_col - y_hat) ** 2))
            ss_tot = float(np.sum((y_col - np.mean(y_col)) ** 2))
            if ss_tot <= 1e-12:
                vif = math.inf
            else:
                r2 = max(0.0, min(0.999999, 1.0 - ss_res / ss_tot))
                vif = 1.0 / (1.0 - r2)
        vif_rows.append({"feature": col, "vif": float(vif)})
    return pd.DataFrame(vif_rows).sort_values("vif", ascending=False).reset_index(drop=True)


def get_algorithm_configs(search_depth: str) -> list[dict[str, Any]]:
    if search_depth == "fast":
        ridge_alphas = [0.1, 1.0, 10.0]
        lasso_alphas = [0.1, 1.0, 10.0]
        elastic_alphas = [0.1, 1.0, 10.0]
        elastic_l1 = [0.5]
        gbt_ne = [100]
        gbt_depth = [3]
        gbt_lr = [0.1]
        rf_ne = [100]
        rf_depth = [5]
    elif search_depth == "exhaustive":
        ridge_alphas = [0.001, 0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 100.0]
        lasso_alphas = [0.001, 0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]
        elastic_alphas = [0.001, 0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]
        elastic_l1 = [0.1, 0.25, 0.5, 0.75, 0.9]
        gbt_ne = [100, 200, 300]
        gbt_depth = [2, 3, 5]
        gbt_lr = [0.03, 0.05, 0.1]
        rf_ne = [100, 200, 400]
        rf_depth = [5, 10, None]
    else:
        ridge_alphas = [0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]
        lasso_alphas = [0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]
        elastic_alphas = [0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]
        elastic_l1 = [0.25, 0.5, 0.75]
        gbt_ne = [100, 200]
        gbt_depth = [3, 5]
        gbt_lr = [0.05, 0.1]
        rf_ne = [100, 200]
        rf_depth = [5, 10, None]

    configs: list[dict[str, Any]] = []
    for alpha in ridge_alphas:
        configs.append({"algorithm": "ridge", "params": {"alpha": float(alpha)}})
    for alpha in lasso_alphas:
        configs.append({"algorithm": "lasso", "params": {"alpha": float(alpha)}})
    for alpha in elastic_alphas:
        for l1 in elastic_l1:
            configs.append({"algorithm": "elasticnet", "params": {"alpha": float(alpha), "l1_ratio": float(l1)}})
    for n_estimators in gbt_ne:
        for max_depth in gbt_depth:
            for learning_rate in gbt_lr:
                configs.append(
                    {
                        "algorithm": "gbr",
                        "params": {
                            "n_estimators": int(n_estimators),
                            "max_depth": int(max_depth),
                            "learning_rate": float(learning_rate),
                        },
                    }
                )
    for n_estimators in rf_ne:
        for max_depth in rf_depth:
            configs.append(
                {
                    "algorithm": "rf",
                    "params": {"n_estimators": int(n_estimators), "max_depth": max_depth},
                }
            )
    return configs


def format_params(params: dict[str, Any]) -> str:
    parts = [f"{k}={v}" for k, v in params.items()]
    return ", ".join(parts)


def markdown_table(df: pd.DataFrame, max_rows: int | None = None, float_digits: int = 4) -> str:
    if df.empty:
        return "_No rows._"
    view = df.head(max_rows) if max_rows is not None else df
    cols = list(view.columns)
    lines = []
    lines.append("| " + " | ".join(cols) + " |")
    lines.append("| " + " | ".join(["---"] * len(cols)) + " |")
    for _, row in view.iterrows():
        rendered: list[str] = []
        for col in cols:
            val = row[col]
            if isinstance(val, (float, np.floating)):
                if np.isfinite(val):
                    rendered.append(f"{float(val):.{float_digits}f}")
                elif np.isnan(val):
                    rendered.append("")
                else:
                    rendered.append("inf")
            else:
                rendered.append(str(val))
        lines.append("| " + " | ".join(rendered) + " |")
    return "\n".join(lines)


def report_axes(figsize: tuple[float, float]) -> tuple[Any, Any]:
    """Figure and axes with the project background behind the axes, not in place of them."""
    fig, ax = plt.subplots(figsize=figsize, constrained_layout=False)
    apply_report_background(fig, [ax])
    return fig, ax


def save_report_figure(fig: Any, path: Path) -> None:
    with warnings.catch_warnings():
        # The full-figure background axes sits outside the layout on purpose.
        warnings.filterwarnings("ignore", message=".*not compatible with tight_layout.*")
        fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def save_missingness_heatmap(df: pd.DataFrame, cols: list[str], path: Path) -> None:
    if not cols:
        return
    fig, ax = report_axes((14, 6))
    sns.heatmap(
        df[cols].isna().astype(int),
        cbar=False,
        cmap=CBB_SEQUENTIAL_CMAP,
        linewidths=0.1,
        linecolor=PROJECT_PALETTE["storm"],
        ax=ax,
    )
    ax.set_title("Missingness Heatmap (Model Candidate Features)")
    ax.set_xlabel("Feature")
    ax.set_ylabel("Row")
    save_report_figure(fig, path)


def save_correlation_matrix(df: pd.DataFrame, cols: list[str], path: Path) -> None:
    if not cols:
        return
    corr = df[cols].apply(pd.to_numeric, errors="coerce").corr()
    fig, ax = report_axes((14, 10))
    sns.heatmap(
        corr,
        cmap=CBB_DIVERGING_CMAP,
        center=0,
        annot=False,
        linewidths=0.15,
        linecolor=PROJECT_PALETTE["storm"],
        ax=ax,
    )
    ax.set_title("Feature Correlation Matrix")
    save_report_figure(fig, path)


def save_model_comparison(df: pd.DataFrame, path: Path) -> None:
    if df.empty:
        return
    view = df.sort_values("cv_mse").head(20).copy()
    view["label"] = view["algorithm"] + " | " + view["params"]
    fig, ax = report_axes((12, 8))
    sns.barplot(
        data=view,
        y="label",
        x="cv_mse",
        hue="algorithm",
        orient="h",
        palette=ALGORITHM_COLORS,
        ax=ax,
    )
    ax.set_title("Top Model Configurations by Season-Grouped CV MSE")
    ax.set_xlabel("CV MSE")
    ax.set_ylabel("Model")
    ax.legend(loc="best")
    save_report_figure(fig, path)


def save_walk_forward_plot(per_season: pd.DataFrame, path: Path) -> None:
    """Per-season walk-forward MAE, one line per model."""
    if per_season.empty:
        return
    colors = [
        PROJECT_PALETTE["steel"],
        PROJECT_PALETTE["lake_blue"],
        PROJECT_PALETTE["sage"],
        PROJECT_PALETTE["sky_blue"],
        PROJECT_PALETTE["fog"],
    ]
    fig, ax = report_axes((11, 6))
    seasons = sorted(per_season["season"].unique())
    positions = {season: i for i, season in enumerate(seasons)}
    for i, model in enumerate(per_season["model"].unique()):
        view = per_season[per_season["model"] == model].sort_values("season")
        ax.plot(
            view["season"].map(positions),
            view["mae"],
            marker="o",
            linewidth=2.2 if model == BASELINE_NAME else 1.6,
            linestyle="--" if model == BASELINE_NAME else "-",
            color=colors[i % len(colors)],
            label=model,
        )
    ax.set_xticks(range(len(seasons)), [str(s) for s in seasons])
    ax.set_title("Walk-Forward MAE by Test Season (train on all earlier seasons)")
    ax.set_xlabel("Test season")
    ax.set_ylabel("MAE (rounds)")
    ax.legend(loc="best")
    save_report_figure(fig, path)


def save_holdout_scatter(df: pd.DataFrame, path: Path, holdout_year: int, label: str) -> None:
    if df.empty:
        return
    fig, ax = report_axes((8, 6))
    sns.scatterplot(
        data=df,
        x="actual_round",
        y="pred_round",
        s=70,
        color=PROJECT_PALETTE["sky_blue"],
        edgecolor=PROJECT_PALETTE["black"],
        ax=ax,
    )
    min_v = min(df["actual_round"].min(), df["pred_round"].min())
    max_v = max(df["actual_round"].max(), df["pred_round"].max())
    ax.plot(
        [min_v, max_v],
        [min_v, max_v],
        linestyle="--",
        color=PROJECT_PALETTE["sage"],
        linewidth=1.8,
    )
    ax.set_title(f"Holdout {holdout_year}: Actual vs Predicted Round ({label})")
    ax.set_xlabel("Actual round")
    ax.set_ylabel("Predicted round")
    save_report_figure(fig, path)


def save_feature_importance_plot(df: pd.DataFrame, path: Path, title: str) -> None:
    if df.empty:
        return
    view = df.sort_values("importance", ascending=False).head(25).copy()
    fig, ax = report_axes((12, 8))
    sns.barplot(
        data=view,
        y="feature",
        x="importance",
        orient="h",
        color=PROJECT_PALETTE["lake_blue"],
        ax=ax,
    )
    ax.set_title(title)
    ax.set_xlabel("Importance")
    ax.set_ylabel("Feature")
    save_report_figure(fig, path)


def evaluate_feature_sets(
    train_df: pd.DataFrame,
    specs: list[FeatureSpec],
    cv_folds: int,
    cv_splits: list[tuple[np.ndarray, np.ndarray]],
    wf_splits: list[tuple[int, np.ndarray, np.ndarray]],
    random_state: int,
) -> tuple[pd.DataFrame, dict[str, dict[str, Any]]]:
    years = season_years(train_df)
    rows: list[dict[str, Any]] = []
    cache: dict[str, dict[str, Any]] = {}
    for spec in specs:
        print(f"[FeatureSearch] Evaluating {spec.name}", flush=True)
        X, y, features = prepare_design(train_df, spec)
        assert_no_target_leakage(train_df, features, TARGET_COL, label=spec.name)
        best_alpha, alpha_grid = tune_ridge_alpha(X, y, years, RIDGE_ALPHA_GRID, cv_folds, random_state)
        estimator = build_estimator("ridge", {"alpha": best_alpha}, random_state=random_state)
        cv = run_grouped_cv(estimator=estimator, X=X, y=y, years=years, splits=cv_splits)
        rows.append(
            {
                "feature_set": spec.name,
                "n_features": len(features),
                "best_alpha": best_alpha,
                "cv_mse": cv["overall_mse"],
                "cv_mae": cv["overall_mae"],
                "cv_rmse": cv["overall_rmse"],
                "cv_spearman": cv["spearman"],
                "cv_mse_std": cv["fold_mse_std"],
                "wf_mae": np.nan,
                "wf_rmse": np.nan,
                "wf_spearman": np.nan,
            }
        )
        cache[spec.name] = {
            "spec": spec,
            "X": X,
            "y": y,
            "features": features,
            "best_alpha": best_alpha,
            "alpha_grid": alpha_grid,
            "cv": cv,
        }

    result = pd.DataFrame(rows).sort_values("cv_mse").reset_index(drop=True)
    for name in result.head(3)["feature_set"].tolist():
        print(f"[FeatureSearch] Walk-forward for {name}", flush=True)
        row_idx = result.index[result["feature_set"] == name][0]
        cached = cache[name]
        recipe = ridge_recipe(cached["spec"], cached["features"], cv_folds, random_state)
        wf = evaluate_recipe(recipe, train_df, wf_splits)
        result.loc[row_idx, "wf_mae"] = wf["mae"]
        result.loc[row_idx, "wf_rmse"] = wf["rmse"]
        result.loc[row_idx, "wf_spearman"] = wf["spearman_within_season"]
    return result, cache


def evaluate_algorithms(
    train_df: pd.DataFrame,
    spec: FeatureSpec,
    fixed_features: list[str],
    cv_splits: list[tuple[np.ndarray, np.ndarray]],
    wf_splits: list[tuple[int, np.ndarray, np.ndarray]],
    random_state: int,
    search_depth: str,
) -> pd.DataFrame:
    X, y, _ = prepare_design(train_df, spec, fixed_features=fixed_features)
    years = season_years(train_df.loc[X.index])
    rows: list[dict[str, Any]] = []
    configs = get_algorithm_configs(search_depth)
    for i, cfg in enumerate(configs, start=1):
        print(
            f"[AlgorithmSearch] {i}/{len(configs)} {cfg['algorithm']} ({format_params(cfg['params'])})",
            flush=True,
        )
        est = build_estimator(cfg["algorithm"], cfg["params"], random_state=random_state)
        cv = run_grouped_cv(estimator=est, X=X, y=y, years=years, splits=cv_splits)
        rows.append(
            {
                "model_id": i,
                "algorithm": cfg["algorithm"],
                "params": format_params(cfg["params"]),
                "params_obj": cfg["params"],
                "cv_mse": cv["overall_mse"],
                "cv_mae": cv["overall_mae"],
                "cv_rmse": cv["overall_rmse"],
                "cv_spearman": cv["spearman"],
                "cv_mse_std": cv["fold_mse_std"],
                "wf_mae": np.nan,
                "wf_rmse": np.nan,
                "wf_spearman": np.nan,
            }
        )

    result = pd.DataFrame(rows).sort_values("cv_mse").reset_index(drop=True)
    for idx in result.head(3).index.tolist():
        row = result.loc[idx]
        print(f"[AlgorithmSearch] Walk-forward for {row['algorithm']} ({row['params']})", flush=True)
        recipe = estimator_recipe(row["algorithm"], row["params_obj"], spec, fixed_features, random_state)
        wf = evaluate_recipe(recipe, train_df, wf_splits)
        result.loc[idx, "wf_mae"] = wf["mae"]
        result.loc[idx, "wf_rmse"] = wf["rmse"]
        result.loc[idx, "wf_spearman"] = wf["spearman_within_season"]
    return result


def best_feature_set_row(df: pd.DataFrame) -> pd.Series:
    return df.sort_values(["cv_mse", "n_features"], ascending=[True, True]).iloc[0]


def best_model_row(df: pd.DataFrame) -> pd.Series:
    return df.sort_values(["cv_mse"], ascending=[True]).iloc[0]


def get_linear_coefficients(
    X: pd.DataFrame,
    y: pd.Series,
    alpha: float,
    random_state: int,
) -> pd.DataFrame:
    est = build_estimator("ridge", {"alpha": alpha}, random_state=random_state)
    est.fit(X, y)
    coefs = est.named_steps["model"].coef_
    out = pd.DataFrame({"feature": X.columns, "coef": coefs})
    out["abs_coef"] = out["coef"].abs()
    return out.sort_values("abs_coef", ascending=False).reset_index(drop=True)


def get_winner_importance(
    winner_algorithm: str,
    winner_params: dict[str, Any],
    X_train: pd.DataFrame,
    y_train: pd.Series,
    random_state: int,
) -> tuple[pd.DataFrame, Pipeline]:
    est = build_estimator(winner_algorithm, winner_params, random_state=random_state)
    est.fit(X_train, y_train)
    if winner_algorithm in {"ridge", "lasso", "elasticnet"}:
        coefs = np.abs(est.named_steps["model"].coef_)
        imp = pd.DataFrame({"feature": X_train.columns, "importance": coefs})
    else:
        perm = permutation_importance(
            est,
            X_train,
            y_train,
            n_repeats=20,
            random_state=random_state,
            scoring="neg_mean_squared_error",
            n_jobs=1,
        )
        imp = pd.DataFrame({"feature": X_train.columns, "importance": np.abs(perm.importances_mean)})
    imp = imp.sort_values("importance", ascending=False).reset_index(drop=True)
    return imp, est


def seed_group_label(seed: float) -> str:
    for lo, hi in SEED_GROUPS:
        if lo <= seed <= hi:
            return f"{lo}-{hi}"
    return "other"


def main() -> int:
    args = parse_args()
    run_tag = str(args.run_tag)

    report_dir = REPO_ROOT / args.report_dir
    figures_dir = REPO_ROOT / args.figures_dir
    models_dir = REPO_ROOT / DEFAULT_MODEL_DIR
    ensure_dirs(report_dir, figures_dir, models_dir)

    report_path = report_dir / f"predictive_analysis_{run_tag}.md"
    correlation_path = figures_dir / "correlation_matrix.png"
    feature_importance_path = figures_dir / "feature_importance.png"
    model_comparison_path = figures_dir / "model_comparison.png"
    walk_forward_path = figures_dir / "walk_forward_mae_by_season.png"
    holdout_scatter_path = figures_dir / f"holdout_{args.holdout_year}_actual_vs_pred.png"
    missingness_path = figures_dir / "missingness_heatmap.png"
    model_artifact_path = models_dir / f"analysis_best_model_{run_tag}.joblib"
    figures_rel = Path(os.path.relpath(figures_dir, report_dir)).as_posix()

    print("[Run] Starting predictive analysis", flush=True)
    set_project_theme()
    csv_path = REPO_ROOT / args.input_csv
    print(f"[Run] Loading data: {csv_path}", flush=True)
    try:
        df = pd.read_csv(csv_path)
    except pd.errors.ParserError:
        df = pd.read_csv(csv_path, engine="python")
    df = add_engineered_features(df)
    rank_base_cols = choose_rank_base_columns(df)
    df, rank_cols = add_rank_features(df, rank_base_cols)

    # Section 1: Data profile.
    print("[Run] Section 1: Data Profile", flush=True)
    year_min = int(pd.to_numeric(df["year"], errors="coerce").min())
    year_max = int(pd.to_numeric(df["year"], errors="coerce").max())
    tournament_mask = pd.to_numeric(df["seed"], errors="coerce").notna() & pd.to_numeric(df["round"], errors="coerce").notna()
    tournament_rows = int(tournament_mask.sum())
    non_tournament_rows = int((~tournament_mask).sum())

    candidate_profile_cols = unique_keep_order(
        [c for c in list(GEN_FEATURES) + UNUSED_CANDIDATES + ENGINEERED_FEATURES if c in df.columns]
    )
    save_missingness_heatmap(df, candidate_profile_cols, missingness_path)
    save_correlation_matrix(df, candidate_profile_cols, correlation_path)

    stat_cols = [c for c in ["AdjOE", "AdjDE", "Barthag", "seed", "round"] if c in df.columns]
    distribution_summary = (
        df[stat_cols]
        .apply(pd.to_numeric, errors="coerce")
        .describe()
        .T[["mean", "std", "min", "25%", "50%", "75%", "max"]]
        .reset_index()
        .rename(columns={"index": "feature"})
    )

    # Modeling scope: the same tournament rows run_pipeline trains on.
    model_df = tournament_model_frame(df, exclude_years=(2020,))
    model_years = season_years(model_df)
    pre_holdout_df = model_df[model_years < args.holdout_year].reset_index(drop=True)
    holdout_df = model_df[model_years == args.holdout_year].reset_index(drop=True)
    if pre_holdout_df.empty:
        raise ValueError("Pre-holdout modeling data is empty.")
    if holdout_df.empty:
        raise ValueError(f"Holdout year {args.holdout_year} has no eligible rows.")

    # Every comparison below runs on these exact splits.
    pre_years = season_years(pre_holdout_df)
    cv_splits = season_group_splits(pre_years, args.cv_folds)
    cv_label = season_cv_label(pre_years, args.cv_folds)
    cv_split_rows = [(i, tr, te) for i, (tr, te) in enumerate(cv_splits, start=1)]
    wf_splits = walk_forward_splits(pre_years, first_test_year=args.walk_forward_start)
    wf_years = [label for label, _, _ in wf_splits]
    holdout_frame = pd.concat([pre_holdout_df, holdout_df], ignore_index=True)
    holdout_splits = walk_forward_splits(season_years(holdout_frame), test_years=[args.holdout_year])
    for _, train_idx, test_idx in cv_split_rows + wf_splits:
        if set(pre_years[train_idx]) & set(pre_years[test_idx]):
            raise AssertionError("a season appears on both sides of a split")

    pipeline_cfg = load_pipeline_model_config()
    pipeline_kwargs = {
        "ridge_alpha": pipeline_cfg["ridge_alpha"],
        "optimize_ridge_alpha": pipeline_cfg["optimize_ridge_alpha"],
        "ridge_alpha_grid": pipeline_cfg["ridge_alpha_grid"],
    }

    # Section 2: Feature optimization.
    print("[Run] Section 2: Feature Optimization", flush=True)
    specs = build_feature_specs(pre_holdout_df, rank_cols=rank_cols)
    feature_results, feature_cache = evaluate_feature_sets(
        train_df=pre_holdout_df,
        specs=specs,
        cv_folds=args.cv_folds,
        cv_splits=cv_splits,
        wf_splits=wf_splits,
        random_state=args.random_state,
    )
    best_feature_row = best_feature_set_row(feature_results)
    best_feature_name = str(best_feature_row["feature_set"])
    best_feature_cached = feature_cache[best_feature_name]
    best_feature_spec = best_feature_cached["spec"]
    best_feature_list = best_feature_cached["features"]
    best_feature_alpha = float(best_feature_cached["best_alpha"])

    vif_df = compute_vif(best_feature_cached["X"])
    coef_df = get_linear_coefficients(
        X=best_feature_cached["X"],
        y=best_feature_cached["y"],
        alpha=best_feature_alpha,
        random_state=args.random_state,
    )

    # Section 3: Algorithm search.
    print("[Run] Section 3: Algorithm Search", flush=True)
    model_results = evaluate_algorithms(
        train_df=pre_holdout_df,
        spec=best_feature_spec,
        fixed_features=best_feature_list,
        cv_splits=cv_splits,
        wf_splits=wf_splits,
        random_state=args.random_state,
        search_depth=args.search_depth,
    )
    winner_row = best_model_row(model_results)
    winner_algorithm = str(winner_row["algorithm"])
    winner_params = winner_row["params_obj"]
    save_model_comparison(model_results.copy(), model_comparison_path)

    # Section 4: Head-to-head on identical folds.
    print("[Run] Section 4: Head-to-Head vs Seed Baseline", flush=True)
    feature_ridge_name = f"ridge_{best_feature_name}"
    winner_name = f"search_winner_{winner_algorithm}"
    candidates: dict[str, Recipe] = {
        BASELINE_NAME: seed_baseline_recipe,
        GENERAL_NAME: partial(fit_predict_general, **pipeline_kwargs),
        STACK_NAME: partial(fit_predict_stack, **pipeline_kwargs),
        feature_ridge_name: ridge_recipe(best_feature_spec, best_feature_list, args.cv_folds, args.random_state),
        winner_name: estimator_recipe(
            winner_algorithm, winner_params, best_feature_spec, best_feature_list, args.random_state
        ),
    }
    evals: dict[str, dict[str, dict[str, Any]]] = {}
    for name, recipe in candidates.items():
        print(f"[HeadToHead] {name}", flush=True)
        evals[name] = {
            "cv": evaluate_recipe(recipe, pre_holdout_df, cv_split_rows),
            "wf": evaluate_recipe(recipe, pre_holdout_df, wf_splits),
            "holdout": evaluate_recipe(recipe, holdout_frame, holdout_splits),
        }

    base = evals[BASELINE_NAME]
    h2h_rows = []
    for name, ev in evals.items():
        h2h_rows.append(
            {
                "model": name,
                "cv_mae": ev["cv"]["mae"],
                "cv_rmse": ev["cv"]["rmse"],
                "cv_spearman": ev["cv"]["spearman_within_season"],
                "wf_mae": ev["wf"]["mae"],
                "wf_rmse": ev["wf"]["rmse"],
                "wf_spearman": ev["wf"]["spearman_within_season"],
                f"holdout_{args.holdout_year}_mae": ev["holdout"]["mae"],
                f"holdout_{args.holdout_year}_rmse": ev["holdout"]["rmse"],
                f"holdout_{args.holdout_year}_spearman": ev["holdout"]["spearman_within_season"],
            }
        )
    h2h_df = pd.DataFrame(h2h_rows)

    vs_rows = []
    boot: dict[str, dict[str, float]] = {}
    for name, ev in evals.items():
        if name == BASELINE_NAME:
            continue
        boot[name] = paired_season_bootstrap(
            ev["wf"]["per_split"], base["wf"]["per_split"], DEFAULT_BOOTSTRAP_DRAWS, args.random_state
        )
        b = boot[name]
        vs_rows.append(
            {
                "model": name,
                "wf_mae_pct_better": pct_better(ev["wf"]["mae"], base["wf"]["mae"]),
                "wf_rmse_pct_better": pct_better(ev["wf"]["rmse"], base["wf"]["rmse"]),
                "wf_d_mae": b["d_mae"],
                "wf_d_mae_95ci": f"[{b['d_mae_lo']:+.4f}, {b['d_mae_hi']:+.4f}]",
                "wf_d_rmse": b["d_rmse"],
                "wf_d_rmse_95ci": f"[{b['d_rmse_lo']:+.4f}, {b['d_rmse_hi']:+.4f}]",
                "seasons_beating_baseline_mae": f"{b['seasons_better_mae']}/{b['n_seasons']}",
                f"holdout_{args.holdout_year}_mae_pct_better": pct_better(ev["holdout"]["mae"], base["holdout"]["mae"]),
                f"holdout_{args.holdout_year}_rmse_pct_better": pct_better(ev["holdout"]["rmse"], base["holdout"]["rmse"]),
            }
        )
    vs_df = pd.DataFrame(vs_rows)

    per_season_rows = []
    for name, ev in evals.items():
        for _, r in ev["wf"]["per_split"].iterrows():
            per_season_rows.append({"season": int(r["split"]), "model": name, "mae": r["mae"], "rmse": r["rmse"]})
    per_season_long = pd.DataFrame(per_season_rows)
    per_season_mae = per_season_long.pivot(index="season", columns="model", values="mae")[list(evals)].reset_index()
    per_season_rmse = per_season_long.pivot(index="season", columns="model", values="rmse")[list(evals)].reset_index()
    for table in (per_season_mae, per_season_rmse):
        table["season"] = table["season"].astype(int).astype(str)
        table.columns.name = None
    save_walk_forward_plot(
        per_season_long[per_season_long["model"].isin([BASELINE_NAME, GENERAL_NAME, STACK_NAME, winner_name])],
        walk_forward_path,
    )

    # Recommendation is decided on pre-holdout walk-forward only; the holdout is just scored.
    non_baseline = [n for n in evals if n != BASELINE_NAME]
    recommended = min(non_baseline, key=lambda n: evals[n]["wf"]["mse"])
    rec_ev = evals[recommended]
    rec_beats_baseline = rec_ev["wf"]["mse"] < base["wf"]["mse"]

    # Section 5: Seed-stratified stack verdict.
    print("[Run] Section 5: Seed-Stratified Models", flush=True)
    stack_vs_general = paired_season_bootstrap(
        evals[STACK_NAME]["wf"]["per_split"], evals[GENERAL_NAME]["wf"]["per_split"],
        DEFAULT_BOOTSTRAP_DRAWS, args.random_state,
    )
    seed_group_rows = []
    wf_rows_mask = ~np.isnan(evals[BASELINE_NAME]["wf"]["pred"])
    wf_actual = pd.to_numeric(pre_holdout_df[TARGET_COL], errors="coerce").to_numpy(dtype=float)[wf_rows_mask]
    wf_groups = pd.to_numeric(pre_holdout_df["seed"], errors="coerce").to_numpy(dtype=float)[wf_rows_mask]
    group_labels = np.array([seed_group_label(s) for s in wf_groups])
    for lo, hi in SEED_GROUPS:
        label = f"{lo}-{hi}"
        mask = group_labels == label
        row: dict[str, Any] = {"seeds": label, "n_rows": int(mask.sum())}
        for name in [BASELINE_NAME, GENERAL_NAME, STACK_NAME]:
            pred = evals[name]["wf"]["pred"][wf_rows_mask][mask]
            row[f"{name}_mae"] = float(np.mean(np.abs(pred - wf_actual[mask])))
        seed_group_rows.append(row)
    seed_group_df = pd.DataFrame(seed_group_rows)
    predround_df = compare_predround_sources(pre_holdout_df, pipeline_kwargs)

    # Rule for model.use_seed_models: keep the stack only if it has lower walk-forward MAE
    # AND MSE than the general model and at least one season-bootstrap interval excludes 0.
    stack_wf, general_wf = evals[STACK_NAME]["wf"], evals[GENERAL_NAME]["wf"]
    mae_clear = stack_vs_general["d_mae_hi"] < 0
    rmse_clear = stack_vs_general["d_rmse_hi"] < 0
    better_both = stack_wf["mae"] < general_wf["mae"] and stack_wf["mse"] < general_wf["mse"]
    stack_helps = bool(better_both and (mae_clear or rmse_clear))
    if better_both and mae_clear and rmse_clear:
        stack_verdict = (
            "beats the general model on both walk-forward MAE and RMSE, and both season-bootstrap "
            "intervals exclude zero."
        )
    elif stack_helps:
        clear, unclear = ("MAE", "RMSE") if mae_clear else ("RMSE", "MAE")
        stack_verdict = (
            f"beats the general model on both walk-forward MAE and RMSE. The {clear} interval excludes "
            f"zero; the {unclear} interval does not. A modest gain, not a large one."
        )
    elif better_both:
        stack_verdict = (
            "is nominally better than the general model in walk-forward, but both season-bootstrap "
            "intervals include zero, so the 14 hand-picked feature lists are not shown to help."
        )
    else:
        stack_verdict = (
            "does not beat the general model on both walk-forward MAE and RMSE; the 14 hand-picked "
            "per-band feature lists add variance, not accuracy."
        )

    # Section 6: Holdout detail + final retrain of the search winner.
    print("[Run] Section 6: Holdout + Final Retrain", flush=True)
    X_train_best, y_train_best, _ = prepare_design(pre_holdout_df, best_feature_spec, fixed_features=best_feature_list)
    winner_importance_df, _ = get_winner_importance(
        winner_algorithm=winner_algorithm,
        winner_params=winner_params,
        X_train=X_train_best,
        y_train=y_train_best,
        random_state=args.random_state,
    )
    save_feature_importance_plot(
        winner_importance_df,
        feature_importance_path,
        title=f"Feature Importance ({winner_algorithm}, fit on seasons < {args.holdout_year})",
    )

    holdout_rows = holdout_frame.index[season_years(holdout_frame) == args.holdout_year]
    holdout_output = holdout_frame.loc[holdout_rows, ["Team", "year", "seed", "round"]].copy()
    holdout_output = holdout_output.rename(columns={"round": "actual_round"})
    holdout_output["pred_round"] = rec_ev["holdout"]["pred"][holdout_rows]
    holdout_output["seed_baseline"] = base["holdout"]["pred"][holdout_rows]
    holdout_output = holdout_output.sort_values("pred_round", ascending=False)
    save_holdout_scatter(holdout_output, holdout_scatter_path, args.holdout_year, recommended)

    X_all, y_all, _ = prepare_design(model_df, best_feature_spec, fixed_features=best_feature_list)
    final_estimator = build_estimator(winner_algorithm, winner_params, random_state=args.random_state)
    final_estimator.fit(X_all, y_all)
    joblib.dump(
        {
            "model": final_estimator,
            "algorithm": winner_algorithm,
            "params": winner_params,
            "feature_set": best_feature_name,
            "features": best_feature_list,
            "poly": best_feature_spec.poly,
            "train_rows": int(len(y_all)),
            "holdout_year": int(args.holdout_year),
            "recommended_model": recommended,
            "created_at": datetime.now().isoformat(timespec="seconds"),
        },
        model_artifact_path,
    )

    # Build report.
    corr_triplet = df[["Barthag", "AdjOE", "AdjDE"]].apply(pd.to_numeric, errors="coerce").corr()
    b_a = float(corr_triplet.loc["Barthag", "AdjOE"])
    b_d = float(corr_triplet.loc["Barthag", "AdjDE"])
    od = float(corr_triplet.loc["AdjOE", "AdjDE"])
    hy = args.holdout_year
    rec_boot = boot[recommended]
    n_pre_seasons = len(np.unique(pre_years))

    lines: list[str] = []
    lines.append(f"# Predictive Analysis Report ({run_tag})")
    lines.append("")
    lines.append("## Headline")
    lines.append(
        f"- Seed-only baseline (mean round of each seed in the training seasons): walk-forward "
        f"{wf_years[0]}-{wf_years[-1]} MAE `{base['wf']['mae']:.4f}`, RMSE `{base['wf']['rmse']:.4f}`; "
        f"{hy} holdout MAE `{base['holdout']['mae']:.4f}`, RMSE `{base['holdout']['rmse']:.4f}`."
    )
    lines.append(
        f"- Best model by pre-holdout walk-forward MSE: `{recommended}`. Walk-forward MAE "
        f"`{rec_ev['wf']['mae']:.4f}` ({pct_better(rec_ev['wf']['mae'], base['wf']['mae']):+.1f}% vs baseline), "
        f"RMSE `{rec_ev['wf']['rmse']:.4f}` ({pct_better(rec_ev['wf']['rmse'], base['wf']['rmse']):+.1f}%); "
        f"{hy} holdout MAE `{rec_ev['holdout']['mae']:.4f}` "
        f"({pct_better(rec_ev['holdout']['mae'], base['holdout']['mae']):+.1f}%), RMSE "
        f"`{rec_ev['holdout']['rmse']:.4f}` ({pct_better(rec_ev['holdout']['rmse'], base['holdout']['rmse']):+.1f}%)."
    )
    lines.append(
        f"- Walk-forward gap to the baseline, 95% season-bootstrap interval: MAE "
        f"`{rec_boot['d_mae']:+.4f}` [{rec_boot['d_mae_lo']:+.4f}, {rec_boot['d_mae_hi']:+.4f}], RMSE "
        f"`{rec_boot['d_rmse']:+.4f}` [{rec_boot['d_rmse_lo']:+.4f}, {rec_boot['d_rmse_hi']:+.4f}]; lower MAE than "
        f"the baseline in {rec_boot['seasons_better_mae']} of {rec_boot['n_seasons']} test seasons."
    )
    if recommended == winner_name:
        lines.append(
            f"- `{winner_name}` kept its grouped-CV hyperparameters in walk-forward, so its walk-forward score is "
            f"mildly optimistic; the {hy} holdout is the clean number for it."
        )
    if not rec_beats_baseline:
        lines.append("- **No model beats the seed-only baseline on walk-forward MSE.**")
    lines.append(f"- Seed-stratified stack {stack_verdict}")
    lines.append(
        "- Positive % = lower error than the baseline. Error is in rounds won (0-6). "
        f"The {hy} holdout is one bracket of {len(holdout_df)} teams, so it is a single noisy draw."
    )
    lines.append("")
    lines.append("## Run Configuration")
    lines.append(f"- input_csv: `{args.input_csv}`")
    lines.append(f"- holdout_year: `{hy}`")
    lines.append(f"- cv_folds_requested: `{args.cv_folds}` -> `{cv_label}`")
    lines.append(f"- walk_forward_test_seasons: `{', '.join(map(str, wf_years))}`")
    lines.append(f"- search_depth: `{args.search_depth}`")
    lines.append(f"- random_state: `{args.random_state}`")
    lines.append(f"- pipeline_ridge_alpha_grid (configs/default.yaml): `{pipeline_cfg['ridge_alpha_grid']}`")
    lines.append(f"- bootstrap_draws: `{DEFAULT_BOOTSTRAP_DRAWS}`")
    lines.append("")
    lines.append("## Validation Protocol")
    lines.append(
        "- **The season is the unit.** One season is one 64-team bracket of 63 games, so the rounds in a season "
        "always sum to 63 and a team's finish depends on its path. Every split keeps seasons whole; the old "
        "row-level StratifiedKFold and LOO put teams from the same bracket on both sides."
    )
    lines.append(
        f"- **Grouped CV** (`{cv_label}`) on the {n_pre_seasons} pre-holdout seasons picks ridge alphas, the feature "
        "set and the algorithm."
    )
    lines.append(
        f"- **Walk-forward**: for each test season Y in {wf_years[0]}-{wf_years[-1]}, fit on every season before Y "
        "and predict Y. Ridge alphas are re-tuned inside each training window. The search winner's "
        "hyperparameters stay at the values grouped CV picked on all pre-holdout seasons, which include these "
        "test seasons, so its walk-forward score is mildly optimistic; the baseline, general ridge and stack "
        "select nothing outside the window."
    )
    lines.append(f"- **Blind holdout**: {hy}, fit on every season before it and scored once. Nothing is selected on it.")
    lines.append(
        "- **Seed-only baseline**: predicted round = mean round of that seed in the training seasons only."
    )
    lines.append(
        f"- **Stack**: seed-model training rows get out-of-fold `predround` (general model refit without that "
        "season, inside the training window); test rows get `predround` from the general model fit on the "
        "whole window. `general_ridge` and `seed_stratified_stack` call the pipeline code in "
        "`src/pipelines/madness.py` directly."
    )
    lines.append(
        "- All predictions are clipped to [0, 6] before scoring. Spearman is the mean within-season rank "
        "correlation of predicted vs actual round."
    )
    lines.append(
        "- Every feature set passes `assert_no_target_leakage()` before it is fitted."
    )
    lines.append("")
    lines.append("## Section 1: Data Profile")
    lines.append(f"- dataset_shape: `{df.shape[0]} rows x {df.shape[1]} columns`")
    lines.append(f"- year_range: `{year_min}-{year_max}`")
    lines.append(f"- tournament_rows (seed+round non-null): `{tournament_rows}`")
    lines.append(f"- non_tournament_rows: `{non_tournament_rows}`")
    lines.append(f"- modeled_rows_excluding_2020: `{len(model_df)}`")
    lines.append(f"- pre_holdout_rows (<{hy}): `{len(pre_holdout_df)}` over `{n_pre_seasons}` seasons")
    lines.append(f"- holdout_rows ({hy}): `{len(holdout_df)}`")
    lines.append("")
    lines.append("### Distribution Summary")
    lines.append(markdown_table(distribution_summary, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Missingness (candidate feature columns)")
    lines.append(f"![Missingness Heatmap]({figures_rel}/{missingness_path.name})")
    lines.append("")
    lines.append("### Correlation Matrix")
    lines.append(f"![Correlation Matrix]({figures_rel}/{correlation_path.name})")
    lines.append("")
    lines.append("### Collinearity Note")
    lines.append(
        f"- corr(Barthag, AdjOE) = `{b_a:.4f}`, corr(Barthag, AdjDE) = `{b_d:.4f}`, corr(AdjOE, AdjDE) = `{od:.4f}`."
    )
    lines.append("- Barthag is derived from offensive and defensive efficiency, so high multicollinearity is expected.")
    lines.append("")
    lines.append("## Section 2: Feature Optimization (ridge, season-grouped CV)")
    lines.append("`wf_*` columns are walk-forward scores for the top 3 sets by CV MSE (alpha re-tuned per window).")
    lines.append("")
    lines.append(markdown_table(feature_results, max_rows=None, float_digits=4))
    lines.append("")
    lines.append(f"- selected_feature_set: `{best_feature_name}`")
    lines.append(f"- selected_feature_count: `{len(best_feature_list)}`")
    lines.append(f"- selected_ridge_alpha: `{best_feature_alpha}`")
    lines.append("")
    lines.append("### VIF (Top 20)")
    lines.append(markdown_table(vif_df.head(20), max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Scaled Ridge Coefficients (Top 25)")
    coef_view = coef_df.head(25).copy()
    coef_view = coef_view[["feature", "coef", "abs_coef"]]
    lines.append(markdown_table(coef_view, max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Section 3: Algorithm Search (season-grouped CV)")
    lines.append("`wf_*` columns are walk-forward scores for the top 3 configurations by CV MSE (hyperparameters fixed).")
    lines.append("")
    lines.append(markdown_table(model_results.drop(columns=["params_obj"]), max_rows=30, float_digits=4))
    lines.append("")
    lines.append(f"![Model Comparison]({figures_rel}/{model_comparison_path.name})")
    lines.append("")
    lines.append(f"- winning_algorithm: `{winner_algorithm}`")
    lines.append(f"- winning_params: `{format_params(winner_params)}`")
    lines.append(f"- winning_cv_mse: `{float(winner_row['cv_mse']):.6f}`")
    lines.append(f"- winning_cv_mae: `{float(winner_row['cv_mae']):.6f}`")
    lines.append("")
    lines.append("## Section 4: Head-to-Head vs the Seed-Only Baseline (identical folds)")
    lines.append(
        f"Every row is fit and scored on the same splits: `cv_*` = {cv_label} on seasons < {hy}; "
        f"`wf_*` = walk-forward {wf_years[0]}-{wf_years[-1]}; `holdout_{hy}_*` = fit on seasons < {hy}, scored on {hy}."
    )
    lines.append("")
    lines.append(markdown_table(h2h_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Gap to the Baseline")
    lines.append(
        "`d_*` = model minus baseline (negative = better). Intervals resample the walk-forward test seasons with "
        f"replacement ({DEFAULT_BOOTSTRAP_DRAWS} draws); with {len(wf_years)} seasons they are wide."
    )
    lines.append("")
    lines.append(markdown_table(vs_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append(
        f"- `{winner_name}`: hyperparameters chosen by grouped CV over seasons that include the walk-forward test "
        "seasons (see Validation Protocol)."
    )
    lines.append(f"- `{feature_ridge_name}`: the CV-selected feature set; its alpha is re-tuned inside each window.")
    lines.append("")
    lines.append(f"![Walk-forward MAE by season]({figures_rel}/{walk_forward_path.name})")
    lines.append("")
    lines.append("### Walk-Forward MAE by Test Season")
    lines.append(markdown_table(per_season_mae, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Walk-Forward RMSE by Test Season")
    lines.append(markdown_table(per_season_rmse, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("## Section 5: Do the Seed-Stratified Models Help?")
    lines.append(
        "The stack is the general model plus 14 ridge models, one per overlapping 3-seed band, each with a "
        "hand-picked feature list from `madness.R` and the general model's `predround` as an input."
    )
    lines.append(
        "The 14 lists were hand-picked, not selected out of sample. 13 of them match `madness.R` as committed on "
        "2025-01-16 (commit 21a1e96) once the `WAB` terms are removed, and band 9 has since dropped `W`, so they "
        f"were fixed before the {hy} tournament but after seeing earlier seasons, including the walk-forward ones."
    )
    lines.append("")
    lines.append(
        "Rule for `model.use_seed_models`: keep the stack only if it has lower walk-forward MAE and MSE than the "
        "general model and at least one season-bootstrap interval for the gap excludes zero."
    )
    lines.append("")
    lines.append(f"- The stack {stack_verdict}")
    lines.append(
        f"- Stack minus general, walk-forward: MAE `{stack_vs_general['d_mae']:+.4f}` "
        f"[{stack_vs_general['d_mae_lo']:+.4f}, {stack_vs_general['d_mae_hi']:+.4f}], RMSE "
        f"`{stack_vs_general['d_rmse']:+.4f}` [{stack_vs_general['d_rmse_lo']:+.4f}, "
        f"{stack_vs_general['d_rmse_hi']:+.4f}]; stack has the lower MAE in "
        f"{stack_vs_general['seasons_better_mae']} of {stack_vs_general['n_seasons']} seasons."
    )
    lines.append(
        f"- Stack vs baseline, walk-forward: MAE {pct_better(stack_wf['mae'], base['wf']['mae']):+.1f}%, "
        f"RMSE {pct_better(stack_wf['rmse'], base['wf']['rmse']):+.1f}%. General vs baseline: MAE "
        f"{pct_better(general_wf['mae'], base['wf']['mae']):+.1f}%, RMSE "
        f"{pct_better(general_wf['rmse'], base['wf']['rmse']):+.1f}%."
    )
    lines.append("")
    lines.append("### Walk-Forward MAE by Seed Group")
    lines.append(markdown_table(seed_group_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Why predround Must Be Out-of-Fold")
    lines.append(
        f"Seed-model leave-one-season-out MSE on seasons < {hy}, with `predround` from the in-sample general fit "
        "(the old pipeline) vs out-of-fold by season (now). `optimism` = how much the in-sample version flatters "
        "the score."
    )
    lines.append("")
    lines.append(markdown_table(predround_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append(
        f"- mean optimism across {len(predround_df)} seed models: `{predround_df['optimism'].mean():+.4f}` MSE; "
        f"in-sample version scored better on `{int((predround_df['optimism'] > 0).sum())}` of `{len(predround_df)}`."
    )
    lines.append("")
    lines.append(f"## Section 6: Holdout {hy} Detail + Final Retrain")
    lines.append(f"- holdout_rows_scored: `{len(holdout_output)}`")
    lines.append(f"- recommended model on the holdout: `{recommended}`")
    lines.append(f"- final_retrain_rows (search winner, all seasons): `{len(y_all)}`")
    lines.append(f"- model_artifact (search winner, not committed): `{model_artifact_path.relative_to(REPO_ROOT)}`")
    lines.append("")
    lines.append(f"![Holdout Actual vs Predicted]({figures_rel}/{holdout_scatter_path.name})")
    lines.append("")
    lines.append(f"### Holdout Prediction Table (`{recommended}`, top 20 by predicted round)")
    lines.append(markdown_table(holdout_output.head(20), max_rows=None, float_digits=4))
    lines.append("")
    lines.append(f"### Search Winner Feature Importance ({winner_algorithm})")
    lines.append(f"![Feature Importance]({figures_rel}/{feature_importance_path.name})")
    lines.append("")
    lines.append(markdown_table(winner_importance_df.head(25), max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Recommendation")
    if rec_beats_baseline:
        lines.append(
            f"Use `{recommended}`: lowest walk-forward MSE on seasons before {hy}, ahead of the seed-only baseline by "
            f"{pct_better(rec_ev['wf']['mae'], base['wf']['mae']):+.1f}% MAE and "
            f"{pct_better(rec_ev['wf']['rmse'], base['wf']['rmse']):+.1f}% RMSE. Read the gap against the bootstrap "
            "interval above before calling it real."
        )
    else:
        lines.append(
            f"None of the models beats the seed-only baseline on walk-forward MSE. `{recommended}` comes closest; "
            "treat the stats-based models as no better than seed until a feature set clears the baseline."
        )
    lines.append("")
    lines.append(
        f"Seed-stratified models: the walk-forward evidence says {'keep them' if stack_helps else 'leave them off'}. "
        f"`model.use_seed_models` in `configs/default.yaml` is currently "
        f"`{str(pipeline_cfg['use_seed_models']).lower()}`."
    )

    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Report written: {report_path}")
    print(f"Model artifact written: {model_artifact_path}")
    print(f"Figures directory: {figures_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
