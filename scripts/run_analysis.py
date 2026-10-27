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
from scipy import stats
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
CI_LEVEL = 0.95

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
# Parse artifacts of the raw T-Rank tables, not statistics: `blank` is all NaN and the
# `_extra_*` columns are unnamed trailing cells (`_extra_42` repeats seed).
PARSE_ARTIFACT_PREFIX = "_extra_"
PARSE_ARTIFACT_COLUMNS = {"blank"}

SEED_GROUPS = [(1, 4), (5, 8), (9, 12), (13, 16)]
# Two seed-only baselines, one per metric. Every model here is fit by least squares, so
# its fair opponent on RMSE is the per-seed mean. MAE is minimised by the median, and
# round is mostly 0 for seeds 9-16, so against the per-seed mean a least-squares model's
# MAE gain mostly measures the mean's mismatch with the metric. MAE is scored against
# the per-seed median instead.
BASELINE_NAME = "seed_mean_baseline"
MEDIAN_BASELINE_NAME = "seed_median_baseline"
BASELINE_NAMES = (BASELINE_NAME, MEDIAN_BASELINE_NAME)
GENERAL_NAME = "general_ridge"
STACK_NAME = "seed_stratified_stack"
# Ablation for the holdout's provenance: the coach features entered GEN_FEATURES after the
# 2025 tournament. Without them the feature list is that of the predround model in
# madness.R at commit 21a1e96 (2025-01-16) minus WAB, which the current training data does
# not have. Only the list carries over: the R model was OLS without an intercept.
NO_COACH_NAME = "general_ridge_no_coach"
COACH_FEATURES = [f for f in GEN_FEATURES if f.startswith("coach_")]
NO_COACH_FEATURES = [f for f in GEN_FEATURES if f not in COACH_FEATURES]

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
    parser.add_argument(
        "--holdout-year",
        type=int,
        default=DEFAULT_HOLDOUT_YEAR,
        help="Held-out season: excluded from every fit and selection step, scored once at the end.",
    )
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

    exclude = EXCLUDE_FROM_FEATURES | PARSE_ARTIFACT_COLUMNS | {"year"}
    numeric_cols = [
        c
        for c in df.select_dtypes(include=[np.number]).columns
        if c not in exclude and not c.startswith(PARSE_ARTIFACT_PREFIX)
    ]
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
        # fit_intercept=False after StandardScaler forces every prediction to average 0
        # instead of the mean round; see _build_regression_model in src/modeling/ridge.py.
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
    """Mean round of each seed in the training seasons only (the RMSE baseline)."""
    return predict_seed_baseline(fit_seed_baseline(train_df, TARGET_COL, stat="mean"), test_df)


def seed_median_baseline_recipe(train_df: pd.DataFrame, test_df: pd.DataFrame) -> np.ndarray:
    """Median round of each seed in the training seasons only (the MAE baseline)."""
    return predict_seed_baseline(fit_seed_baseline(train_df, TARGET_COL, stat="median"), test_df)


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


def paired_season_test(
    model_per_season: pd.DataFrame,
    ref_per_season: pd.DataFrame,
    metric: str,
    level: float = CI_LEVEL,
) -> dict[str, float]:
    """
    Model minus reference on one per-season metric ("mae" or "mse").

    The season is the independent unit, so the interval is a Student-t interval on the
    per-season differences (n - 1 df) and the p-value a one-sample t-test on them. A
    percentile bootstrap over ~11 seasons gives intervals that are too narrow. Seasons
    all have 64 rows, so the mean per-season MSE gap equals the pooled MSE gap.
    """
    merged = model_per_season[["split", metric]].merge(
        ref_per_season[["split", metric]], on="split", suffixes=("_model", "_ref")
    )
    d = (merged[f"{metric}_model"] - merged[f"{metric}_ref"]).to_numpy(dtype=float)
    n = len(d)
    mean = float(d.mean())
    se = float(d.std(ddof=1) / math.sqrt(n)) if n > 1 else float("nan")
    if n > 1 and se > 0:
        half = float(stats.t.ppf(0.5 + level / 2, n - 1) * se)
        p_t = float(2 * stats.t.sf(abs(mean / se), n - 1))
    else:
        half, p_t = float("nan"), float("nan")
    better, worse = int((d < 0).sum()), int((d > 0).sum())
    p_sign = float(stats.binomtest(better, better + worse, 0.5).pvalue) if better + worse else float("nan")
    return {
        "d": mean,
        "lo": mean - half,
        "hi": mean + half,
        "p_t": p_t,
        "seasons_better": better,
        "n_seasons": int(n),
        "p_sign": p_sign,
    }


def holm_adjust(p_values: list[float]) -> list[float]:
    """Holm step-down adjustment for a family of p-values (controls family-wise error)."""
    p = np.asarray(p_values, dtype=float)
    m = len(p)
    adjusted = np.empty(m)
    running = 0.0
    for rank, idx in enumerate(np.argsort(p)):
        running = max(running, min(1.0, float((m - rank) * p[idx])))
        adjusted[idx] = running
    return adjusted.tolist()


def format_ci(test: dict[str, float]) -> str:
    return f"[{test['lo']:+.4f}, {test['hi']:+.4f}]"


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
    # The grain background does not compress, so file size tracks pixel count.
    fig.savefig(path, dpi=100)
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
    """Per-season walk-forward RMSE and MAE, one line per model."""
    if per_season.empty:
        return
    # color, linestyle, marker: each model differs on all three, not just a shade of blue.
    styles = {
        BASELINE_NAME: (PROJECT_PALETTE["steel"], "--", "o"),
        MEDIAN_BASELINE_NAME: (PROJECT_PALETTE["sage"], ":", "^"),
        GENERAL_NAME: (PROJECT_PALETTE["lake_blue"], "-", "s"),
        STACK_NAME: (PROJECT_PALETTE["paper"], "-", "D"),
    }
    fig, axes = plt.subplots(1, 2, figsize=(15, 5.8), constrained_layout=False)
    apply_report_background(fig, list(axes))
    seasons = sorted(per_season["season"].unique())
    positions = {season: i for i, season in enumerate(seasons)}
    panels = [
        (axes[0], "rmse", "RMSE (rounds)", "RMSE (reference: seed mean)"),
        (axes[1], "mae", "MAE (rounds)", "MAE (reference: seed median)"),
    ]
    for ax, metric, ylabel, title in panels:
        for model, (color, linestyle, marker) in styles.items():
            view = per_season[per_season["model"] == model].sort_values("season")
            if view.empty:
                continue
            ax.plot(
                view["season"].map(positions),
                view[metric],
                marker=marker,
                markersize=5,
                linewidth=1.8,
                linestyle=linestyle,
                color=color,
                label=model,
            )
        ax.set_xticks(range(len(seasons)), [str(s) for s in seasons], rotation=45)
        ax.set_title(f"Walk-Forward {title}")
        ax.set_xlabel("Test season (fit on every earlier season)")
        ax.set_ylabel(ylabel)
    axes[0].legend(loc="best")
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
    walk_forward_path = figures_dir / "walk_forward_error_by_season.png"
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
    input_shape = df.shape
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
    print("[Run] Section 4: Head-to-Head vs the Seed-Only Baselines", flush=True)
    hy = args.holdout_year
    feature_ridge_name = f"ridge_{best_feature_name}"
    winner_name = f"search_winner_{winner_algorithm}"
    candidates: dict[str, Recipe] = {
        BASELINE_NAME: seed_baseline_recipe,
        MEDIAN_BASELINE_NAME: seed_median_baseline_recipe,
        GENERAL_NAME: partial(fit_predict_general, **pipeline_kwargs),
        NO_COACH_NAME: partial(fit_predict_general, features=NO_COACH_FEATURES, **pipeline_kwargs),
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
    med = evals[MEDIAN_BASELINE_NAME]
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
                f"holdout_{hy}_mae": ev["holdout"]["mae"],
                f"holdout_{hy}_rmse": ev["holdout"]["rmse"],
                f"holdout_{hy}_spearman": ev["holdout"]["spearman_within_season"],
            }
        )
    h2h_df = pd.DataFrame(h2h_rows)

    # Per-season paired tests on walk-forward. Two families, each Holm-adjusted on its own:
    # primary = MSE vs the seed mean for every model, stack vs general, and general with vs
    # without the coach features; secondary = the same comparisons on MAE, vs the seed median.
    non_baseline = [n for n in evals if n not in BASELINE_NAMES]
    wf_per = {name: evals[name]["wf"]["per_split"] for name in evals}
    mse_tests = {n: paired_season_test(wf_per[n], wf_per[BASELINE_NAME], "mse") for n in non_baseline}
    mae_tests = {n: paired_season_test(wf_per[n], wf_per[MEDIAN_BASELINE_NAME], "mae") for n in non_baseline}
    stack_vs_general_mse = paired_season_test(wf_per[STACK_NAME], wf_per[GENERAL_NAME], "mse")
    stack_vs_general_mae = paired_season_test(wf_per[STACK_NAME], wf_per[GENERAL_NAME], "mae")
    coach_vs_no_coach_mse = paired_season_test(wf_per[GENERAL_NAME], wf_per[NO_COACH_NAME], "mse")
    coach_vs_no_coach_mae = paired_season_test(wf_per[GENERAL_NAME], wf_per[NO_COACH_NAME], "mae")
    families = (
        (mse_tests, (stack_vs_general_mse, coach_vs_no_coach_mse)),
        (mae_tests, (stack_vs_general_mae, coach_vs_no_coach_mae)),
    )
    for family, extras in families:
        adjusted = holm_adjust([family[n]["p_t"] for n in non_baseline] + [e["p_t"] for e in extras])
        for n, p_holm in zip(non_baseline, adjusted):
            family[n]["p_holm"] = p_holm
        for extra, p_holm in zip(extras, adjusted[len(non_baseline):]):
            extra["p_holm"] = p_holm
    n_primary = len(non_baseline) + len(families[0][1])

    rmse_rows = []
    mae_rows = []
    for name in non_baseline:
        ev, t_mse, t_mae = evals[name], mse_tests[name], mae_tests[name]
        rmse_rows.append(
            {
                "model": name,
                "wf_rmse": ev["wf"]["rmse"],
                "wf_rmse_pct_better": pct_better(ev["wf"]["rmse"], base["wf"]["rmse"]),
                "wf_d_mse": t_mse["d"],
                "wf_d_mse_95ci": format_ci(t_mse),
                "p_t": t_mse["p_t"],
                "p_holm": t_mse["p_holm"],
                "seasons_better": f"{t_mse['seasons_better']}/{t_mse['n_seasons']}",
                "p_sign": t_mse["p_sign"],
                f"holdout_{hy}_rmse_pct_better": pct_better(ev["holdout"]["rmse"], base["holdout"]["rmse"]),
            }
        )
        mae_rows.append(
            {
                "model": name,
                "wf_mae": ev["wf"]["mae"],
                "wf_mae_pct_better": pct_better(ev["wf"]["mae"], med["wf"]["mae"]),
                "wf_d_mae": t_mae["d"],
                "wf_d_mae_95ci": format_ci(t_mae),
                "p_t": t_mae["p_t"],
                "p_holm": t_mae["p_holm"],
                "seasons_better": f"{t_mae['seasons_better']}/{t_mae['n_seasons']}",
                "p_sign": t_mae["p_sign"],
                f"holdout_{hy}_mae_pct_better": pct_better(ev["holdout"]["mae"], med["holdout"]["mae"]),
                "wf_mae_pct_vs_seed_mean": pct_better(ev["wf"]["mae"], base["wf"]["mae"]),
            }
        )
    rmse_vs_df = pd.DataFrame(rmse_rows)
    mae_vs_df = pd.DataFrame(mae_rows)

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
    save_walk_forward_plot(per_season_long, walk_forward_path)

    # Lowest walk-forward MSE is reported, but it is a max over candidates scored on the
    # same seasons, so it carries a winner's-curse bias; the holdout is its less biased number.
    lowest_mse = min(non_baseline, key=lambda n: evals[n]["wf"]["mse"])

    # Section 5: Seed-stratified stack verdict.
    print("[Run] Section 5: Seed-Stratified Models", flush=True)
    # Rule for model.use_seed_models, on MSE: the loss every model is fit with and the metric
    # candidates are ranked by. The stack (general model + 14 hand-picked per-band models)
    # has to have lower walk-forward MSE than the general model alone AND a 95% t-interval
    # on the per-season MSE gap that lies below zero. This rule was written after the
    # walk-forward results had been seen (an earlier rule on MAE kept the stack).
    stack_wf, general_wf = evals[STACK_NAME]["wf"], evals[GENERAL_NAME]["wf"]
    stack_lower_mse = bool(stack_wf["mse"] < general_wf["mse"])
    stack_helps = bool(stack_lower_mse and stack_vs_general_mse["hi"] < 0)
    default_model = STACK_NAME if stack_helps else GENERAL_NAME
    if stack_helps:
        stack_verdict = (
            "has lower walk-forward MSE than the general model, and the 95% interval on the per-season MSE gap "
            "excludes zero, so the seed models earn their place."
        )
    elif stack_lower_mse:
        stack_verdict = (
            "has nominally lower walk-forward MSE than the general model, but the 95% interval on the per-season "
            "MSE gap includes zero: the 14 hand-picked per-band feature lists are not shown to help."
        )
    else:
        stack_verdict = (
            "does not have lower walk-forward MSE than the general model: the 14 hand-picked per-band feature "
            "lists add variance, not accuracy."
        )

    wf_rows_mask = ~np.isnan(base["wf"]["pred"])
    wf_actual = pd.to_numeric(pre_holdout_df[TARGET_COL], errors="coerce").to_numpy(dtype=float)[wf_rows_mask]
    wf_seeds = pd.to_numeric(pre_holdout_df["seed"], errors="coerce").to_numpy(dtype=float)[wf_rows_mask]
    group_labels = np.array([seed_group_label(s) for s in wf_seeds])
    group_models = [BASELINE_NAME, MEDIAN_BASELINE_NAME, GENERAL_NAME, STACK_NAME]
    level_models = [MEDIAN_BASELINE_NAME, GENERAL_NAME, STACK_NAME]
    seed_mae_rows, seed_mse_rows, seed_level_rows = [], [], []
    n_wf_rows = int(wf_rows_mask.sum())
    total_mae_gap = (stack_wf["mae"] - general_wf["mae"]) * n_wf_rows
    for lo, hi in SEED_GROUPS:
        label = f"{lo}-{hi}"
        mask = group_labels == label
        actual = wf_actual[mask]
        preds = {name: evals[name]["wf"]["pred"][wf_rows_mask][mask] for name in group_models}
        mae_row: dict[str, Any] = {"seeds": label, "n_rows": int(mask.sum())}
        mse_row: dict[str, Any] = {"seeds": label, "n_rows": int(mask.sum())}
        for name in group_models:
            mae_row[name] = float(np.mean(np.abs(preds[name] - actual)))
            mse_row[name] = float(np.mean((preds[name] - actual) ** 2))
        group_gap = float(np.sum(np.abs(preds[STACK_NAME] - actual)) - np.sum(np.abs(preds[GENERAL_NAME] - actual)))
        mae_row["share_of_stack_mae_gain"] = group_gap / total_mae_gap if total_mae_gap < 0 else float("nan")
        level_row: dict[str, Any] = {
            "seeds": label,
            "actual_mean": float(actual.mean()),
            "actual_share_at_0": float(np.mean(actual == 0)),
        }
        for name in level_models:
            level_row[f"{name}_mean"] = float(preds[name].mean())
            level_row[f"{name}_share_at_0"] = float(np.mean(preds[name] <= 0))
        seed_mae_rows.append(mae_row)
        seed_mse_rows.append(mse_row)
        seed_level_rows.append(level_row)
    seed_mae_df = pd.DataFrame(seed_mae_rows)
    seed_mse_df = pd.DataFrame(seed_mse_rows)
    seed_level_df = pd.DataFrame(seed_level_rows)
    low_seeds = seed_mae_df.iloc[-1]
    low_level = seed_level_df.iloc[-1]
    predround_df = compare_predround_sources(pre_holdout_df, pipeline_kwargs)

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
        title=f"Feature Importance ({winner_algorithm}, fit on seasons < {hy})",
    )

    holdout_rows = holdout_frame.index[season_years(holdout_frame) == hy]
    holdout_output = holdout_frame.loc[holdout_rows, ["Team", "year", "seed", "round"]].copy()
    holdout_output = holdout_output.rename(columns={"round": "actual_round"})
    for name in [GENERAL_NAME, STACK_NAME, BASELINE_NAME, MEDIAN_BASELINE_NAME]:
        holdout_output[name] = evals[name]["holdout"]["pred"][holdout_rows]
    holdout_output = holdout_output.sort_values(default_model, ascending=False)
    save_holdout_scatter(
        holdout_output.rename(columns={default_model: "pred_round"}), holdout_scatter_path, hy, default_model
    )

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
            "holdout_year": int(hy),
            "pipeline_default_model": default_model,
            "created_at": datetime.now().isoformat(timespec="seconds"),
        },
        model_artifact_path,
    )

    # Build report.
    corr_triplet = df[["Barthag", "AdjOE", "AdjDE"]].apply(pd.to_numeric, errors="coerce").corr()
    b_a = float(corr_triplet.loc["Barthag", "AdjOE"])
    b_d = float(corr_triplet.loc["Barthag", "AdjDE"])
    od = float(corr_triplet.loc["AdjOE", "AdjDE"])
    n_pre_seasons = len(np.unique(pre_years))
    pre_lo, pre_hi = int(pre_years.min()), int(pre_years.max())
    n_wf = len(wf_years)
    config_matches = bool(pipeline_cfg["use_seed_models"]) == stack_helps

    def includes_zero(test: dict[str, float]) -> str:
        return "includes zero" if test["lo"] <= 0 <= test["hi"] else "excludes zero"

    def model_line(name: str) -> str:
        ev, t_mse, t_mae = evals[name], mse_tests[name], mae_tests[name]
        return (
            f"walk-forward RMSE `{ev['wf']['rmse']:.4f}` ({pct_better(ev['wf']['rmse'], base['wf']['rmse']):+.1f}% vs "
            f"seed mean; per-season MSE gap `{t_mse['d']:+.4f}` {format_ci(t_mse)}, which {includes_zero(t_mse)}; "
            f"Holm p `{t_mse['p_holm']:.3f}`), MAE `{ev['wf']['mae']:.4f}` "
            f"({pct_better(ev['wf']['mae'], med['wf']['mae']):+.1f}% vs seed median; gap {format_ci(t_mae)}, which "
            f"{includes_zero(t_mae)}). {hy} holdout RMSE `{ev['holdout']['rmse']:.4f}` "
            f"({pct_better(ev['holdout']['rmse'], base['holdout']['rmse']):+.1f}%), MAE `{ev['holdout']['mae']:.4f}` "
            f"({pct_better(ev['holdout']['mae'], med['holdout']['mae']):+.1f}%)."
        )

    n_mse_holm_sig = sum(mse_tests[n]["p_holm"] < 0.05 for n in non_baseline) + sum(
        e["p_holm"] < 0.05 for e in families[0][1]
    )
    coach_lower_mse = bool(evals[GENERAL_NAME]["wf"]["mse"] < evals[NO_COACH_NAME]["wf"]["mse"])
    coach_helps = bool(coach_lower_mse and coach_vs_no_coach_mse["hi"] < 0)
    general_terms = set(GEN_FEATURES) | {f"{b}_2" for b in GEN_POLY} | {f"{b}_3" for b, d in GEN_POLY.items() if d >= 3}
    feature_ridge_matches_general = set(best_feature_list) == general_terms

    lines: list[str] = []
    lines.append(f"# Predictive Analysis Report ({run_tag})")
    lines.append("")
    lines.append("## Headline")
    lines.append(
        "- **Metrics.** Every model is fit by least squares, so the headline is walk-forward RMSE against the "
        "per-seed **mean**, the best seed-only predictor under squared error. MAE is minimised by the median, so "
        "MAE is scored against the per-seed **median**. Scored against the seed mean instead, a least-squares "
        "model's MAE gain mostly measures the mean's mismatch with the metric (Section 4 shows both)."
    )
    lines.append(
        f"- **Seed-only baselines** (fit on training seasons only), walk-forward {wf_years[0]}-{wf_years[-1]}: "
        f"seed mean RMSE `{base['wf']['rmse']:.4f}` (MAE `{base['wf']['mae']:.4f}`); seed median MAE "
        f"`{med['wf']['mae']:.4f}` (RMSE `{med['wf']['rmse']:.4f}`). {hy} holdout: seed mean RMSE "
        f"`{base['holdout']['rmse']:.4f}`, seed median MAE `{med['holdout']['mae']:.4f}`."
    )
    rmse_gains = [pct_better(evals[n]["wf"]["rmse"], base["wf"]["rmse"]) for n in non_baseline]
    mse_clear = [n for n in non_baseline if mse_tests[n]["hi"] < 0]
    mse_holm = [n for n in non_baseline if mse_tests[n]["p_holm"] < 0.05]
    mae_clear = [n for n in non_baseline if mae_tests[n]["hi"] < 0]

    def name_list(names: list[str]) -> str:
        return ", ".join(f"`{n}`" for n in names) if names else "none"

    lines.append(
        f"- **Bottom line.** On walk-forward RMSE the {len(non_baseline)} candidates are "
        f"{min(rmse_gains):+.1f}% to {max(rmse_gains):+.1f}% against the seed mean. The 95% interval on the "
        f"per-season MSE gap excludes zero for {len(mse_clear)} of them ({name_list(mse_clear)}); after Holm "
        f"adjustment, {len(mse_holm)} keep p < 0.05. On MAE, the candidates that beat the per-seed median with an "
        f"interval below zero: {name_list(mae_clear)}. Only the seed baselines are free of selection on the "
        "walk-forward seasons, so every model-vs-seed walk-forward gap is optimistic by an unknown amount "
        f"(Validation Protocol); the {hy} holdout is the least-biased comparison."
    )
    lines.append(f"- **`{GENERAL_NAME}`** (pipeline default when the stack is off): {model_line(GENERAL_NAME)}")
    lines.append(f"- **`{STACK_NAME}`** (general model + 14 seed-band models): {model_line(STACK_NAME)}")
    lines.append(
        f"- **Do the seed models help?** Stack minus general, walk-forward per-season MSE gap "
        f"`{stack_vs_general_mse['d']:+.4f}` {format_ci(stack_vs_general_mse)}, lower in "
        f"{stack_vs_general_mse['seasons_better']} of {stack_vs_general_mse['n_seasons']} seasons. The stack "
        f"{stack_verdict} Pipeline default: `{default_model}` (`model.use_seed_models: {str(stack_helps).lower()}`)."
    )
    lowest_ev = evals[lowest_mse]
    lines.append(
        f"- **Lowest walk-forward MSE: `{lowest_mse}`** (RMSE `{lowest_ev['wf']['rmse']:.4f}`). It is the best of "
        f"{len(non_baseline)} candidates on the same {n_wf} seasons it is scored on, so that walk-forward number is "
        f"optimistic (winner's curse). Its {hy} holdout score is the less biased number (see Holdout provenance): "
        f"RMSE `{lowest_ev['holdout']['rmse']:.4f}` "
        f"({pct_better(lowest_ev['holdout']['rmse'], base['holdout']['rmse']):+.1f}% vs seed mean)."
    )
    lines.append(
        f"- **Multiplicity.** After Holm adjustment over the {n_primary} primary (MSE) comparisons, "
        f"{n_mse_holm_sig} of {n_primary} gaps have adjusted p < 0.05. Intervals in this report are unadjusted "
        "95% Student-t intervals over seasons."
    )
    lines.append(
        f"- **Holdout provenance.** {hy} is excluded from every fit and every selection step in this script and "
        f"scored once. It is not blind to feature design: the {len(COACH_FEATURES)} coach features in `GEN_FEATURES` "
        "were added after the 2025 tournament (absent at commit 0f6eb0c, 2026-01-31; the decision log in "
        "`docs/modeling_plan.md` dates them 2026-03), and every seed model consumes the general model's `predround`. "
        f"`{NO_COACH_NAME}` drops them, which leaves the feature list of the `predround` model in `madness.R` at "
        "commit 21a1e96 (2025-01-16) minus `WAB`, a column the current data does not have. Only the list carries "
        "over: the R model was OLS without an intercept, scored by row-level jackknife; this one is ridge with an "
        f"intercept and a season-grouped alpha. It is the only candidate whose whole feature list predates {hy}: "
        f"{model_line(NO_COACH_NAME)} The 14 seed-band lists come from the same commit, less `WAB` (Section 5)."
    )
    lines.append(
        f"- **Do the coach features help?** `{GENERAL_NAME}` minus `{NO_COACH_NAME}`, walk-forward per-season MSE gap "
        f"`{coach_vs_no_coach_mse['d']:+.4f}` {format_ci(coach_vs_no_coach_mse)} (Holm p "
        f"`{coach_vs_no_coach_mse['p_holm']:.3f}`), lower with them in {coach_vs_no_coach_mse['seasons_better']} of "
        f"{coach_vs_no_coach_mse['n_seasons']} seasons; {hy} holdout RMSE `{evals[GENERAL_NAME]['holdout']['rmse']:.4f}` "
        f"with them vs `{evals[NO_COACH_NAME]['holdout']['rmse']:.4f}` without. "
        + (
            "They earn their place by the same MSE rule as the seed models."
            if coach_helps
            else "By the same MSE rule used for the seed models, they are not shown to help."
        )
    )
    lines.append(
        "- Positive % = lower error than the reference baseline. Error is in rounds won (0-6). "
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
    lines.append(f"- pipeline_use_seed_models (configs/default.yaml): `{str(pipeline_cfg['use_seed_models']).lower()}`")
    lines.append(f"- interval: `{int(CI_LEVEL * 100)}% Student-t over walk-forward seasons, Holm within each family`")
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
        "and predict Y. Ridge alphas are re-tuned inside each training window; feature lists are not. Only the "
        "seed baselines are free of selection on these seasons. `GEN_FEATURES` (less the coach features) and the "
        "14 seed-band lists were hand-tuned in `madness.R` (commit 21a1e96, 2025-01-16), which kept a formula only "
        "if its row-level jackknife MSE over the 2008-2024 tournament rows beat the best so far. The coach "
        "features were added in 2026-03, with every season through 2025 available. The feature set behind "
        f"`{feature_ridge_name}` and `{winner_name}`, and the search winner's algorithm and hyperparameters, were "
        f"picked by grouped CV over {pre_lo}-{pre_hi}. So every model-vs-seed walk-forward gap is optimistic by "
        f"an unknown amount, and so is every gap between models. The {hy} holdout is the least-biased comparison, "
        f"and `{NO_COACH_NAME}` is the only candidate whose whole feature list predates {hy}."
    )
    lines.append(
        f"- **Holdout**: {hy}, fit on every season before it and scored once. Nothing in this script is selected "
        "on it; see Holdout provenance above for what was decided outside the script after that tournament."
    )
    lines.append(
        "- **Seed-only baselines**: predicted round = mean (RMSE reference) or median (MAE reference) round of "
        "that seed in the training seasons only."
    )
    lines.append(
        "- **Stack**: seed-model training rows get out-of-fold `predround` (general model refit without that "
        "season, inside the training window); test rows get `predround` from the general model fit on the "
        "whole window. `general_ridge` and `seed_stratified_stack` call the pipeline code in "
        "`src/pipelines/madness.py` directly."
    )
    lines.append(
        f"- **Uncertainty**: every gap is a mean of per-season differences over the {n_wf} walk-forward seasons, "
        f"with a {int(CI_LEVEL * 100)}% Student-t interval ({n_wf - 1} df), a t-test p-value, a Holm-adjusted p "
        "within its family, and a two-sided sign test on the season count. Primary family: MSE vs the seed mean "
        "for each model, stack vs general, and the general model with vs without the coach features. Secondary "
        "family: the same comparisons on MAE, with the seed median as the baseline."
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
    lines.append(f"- input_csv_shape: `{input_shape[0]} rows x {input_shape[1]} columns`")
    lines.append(
        f"- dataset_shape (with engineered and rank features added): `{df.shape[0]} rows x {df.shape[1]} columns`"
    )
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
    lines.append(
        "The kitchen-sink set is every numeric column except outcomes, `PAKE`/`PASE`, `year` and table-parse "
        "artifacts (`blank`, `_extra_*`)."
    )
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
    lines.append("## Section 4: Head-to-Head vs the Seed-Only Baselines (identical folds)")
    lines.append(
        f"Every row is fit and scored on the same splits: `cv_*` = {cv_label} on seasons < {hy}; "
        f"`wf_*` = walk-forward {wf_years[0]}-{wf_years[-1]}; `holdout_{hy}_*` = fit on seasons < {hy}, scored on {hy}."
    )
    lines.append("")
    lines.append(markdown_table(h2h_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### RMSE vs the Seed-Mean Baseline (primary)")
    lines.append(
        "`wf_d_mse` = mean over walk-forward seasons of (model MSE - seed-mean MSE); negative = better. "
        f"`p_holm` is adjusted over {n_primary} comparisons: these rows, stack vs general (Section 5) and "
        f"`{GENERAL_NAME}` vs `{NO_COACH_NAME}` (Headline). "
        "`p_sign` is a two-sided sign test on `seasons_better`."
    )
    lines.append("")
    lines.append(markdown_table(rmse_vs_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### MAE vs the Seed-Median Baseline (secondary)")
    lines.append(
        "`wf_d_mae` = mean over walk-forward seasons of (model MAE - seed-median MAE). The last column scores the "
        "same MAE against the seed mean instead, to show how much a mean baseline flatters a least-squares model "
        "on MAE."
    )
    lines.append("")
    lines.append(markdown_table(mae_vs_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append(
        "- Every row except the seed baselines carries selection on these seasons (see Validation Protocol): "
        "feature lists hand-tuned in `madness.R` on 2008-2024, coach features added in 2026-03, and, for "
        f"`{winner_name}` and `{feature_ridge_name}`, a feature set (and, for the search winner, hyperparameters) "
        "chosen by grouped CV over seasons that include the walk-forward test seasons."
    )
    if feature_ridge_matches_general:
        lines.append(
            f"- `{feature_ridge_name}` uses the same features as `{GENERAL_NAME}`. They differ in the alpha grid, the "
            "CV used to tune alpha, and missing values (median imputation here, 0 in the pipeline)."
        )
    lines.append(
        f"- `{NO_COACH_NAME}` is `{GENERAL_NAME}` without the {len(COACH_FEATURES)} coach features (see Holdout "
        "provenance)."
    )
    lines.append("")
    lines.append(f"![Walk-forward error by season]({figures_rel}/{walk_forward_path.name})")
    lines.append("")
    lines.append("### Walk-Forward RMSE by Test Season")
    lines.append(markdown_table(per_season_rmse, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Walk-Forward MAE by Test Season")
    lines.append(markdown_table(per_season_mae, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("## Section 5: Do the Seed-Stratified Models Help?")
    lines.append(
        "The stack is the general model plus 14 ridge models, one per overlapping 3-seed band, each with a "
        "hand-picked feature list from `madness.R` and the general model's `predround` as an input."
    )
    lines.append(
        "The 14 lists were hand-picked, not selected out of sample. 13 of them match `madness.R` as committed on "
        "2025-01-16 (commit 21a1e96) once the `WAB` terms are removed, and band 9 has since dropped `W`, so they "
        f"were fixed before the {hy} tournament but after seeing earlier seasons, including the walk-forward ones. "
        "There each list was kept only if its row-level jackknife MSE over the band's 2008-2024 tournament rows "
        "beat the best so far, with the in-sample `predround` as an input. Any gain from that selection favours "
        "the stack in the comparison below."
    )
    lines.append("")
    lines.append(
        "Rule for `model.use_seed_models`, decided on MSE (the loss every model is fit with and the metric "
        "candidates are ranked by): keep the stack only if its walk-forward MSE is lower than the general model's "
        "AND the 95% t-interval on the per-season MSE gap lies below zero. This rule was written after the "
        "walk-forward results had been seen. An earlier version of this report used an MAE-based rule and kept "
        "the stack; MAE favours models that shrink seeds 13-16 toward 0 (the median), which is not evidence that "
        "the hand-picked lists add signal."
    )
    lines.append("")
    lines.append(f"- The stack {stack_verdict}")
    lines.append(
        f"- Stack minus general, walk-forward: MSE `{stack_vs_general_mse['d']:+.4f}` {format_ci(stack_vs_general_mse)} "
        f"(t-test p `{stack_vs_general_mse['p_t']:.3f}`, Holm p `{stack_vs_general_mse['p_holm']:.3f}`; lower in "
        f"{stack_vs_general_mse['seasons_better']} of {stack_vs_general_mse['n_seasons']} seasons, sign-test p "
        f"`{stack_vs_general_mse['p_sign']:.3f}`); MAE `{stack_vs_general_mae['d']:+.4f}` "
        f"{format_ci(stack_vs_general_mae)} (Holm p `{stack_vs_general_mae['p_holm']:.3f}`; lower in "
        f"{stack_vs_general_mae['seasons_better']} of {stack_vs_general_mae['n_seasons']} seasons, sign-test p "
        f"`{stack_vs_general_mae['p_sign']:.3f}`)."
    )
    if stack_wf["mae"] < general_wf["mae"]:
        lines.append(
            f"- Where the MAE edge comes from: seeds {low_seeds['seeds']} contribute "
            f"`{100 * low_seeds['share_of_stack_mae_gain']:.0f}%` of the stack's walk-forward MAE gain over the general "
            f"model. There the stack predicts exactly 0 for `{100 * low_level[f'{STACK_NAME}_share_at_0']:.0f}%` of "
            f"teams (general: `{100 * low_level[f'{GENERAL_NAME}_share_at_0']:.0f}%`) and averages "
            f"`{low_level[f'{STACK_NAME}_mean']:.3f}` against an actual mean of `{low_level['actual_mean']:.3f}` "
            f"(general: `{low_level[f'{GENERAL_NAME}_mean']:.3f}`). The seed median, which predicts 0 for "
            f"`{100 * low_level[f'{MEDIAN_BASELINE_NAME}_share_at_0']:.0f}%` of them, has MAE "
            f"`{low_seeds[MEDIAN_BASELINE_NAME]:.4f}` there vs the stack's `{low_seeds[STACK_NAME]:.4f}`."
        )
    lines.append("")
    lines.append("### Walk-Forward MAE by Seed Group")
    lines.append(
        "`share_of_stack_mae_gain` = the group's part of the stack's total MAE gain over the general model "
        "(groups are equal-sized; a negative share means the stack is worse there)."
    )
    lines.append("")
    lines.append(markdown_table(seed_mae_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Walk-Forward MSE by Seed Group")
    lines.append(markdown_table(seed_mse_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Prediction Level by Seed Group (walk-forward)")
    lines.append(
        "Mean prediction and share of predictions at exactly 0 (after clipping to [0, 6]), next to the actual "
        "mean round and the share of teams that actually won 0 games."
    )
    lines.append("")
    lines.append(markdown_table(seed_level_df, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Why predround Must Be Out-of-Fold")
    lines.append(
        f"Seed-model leave-one-season-out MSE on seasons < {hy}, with `predround` from the in-sample general fit "
        "(the old pipeline) vs out-of-fold by season (now). `optimism` = how much the in-sample version flatters "
        "the score."
    )
    lines.append(
        "Caveat: the out-of-fold column is built once over all these seasons, so when season s is held out of a "
        "seed model's CV, its training rows' `predround` came from general models that saw s. That second-order "
        "leak can make the `oof` column slightly optimistic, which would understate `optimism`. Walk-forward and "
        "holdout scores rebuild the column inside each training window and are not affected."
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
    lines.append(f"- pipeline default model: `{default_model}`")
    lines.append(f"- final_retrain_rows (search winner, all seasons): `{len(y_all)}`")
    lines.append(f"- model_artifact (search winner, not committed): `{model_artifact_path.relative_to(REPO_ROOT)}`")
    lines.append("")
    lines.append(f"![Holdout Actual vs Predicted]({figures_rel}/{holdout_scatter_path.name})")
    lines.append("")
    lines.append(f"### Holdout Prediction Table (top 20 by `{default_model}`)")
    lines.append(markdown_table(holdout_output.head(20), max_rows=None, float_digits=4))
    lines.append("")
    lines.append(f"### Search Winner Feature Importance ({winner_algorithm})")
    lines.append(f"![Feature Importance]({figures_rel}/{feature_importance_path.name})")
    lines.append("")
    lines.append(markdown_table(winner_importance_df.head(25), max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Recommendation")
    default_mse = mse_tests[default_model]
    default_mae = mae_tests[default_model]
    lines.append(
        f"- Pipeline default: `{default_model}` (`model.use_seed_models: {str(stack_helps).lower()}`), from the MSE "
        f"rule in Section 5. `configs/default.yaml` currently has `use_seed_models: "
        f"{str(pipeline_cfg['use_seed_models']).lower()}`"
        + (", which matches." if config_matches else ", which does NOT match; change it.")
    )
    lines.append(
        f"- Against seed alone, `{default_model}` is {pct_better(evals[default_model]['wf']['rmse'], base['wf']['rmse']):+.1f}% "
        f"on walk-forward RMSE (per-season MSE gap interval {includes_zero(default_mse)}) and "
        f"{pct_better(evals[default_model]['wf']['mae'], med['wf']['mae']):+.1f}% on walk-forward MAE vs the seed "
        f"median (interval {includes_zero(default_mae)})."
    )
    lines.append(
        f"- Lowest walk-forward MSE: `{lowest_mse}`, "
        f"{pct_better(evals[lowest_mse]['wf']['rmse'], base['wf']['rmse']):+.1f}% RMSE vs seed mean in walk-forward "
        f"(unadjusted interval {includes_zero(mse_tests[lowest_mse])}; Holm p `{mse_tests[lowest_mse]['p_holm']:.3f}`; "
        "optimistic as the best of several) and "
        f"{pct_better(evals[lowest_mse]['holdout']['rmse'], base['holdout']['rmse']):+.1f}% on the {hy} holdout."
    )
    lines.append(
        f"- Coach features: {'kept by' if coach_helps else 'not supported by'} the same walk-forward MSE rule "
        f"(gap {format_ci(coach_vs_no_coach_mse)}). `GEN_FEATURES` still includes them; this script reports the "
        "ablation and does not change the pipeline's feature list."
        + (
            ""
            if coach_helps
            else " Applied evenly, the MSE rule used for the seed stack would drop them. The decision log's "
            "reason for adding them (2026-03) is a prior, that tournament coaching experience predicts results, "
            "not a measured gain. Keeping or dropping them is an open decision (`docs/modeling_plan.md`, roadmap 5c)."
        )
    )
    lines.append(
        "- Read every gap here against its interval: with "
        f"{n_wf} walk-forward seasons, a few percent is within season-to-season noise."
    )

    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Report written: {report_path}")
    print(f"Model artifact written: {model_artifact_path}")
    print(f"Figures directory: {figures_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
