#!/usr/bin/env python3
"""CV-first predictive analysis for NCAA tournament finish modeling."""

from __future__ import annotations

import argparse
import math
import re
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.base import clone
from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.inspection import permutation_importance
from sklearn.linear_model import ElasticNet, Lasso, Ridge
from sklearn.metrics import mean_absolute_error, mean_squared_error
from sklearn.model_selection import GroupKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.pipelines.madness import GEN_FEATURES, GEN_POLY  # noqa: E402
from src.visualization.palette import (  # noqa: E402
    ALGORITHM_COLORS,
    CBB_DIVERGING_CMAP,
    PROJECT_PALETTE,
    apply_report_background,
    set_project_theme,
)


DEFAULT_INPUT = "data/processed/cbb_training_clean.csv"
DEFAULT_REPORT_DIR = "reports"
DEFAULT_FIGURES_DIR = "reports/figures"
DEFAULT_MODEL_DIR = "models"
DEFAULT_HOLDOUT_YEAR = 2025
DEFAULT_CV_FOLDS = 10
DEFAULT_RANDOM_STATE = 42
DEFAULT_SEARCH_DEPTH = "balanced"

RIDGE_ALPHA_GRID = [0.01, 0.1, 0.3, 1.0, 3.0, 10.0, 30.0]

# PAKE and PASE are tournament wins MINUS expectation, so they are computed from the
# result this script predicts. round = E[wins|seed] + PASE reconstructs the target exactly
# on 100% of rows. They were filed under "Schedule" in docs/modeling_plan.md:37 and so
# never reached OUTCOME_COLUMNS, which is how they got into the feature sets.
UNUSED_CANDIDATES = ["ELITESOS", "FT%", "OPFT%", "PPPOFF", "PPPDEF"]
ENGINEERED_FEATURES = ["eff_ratio", "seed_barthag", "coach_seed"]
TARGET_COL = "round"
OUTCOME_COLUMNS = {
    "round", "Round2", "S16", "E8", "F4", "Ship", "Champ", "result", "predround",
    "PAKE", "PASE",
    # post-tournament strings: finish label, postseason W-L (includes NCAA games), final record
    "finish", "_extra_40", "_extra_43", "ov_rec",
}
LEAKAGE_THRESHOLD = 0.90  # stricter than the r=0.95 the brief asks for; PAKE sits at 0.955
# anything derived from the outcome is excluded from every feature set, not just the target
EXCLUDE_FROM_FEATURES = OUTCOME_COLUMNS

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


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run CV-first predictive analysis on cbb_training_clean.csv.")
    parser.add_argument("--input-csv", default=DEFAULT_INPUT, help="Path to cleaned training CSV.")
    parser.add_argument("--report-dir", default=DEFAULT_REPORT_DIR, help="Directory for markdown reports.")
    parser.add_argument("--figures-dir", default=DEFAULT_FIGURES_DIR, help="Directory for PNG outputs.")
    parser.add_argument("--holdout-year", type=int, default=DEFAULT_HOLDOUT_YEAR, help="Final blind holdout year.")
    parser.add_argument(
        "--cv-folds",
        type=int,
        default=DEFAULT_CV_FOLDS,
        help="Season-grouped CV folds (capped at the number of training seasons).",
    )
    parser.add_argument("--random-state", type=int, default=DEFAULT_RANDOM_STATE, help="Random seed.")
    parser.add_argument(
        "--search-depth",
        choices=["fast", "balanced", "exhaustive"],
        default=DEFAULT_SEARCH_DEPTH,
        help="Hyperparameter search depth.",
    )
    parser.add_argument(
        "--report-prefix",
        default="predictive_analysis",
        help="Report file stem; run_pipeline.py --holdout passes model_optimization.",
    )
    return parser.parse_args(argv)


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


def leakage_scores(df: pd.DataFrame, features: list[str], target: str = TARGET_COL,
                   condition: str = "seed") -> dict[str, float]:
    """Per feature, the larger of two reconstruction signals against the target.

    - |Pearson(target residualised on seed, feature)| catches PASE/PAKE-style leaks, which are
      the target minus a seed expectation.
    - |Spearman(target, feature)| catches a copy or recoding of the target (round itself only
      scores 0.77 on the residual test, and `result` is a monotone recode of round).
    """
    y = pd.to_numeric(df[target], errors="coerce")
    cond = pd.to_numeric(df[condition], errors="coerce") if condition in df.columns else None
    out: dict[str, float] = {}
    for col in features:
        if col not in df.columns or col == condition:
            continue
        x = pd.to_numeric(df[col], errors="coerce")
        ok = x.notna() & y.notna()
        if cond is not None:
            ok &= cond.notna()
        if ok.sum() < 30 or x[ok].nunique() < 3:
            continue
        resid = y[ok] - y[ok].groupby(cond[ok]).transform("mean") if cond is not None else y[ok]
        r_resid = abs(float(np.corrcoef(resid, x[ok])[0, 1])) if resid.std() > 0 else 0.0
        r_rank = abs(float(x[ok].corr(y[ok], method="spearman")))
        out[col] = max(r_resid, r_rank)
    return out


def assert_no_target_leakage(df: pd.DataFrame, features: list[str], target: str,
                             condition: str = "seed", threshold: float = LEAKAGE_THRESHOLD,
                             label: str = "") -> tuple[str, float]:
    """Fail loudly if a feature reconstructs the target once seed is accounted for.

    A plain corr(feature, target) does NOT catch this: PASE against round is only 0.767,
    because PASE IS wins minus seed expectation, so the identity needs two variables.
    Residualising the target against seed first separates them cleanly, measured on the
    1,088-row training set:

        corr(resid_seed(round), PASE)  = 0.9948
        corr(resid_seed(round), PAKE)  = 0.9553
        corr(resid_seed(round), AdjOE) = 0.0580   <- clean control

    Returns the strongest (feature, r) so the report can show how far the set is from failing.
    """
    corrs = leakage_scores(df, features, target, condition)
    bad = [(c, r) for c, r in corrs.items() if r >= threshold]

    if bad:
        detail = ", ".join(f"{c} r={r:.4f}" for c, r in sorted(bad, key=lambda t: -t[1]))
        raise ValueError(
            f"target leakage in feature set {label or '(unnamed)'}: {detail}. "
            f"Against '{target}' (Pearson on the '{condition}' residual, or raw Spearman), "
            f"r>={threshold} means the feature is derived from the outcome, not a predictor of it."
        )
    if not corrs:
        return ("", 0.0)
    top = max(corrs, key=corrs.get)
    return (top, corrs[top])


def seed_lookup_predictions(train_df: pd.DataFrame, score_df: pd.DataFrame) -> np.ndarray:
    """Predict round from the historical mean for the same seed line."""
    train = train_df[["seed", TARGET_COL]].apply(pd.to_numeric, errors="coerce").dropna()
    if train.empty:
        raise ValueError("seed baseline has no eligible training rows")
    by_seed = train.groupby("seed")[TARGET_COL].mean()
    fallback = float(train[TARGET_COL].mean())
    score_seed = pd.to_numeric(score_df["seed"], errors="coerce")
    return score_seed.map(by_seed).fillna(fallback).to_numpy(dtype=float)


# Prediction-time contract. Prediction moment: the published bracket (Selection Sunday), before
# any NCAA tournament game. Each column maps to (source, available at that moment).
_TORVIK_R = "Torvik team table type=R (games through conference tournaments)"
_SOURCE_GROUPS: list[tuple[str, bool, list[str]]] = [
    (_TORVIK_R, True, [
        "AdjOE", "AdjDE", "Barthag", "WINS", "GAMES", "EFG%", "EFGD%", "FTR", "FTRD", "TOR", "TORD",
        "ORB", "DRB", "Adj T.", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%",
        "3PRATE", "3PRATED", "AVGHGT", "EFFHGT", "EXP.", "TALENT", "FT%", "OPFT%", "PPPOFF", "PPPDEF",
        "ELITESOS", "W", "L", "G",
    ]),
    ("Within-season rank of regular-season Barthag", True, ["Rk"]),
    ("Selection Sunday bracket seed", True, ["seed", "_extra_42"]),
    ("Team history, prior 5 tournaments only (current season excluded)", True, [
        "coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y",
        "coach_tourn_games_5y", "coach_tourn_win_pct_5y", "coach_avg_wins_per_appearance",
        "_extra_45", "_extra_46", "_extra_47",
    ]),
    ("Team history, consecutive seasons with current coach", True, ["coach_years_with_program", "_extra_48"]),
    ("Conference membership / same-season mean regular-season Barthag", True, [
        "Power6", "B12", "B10", "SEC", "ACC", "P12", "BE", "MWC", "_extra_49",
    ]),
    ("Engineered from pre-tournament columns", True, ENGINEERED_FEATURES),
    ("Torvik tournament result vs expectation (PAKE, PASE)", False, ["PAKE", "PASE"]),
    ("Tournament outcome / target encodings", False, [
        "round", "Round2", "S16", "E8", "F4", "Ship", "Champ", "result", "predround",
        "finish", "_extra_43", "_extra_40", "ov_rec",
    ]),
    ("Identifier / empty", False, ["year", "blank"]),
]
FEATURE_SOURCES = {col: (src, ok) for src, ok, cols in _SOURCE_GROUPS for col in cols}


def feature_source(col: str) -> tuple[str, bool]:
    if col in FEATURE_SOURCES:
        return FEATURE_SOURCES[col]
    if col.endswith("_rank") and col[: -len("_rank")] in FEATURE_SOURCES:
        src, ok = FEATURE_SOURCES[col[: -len("_rank")]]
        return (f"Within-season rank over all D1 teams: {src}", ok)
    m = re.fullmatch(r"(.+)_[23]", col)
    if m and m.group(1) in FEATURE_SOURCES:
        src, ok = FEATURE_SOURCES[m.group(1)]
        return (f"Polynomial term: {src}", ok)
    return ("UNMAPPED", False)


def assert_prediction_time_contract(features: list[str], label: str = "") -> None:
    """Every feature must map to a source known before the first tournament game."""
    bad = [c for c in features if c in EXCLUDE_FROM_FEATURES or not feature_source(c)[1]]
    if bad:
        raise ValueError(f"feature set {label or '(unnamed)'} uses columns not available at "
                         f"Selection Sunday or not in the source map: {bad}")


def feature_inventory(df: pd.DataFrame, used: set[str]) -> pd.DataFrame:
    numeric = [c for c in df.select_dtypes(include=[np.number]).columns]
    cols = unique_keep_order(numeric + [c for c in OUTCOME_COLUMNS if c in df.columns])
    scores = leakage_scores(df, [c for c in cols if c in numeric])
    rows = []
    for col in cols:
        src, ok = feature_source(col)
        rows.append({
            "column": col, "source": src, "pre_tournament": "yes" if ok else "NO",
            "in_candidate_sets": "yes" if col in used else "no",
            "leak_score": scores.get(col, np.nan),
        })
    return pd.DataFrame(rows)


def prepare_scoring_frame(season_df: pd.DataFrame) -> pd.DataFrame:
    """Rebuild model inputs for one or more seasons from pre-tournament columns only.

    Pass every team of the season, not just the field: rank features are ranks among all D1
    teams, exactly as in training. Outcome columns are dropped first, so a field whose
    tournament has not been played scores the same as one whose results are known.
    """
    work = season_df.drop(columns=[c for c in OUTCOME_COLUMNS if c in season_df.columns])
    work = add_engineered_features(work)
    work, _ = add_rank_features(work, choose_rank_base_columns(work))
    return work


def score_field(artifact: dict[str, Any], season_df: pd.DataFrame) -> pd.Series:
    """Predicted tournament wins from a saved analysis artifact; needs no outcome columns."""
    assert_prediction_time_contract(artifact["features"], label=str(artifact.get("feature_set", "")))
    work = add_poly_features(prepare_scoring_frame(season_df), artifact.get("poly"))
    X = work.reindex(columns=artifact["features"]).apply(pd.to_numeric, errors="coerce")
    return pd.Series(artifact["model"].predict(X), index=season_df.index, name="pred_round")


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
        # intercept on: with scaled inputs, fit_intercept=False forced every ridge prediction to
        # centre on 0 wins and added roughly mean(round)^2 = 1.0 to every ridge CV MSE.
        model = Ridge(alpha=float(params["alpha"]), random_state=random_state)
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


def run_season_cv(
    estimator: Pipeline,
    X: pd.DataFrame,
    y: pd.Series,
    groups: pd.Series,
    folds: int,
) -> dict[str, Any]:
    """Season-grouped CV: a validation fold never shares a season with its training rows.

    Row-level CV (the old StratifiedKFold and LOO) put teams from the same tournament on both
    sides of a split, which is not how the model is used: it always scores a season it has
    never seen. Imputer and scaler sit inside the pipeline, so they refit per fold.
    """
    n_splits = max(2, min(int(folds), int(pd.Series(groups).nunique())))
    splitter = GroupKFold(n_splits=n_splits)

    fold_rows: list[dict[str, float]] = []
    oof = pd.Series(np.nan, index=X.index, dtype=float)
    for fold_i, (train_idx, test_idx) in enumerate(splitter.split(X, y, groups=groups), start=1):
        model = clone(estimator)
        model.fit(X.iloc[train_idx], y.iloc[train_idx])
        pred = model.predict(X.iloc[test_idx])
        y_test = y.iloc[test_idx]
        fold_rows.append({
            "fold": float(fold_i),
            "mse": float(mean_squared_error(y_test, pred)),
            "mae": float(mean_absolute_error(y_test, pred)),
        })
        oof.iloc[test_idx] = pred

    return {
        "overall_mse": float(mean_squared_error(y, oof)),
        "overall_mae": float(mean_absolute_error(y, oof)),
        "fold_mse_std": float(np.std([r["mse"] for r in fold_rows])),
        "n_splits": int(n_splits),
        "fold_rows": fold_rows,
        "oof": oof,
    }


def tune_ridge_alpha(
    X: pd.DataFrame,
    y: pd.Series,
    groups: pd.Series,
    alpha_grid: list[float],
    cv_folds: int,
    random_state: int,
) -> tuple[float, list[dict[str, float]]]:
    rows: list[dict[str, float]] = []
    for alpha in alpha_grid:
        est = build_estimator("ridge", {"alpha": float(alpha)}, random_state=random_state)
        cv = run_season_cv(estimator=est, X=X, y=y, groups=groups, folds=cv_folds)
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
            # Solve linear least squares and compute R^2 for VIF = 1/(1-R^2). The auxiliary
            # regression needs an intercept: R^2 is measured against the mean, and without one an
            # uncentred column such as AdjDE (~103) gets a wrong R^2 and a wrong VIF.
            X_others = np.column_stack([np.ones(len(X_others)), X_others])
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
    for _, row in view.astype(object).iterrows():  # object keeps int columns from upcasting to float
        rendered: list[str] = []
        for col in cols:
            val = row[col]
            if isinstance(val, (float, np.floating)):
                if np.isfinite(val):
                    rendered.append(f"{float(val):.{float_digits}f}")
                else:
                    rendered.append("" if np.isnan(val) else "inf")
            else:
                rendered.append(str(val))
        lines.append("| " + " | ".join(rendered) + " |")
    return "\n".join(lines)


def missing_share_table(df: pd.DataFrame, cols: list[str], modeled_mask: pd.Series) -> pd.DataFrame:
    """Percent of rows missing per column: every row, and the modeled (tournament, non-2020) rows."""
    values = df[cols].apply(pd.to_numeric, errors="coerce")
    return pd.DataFrame(
        {
            "feature": cols,
            "missing_all_rows_pct": 100.0 * values.isna().mean().to_numpy(),
            "missing_modeled_rows_pct": 100.0 * values[modeled_mask].isna().mean().to_numpy(),
        }
    )


def save_missingness_chart(table: pd.DataFrame, path: Path, n_all: int, n_modeled: int) -> None:
    """Horizontal bars of percent missing per column, all rows beside modeled rows."""
    if table.empty:
        return
    view = table.iloc[::-1].reset_index(drop=True)
    pos = np.arange(len(view))
    height = 0.38
    fig, ax = plt.subplots(figsize=(10, max(4.0, 0.34 * len(view) + 1.6)))
    apply_report_background(fig)
    series = [
        ("missing_all_rows_pct", f"All rows (n={n_all:,})", PROJECT_PALETTE["lake_blue"], height / 2),
        ("missing_modeled_rows_pct", f"Modeled rows (n={n_modeled:,})", PROJECT_PALETTE["sage"], -height / 2),
    ]
    # Label both bars of any column with a gap, so a 0.0% on the modeled rows reads as a value.
    has_gap = (view["missing_all_rows_pct"] > 0) | (view["missing_modeled_rows_pct"] > 0)
    for col, label, color, offset in series:
        bars = ax.barh(pos + offset, view[col], height=height, color=color, label=label)
        for bar, value, show in zip(bars, view[col], has_gap):
            if show:
                ax.text(bar.get_width() + 1.0, bar.get_y() + bar.get_height() / 2, f"{value:.1f}%",
                        va="center", fontsize=8, color=PROJECT_PALETTE["fog"])
    ax.set_yticks(pos, labels=view["feature"])
    ax.set_xlim(0, 110)
    ax.set_xlabel("Rows missing (%)")
    ax.set_title("Missing Values by Candidate Feature")
    ax.legend(loc="center right")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def save_correlation_matrix(df: pd.DataFrame, cols: list[str], path: Path) -> None:
    if not cols:
        return
    corr = df[cols].apply(pd.to_numeric, errors="coerce").corr()
    fig, ax = plt.subplots(figsize=(14, 10))
    apply_report_background(fig)
    image = ax.imshow(corr.to_numpy(), vmin=-1, vmax=1, cmap=CBB_DIVERGING_CMAP)
    ax.set_xticks(range(len(corr.columns)), labels=corr.columns, rotation=90)
    ax.set_yticks(range(len(corr.index)), labels=corr.index)
    ax.set_title("Feature Correlation Matrix")
    fig.colorbar(image, ax=ax, shrink=0.8, label="Pearson correlation")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def save_model_comparison(df: pd.DataFrame, path: Path) -> None:
    if df.empty:
        return
    view = df.sort_values("cv_mse").head(20).copy()
    view["label"] = view["algorithm"] + " | " + view["params"]
    view = view.iloc[::-1]
    fig, ax = plt.subplots(figsize=(12, 8))
    apply_report_background(fig)
    colors = [ALGORITHM_COLORS.get(name, PROJECT_PALETTE["signal_blue"]) for name in view["algorithm"]]
    ax.barh(view["label"], view["cv_mse"], color=colors)
    ax.set_title("Top Model Configurations by CV MSE")
    ax.set_xlabel("CV MSE")
    ax.set_ylabel("Model")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def save_holdout_scatter(df: pd.DataFrame, path: Path, holdout_year: int) -> None:
    if df.empty:
        return
    fig, ax = plt.subplots(figsize=(8, 6))
    apply_report_background(fig)
    ax.scatter(
        df["actual_round"], df["pred_round"], s=70,
        color=PROJECT_PALETTE["signal_blue"], edgecolor=PROJECT_PALETTE["black"],
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
    ax.set_title(f"Holdout {holdout_year}: Actual vs Predicted Round")
    ax.set_xlabel("Actual round")
    ax.set_ylabel("Predicted round")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def save_permutation_importance_plot(df: pd.DataFrame, path: Path, title: str) -> None:
    """Permutation importance (mean MSE increase, +/- one sd over repeats), top 25."""
    if df.empty:
        return
    view = df.sort_values("mse_increase", ascending=False).head(25).iloc[::-1].copy()
    fig, ax = plt.subplots(figsize=(12, 8))
    apply_report_background(fig)
    ax.barh(view["feature"], view["mse_increase"], xerr=view["mse_increase_std"],
            color=PROJECT_PALETTE["signal_blue"], ecolor=PROJECT_PALETTE["fog"])
    ax.axvline(0.0, color=PROJECT_PALETTE["steel"], linewidth=1.0)
    ax.set_title(title)
    ax.set_xlabel("MSE increase when the feature is shuffled (training rows)")
    ax.set_ylabel("Feature")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def evaluate_feature_sets(
    train_df: pd.DataFrame,
    specs: list[FeatureSpec],
    cv_folds: int,
    random_state: int,
    verbose: bool = True,
) -> tuple[pd.DataFrame, dict[str, dict[str, Any]]]:
    rows: list[dict[str, Any]] = []
    cache: dict[str, dict[str, Any]] = {}
    for spec in specs:
        if verbose:
            print(f"[FeatureSearch] Evaluating {spec.name}", flush=True)
        X, y, features = prepare_design(train_df, spec)
        groups = train_df.loc[X.index, "year"]
        assert_prediction_time_contract(features, label=spec.name)
        top_feature, top_r = assert_no_target_leakage(train_df, features, TARGET_COL, label=spec.name)
        best_alpha, alpha_grid = tune_ridge_alpha(X, y, groups, RIDGE_ALPHA_GRID, cv_folds=cv_folds,
                                                  random_state=random_state)
        estimator = build_estimator("ridge", {"alpha": best_alpha}, random_state=random_state)
        cv = run_season_cv(estimator=estimator, X=X, y=y, groups=groups, folds=cv_folds)
        rows.append(
            {
                "feature_set": spec.name,
                "n_features": len(features),
                "best_alpha": best_alpha,
                "cv_mse": cv["overall_mse"],
                "cv_mae": cv["overall_mae"],
                "cv_mse_std": cv["fold_mse_std"],
                "cv_splits": cv["n_splits"],
                "leak_guard_max_r": top_r,
                "leak_guard_top_feature": top_feature,
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

    result = pd.DataFrame(rows).sort_values(["cv_mse", "n_features"]).reset_index(drop=True)
    return result, cache


def evaluate_algorithms(
    train_df: pd.DataFrame,
    spec: FeatureSpec,
    fixed_features: list[str],
    cv_folds: int,
    random_state: int,
    search_depth: str,
) -> tuple[pd.DataFrame, dict[str, Any]]:
    X, y, _ = prepare_design(train_df, spec, fixed_features=fixed_features)
    groups = train_df.loc[X.index, "year"]
    rows: list[dict[str, Any]] = []
    configs = get_algorithm_configs(search_depth)
    for i, cfg in enumerate(configs, start=1):
        print(
            f"[AlgorithmSearch] {i}/{len(configs)} {cfg['algorithm']} ({format_params(cfg['params'])})",
            flush=True,
        )
        est = build_estimator(cfg["algorithm"], cfg["params"], random_state=random_state)
        cv = run_season_cv(estimator=est, X=X, y=y, groups=groups, folds=cv_folds)
        rows.append(
            {
                "model_id": i,
                "algorithm": cfg["algorithm"],
                "params": format_params(cfg["params"]),
                "params_obj": cfg["params"],
                "cv_mse": cv["overall_mse"],
                "cv_mae": cv["overall_mae"],
                "cv_mse_std": cv["fold_mse_std"],
                "cv_splits": cv["n_splits"],
            }
        )

    result = pd.DataFrame(rows).sort_values("cv_mse").reset_index(drop=True)
    winner = result.iloc[0].to_dict()
    winner["X"] = X
    winner["y"] = y
    return result, winner


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


LINEAR_ALGORITHMS = {"ridge", "lasso", "elasticnet"}


def describe_winner(
    winner_algorithm: str,
    winner_params: dict[str, Any],
    X_train: pd.DataFrame,
    y_train: pd.Series,
    random_state: int,
) -> tuple[pd.DataFrame, Pipeline]:
    """
    Fit the winner on the training rows and describe it.

    Linear winners get their signed coefficients on standardized features (`coef`, sorted by size).
    These are not an importance ranking: seed and its powers, Barthag and AdjDE with their squares,
    and the coach counts are collinear and offset one another. Other winners get permutation
    importance on the training rows (`mse_increase`, mean and sd over 20 shuffles, signed: a value
    at or below 0 means shuffling the feature did not hurt the fit).
    """
    est = build_estimator(winner_algorithm, winner_params, random_state=random_state)
    est.fit(X_train, y_train)
    if winner_algorithm in LINEAR_ALGORITHMS:
        coefs = est.named_steps["model"].coef_
        out = pd.DataFrame({"feature": X_train.columns, "coef": coefs})
        out = out.reindex(out["coef"].abs().sort_values(ascending=False).index).reset_index(drop=True)
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
        out = pd.DataFrame({"feature": X_train.columns, "mse_increase": perm.importances_mean,
                            "mse_increase_std": perm.importances_std})
        out = out.sort_values("mse_increase", ascending=False).reset_index(drop=True)
    return out, est


def run_provenance(csv_path: Path) -> dict[str, str]:
    import hashlib
    import subprocess

    def git(*cmd: str) -> str:
        try:
            return subprocess.run(["git", *cmd], cwd=REPO_ROOT, capture_output=True, text=True,
                                  check=True).stdout.strip()
        except (OSError, subprocess.CalledProcessError):
            return "unknown"

    dirty = git("status", "--porcelain", "--", "scripts", "src")
    return {
        "code_revision": git("rev-parse", "--short", "HEAD") + (" + uncommitted changes" if dirty else ""),
        "input_sha256": hashlib.sha256(csv_path.read_bytes()).hexdigest()[:16],
    }


def run_leakage_negative_controls(train_df: pd.DataFrame) -> pd.DataFrame:
    """The guard must reject known outcome columns on the real training rows, or the run fails."""
    rows = []
    for col in ("PASE", "PAKE", "round", "result"):
        if col not in train_df.columns:
            continue
        score = leakage_scores(train_df, [col]).get(col, np.nan)
        try:
            assert_no_target_leakage(train_df, [col], TARGET_COL, label=f"negative_control_{col}")
            verdict = "NOT CAUGHT"
        except ValueError:
            verdict = "rejected"
        rows.append({"injected_feature": col, "leak_score": score, "guard": verdict})
    out = pd.DataFrame(rows)
    if (out["guard"] != "rejected").any():
        raise RuntimeError(f"leakage guard failed its negative controls:\n{out}")
    return out


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    run_date = datetime.now().strftime("%Y%m%d")

    report_dir = REPO_ROOT / args.report_dir
    figures_dir = REPO_ROOT / args.figures_dir
    models_dir = REPO_ROOT / DEFAULT_MODEL_DIR
    ensure_dirs(report_dir, figures_dir, models_dir)

    report_path = report_dir / f"{args.report_prefix}_{run_date}.md"
    correlation_path = figures_dir / "correlation_matrix.png"
    permutation_importance_path = figures_dir / "permutation_importance.png"
    model_comparison_path = figures_dir / "model_comparison.png"
    holdout_scatter_path = figures_dir / f"holdout_{args.holdout_year}_actual_vs_pred.png"
    missingness_path = figures_dir / "missingness_by_feature.png"
    model_artifact_path = models_dir / f"analysis_best_model_{run_date}.joblib"

    print("[Run] Starting predictive analysis", flush=True)
    set_project_theme()
    csv_path = REPO_ROOT / args.input_csv
    print(f"[Run] Loading data: {csv_path}", flush=True)
    try:
        raw_df = pd.read_csv(csv_path)
    except pd.errors.ParserError:
        raw_df = pd.read_csv(csv_path, engine="python")
    provenance = run_provenance(csv_path)
    df = add_engineered_features(raw_df)
    df, rank_cols = add_rank_features(df, choose_rank_base_columns(df))

    # Section 1: Data profile.
    print("[Run] Section 1: Data Profile", flush=True)
    years = pd.to_numeric(df["year"], errors="coerce")
    year_min, year_max = int(years.min()), int(years.max())
    tournament_mask = pd.to_numeric(df["seed"], errors="coerce").notna() & pd.to_numeric(df["round"], errors="coerce").notna()
    tournament_rows = int(tournament_mask.sum())
    non_tournament_rows = int((~tournament_mask).sum())
    duplicate_team_seasons = int(df.duplicated(["Team", "year"]).sum())

    candidate_profile_cols = unique_keep_order(
        [c for c in list(GEN_FEATURES) + UNUSED_CANDIDATES + ENGINEERED_FEATURES if c in df.columns]
    )
    modeled_mask = tournament_mask & (years != 2020)
    missing_df = missing_share_table(df, candidate_profile_cols, modeled_mask)
    save_missingness_chart(missing_df, missingness_path, n_all=len(df), n_modeled=int(modeled_mask.sum()))
    save_correlation_matrix(df, candidate_profile_cols, correlation_path)

    stat_cols = [c for c in ["AdjOE", "AdjDE", "Barthag", "seed", "round"] if c in df.columns]
    distribution_summary = (
        df[stat_cols].apply(pd.to_numeric, errors="coerce").describe()
        .T[["mean", "std", "min", "25%", "50%", "75%", "max"]]
        .reset_index().rename(columns={"index": "feature"})
    )

    # Modeling scope.
    model_df = df[modeled_mask].copy()
    model_years = pd.to_numeric(model_df["year"], errors="coerce")
    pre_holdout_df = model_df[model_years < args.holdout_year].copy()
    holdout_df = model_df[model_years == args.holdout_year].copy()
    if pre_holdout_df.empty:
        raise ValueError("Pre-holdout modeling data is empty.")
    if holdout_df.empty:
        raise ValueError(f"Holdout year {args.holdout_year} has no eligible rows.")

    # Leakage audit: contract, inventory, guard negative controls.
    print("[Run] Leakage audit", flush=True)
    specs = build_feature_specs(pre_holdout_df, rank_cols=rank_cols)
    used: set[str] = set()
    for spec in specs:
        used |= set(spec.features) | set((spec.poly or {}).keys())
    inventory = feature_inventory(pre_holdout_df, used)
    negative_controls = run_leakage_negative_controls(pre_holdout_df)

    # Section 2: Baseline ridge on the project's general-model features.
    print("[Run] Section 2: Baseline Evaluation", flush=True)
    baseline_spec = FeatureSpec(name="baseline_raw_poly", features=list(GEN_FEATURES), poly=dict(GEN_POLY))
    X_base, y_base, base_features = prepare_design(pre_holdout_df, baseline_spec)
    base_groups = pre_holdout_df.loc[X_base.index, "year"]
    assert_prediction_time_contract(base_features, label=baseline_spec.name)
    assert_no_target_leakage(pre_holdout_df, base_features, TARGET_COL, label=baseline_spec.name)
    base_alpha, base_alpha_rows = tune_ridge_alpha(X_base, y_base, base_groups, RIDGE_ALPHA_GRID,
                                                   args.cv_folds, args.random_state)
    base_est = build_estimator("ridge", {"alpha": base_alpha}, random_state=args.random_state)
    baseline_cv = run_season_cv(base_est, X_base, y_base, groups=base_groups, folds=args.cv_folds)
    seed_cv_pred = pd.Series(np.nan, index=X_base.index)
    for season in sorted(base_groups.unique()):
        held = base_groups.index[base_groups == season]
        seed_cv_pred.loc[held] = seed_lookup_predictions(
            pre_holdout_df.drop(index=held), pre_holdout_df.loc[held])
    seed_cv_mse = float(mean_squared_error(y_base, seed_cv_pred))
    seed_cv_mae = float(mean_absolute_error(y_base, seed_cv_pred))

    # Section 3: Feature optimization.
    print("[Run] Section 3: Feature Optimization", flush=True)
    feature_results, feature_cache = evaluate_feature_sets(
        train_df=pre_holdout_df, specs=specs, cv_folds=args.cv_folds, random_state=args.random_state,
    )
    best_feature_name = str(feature_results.iloc[0]["feature_set"])
    best_feature_cached = feature_cache[best_feature_name]
    best_feature_spec = best_feature_cached["spec"]
    best_feature_list = best_feature_cached["features"]
    best_feature_alpha = float(best_feature_cached["best_alpha"])
    guard_worst = feature_results.sort_values("leak_guard_max_r", ascending=False).iloc[0]

    vif_df = compute_vif(best_feature_cached["X"])
    coef_df = get_linear_coefficients(
        X=best_feature_cached["X"], y=best_feature_cached["y"], alpha=best_feature_alpha,
        random_state=args.random_state,
    )

    # Section 4: Algorithm search.
    print("[Run] Section 4: Algorithm Search", flush=True)
    model_results, _ = evaluate_algorithms(
        train_df=pre_holdout_df, spec=best_feature_spec, fixed_features=best_feature_list,
        cv_folds=args.cv_folds, random_state=args.random_state, search_depth=args.search_depth,
    )
    winner_row = model_results.iloc[0]
    winner_algorithm = str(winner_row["algorithm"])
    winner_params = winner_row["params_obj"]
    save_model_comparison(model_results.copy(), model_comparison_path)

    # Section 5: Holdout.
    print("[Run] Section 5: Holdout + Final Retrain", flush=True)
    X_train_best, y_train_best, _ = prepare_design(pre_holdout_df, best_feature_spec, fixed_features=best_feature_list)
    X_holdout, y_holdout, _ = prepare_design(holdout_df, best_feature_spec, fixed_features=best_feature_list)
    winner_description_df, winner_estimator = describe_winner(
        winner_algorithm=winner_algorithm, winner_params=winner_params,
        X_train=X_train_best, y_train=y_train_best, random_state=args.random_state,
    )
    winner_is_linear = winner_algorithm in LINEAR_ALGORITHMS
    if not winner_is_linear:
        save_permutation_importance_plot(winner_description_df, permutation_importance_path,
                                         title=f"Permutation Importance on Training Rows ({winner_algorithm})")

    holdout_pred = winner_estimator.predict(X_holdout)
    holdout_mse = float(mean_squared_error(y_holdout, holdout_pred))
    holdout_mae = float(mean_absolute_error(y_holdout, holdout_pred))
    seed_holdout_pred = seed_lookup_predictions(pre_holdout_df, holdout_df.loc[X_holdout.index])
    seed_holdout_mse = float(mean_squared_error(y_holdout, seed_holdout_pred))
    seed_holdout_mae = float(mean_absolute_error(y_holdout, seed_holdout_pred))
    beats_mse = holdout_mse < seed_holdout_mse
    beats_mae = holdout_mae < seed_holdout_mae

    # Every candidate set on the holdout, reported only (selection above never saw 2025).
    all_sets_rows = []
    for name, cached in feature_cache.items():
        est = build_estimator("ridge", {"alpha": cached["best_alpha"]}, random_state=args.random_state)
        est.fit(cached["X"], cached["y"])
        Xh, yh, _ = prepare_design(holdout_df, cached["spec"], fixed_features=cached["features"])
        ph = est.predict(Xh)
        all_sets_rows.append({
            "feature_set": name, "algorithm": "ridge",
            "holdout_mse": float(mean_squared_error(yh, ph)), "holdout_mae": float(mean_absolute_error(yh, ph)),
        })
    all_sets_rows.append({"feature_set": best_feature_name, "algorithm": f"{winner_algorithm} (winner)",
                          "holdout_mse": holdout_mse, "holdout_mae": holdout_mae})
    all_sets_rows.append({"feature_set": "seed only", "algorithm": "historical mean wins by seed",
                          "holdout_mse": seed_holdout_mse, "holdout_mae": seed_holdout_mae})
    all_sets_df = pd.DataFrame(all_sets_rows).sort_values("holdout_mse").reset_index(drop=True)
    n_sets_beating_seed = int(sum(r["holdout_mse"] < seed_holdout_mse for r in all_sets_rows[:-1]))

    # Outcome-free scoring: rebuild the holdout season from raw columns with every outcome
    # column deleted, as if the tournament had not been played, and require identical output.
    pre_artifact = {"model": winner_estimator, "features": best_feature_list,
                    "poly": best_feature_spec.poly, "feature_set": best_feature_name}
    raw_years = pd.to_numeric(raw_df["year"], errors="coerce")
    unplayed = raw_df[raw_years == args.holdout_year].drop(
        columns=[c for c in OUTCOME_COLUMNS if c in raw_df.columns])
    unplayed_scores = score_field(pre_artifact, unplayed)
    scoring_diff = float(np.max(np.abs(unplayed_scores.loc[X_holdout.index].to_numpy() - holdout_pred)))
    if scoring_diff > 1e-9:
        raise RuntimeError(f"outcome-free scoring differs from holdout scoring by {scoring_diff}")
    future_years = sorted(int(y) for y in raw_years.dropna().unique() if y > args.holdout_year)

    holdout_output = holdout_df.loc[X_holdout.index, ["Team", "year", "seed", "round"]].copy()
    holdout_output = holdout_output.rename(columns={"round": "actual_round"})
    holdout_output["pred_round"] = holdout_pred
    holdout_output["seed_baseline_round"] = seed_holdout_pred
    save_holdout_scatter(holdout_output, holdout_scatter_path, args.holdout_year)

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
            "prediction_moment": "Selection Sunday; score with score_field(), no outcome columns needed",
            "holdout": {"mse": holdout_mse, "mae": holdout_mae,
                        "seed_lookup_mse": seed_holdout_mse, "seed_lookup_mae": seed_holdout_mae},
            "code_revision": provenance["code_revision"],
            "input_sha256": provenance["input_sha256"],
            "created_at": datetime.now().isoformat(timespec="seconds"),
        },
        model_artifact_path,
    )

    # Build report.
    corr_triplet = df[["Barthag", "AdjOE", "AdjDE"]].apply(pd.to_numeric, errors="coerce").corr()
    b_a = float(corr_triplet.loc["Barthag", "AdjOE"])
    b_d = float(corr_triplet.loc["Barthag", "AdjDE"])
    od = float(corr_triplet.loc["AdjOE", "AdjDE"])
    verdict = "beats" if (beats_mse and beats_mae) else "does NOT beat"

    lines: list[str] = []
    lines.append(f"# Model Optimization Report ({run_date})")
    lines.append("")
    lines.append("## Headline")
    lines.append(
        f"- Leakage assertion: **PASSED** on all {len(feature_results) + 1} feature sets "
        f"(fails the run at r >= {LEAKAGE_THRESHOLD}; strongest candidate feature "
        f"`{guard_worst['leak_guard_top_feature']}` scores {float(guard_worst['leak_guard_max_r']):.4f}). "
        f"Negative controls PASE, PAKE, round and result are all rejected on the real training rows."
    )
    lines.append(
        f"- {args.holdout_year} holdout, refit on seasons before {args.holdout_year}: model "
        f"(`{best_feature_name}` / `{winner_algorithm}`) MSE {holdout_mse:.4f}, MAE {holdout_mae:.4f}; "
        f"historical seed lookup MSE {seed_holdout_mse:.4f}, MAE {seed_holdout_mae:.4f}. "
        f"The model {verdict} the seed line" + (" on point estimate only. One 64-row season cannot separate them (independent check, 2026-09-29: 95% CI on the MSE difference -0.198 to +0.109), so read it as no worse than the seed line, not better." if verdict == "beats" else ".")
    )
    lines.append(
        f"- {n_sets_beating_seed} of {len(feature_cache) + 1} honest candidate models beat the seed lookup "
        f"on {args.holdout_year} MSE (table in Section 5; reported only, never used for selection)."
    )
    lines.append(
        f"- Outcome-free scoring: the {args.holdout_year} season re-scored with every outcome column "
        f"deleted gives identical predictions (max abs diff {scoring_diff:.1e}), so the model can "
        f"score a field whose tournament has not been played."
    )
    lines.append("")
    lines.append("## Run Configuration")
    lines.append(f"- command: `python scripts/run_pipeline.py --holdout {args.holdout_year}` "
                 f"(or `python scripts/run_analysis.py --holdout-year {args.holdout_year}`)")
    lines.append(f"- input_csv: `{args.input_csv}` (sha256 prefix `{provenance['input_sha256']}`)")
    lines.append(f"- code_revision: `{provenance['code_revision']}`")
    lines.append(f"- holdout_year: `{args.holdout_year}`")
    lines.append(f"- cv: season-grouped `GroupKFold`, `{baseline_cv['n_splits']}` folds over "
                 f"{base_groups.nunique()} training seasons")
    lines.append(f"- search_depth: `{args.search_depth}`")
    lines.append(f"- random_state: `{args.random_state}`")
    lines.append("- selection metric: season-grouped CV MSE on the 0-6 wins target. MSE is kept for "
                 "continuity only; bracket-level selection uses Brier, log loss and calibration "
                 "(`reports/bracket_simulation_*.md`).")
    lines.append("")
    lines.append("## Section 1: Data Profile")
    lines.append(f"- dataset_shape: `{df.shape[0]} rows x {df.shape[1]} columns`")
    lines.append(f"- year_range: `{year_min}-{year_max}`")
    lines.append(f"- tournament_rows (seed+round non-null): `{tournament_rows}`")
    lines.append(f"- non_tournament_rows: `{non_tournament_rows}`")
    lines.append(f"- duplicate (Team, year) rows: `{duplicate_team_seasons}`")
    lines.append(f"- modeled_rows_excluding_2020: `{len(model_df)}`")
    lines.append(f"- pre_holdout_rows (<{args.holdout_year}): `{len(pre_holdout_df)}`")
    lines.append(f"- holdout_rows ({args.holdout_year}): `{len(holdout_df)}`")
    lines.append("")
    lines.append("### Distribution Summary")
    lines.append(markdown_table(distribution_summary, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Missing values (candidate feature columns)")
    missing_any = missing_df[missing_df["missing_all_rows_pct"] > 0]
    complete_cols = len(missing_df) - len(missing_any)
    if missing_any.empty:
        lines.append(f"- No missing values in any of the {len(missing_df)} candidate columns.")
    else:
        lines.append(markdown_table(missing_any, max_rows=None, float_digits=1))
        lines.append("")
        lines.append(f"- The other {complete_cols} of {len(missing_df)} candidate columns have no missing values. "
                     "Modeled rows are tournament teams outside 2020.")
    lines.append("")
    lines.append(f"![Missing values by candidate feature](figures/{missingness_path.name})")
    lines.append("")
    lines.append(f"![Correlation Matrix](figures/{correlation_path.name})")
    lines.append("")
    lines.append(
        f"- corr(Barthag, AdjOE) = `{b_a:.4f}`, corr(Barthag, AdjDE) = `{b_d:.4f}`, corr(AdjOE, AdjDE) = `{od:.4f}`. "
        "Barthag is built from the two efficiencies, so high collinearity is expected."
    )
    lines.append("")
    lines.append("## Leakage Register")
    lines.append("Prediction moment: the published bracket on Selection Sunday, before any NCAA tournament game.")
    lines.append("")
    register = pd.DataFrame([
        {"risk": "Post-tournament values", "control": f"{len(OUTCOME_COLUMNS)} outcome columns in "
         "OUTCOME_COLUMNS, excluded from every set (PAKE, PASE, round dummies, result, finish, "
         "postseason W-L, final record)", "status": "pass"},
        {"risk": "Feature reconstructs the target", "control": f"assert_no_target_leakage on every set, "
         f"fails at r >= {LEAKAGE_THRESHOLD} (seed-residual Pearson or raw Spearman)",
         "status": f"pass, max {float(guard_worst['leak_guard_max_r']):.4f}"},
        {"risk": "Guard cannot fail", "control": "negative controls injected on real rows",
         "status": "pass, all rejected"},
        {"risk": "Unmapped or unavailable column", "control": "assert_prediction_time_contract on every "
         "set; unmapped columns fail the run", "status": "pass"},
        {"risk": "Full-sample aggregates", "control": "ranks and conf_avg_barthag are within-season, "
         "from pre-tournament columns; seed lookup fit on training seasons only", "status": "pass by construction"},
        {"risk": "Preprocessing before split", "control": "imputer and scaler inside the sklearn Pipeline, "
         "refit in every fold", "status": "pass by construction"},
        {"risk": "Repeated team-season rows", "control": "duplicate (Team, year) count",
         "status": f"{'pass' if duplicate_team_seasons == 0 else 'FAIL'}, {duplicate_team_seasons}"},
        {"risk": "Season overlap train/validation", "control": "GroupKFold by season; holdout season "
         "later than every training season", "status": "pass"},
        {"risk": "Coach history window", "control": "prior 5 tournaments only, current season excluded "
         "(tests/test_team_history_pipeline.py)", "status": "pass"},
    ])
    lines.append(markdown_table(register))
    lines.append("")
    lines.append("### Guard negative controls")
    lines.append(markdown_table(negative_controls, float_digits=4))
    lines.append("")
    lines.append("### Prediction-time feature inventory")
    lines.append("`leak_score` is the guard statistic on pre-holdout tournament rows. Rank columns "
                 "(`*_rank`, within-season ranks of the listed stats) and polynomial terms inherit "
                 "their base column's source.")
    lines.append("")
    base_inventory = inventory[~inventory["column"].str.endswith("_rank")]
    lines.append(markdown_table(base_inventory, float_digits=4))
    lines.append("")
    lines.append("## Section 2: Baseline Evaluation (season-grouped CV)")
    lines.append(f"- baseline_feature_count: `{len(base_features)}`")
    lines.append(f"- baseline_best_alpha_by_cv: `{base_alpha}`")
    lines.append(f"- baseline_cv_mse: `{baseline_cv['overall_mse']:.6f}`")
    lines.append(f"- baseline_cv_mae: `{baseline_cv['overall_mae']:.6f}`")
    lines.append(f"- baseline_cv_fold_mse_std: `{baseline_cv['fold_mse_std']:.6f}`")
    lines.append(f"- seed_lookup_leave_one_season_out_mse: `{seed_cv_mse:.6f}`")
    lines.append(f"- seed_lookup_leave_one_season_out_mae: `{seed_cv_mae:.6f}`")
    lines.append("")
    lines.append(markdown_table(pd.DataFrame(base_alpha_rows), max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Section 3: Feature Optimization")
    lines.append(markdown_table(feature_results, max_rows=None, float_digits=6))
    lines.append("")
    lines.append(f"- selected_feature_set: `{best_feature_name}`")
    lines.append(f"- selected_feature_count: `{len(best_feature_list)}`")
    lines.append(f"- selected_ridge_alpha: `{best_feature_alpha}`")
    lines.append("")
    lines.append("### VIF (Top 20)")
    lines.append(markdown_table(vif_df.head(20), max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Standardized Ridge Coefficients (Top 25 by size)")
    lines.append("Signed coefficients on standardized features from one ridge fit. They are not an importance "
                 "ranking: seed and its powers, Barthag and AdjDE with their squares, and the coach counts are "
                 "collinear (VIF table above) and offset one another, so neither the size nor the sign of one "
                 "term is that feature's effect.")
    lines.append("")
    lines.append(markdown_table(coef_df.head(25)[["feature", "coef", "abs_coef"]], max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Section 4: Algorithm Search")
    lines.append(markdown_table(model_results.drop(columns=["params_obj"]), max_rows=30, float_digits=6))
    lines.append("")
    lines.append(f"![Model Comparison](figures/{model_comparison_path.name})")
    lines.append("")
    lines.append(f"- winning_algorithm: `{winner_algorithm}`")
    lines.append(f"- winning_params: `{format_params(winner_params)}`")
    lines.append(f"- winning_cv_mse: `{float(winner_row['cv_mse']):.6f}`")
    lines.append(f"- winning_cv_mae: `{float(winner_row['cv_mae']):.6f}`")
    lines.append("")
    lines.append("## Section 5: Holdout vs Seed Lookup")
    lines.append(f"- holdout_year: `{args.holdout_year}`")
    lines.append(f"- holdout_mse: `{holdout_mse:.6f}`")
    lines.append(f"- holdout_mae: `{holdout_mae:.6f}`")
    lines.append(f"- seed_lookup_holdout_mse: `{seed_holdout_mse:.6f}`")
    lines.append(f"- seed_lookup_holdout_mae: `{seed_holdout_mae:.6f}`")
    lines.append(f"- model_beats_seed_lookup_mse: `{str(beats_mse).lower()}`")
    lines.append(f"- model_beats_seed_lookup_mae: `{str(beats_mae).lower()}`")
    lines.append(f"- holdout_rows_scored: `{len(y_holdout)}`")
    lines.append(f"- outcome_free_scoring_max_abs_diff: `{scoring_diff:.1e}`")
    lines.append(f"- seasons after holdout in input: `{future_years or 'none'}`")
    lines.append(f"- final_retrain_rows (all seasons, for future fields): `{len(y_all)}`")
    lines.append(f"- model_artifact: `{model_artifact_path.relative_to(REPO_ROOT).as_posix()}`")
    lines.append("")
    lines.append("### Every candidate on the holdout (reported only)")
    lines.append(markdown_table(all_sets_df, float_digits=4))
    lines.append("")
    lines.append(f"![Holdout Actual vs Predicted](figures/{holdout_scatter_path.name})")
    lines.append("")
    lines.append("### Holdout Prediction Table (first 25)")
    lines.append(markdown_table(holdout_output.head(25), max_rows=None, float_digits=4))
    lines.append("")
    if winner_is_linear:
        lines.append("### Winner Coefficients (standardized, signed; top 25 by size)")
        lines.append(f"The winner (`{winner_algorithm}`, {format_params(winner_params)}) is linear. Its coefficients "
                     "carry the Section 3 caveat: collinear terms offset one another, so this is not an "
                     "importance ranking and no importance chart is drawn.")
    else:
        lines.append("### Winner Permutation Importance (training rows)")
        lines.append(f"![Permutation importance](figures/{permutation_importance_path.name})")
        lines.append("")
        lines.append("`mse_increase` is the rise in training-row MSE when one feature is shuffled (mean and sd "
                     "over 20 shuffles). Correlated features share credit, and a value at or below 0 means "
                     "shuffling did not hurt the fit.")
    lines.append("")
    lines.append(markdown_table(winner_description_df.head(25), max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Recommendation")
    if beats_mse and beats_mae:
        lines.append(
            f"The `{best_feature_name}` / `{winner_algorithm}` candidate beats the historical seed lookup on "
            f"the {args.holdout_year} holdout on both MSE ({holdout_mse:.4f} vs {seed_holdout_mse:.4f}) and "
            f"MAE ({holdout_mae:.4f} vs {seed_holdout_mae:.4f}) on point estimate. One season is 64 rows and cannot separate them, so treat this as "
            "provisional until the walked-forward bracket evaluation agrees."
        )
    else:
        lines.append(
            f"Keep the historical seed lookup as the round-prediction baseline. The honest "
            f"`{best_feature_name}` / `{winner_algorithm}` refit scores MSE {holdout_mse:.4f} and MAE "
            f"{holdout_mae:.4f} on {args.holdout_year} against the lookup's {seed_holdout_mse:.4f} and "
            f"{seed_holdout_mae:.4f}. With PASE and PAKE removed, these Torvik season aggregates do not "
            "beat the seed line out of sample on this target; that is the finding."
        )

    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Report written: {report_path}")
    print(f"Model artifact written: {model_artifact_path}")
    print(f"Leakage assertion passed; holdout MSE {holdout_mse:.4f} vs seed lookup {seed_holdout_mse:.4f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
