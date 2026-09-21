#!/usr/bin/env python3
"""CV-first predictive analysis for NCAA tournament finish modeling."""

from __future__ import annotations

import argparse
import math
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.base import clone
from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.inspection import permutation_importance
from sklearn.linear_model import ElasticNet, Lasso, Ridge
from sklearn.metrics import mean_absolute_error, mean_squared_error
from sklearn.model_selection import LeaveOneOut, StratifiedKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.pipelines.madness import GEN_FEATURES, GEN_POLY  # noqa: E402
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
LEAKY_COLUMNS = {"PAKE", "PASE"}
UNUSED_CANDIDATES = ["ELITESOS", "FT%", "OPFT%", "PPPOFF", "PPPDEF"]
ENGINEERED_FEATURES = ["eff_ratio", "seed_barthag", "coach_seed"]
TARGET_COL = "round"
OUTCOME_COLUMNS = {"round", "Round2", "S16", "E8", "F4", "Ship", "Champ", "result", "predround"}
# anything derived from the outcome is excluded from every feature set, not just the target
EXCLUDE_FROM_FEATURES = OUTCOME_COLUMNS | LEAKY_COLUMNS

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
    parser = argparse.ArgumentParser(description="Run CV-first predictive analysis on cbb_training_clean.csv.")
    parser.add_argument("--input-csv", default=DEFAULT_INPUT, help="Path to cleaned training CSV.")
    parser.add_argument("--report-dir", default=DEFAULT_REPORT_DIR, help="Directory for markdown reports.")
    parser.add_argument("--figures-dir", default=DEFAULT_FIGURES_DIR, help="Directory for PNG outputs.")
    parser.add_argument("--holdout-year", type=int, default=DEFAULT_HOLDOUT_YEAR, help="Final blind holdout year.")
    parser.add_argument(
        "--cv-folds",
        type=int,
        default=DEFAULT_CV_FOLDS,
        help="Requested stratified CV folds; reduced automatically when needed.",
    )
    parser.add_argument("--random-state", type=int, default=DEFAULT_RANDOM_STATE, help="Random seed.")
    parser.add_argument(
        "--search-depth",
        choices=["fast", "balanced", "exhaustive"],
        default=DEFAULT_SEARCH_DEPTH,
        help="Hyperparameter search depth.",
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


def assert_no_target_leakage(df: pd.DataFrame, features: list[str], target: str,
                             condition: str = "seed", threshold: float = 0.90,
                             label: str = "") -> None:
    """Fail loudly if a feature reconstructs the target once seed is accounted for.

    A plain corr(feature, target) does NOT catch this: PASE against round is only 0.767,
    because PASE IS wins minus seed expectation, so the identity needs two variables.
    Residualising the target against seed first separates them cleanly, measured on the
    1,088-row training set:

        corr(resid_seed(round), PASE)  = 0.9948
        corr(resid_seed(round), PAKE)  = 0.9553
        corr(resid_seed(round), AdjOE) = 0.0580   <- clean control

    The PASE leak scored 0.060 MAE on a 0-6 target and read as a great model for months.
    """
    y = pd.to_numeric(df[target], errors="coerce")
    cond = pd.to_numeric(df[condition], errors="coerce") if condition in df.columns else None

    bad = []
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
        if resid.std() == 0:
            continue
        r = abs(float(np.corrcoef(resid, x[ok])[0, 1]))
        if r >= threshold:
            bad.append((col, r))

    if bad:
        detail = ", ".join(f"{c} r={r:.4f}" for c, r in sorted(bad, key=lambda t: -t[1]))
        raise ValueError(
            f"target leakage in feature set {label or '(unnamed)'}: {detail}. "
            f"Against '{target}' residualised on '{condition}', r>={threshold} means the "
            f"feature is derived from the outcome, not a predictor of it."
        )


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


def effective_cv_folds(y: pd.Series, requested_folds: int) -> int:
    labels = pd.to_numeric(y, errors="coerce").round().astype(int)
    min_class = int(labels.value_counts().min())
    return max(2, min(int(requested_folds), min_class))


def build_estimator(algorithm: str, params: dict[str, Any], random_state: int) -> Pipeline:
    if algorithm == "ridge":
        model = Ridge(alpha=float(params["alpha"]), random_state=random_state, fit_intercept=False)
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


def run_stratified_cv(
    estimator: Pipeline,
    X: pd.DataFrame,
    y: pd.Series,
    folds: int,
    random_state: int,
) -> dict[str, Any]:
    labels = pd.to_numeric(y, errors="coerce").round().astype(int)
    n_splits = effective_cv_folds(y, folds)
    splitter = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=random_state)

    fold_rows: list[dict[str, float]] = []
    all_pred: list[float] = []
    all_actual: list[float] = []
    for fold_i, (train_idx, test_idx) in enumerate(splitter.split(X, labels), start=1):
        model = clone(estimator)
        X_train = X.iloc[train_idx]
        X_test = X.iloc[test_idx]
        y_train = y.iloc[train_idx]
        y_test = y.iloc[test_idx]

        model.fit(X_train, y_train)
        pred = model.predict(X_test)
        mse = mean_squared_error(y_test, pred)
        mae = mean_absolute_error(y_test, pred)
        fold_rows.append({"fold": float(fold_i), "mse": float(mse), "mae": float(mae)})
        all_pred.extend(pred.tolist())
        all_actual.extend(y_test.tolist())

    return {
        "overall_mse": float(mean_squared_error(all_actual, all_pred)),
        "overall_mae": float(mean_absolute_error(all_actual, all_pred)),
        "fold_mse_std": float(np.std([r["mse"] for r in fold_rows])),
        "n_splits": int(n_splits),
        "fold_rows": fold_rows,
    }


def run_loo(
    estimator: Pipeline,
    X: pd.DataFrame,
    y: pd.Series,
    progress_label: str | None = None,
) -> dict[str, float]:
    loo = LeaveOneOut()
    preds = np.zeros(len(X), dtype=float)
    actual = y.to_numpy(dtype=float)
    if progress_label:
        print(f"[LOO] {progress_label}: {len(X)} fits", flush=True)
    for i, (train_idx, test_idx) in enumerate(loo.split(X)):
        model = clone(estimator)
        X_train = X.iloc[train_idx]
        X_test = X.iloc[test_idx]
        y_train = y.iloc[train_idx]
        model.fit(X_train, y_train)
        preds[i] = float(model.predict(X_test)[0])
        if progress_label and (i + 1) % 250 == 0:
            print(f"[LOO] {progress_label}: {i + 1}/{len(X)}", flush=True)
    return {
        "mse": float(mean_squared_error(actual, preds)),
        "mae": float(mean_absolute_error(actual, preds)),
    }


def tune_ridge_alpha(
    X: pd.DataFrame,
    y: pd.Series,
    alpha_grid: list[float],
    cv_folds: int,
    random_state: int,
) -> tuple[float, list[dict[str, float]]]:
    rows: list[dict[str, float]] = []
    for alpha in alpha_grid:
        est = build_estimator("ridge", {"alpha": float(alpha)}, random_state=random_state)
        cv = run_stratified_cv(estimator=est, X=X, y=y, folds=cv_folds, random_state=random_state)
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
                else:
                    rendered.append("inf")
            else:
                rendered.append(str(val))
        lines.append("| " + " | ".join(rendered) + " |")
    return "\n".join(lines)


def save_missingness_heatmap(df: pd.DataFrame, cols: list[str], path: Path) -> None:
    if not cols:
        return
    fig = plt.figure(figsize=(14, 6))
    apply_report_background(fig)
    sns.heatmap(
        df[cols].isna().astype(int),
        cbar=False,
        cmap=CBB_SEQUENTIAL_CMAP,
        linewidths=0.1,
        linecolor=PROJECT_PALETTE["storm"],
    )
    plt.title("Missingness Heatmap (Model Candidate Features)")
    plt.xlabel("Feature")
    plt.ylabel("Row")
    plt.tight_layout()
    plt.savefig(path, dpi=160)
    plt.close()


def save_correlation_matrix(df: pd.DataFrame, cols: list[str], path: Path) -> None:
    if not cols:
        return
    corr = df[cols].apply(pd.to_numeric, errors="coerce").corr()
    fig = plt.figure(figsize=(14, 10))
    apply_report_background(fig)
    sns.heatmap(
        corr,
        cmap=CBB_DIVERGING_CMAP,
        center=0,
        annot=False,
        linewidths=0.15,
        linecolor=PROJECT_PALETTE["storm"],
    )
    plt.title("Feature Correlation Matrix")
    plt.tight_layout()
    plt.savefig(path, dpi=160)
    plt.close()


def save_model_comparison(df: pd.DataFrame, path: Path) -> None:
    if df.empty:
        return
    view = df.sort_values("cv_mse").head(20).copy()
    view["label"] = view["algorithm"] + " | " + view["params"]
    fig = plt.figure(figsize=(12, 8))
    apply_report_background(fig)
    sns.barplot(
        data=view,
        y="label",
        x="cv_mse",
        hue="algorithm",
        orient="h",
        palette=ALGORITHM_COLORS,
    )
    plt.title("Top Model Configurations by CV MSE")
    plt.xlabel("CV MSE")
    plt.ylabel("Model")
    plt.legend(loc="best")
    plt.tight_layout()
    plt.savefig(path, dpi=160)
    plt.close()


def save_holdout_scatter(df: pd.DataFrame, path: Path, holdout_year: int) -> None:
    if df.empty:
        return
    fig = plt.figure(figsize=(8, 6))
    apply_report_background(fig)
    sns.scatterplot(
        data=df,
        x="actual_round",
        y="pred_round",
        s=70,
        color=PROJECT_PALETTE["signal_blue"],
        edgecolor=PROJECT_PALETTE["black"],
    )
    min_v = min(df["actual_round"].min(), df["pred_round"].min())
    max_v = max(df["actual_round"].max(), df["pred_round"].max())
    plt.plot(
        [min_v, max_v],
        [min_v, max_v],
        linestyle="--",
        color=PROJECT_PALETTE["sage"],
        linewidth=1.8,
    )
    plt.title(f"Holdout {holdout_year}: Actual vs Predicted Round")
    plt.xlabel("Actual round")
    plt.ylabel("Predicted round")
    plt.tight_layout()
    plt.savefig(path, dpi=160)
    plt.close()


def save_feature_importance_plot(df: pd.DataFrame, path: Path, title: str) -> None:
    if df.empty:
        return
    view = df.sort_values("importance", ascending=False).head(25).copy()
    fig = plt.figure(figsize=(12, 8))
    apply_report_background(fig)
    sns.barplot(
        data=view,
        y="feature",
        x="importance",
        orient="h",
        color=PROJECT_PALETTE["signal_blue"],
    )
    plt.title(title)
    plt.xlabel("Importance")
    plt.ylabel("Feature")
    plt.tight_layout()
    plt.savefig(path, dpi=160)
    plt.close()


def evaluate_feature_sets(
    train_df: pd.DataFrame,
    specs: list[FeatureSpec],
    cv_folds: int,
    random_state: int,
) -> tuple[pd.DataFrame, dict[str, dict[str, Any]]]:
    rows: list[dict[str, Any]] = []
    cache: dict[str, dict[str, Any]] = {}
    for spec in specs:
        print(f"[FeatureSearch] Evaluating {spec.name}", flush=True)
        X, y, features = prepare_design(train_df, spec)
        assert_no_target_leakage(train_df, features, TARGET_COL, label=spec.name)
        best_alpha, alpha_grid = tune_ridge_alpha(X, y, RIDGE_ALPHA_GRID, cv_folds=cv_folds, random_state=random_state)
        estimator = build_estimator("ridge", {"alpha": best_alpha}, random_state=random_state)
        cv = run_stratified_cv(estimator=estimator, X=X, y=y, folds=cv_folds, random_state=random_state)
        rows.append(
            {
                "feature_set": spec.name,
                "n_features": len(features),
                "best_alpha": best_alpha,
                "cv_mse": cv["overall_mse"],
                "cv_mae": cv["overall_mae"],
                "cv_mse_std": cv["fold_mse_std"],
                "cv_splits": cv["n_splits"],
                "loo_mse": np.nan,
                "loo_mae": np.nan,
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
    top_feature_names = result.head(3)["feature_set"].tolist()
    for name in top_feature_names:
        row_idx = result.index[result["feature_set"] == name][0]
        cached = cache[name]
        est = build_estimator("ridge", {"alpha": cached["best_alpha"]}, random_state=random_state)
        loo = run_loo(estimator=est, X=cached["X"], y=cached["y"], progress_label=f"feature_set={name}")
        result.loc[row_idx, "loo_mse"] = loo["mse"]
        result.loc[row_idx, "loo_mae"] = loo["mae"]
        cache[name]["loo"] = loo
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
    rows: list[dict[str, Any]] = []
    configs = get_algorithm_configs(search_depth)
    for i, cfg in enumerate(configs, start=1):
        print(
            f"[AlgorithmSearch] {i}/{len(configs)} {cfg['algorithm']} ({format_params(cfg['params'])})",
            flush=True,
        )
        est = build_estimator(cfg["algorithm"], cfg["params"], random_state=random_state)
        cv = run_stratified_cv(estimator=est, X=X, y=y, folds=cv_folds, random_state=random_state)
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
                "loo_mse": np.nan,
                "loo_mae": np.nan,
            }
        )

    result = pd.DataFrame(rows).sort_values("cv_mse").reset_index(drop=True)
    if search_depth == "fast":
        linear_mask = result["algorithm"].isin(["ridge", "lasso", "elasticnet"])
        loo_indices = result[linear_mask].head(3).index.tolist()
        if len(loo_indices) < 3:
            fill = [i for i in result.index.tolist() if i not in loo_indices][: 3 - len(loo_indices)]
            loo_indices.extend(fill)
    else:
        loo_indices = result.head(3).index.tolist()

    for idx in loo_indices:
        row = result.loc[idx]
        est = build_estimator(row["algorithm"], row["params_obj"], random_state=random_state)
        label = f"algorithm={row['algorithm']}, params={row['params']}"
        loo = run_loo(estimator=est, X=X, y=y, progress_label=label)
        result.loc[idx, "loo_mse"] = loo["mse"]
        result.loc[idx, "loo_mae"] = loo["mae"]

    winner = result.iloc[0].to_dict()
    winner["X"] = X
    winner["y"] = y
    return result, winner


def best_feature_set_row(df: pd.DataFrame) -> pd.Series:
    ranked = df.copy()
    ranked["loo_for_rank"] = ranked["loo_mse"].fillna(np.inf)
    return ranked.sort_values(["cv_mse", "loo_for_rank", "n_features"], ascending=[True, True, True]).iloc[0]


def best_model_row(df: pd.DataFrame) -> pd.Series:
    ranked = df.copy()
    ranked["loo_for_rank"] = ranked["loo_mse"].fillna(np.inf)
    return ranked.sort_values(["cv_mse", "loo_for_rank"], ascending=[True, True]).iloc[0]


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


def main() -> int:
    args = parse_args()
    run_date = datetime.now().strftime("%Y%m%d")

    report_dir = REPO_ROOT / args.report_dir
    figures_dir = REPO_ROOT / args.figures_dir
    models_dir = REPO_ROOT / DEFAULT_MODEL_DIR
    ensure_dirs(report_dir, figures_dir, models_dir)

    report_path = report_dir / f"predictive_analysis_{run_date}.md"
    correlation_path = figures_dir / "correlation_matrix.png"
    feature_importance_path = figures_dir / "feature_importance.png"
    model_comparison_path = figures_dir / "model_comparison.png"
    holdout_scatter_path = figures_dir / f"holdout_{args.holdout_year}_actual_vs_pred.png"
    missingness_path = figures_dir / "missingness_heatmap.png"
    model_artifact_path = models_dir / f"analysis_best_model_{run_date}.joblib"

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

    # Modeling scope.
    model_df = df[tournament_mask].copy()
    model_df = model_df[pd.to_numeric(model_df["year"], errors="coerce") != 2020].copy()
    pre_holdout_df = model_df[pd.to_numeric(model_df["year"], errors="coerce") < args.holdout_year].copy()
    holdout_df = model_df[pd.to_numeric(model_df["year"], errors="coerce") == args.holdout_year].copy()
    if pre_holdout_df.empty:
        raise ValueError("Pre-holdout modeling data is empty.")
    if holdout_df.empty:
        raise ValueError(f"Holdout year {args.holdout_year} has no eligible rows.")

    # Section 2: Baseline evaluation (CV + LOO).
    print("[Run] Section 2: Baseline Evaluation", flush=True)
    baseline_spec = FeatureSpec(name="baseline_raw_poly", features=list(GEN_FEATURES), poly=dict(GEN_POLY))
    X_base, y_base, base_features = prepare_design(pre_holdout_df, baseline_spec)
    base_alpha, base_alpha_rows = tune_ridge_alpha(X_base, y_base, RIDGE_ALPHA_GRID, args.cv_folds, args.random_state)
    base_est = build_estimator("ridge", {"alpha": base_alpha}, random_state=args.random_state)
    baseline_cv = run_stratified_cv(base_est, X_base, y_base, folds=args.cv_folds, random_state=args.random_state)
    baseline_loo = run_loo(base_est, X_base, y_base, progress_label="baseline_ridge")

    # Section 3: Feature optimization.
    print("[Run] Section 3: Feature Optimization", flush=True)
    specs = build_feature_specs(pre_holdout_df, rank_cols=rank_cols)
    feature_results, feature_cache = evaluate_feature_sets(
        train_df=pre_holdout_df,
        specs=specs,
        cv_folds=args.cv_folds,
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

    # Section 4: Algorithm search.
    print("[Run] Section 4: Algorithm Search", flush=True)
    model_results, _ = evaluate_algorithms(
        train_df=pre_holdout_df,
        spec=best_feature_spec,
        fixed_features=best_feature_list,
        cv_folds=args.cv_folds,
        random_state=args.random_state,
        search_depth=args.search_depth,
    )
    winner_row = best_model_row(model_results)
    winner_algorithm = str(winner_row["algorithm"])
    winner_params = winner_row["params_obj"]
    save_model_comparison(model_results.copy(), model_comparison_path)

    # Section 5: Holdout + final retrain.
    print("[Run] Section 5: Holdout + Final Retrain", flush=True)
    X_train_best, y_train_best, _ = prepare_design(pre_holdout_df, best_feature_spec, fixed_features=best_feature_list)
    X_holdout, y_holdout, _ = prepare_design(holdout_df, best_feature_spec, fixed_features=best_feature_list)
    winner_importance_df, winner_estimator = get_winner_importance(
        winner_algorithm=winner_algorithm,
        winner_params=winner_params,
        X_train=X_train_best,
        y_train=y_train_best,
        random_state=args.random_state,
    )
    save_feature_importance_plot(
        winner_importance_df,
        feature_importance_path,
        title=f"Feature Importance ({winner_algorithm})",
    )

    holdout_pred = winner_estimator.predict(X_holdout)
    holdout_mse = float(mean_squared_error(y_holdout, holdout_pred))
    holdout_mae = float(mean_absolute_error(y_holdout, holdout_pred))

    holdout_output = holdout_df.loc[X_holdout.index, ["Team", "year", "seed", "round"]].copy()
    holdout_output = holdout_output.rename(columns={"round": "actual_round"})
    holdout_output["pred_round"] = holdout_pred
    save_holdout_scatter(holdout_output, holdout_scatter_path, args.holdout_year)

    all_eligible_df = model_df.copy()
    X_all, y_all, _ = prepare_design(all_eligible_df, best_feature_spec, fixed_features=best_feature_list)
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
            "created_at": datetime.now().isoformat(timespec="seconds"),
        },
        model_artifact_path,
    )

    # Build report.
    corr_triplet = df[["Barthag", "AdjOE", "AdjDE"]].apply(pd.to_numeric, errors="coerce").corr()
    b_a = float(corr_triplet.loc["Barthag", "AdjOE"])
    b_d = float(corr_triplet.loc["Barthag", "AdjDE"])
    od = float(corr_triplet.loc["AdjOE", "AdjDE"])

    lines: list[str] = []
    lines.append(f"# Predictive Analysis Report ({run_date})")
    lines.append("")
    lines.append("## Run Configuration")
    lines.append(f"- input_csv: `{args.input_csv}`")
    lines.append(f"- holdout_year: `{args.holdout_year}`")
    lines.append(f"- cv_folds_requested: `{args.cv_folds}`")
    lines.append(f"- search_depth: `{args.search_depth}`")
    lines.append(f"- random_state: `{args.random_state}`")
    lines.append("")
    lines.append("## Section 1: Data Profile")
    lines.append(f"- dataset_shape: `{df.shape[0]} rows x {df.shape[1]} columns`")
    lines.append(f"- year_range: `{year_min}-{year_max}`")
    lines.append(f"- tournament_rows (seed+round non-null): `{tournament_rows}`")
    lines.append(f"- non_tournament_rows: `{non_tournament_rows}`")
    lines.append(f"- modeled_rows_excluding_2020: `{len(model_df)}`")
    lines.append(f"- pre_holdout_rows (<{args.holdout_year}): `{len(pre_holdout_df)}`")
    lines.append(f"- holdout_rows ({args.holdout_year}): `{len(holdout_df)}`")
    lines.append("")
    lines.append("### Distribution Summary")
    lines.append(markdown_table(distribution_summary, max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Missingness (candidate feature columns)")
    lines.append(f"![Missingness Heatmap](figures/{missingness_path.name})")
    lines.append("")
    lines.append("### Correlation Matrix")
    lines.append(f"![Correlation Matrix](figures/{correlation_path.name})")
    lines.append("")
    lines.append("### Collinearity Note")
    lines.append(
        f"- corr(Barthag, AdjOE) = `{b_a:.4f}`, corr(Barthag, AdjDE) = `{b_d:.4f}`, corr(AdjOE, AdjDE) = `{od:.4f}`."
    )
    lines.append("- Barthag is derived from offensive and defensive efficiency, so high multicollinearity is expected.")
    lines.append("")
    lines.append("## Section 2: Baseline Evaluation (CV + LOO)")
    lines.append(f"- baseline_feature_count: `{len(base_features)}`")
    lines.append(f"- baseline_best_alpha_by_cv: `{base_alpha}`")
    lines.append(f"- baseline_cv_mse: `{baseline_cv['overall_mse']:.6f}`")
    lines.append(f"- baseline_cv_mae: `{baseline_cv['overall_mae']:.6f}`")
    lines.append(f"- baseline_cv_fold_mse_std: `{baseline_cv['fold_mse_std']:.6f}`")
    lines.append(f"- baseline_cv_effective_splits: `{baseline_cv['n_splits']}`")
    lines.append(f"- baseline_loo_mse: `{baseline_loo['mse']:.6f}`")
    lines.append(f"- baseline_loo_mae: `{baseline_loo['mae']:.6f}`")
    lines.append("")
    lines.append("### Baseline Alpha Grid")
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
    lines.append("### Scaled Ridge Coefficients (Top 25)")
    coef_view = coef_df.head(25).copy()
    coef_view = coef_view[["feature", "coef", "abs_coef"]]
    lines.append(markdown_table(coef_view, max_rows=None, float_digits=6))
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
    if pd.notna(winner_row["loo_mse"]):
        lines.append(f"- winning_loo_mse: `{float(winner_row['loo_mse']):.6f}`")
        lines.append(f"- winning_loo_mae: `{float(winner_row['loo_mae']):.6f}`")
    lines.append("")
    lines.append("## Section 5: Holdout + Final Retrain")
    lines.append(f"- holdout_year: `{args.holdout_year}`")
    lines.append(f"- holdout_mse: `{holdout_mse:.6f}`")
    lines.append(f"- holdout_mae: `{holdout_mae:.6f}`")
    lines.append(f"- holdout_rows_scored: `{len(y_holdout)}`")
    lines.append(f"- final_retrain_rows: `{len(y_all)}`")
    lines.append(f"- model_artifact: `{model_artifact_path.relative_to(REPO_ROOT)}`")
    lines.append("")
    lines.append(f"![Holdout Actual vs Predicted](figures/{holdout_scatter_path.name})")
    lines.append("")
    lines.append("### Holdout Prediction Table")
    lines.append(markdown_table(holdout_output.head(25), max_rows=None, float_digits=4))
    lines.append("")
    lines.append("### Winner Feature Importance")
    lines.append(f"![Feature Importance](figures/{feature_importance_path.name})")
    lines.append("")
    lines.append(markdown_table(winner_importance_df.head(25), max_rows=None, float_digits=6))
    lines.append("")
    lines.append("## Recommendation")
    lines.append(
        f"Replace the current general model candidate with feature set `{best_feature_name}` and "
        f"algorithm `{winner_algorithm}` ({format_params(winner_params)}), based on lowest "
        "pre-holdout CV MSE and confirmed holdout performance on 2025."
    )

    report_path.write_text("\n".join(lines), encoding="utf-8")
    print(f"Report written: {report_path}")
    print(f"Model artifact written: {model_artifact_path}")
    print(f"Figures directory: {figures_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
