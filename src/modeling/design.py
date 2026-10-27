"""Design matrices: model feature lists, polynomial terms, and the tournament rows the models use."""

from __future__ import annotations

import pandas as pd

from src.config import DEFAULT_CONFIG

# General model features. Without the coach columns this is the feature list of the predround
# model in madness.R (commit 21a1e96) minus WAB, hand-tuned there by row-level jackknife MSE on
# the 2008-2024 tournaments; the coach columns were added later (see docs/modeling_plan.md).
# This is every candidate; general_features() gives the list a general model is fit on.
GEN_FEATURES = [
    "TOR", "Barthag", "AdjOE", "ORB", "EFGD%", "3P%", "AdjDE",
    "seed", "TALENT", "AST%",
    "coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_years_with_program",
    "coach_tourn_win_pct_5y", "coach_tourn_games_5y", "coach_avg_wins_per_appearance",
]
COACH_FEATURES = [f for f in GEN_FEATURES if f.startswith("coach_")]

GEN_POLY = {"Barthag": 2, "AdjDE": 2, "seed": 3}


def general_features(use_coach_features: bool = DEFAULT_CONFIG["model"]["use_coach_features"]) -> list[str]:
    """
    The general model's feature list: GEN_FEATURES, less COACH_FEATURES unless use_coach_features.

    Off by default (model.use_coach_features: false), by the same walk-forward MSE rule that
    turned the seed stack off: the general model's MSE is not shown to be lower with the coach
    columns than without them. Without them every feature in the list predates the 2025
    tournament. The coach columns stay in the training data. The numbers are in the Headline of
    the latest reports/predictive_analysis_*.md.
    """
    return [f for f in GEN_FEATURES if use_coach_features or f not in COACH_FEATURES]


# Seed model config: (hist_seeds, holdout_seeds, features)
#
# These 14 per-band feature lists were hand-picked in madness.R (fit1..fit14), each kept if
# its row-level jackknife MSE on the 2008-2024 tournaments improved, not selected out of
# sample, so on their own they are 14 forking paths. They are off by default
# (model.use_seed_models: false). In walk-forward the stack's MSE edge over the
# general model alone is not distinguishable from zero, and its MAE edge comes mostly
# from pulling seeds 13-16 toward 0; overall its MAE is level with a per-seed median.
# The numbers are in Section 5 of the latest reports/predictive_analysis_*.md.
# Bands overlap and _predict_with_seed_models applies them in order, so the last band
# holding a seed wins: seed s (1-13) is scored by the 3-seed band that starts at s,
# seeds 14-15 by [13, 14, 15], seed 16 by the general model, and band 1 never scores.
SEED_MODELS = [
    ([1, 2], [1, 2], ["Barthag", "AdjOE", "OPAST%", "EXP.", "AdjDE", "seed", "predround"]),
    ([1, 2, 3], [1, 2, 3], ["Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([2, 3, 4], [2, 3, 4], ["Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([3, 4, 5], [3, 4, 5], ["AST%", "Barthag", "AdjOE", "2P%", "TORD", "AdjDE", "seed", "predround"]),
    ([4, 5, 6], [4, 5, 6], ["Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([5, 6, 7], [5, 6, 7], ["FTRD", "AdjOE", "AdjDE", "seed", "predround"]),
    ([6, 7, 8], [6, 7, 8], ["Barthag", "AdjOE", "TOR", "Adj T.", "2P%", "seed", "predround"]),
    ([7, 8, 9], [7, 8, 9], ["Barthag", "AdjOE", "AdjDE", "TOR", "BLKED%", "AST%", "OPAST%", "seed", "predround"]),
    ([8, 9, 10], [8, 9, 10], ["Barthag", "AdjOE", "AdjDE", "seed", "predround", "TOR", "BLKED%", "AST%", "3PRATE", "ELITESOS"]),
    ([9, 10, 11], [9, 10, 11], ["Barthag", "AdjOE", "AdjDE", "seed", "predround", "2P%D", "WINS", "3PRATED", "TALENT", "ELITESOS"]),
    ([10, 11, 12], [10, 11, 12], ["FTR", "AST%", "OPAST%", "ELITESOS", "Barthag", "AdjDE", "AdjOE", "seed", "predround"]),
    ([11, 12, 13], [11, 12, 13], ["BLK%", "AST%", "OPAST%", "EFFHGT", "TALENT", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([12, 13, 14], [12, 13, 14], ["W", "FTR", "FTRD", "TOR", "TORD", "DRB", "TALENT", "ELITESOS", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([13, 14, 15], [13, 14, 15], ["FTR", "TORD", "DRB", "EXP.", "ELITESOS", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
]

ROUND_MIN, ROUND_MAX = 0.0, 6.0


def _add_poly_features(df: pd.DataFrame, poly: dict) -> pd.DataFrame:
    """Add polynomial columns to df."""
    df = df.copy()
    for feat, deg in poly.items():
        if feat in df.columns:
            df[f"{feat}_2"] = df[feat].astype(float) ** 2
            if deg >= 3:
                df[f"{feat}_3"] = df[feat].astype(float) ** 3
    return df


def _resolve_features(features: list[str], poly: dict, df: pd.DataFrame) -> list[str]:
    """Expand feature list with poly terms; return only columns that exist in df."""
    expanded = list(features)
    for base, deg in poly.items():
        if f"{base}_2" in df.columns and f"{base}_2" not in expanded:
            expanded.append(f"{base}_2")
        if deg >= 3 and f"{base}_3" in df.columns and f"{base}_3" not in expanded:
            expanded.append(f"{base}_3")
    return [c for c in expanded if c in df.columns]


def _get_Xy(
    df: pd.DataFrame,
    features: list[str],
    target: str = "round",
    poly: dict | None = None,
) -> tuple[pd.DataFrame, pd.Series]:
    df = df.copy()
    poly = poly or {}
    df = _add_poly_features(df, poly)
    avail = _resolve_features(features, poly, df)
    if not avail:
        return pd.DataFrame(), df[target] if target in df.columns else pd.Series(dtype=float)
    X = df[avail].copy()
    for c in X.columns:
        X[c] = pd.to_numeric(X[c], errors="coerce")
    X = X.fillna(0)
    y = df[target] if target in df.columns else pd.Series(dtype=float)
    return X, y


def tournament_model_frame(df: pd.DataFrame, exclude_years: tuple[int, ...] = (2020,)) -> pd.DataFrame:
    """Rows the models train and are scored on: tournament teams with seed, round and season."""
    seed = pd.to_numeric(df["seed"], errors="coerce")
    rnd = pd.to_numeric(df["round"], errors="coerce")
    years = pd.to_numeric(df["year"], errors="coerce")
    mask = seed.notna() & rnd.notna() & years.notna() & ~years.isin(list(exclude_years))
    return df.loc[mask].reset_index(drop=True)
