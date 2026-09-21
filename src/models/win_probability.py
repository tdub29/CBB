"""Win probability and team-strength helpers."""

from __future__ import annotations

from math import log2
from typing import Any

import numpy as np
import pandas as pd


def _ensure_numeric_frame(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    """Return a numeric, fillna(0) feature matrix for prediction."""
    matrix = df.reindex(columns=columns, fill_value=0).copy()
    for col in matrix.columns:
        matrix[col] = pd.to_numeric(matrix[col], errors="coerce")
    return matrix.fillna(0.0)


def _add_power_terms(df: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """
    Add missing power-term features like foo_2, foo_3 from base columns.

    Keeps logic generic so existing pipeline feature names can be reused.
    """
    out = df.copy()
    for feat in features:
        if feat in out.columns:
            continue
        if feat.endswith("_2"):
            base = feat[:-2]
            if base in out.columns:
                out[feat] = pd.to_numeric(out[base], errors="coerce").fillna(0.0) ** 2
        elif feat.endswith("_3"):
            base = feat[:-2]
            if base in out.columns:
                out[feat] = pd.to_numeric(out[base], errors="coerce").fillna(0.0) ** 3
    return out


def compute_strength(df: pd.DataFrame, model: Any, features: list[str]) -> pd.Series:
    """Generate a team strength score from a fitted sklearn-compatible model."""
    if not features:
        return pd.Series(np.zeros(len(df)), index=df.index, dtype=float)
    work = _add_power_terms(df, features)
    X = _ensure_numeric_frame(work, features)
    preds = model.predict(X)
    return pd.Series(np.asarray(preds, dtype=float), index=df.index)


def p_win(strength_a: float, strength_b: float, k: float = 1.0) -> float:
    """
    Bradley-Terry/logistic win probability for team A vs team B.

    P(A beats B) = 1 / (1 + exp(-k * (strength_a - strength_b)))
    """
    if k <= 0:
        raise ValueError(f"k must be positive; got {k}")
    diff = float(strength_a) - float(strength_b)
    scaled = np.clip(-k * diff, -700, 700)
    return float(1.0 / (1.0 + np.exp(scaled)))


def seed_fallback_strength(seed: float | int | None) -> float:
    """Fallback strength when a team is missing from lookup."""
    if seed is None:
        return 0.0
    try:
        seed_val = int(seed)
    except (TypeError, ValueError):
        return 0.0
    if seed_val <= 0:
        return 0.0
    return float(6.0 - log2(seed_val))


def build_strength_lookup(
    df: pd.DataFrame,
    team_col: str = "Team",
    strength_col: str = "predround",
    year_col: str = "year",
) -> dict[str, float]:
    """
    Build {team -> strength} using the latest available season for each team.
    """
    required = {team_col, strength_col}
    missing = required - set(df.columns)
    if missing:
        raise KeyError(f"Missing required columns for strength lookup: {sorted(missing)}")

    work = df[[team_col, strength_col] + ([year_col] if year_col in df.columns else [])].copy()
    work[team_col] = work[team_col].astype(str).str.strip()
    work[strength_col] = pd.to_numeric(work[strength_col], errors="coerce")
    work = work.dropna(subset=[team_col, strength_col])
    work = work[work[team_col] != ""]

    if year_col in work.columns:
        work[year_col] = pd.to_numeric(work[year_col], errors="coerce").fillna(-1)
        work = work.sort_values([team_col, year_col])

    latest = work.groupby(team_col, as_index=False).tail(1)
    return dict(zip(latest[team_col], latest[strength_col], strict=False))
