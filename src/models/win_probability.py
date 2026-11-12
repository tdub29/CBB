"""Win probability and team-strength helpers."""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd


def _is_seed_term(feature: str) -> bool:
    """`seed` or one of its power terms (seed_2, seed_3)."""
    return feature == "seed" or (feature.startswith("seed_") and feature[len("seed_"):].isdigit())


def _name_rows(df: pd.DataFrame, mask: pd.Series) -> str:
    """Up to five Team names for an error message."""
    if "Team" not in df.columns:
        return ""
    teams = df.loc[mask, "Team"].astype(str).tolist()
    more = f" and {len(teams) - 5} more" if len(teams) > 5 else ""
    return f" ({', '.join(teams[:5])}{more})"


def _ensure_numeric_frame(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    """
    Numeric feature matrix for prediction.

    Gaps in ordinary features are filled with 0, as the training matrix is. A missing feature
    column and a missing seed are errors instead: the models are fit on tournament rows only, so a
    seed of 0 (and seed^2 = seed^3 = 0) lies outside anything they saw and scores a team that
    missed the tournament above a 1 seed.
    """
    absent = [c for c in columns if c not in df.columns]
    if absent:
        raise KeyError(f"Missing feature columns for prediction: {absent}")
    matrix = df[columns].copy()
    for col in matrix.columns:
        matrix[col] = pd.to_numeric(matrix[col], errors="coerce")
    seed_cols = [c for c in columns if _is_seed_term(c)]
    if seed_cols:
        no_seed = matrix[seed_cols].isna().any(axis=1)
        if no_seed.any():
            raise ValueError(
                f"{int(no_seed.sum())} of {len(matrix)} rows have no seed{_name_rows(df, no_seed)}. "
                "A missing seed is never filled with 0; give each row its seed first (for a bracket, "
                "its bracket seed)."
            )
    return matrix.fillna(0.0)


def _add_power_terms(df: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """
    Add missing power-term features like foo_2, foo_3 from base columns.

    Keeps logic generic so existing pipeline feature names can be reused. A missing base value
    gives a missing power term, which _ensure_numeric_frame then fills or rejects.
    """
    out = df.copy()
    for feat in features:
        if feat in out.columns:
            continue
        if feat.endswith("_2"):
            base = feat[:-2]
            if base in out.columns:
                out[feat] = pd.to_numeric(out[base], errors="coerce") ** 2
        elif feat.endswith("_3"):
            base = feat[:-2]
            if base in out.columns:
                out[feat] = pd.to_numeric(out[base], errors="coerce") ** 3
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


def fit_bradley_terry_k(winner_minus_loser: np.ndarray, bounds: tuple[float, float] = (1e-3, 20.0)) -> float:
    """Maximum-likelihood k for P(win) = 1 / (1 + exp(-k * strength_diff)).

    Pass winner-minus-loser strength differences from games the strengths did NOT see
    (out-of-fold predictions), or k comes out overconfident.
    """
    from scipy.optimize import minimize_scalar

    d = np.asarray(winner_minus_loser, dtype=float)
    d = d[np.isfinite(d)]
    if d.size == 0:
        raise ValueError("no games to fit k on")
    result = minimize_scalar(lambda k: float(np.logaddexp(0.0, -k * d).sum()), bounds=bounds, method="bounded")
    return float(result.x)


def bracket_season_rows(
    bracket_df: pd.DataFrame,
    team_rows: pd.DataFrame,
    season: int,
    team_col: str = "Team",
    year_col: str = "year",
) -> pd.DataFrame:
    """
    Each bracket team's own row for `season`, in bracket order, carrying its bracket seed.

    team_rows may hold several seasons; only rows with year == season are read, so a team is never
    scored on an earlier or later season than the bracket's. The bracket Seed is written into
    `seed`, and seed power terms already in the rows (seed_2, seed_3) are dropped so they are
    rebuilt from it. The row's own seed is kept as `data_seed`: it is NaN for a team the data has
    outside that season's tournament, and need not match a what-if bracket.

    A bracket team with no row for the season, or with more than one, is an error. There is no
    fallback strength: a stand-in on another scale would distort every game the team plays.
    """
    out = bracket_df[["Team", "Seed", "Region"]].copy()
    out["Team"] = out["Team"].astype(str).str.strip()
    out["Seed"] = pd.to_numeric(out["Seed"], errors="coerce")
    if out["Seed"].isna().any():
        raise ValueError("Every bracket team needs a numeric Seed.")

    years = pd.to_numeric(team_rows[year_col], errors="coerce")
    rows = team_rows.loc[years == season].rename(columns={team_col: "Team"}).copy()
    rows["Team"] = rows["Team"].astype(str).str.strip()
    rows = rows[rows["Team"].isin(set(out["Team"]))]
    dupes = sorted(rows.loc[rows["Team"].duplicated(), "Team"].unique().tolist())
    if dupes:
        raise ValueError(f"More than one {season} row for bracket team(s): {', '.join(dupes)}")
    have = set(rows["Team"])
    missing = [team for team in out["Team"] if team not in have]
    if missing:
        more = f" and {len(missing) - 5} more" if len(missing) > 5 else ""
        raise ValueError(
            f"{len(missing)} of {len(out)} bracket teams have no {season} row: {', '.join(missing[:5])}{more}. "
            f"Check the team names (Torvik spelling) and that the data covers the {season} season."
        )

    data_seed = pd.to_numeric(rows["seed"], errors="coerce") if "seed" in rows.columns else np.nan
    rows = rows.assign(data_seed=data_seed)
    rows = rows.drop(columns=[c for c in rows.columns if _is_seed_term(c) or c in ("Seed", "Region")])
    merged = out.merge(rows, on="Team", how="left", validate="one_to_one")
    merged["seed"] = merged["Seed"].astype(float)
    return merged
