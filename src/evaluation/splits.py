"""Season-grouped validation splits: grouped K-fold, leave-one-season-out and walk-forward."""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd
from sklearn.model_selection import GroupKFold, LeaveOneGroupOut

# One season is one bracket: 63 games, so the 64 rounds in a year always sum to 63 and a
# team's finish depends on who else is in its path. Row-level KFold/LOO puts teams from
# the same bracket on both sides of a split, so every split below keeps a season whole.

WALK_FORWARD_FIRST_TEST_YEAR = 2013  # five training seasons (2008-2012) before the first test


def season_years(df: pd.DataFrame, year_col: str = "year") -> np.ndarray:
    """Return the season of every row as ints; fail if any row has no season."""
    if year_col not in df.columns:
        raise ValueError(f"season-grouped validation needs a '{year_col}' column")
    years = pd.to_numeric(df[year_col], errors="coerce")
    if years.isna().any():
        raise ValueError(f"{int(years.isna().sum())} rows have no '{year_col}'")
    return years.astype(int).to_numpy()


def season_group_splits(
    years: np.ndarray | list[int],
    n_splits: int | None = None,
) -> list[tuple[np.ndarray, np.ndarray]]:
    """
    Train/test index pairs that never split a season.

    n_splits=None (or >= number of seasons) gives leave-one-season-out; otherwise
    GroupKFold(n_splits) with the season as the group.
    """
    years = np.asarray(years)
    n_seasons = len(np.unique(years))
    if n_seasons < 2:
        raise ValueError(f"need at least 2 seasons for grouped CV (got {n_seasons})")
    if n_splits is None or n_splits >= n_seasons:
        splitter: Any = LeaveOneGroupOut()
    else:
        splitter = GroupKFold(n_splits=max(2, int(n_splits)))
    placeholder = np.zeros(len(years))
    return [(train_idx, test_idx) for train_idx, test_idx in splitter.split(placeholder, groups=years)]


def season_cv_label(years: np.ndarray | list[int], n_splits: int | None = None) -> str:
    n_seasons = len(np.unique(np.asarray(years)))
    if n_splits is None or n_splits >= n_seasons:
        return f"LeaveOneGroupOut(groups=year, {n_seasons} seasons)"
    return f"GroupKFold(n_splits={max(2, int(n_splits))}, groups=year, {n_seasons} seasons)"


def walk_forward_splits(
    years: np.ndarray | list[int],
    test_years: list[int] | None = None,
    first_test_year: int = WALK_FORWARD_FIRST_TEST_YEAR,
) -> list[tuple[int, np.ndarray, np.ndarray]]:
    """
    Expanding-window splits: for each test season Y, train on every season < Y, test on Y.

    Returns (Y, train_idx, test_idx). Seasons with no rows (2020) simply do not appear.
    """
    years = np.asarray(years).astype(int)
    if test_years is None:
        test_years = [int(y) for y in np.unique(years) if y >= first_test_year]
    splits: list[tuple[int, np.ndarray, np.ndarray]] = []
    for year in test_years:
        train_idx = np.flatnonzero(years < year)
        test_idx = np.flatnonzero(years == year)
        if len(train_idx) == 0 or len(test_idx) == 0:
            continue
        splits.append((int(year), train_idx, test_idx))
    return splits
