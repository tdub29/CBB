"""Evaluation: season-grouped splits, the target-leakage guard and run reports."""

from .leakage import assert_no_target_leakage
from .splits import (
    WALK_FORWARD_FIRST_TEST_YEAR,
    season_cv_label,
    season_group_splits,
    season_years,
    walk_forward_splits,
)

__all__ = [
    "assert_no_target_leakage",
    "WALK_FORWARD_FIRST_TEST_YEAR",
    "season_cv_label",
    "season_group_splits",
    "season_years",
    "walk_forward_splits",
]
