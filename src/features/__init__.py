"""Feature building: outcome encodings, coach experience and training-frame assembly."""

from .coach import compute_coach_stats_from_history
from .training_frame import (
    build_clean_training_dataset,
    build_features,
    merge_regular_into_base,
    merge_seed_result_from_tourn,
)

__all__ = [
    "compute_coach_stats_from_history",
    "build_clean_training_dataset",
    "build_features",
    "merge_regular_into_base",
    "merge_seed_result_from_tourn",
]
