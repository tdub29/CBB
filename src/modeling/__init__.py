"""Round models: feature lists, the general ridge model, the seed-stratified stack and the seed baseline.

Bracket simulation and win probabilities are in src/models/.
"""

from .baseline import fit_seed_baseline, predict_seed_baseline
from .design import (
    COACH_FEATURES,
    GEN_FEATURES,
    GEN_POLY,
    ROUND_MAX,
    ROUND_MIN,
    SEED_MODELS,
    general_features,
    tournament_model_frame,
)
from .ridge import (
    fit_predict_general,
    season_cv_predict,
    train_general_model,
    tune_ridge_alpha,
)
from .stack import (
    PREDROUND_OOF_COL,
    fit_predict_stack,
    oof_predround,
    train_seed_models,
)

__all__ = [
    "fit_seed_baseline",
    "predict_seed_baseline",
    "COACH_FEATURES",
    "GEN_FEATURES",
    "GEN_POLY",
    "ROUND_MAX",
    "ROUND_MIN",
    "SEED_MODELS",
    "general_features",
    "tournament_model_frame",
    "fit_predict_general",
    "season_cv_predict",
    "train_general_model",
    "tune_ridge_alpha",
    "PREDROUND_OOF_COL",
    "fit_predict_stack",
    "oof_predround",
    "train_seed_models",
]
