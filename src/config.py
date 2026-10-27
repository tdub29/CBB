"""Pipeline configuration: defaults, configs/default.yaml loading and runtime validation."""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

LOGGER = logging.getLogger(__name__)


DEFAULT_CONFIG = {
    "paths": {
        "output_training_csv": "data/processed/cbbtrainingdata",
        "output_training_clean": "data/processed/cbb_training_clean",
        "data_external": "data/external",
        "data_raw": "data/raw",
        "data_processed": "data/processed",
        "all_trank_team_tables": "data/raw/assembled/all_trank_team_tables.csv",
        "team_history_cache": "data/raw/team_history_cache.csv",
        "bracket_csv": "",
        "models": "models",
        "reports": "reports",
    },
    "ingestion": {
        "team_history_delay_sec": 0.8,
        "team_history_force_playwright": False,
    },
    "model": {
        "k_bradley_terry": 1.0,
        "n_sims": 10000,
        "ridge_alpha": 1.0,
        "optimize_ridge_alpha": True,
        "ridge_alpha_grid": [0.0, 0.01, 0.1, 0.3, 1.0, 3.0, 10.0],
        "use_seed_models": False,
        "use_coach_features": False,
    },
}


def _deep_merge(base: dict, override: dict) -> dict:
    """Deep-merge two dictionaries."""
    out = dict(base)
    for key, value in (override or {}).items():
        if isinstance(value, dict) and isinstance(out.get(key), dict):
            out[key] = _deep_merge(out[key], value)
        else:
            out[key] = value
    return out


def load_config() -> dict:
    """Load config from YAML; fallback to defaults."""
    cfg = dict(DEFAULT_CONFIG)
    try:
        import yaml

        cfg_path = Path("configs/default.yaml")
        if cfg_path.exists():
            with open(cfg_path) as f:
                loaded = yaml.safe_load(f) or {}
                cfg = _deep_merge(cfg, loaded)
    except ImportError:
        LOGGER.warning("PyYAML not installed; using default config.")
    return cfg


def _get_cfg_path(cfg: dict, key: str, default: str) -> str:
    return cfg.get("paths", {}).get(key, default)


def _get_cfg_model(cfg: dict, key: str, default: float | int) -> float | int:
    return cfg.get("model", {}).get(key, default)


def _validate_runtime_config(cfg: dict) -> dict[str, Any]:
    """Validate simulation-related runtime settings."""
    n_sims = int(_get_cfg_model(cfg, "n_sims", DEFAULT_CONFIG["model"]["n_sims"]))
    k_bradley_terry = float(
        _get_cfg_model(cfg, "k_bradley_terry", DEFAULT_CONFIG["model"]["k_bradley_terry"])
    )
    ridge_alpha = float(
        _get_cfg_model(cfg, "ridge_alpha", DEFAULT_CONFIG["model"]["ridge_alpha"])
    )
    optimize_ridge_alpha = bool(
        _get_cfg_model(cfg, "optimize_ridge_alpha", DEFAULT_CONFIG["model"]["optimize_ridge_alpha"])
    )
    alpha_grid_raw = cfg.get("model", {}).get(
        "ridge_alpha_grid",
        DEFAULT_CONFIG["model"]["ridge_alpha_grid"],
    )
    if not isinstance(alpha_grid_raw, list):
        raise ValueError("model.ridge_alpha_grid must be a list of non-negative numbers.")
    alpha_grid: list[float] = []
    for val in alpha_grid_raw:
        try:
            alpha_grid.append(float(val))
        except (TypeError, ValueError) as exc:
            raise ValueError(f"model.ridge_alpha_grid contains non-numeric value: {val!r}") from exc
    alpha_grid = sorted(set(alpha_grid))
    use_seed_models = bool(
        _get_cfg_model(cfg, "use_seed_models", DEFAULT_CONFIG["model"]["use_seed_models"])
    )
    use_coach_features = bool(
        _get_cfg_model(cfg, "use_coach_features", DEFAULT_CONFIG["model"]["use_coach_features"])
    )

    if n_sims <= 0:
        raise ValueError(f"model.n_sims must be > 0 (got {n_sims})")
    if not (0 < k_bradley_terry <= 10):
        raise ValueError(
            f"model.k_bradley_terry must be in (0, 10] (got {k_bradley_terry})"
        )
    if ridge_alpha < 0:
        raise ValueError(f"model.ridge_alpha must be >= 0 (got {ridge_alpha})")
    if any(a < 0 for a in alpha_grid):
        raise ValueError("model.ridge_alpha_grid must contain only non-negative values.")
    if optimize_ridge_alpha and len(alpha_grid) == 0:
        raise ValueError("model.optimize_ridge_alpha is true but model.ridge_alpha_grid is empty.")

    return {
        "n_sims": n_sims,
        "k_bradley_terry": k_bradley_terry,
        "ridge_alpha": ridge_alpha,
        "optimize_ridge_alpha": optimize_ridge_alpha,
        "ridge_alpha_grid": alpha_grid,
        "use_seed_models": use_seed_models,
        "use_coach_features": use_coach_features,
    }
