"""Model artifact persistence (joblib)."""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

from sklearn.pipeline import Pipeline

LOGGER = logging.getLogger(__name__)


def _save_model_artifacts(
    gen_model: Pipeline,
    seed_models: dict[int, tuple[Pipeline, list[str]]],
    models_dir: Path,
    run_tag: str,
    meta: dict[str, Any] | None = None,
) -> None:
    """Best-effort model artifact persistence."""
    try:
        import joblib
    except ImportError:
        LOGGER.warning("joblib not available; skipping model artifact save.")
        return

    models_dir.mkdir(parents=True, exist_ok=True)
    general_path = models_dir / f"general_model_{run_tag}.joblib"
    seed_path = models_dir / f"seed_models_{run_tag}.joblib"
    meta_path = models_dir / f"model_meta_{run_tag}.joblib"
    joblib.dump(gen_model, general_path)
    joblib.dump(seed_models, seed_path)
    if meta is not None:
        joblib.dump(meta, meta_path)
    LOGGER.info(
        {
            "event": "model_artifacts_saved",
            "general_model_path": str(general_path),
            "seed_models_path": str(seed_path),
            "model_meta_path": str(meta_path) if meta is not None else None,
        }
    )
