"""Frozen inputs of the powered comparison: their bytes are the evidence, so no writer may replace them.

The three CSVs were hashed into reports/model_selection_frozen.json when log5 was frozen on
2026-09-29. `scripts/run_powered_comparison.py score` refuses to run if any of them change, and
cbb_training_clean.csv cannot be rebuilt from live Torvik (data/raw/torvik_2026/MANIFEST.md), so an
overwrite is unrecoverable. Every script that writes a training CSV goes through `write_csv`.
"""

from __future__ import annotations

import hashlib
import os
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]

# Full sha256 of each frozen file; the first 16 hex are the `inputs` block of model_selection_frozen.json.
FROZEN_INPUTS = {
    "data/processed/cbb_training_clean.csv": "a35137c1619032e600f42b45eaff1e9c2b4db12348fdf5e1408be15436b77980",
    "data/raw/tournament_games.csv": "170ba15342239c73cf89aaf02c9b2dcaebf6ac94ebd337beef3d1fad8b5cf369",
    "data/raw/seed_history_games.csv": "a210e08449f488751915094efeb67dd3ff51a991e9bcbe7be9f4ab05d67a2488",
    # Provenance of the two game files (source, retrieval time, sha256 of the CSV).
    "data/raw/tournament_games_manifest.json": "2d65f2a92917c0bb1898b2a8a23fec01a4afdc869912a96236bea3107109b3a8",
    "data/raw/seed_history_games_manifest.json": "61360bf2547298f3a16dc21f699c766d96640f57350c5abe1e97f8ef84cf7462",
    "reports/model_selection_frozen.json": "624bcd737db6f3d088fdf05b52958f56c6f6034f6700076d7fae5719243b9d85",
}


class FrozenInputError(RuntimeError):
    """Raised when a write would replace a frozen input."""


def _key(path: str | Path) -> str:
    return os.path.normcase(str(Path(path).resolve()))


def is_frozen(path: str | Path, root: Path = REPO_ROOT) -> bool:
    return _key(path) in {_key(root / rel) for rel in FROZEN_INPUTS}


def assert_writable(path: str | Path, root: Path = REPO_ROOT) -> None:
    if is_frozen(path, root):
        raise FrozenInputError(
            f"refusing to write {path}: it is a frozen input of the log5 test (src/frozen_inputs.py). "
            "Write to a run-tagged path instead."
        )


def write_csv(df: pd.DataFrame, path: str | Path, root: Path = REPO_ROOT) -> Path:
    """`df.to_csv(path, index=False)` that refuses frozen inputs."""
    path = Path(path)
    assert_writable(path, root)
    path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(path, index=False)
    return path


def check_frozen_inputs(root: Path = REPO_ROOT) -> dict[str, str]:
    """Return {relative path: problem} for every frozen input that is missing or changed."""
    problems = {}
    for rel, want in FROZEN_INPUTS.items():
        p = root / rel
        if not p.exists():
            problems[rel] = "missing"
        elif hashlib.sha256(p.read_bytes()).hexdigest() != want:
            problems[rel] = "sha256 changed"
    return problems
