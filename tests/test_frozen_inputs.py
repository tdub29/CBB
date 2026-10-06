from __future__ import annotations

import importlib
import json
import os

import pandas as pd
import pytest

from src.frozen_inputs import FROZEN_INPUTS, REPO_ROOT, FrozenInputError, check_frozen_inputs, is_frozen, write_csv


def test_frozen_inputs_are_byte_identical_and_match_the_frozen_selection() -> None:
    assert check_frozen_inputs() == {}
    hashed = json.loads((REPO_ROOT / "reports/model_selection_frozen.json").read_text(encoding="utf-8"))["inputs"]
    assert hashed == {
        "training_csv": FROZEN_INPUTS["data/processed/cbb_training_clean.csv"][:16],
        "games_csv": FROZEN_INPUTS["data/raw/tournament_games.csv"][:16],
        "history_csv": FROZEN_INPUTS["data/raw/seed_history_games.csv"][:16],
    }


def test_write_csv_refuses_every_spelling_of_a_frozen_path(tmp_path) -> None:
    target = tmp_path / "data/processed/cbb_training_clean.csv"
    target.parent.mkdir(parents=True)
    target.write_bytes(b"original")
    spellings = [target, tmp_path / "data/processed/../processed/cbb_training_clean.csv"]
    if os.name == "nt":
        spellings.append(str(target).upper())
    for p in spellings:
        with pytest.raises(FrozenInputError):
            write_csv(pd.DataFrame({"a": [1]}), p, root=tmp_path)
    assert target.read_bytes() == b"original"
    ok = write_csv(pd.DataFrame({"a": [1]}), tmp_path / "data/processed/cbb_training_clean_20261006.csv", root=tmp_path)
    assert ok.exists()


@pytest.mark.parametrize("module", ["scripts.fetch_tournament_games", "scripts.fetch_seed_history"])
def test_refetch_scripts_never_target_frozen_files(module) -> None:
    mod = importlib.import_module(module)
    assert not is_frozen(mod.OUT_CSV) and not is_frozen(mod.OUT_MANIFEST)
    assert "refetch" in mod.OUT_CSV.parts
