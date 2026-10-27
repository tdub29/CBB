"""Selection Sunday log5 command: reproduces the frozen numbers and never reads an outcome.

Runs on the committed 2026 inputs: the frozen training CSV, the 68-team Selection Sunday bracket and
the saved powered predictions.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from scripts.predict_selection_sunday import (
    REPO_ROOT,
    ROUND_COLS,
    frozen_log5,
    main,
    read_bracket,
    selection_sunday,
)
from scripts.run_analysis import OUTCOME_COLUMNS
from scripts.run_powered_comparison import team_features
from src.frozen_inputs import FrozenInputError

BRACKET = "data/raw/torvik_2026/selection_sunday_bracket_2026.csv"
SEMIS = "East-South,West-Midwest"
RAW = pd.read_csv(REPO_ROOT / "data/processed/cbb_training_clean.csv")


@pytest.fixture(scope="module")
def sel() -> pd.DataFrame:
    return selection_sunday(read_bracket(REPO_ROOT / BRACKET), SEMIS, 2026, team_features(RAW), frozen_log5())


def test_check_reproduces_saved_2026_p_frozen_to_1e12():
    assert main(["--year", "2026", "--check", "--bracket", str(REPO_ROOT / BRACKET), "--semis", SEMIS]) == 0


def test_68_team_mode_is_a_valid_distribution(sel):
    assert len(sel) == 68 and (sel["timing"] == "selection_sunday").all()
    p = sel[ROUND_COLS].to_numpy()
    assert np.allclose(p.sum(0), [64, 32, 16, 8, 4, 2, 1], rtol=0, atol=1e-9)
    assert (np.diff(p, axis=1) <= 1e-15).all()
    play_in = sel[sel["p_round0"] < 1]
    assert len(play_in) == 8
    assert np.allclose(play_in.groupby(["Region", "Seed"])["p_round0"].sum(), 1.0, rtol=0, atol=1e-15)


def test_outcomes_do_not_move_a_single_bit(sel):
    rng = np.random.default_rng(0)
    scrambled = RAW.copy()
    for col in OUTCOME_COLUMNS & set(RAW.columns):
        scrambled[col] = rng.permutation(scrambled[col].to_numpy())
    again = selection_sunday(read_bracket(REPO_ROOT / BRACKET), SEMIS, 2026, team_features(scrambled), frozen_log5())
    pd.testing.assert_frame_equal(sel, again, check_exact=True)


def test_bracket_team_missing_from_training_rows_is_refused():
    b = read_bracket(REPO_ROOT / BRACKET)
    b.loc[0, "Team"] = "Not A Team"
    with pytest.raises(ValueError, match="not in the training CSV"):
        selection_sunday(b, SEMIS, 2026, team_features(RAW), frozen_log5())


def test_semis_must_pair_the_four_regions():
    with pytest.raises(ValueError, match="--semis"):
        selection_sunday(read_bracket(REPO_ROOT / BRACKET), "East-South,West-North", 2026, team_features(RAW),
                         frozen_log5())


def test_output_cannot_overwrite_a_frozen_input():
    with pytest.raises(FrozenInputError):
        main(["--year", "2026", "--from-games", "--out", str(REPO_ROOT / "data/processed/cbb_training_clean.csv")])
