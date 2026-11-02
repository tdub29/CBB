from __future__ import annotations

import numpy as np
import pandas as pd
import pytest
from sklearn.linear_model import LinearRegression

from src.models.win_probability import bracket_season_rows, compute_strength, p_win


def test_p_win_is_bounded_and_symmetric() -> None:
    p_a = p_win(1.75, -0.25, k=1.2)
    p_b = p_win(-0.25, 1.75, k=1.2)
    assert 0.0 <= p_a <= 1.0
    assert 0.0 <= p_b <= 1.0
    assert np.isclose(p_a + p_b, 1.0, atol=1e-10)


def test_compute_strength_builds_power_features() -> None:
    df = pd.DataFrame({"x": [1.0, 2.0, 3.0]})
    X_train = pd.DataFrame({"x": [1.0, 2.0, 3.0], "x_2": [1.0, 4.0, 9.0]})
    y_train = np.array([2.0, 3.0, 4.0])
    model = LinearRegression().fit(X_train, y_train)

    preds = compute_strength(df, model, ["x", "x_2"])
    assert len(preds) == 3
    assert preds.notna().all()


class _RecordingModel:
    """Predicts x - seed / 10 and keeps the matrix it was given."""

    def __init__(self) -> None:
        self.seen: pd.DataFrame | None = None

    def predict(self, X: pd.DataFrame) -> np.ndarray:
        self.seen = X.copy()
        return X["x"].to_numpy(dtype=float) - X["seed"].to_numpy(dtype=float) / 10.0


def test_compute_strength_refuses_a_missing_seed() -> None:
    # The old fillna(0) scored a team with no seed at seed 0, better than any 1 seed on the seed terms.
    df = pd.DataFrame({"Team": ["Top", "Bubble"], "seed": [1.0, np.nan], "x": [0.5, 0.5]})
    with pytest.raises(ValueError, match=r"1 of 2 rows have no seed \(Bubble\)"):
        compute_strength(df, _RecordingModel(), ["seed", "seed_2", "x"])


def test_compute_strength_refuses_a_missing_feature_column() -> None:
    # The old reindex(fill_value=0) turned an absent column, seed included, into zeros.
    df = pd.DataFrame({"Team": ["Top"], "x": [0.5]})
    with pytest.raises(KeyError, match="seed"):
        compute_strength(df, _RecordingModel(), ["seed", "x"])


def test_compute_strength_fills_other_gaps_with_zero_as_in_training() -> None:
    df = pd.DataFrame({"Team": ["Top"], "seed": [2.0], "x": [np.nan]})
    model = _RecordingModel()
    assert compute_strength(df, model, ["seed", "x"]).tolist() == pytest.approx([-0.2])
    assert model.seen["x"].tolist() == [0.0]


def _team_rows() -> pd.DataFrame:
    # "Bubble" missed the 2025 tournament (no seed in the data); both teams have a later 2026 row.
    return pd.DataFrame(
        {
            "Team": ["Top", "Top", "Bubble", "Bubble", "Late"],
            "year": [2025, 2026, 2025, 2026, 2026],
            "seed": [1.0, 3.0, np.nan, 5.0, 12.0],
            "seed_2": [1.0, 9.0, np.nan, 25.0, 144.0],  # stale power term, rebuilt from the bracket seed
            "x": [0.9, 0.2, 0.4, 0.8, 0.5],
        }
    )


def test_bracket_season_rows_use_the_bracket_season_and_bracket_seeds() -> None:
    bracket = pd.DataFrame({"Team": ["Top", "Bubble"], "Seed": [1, 16], "Region": ["East", "East"]})
    rows = bracket_season_rows(bracket, _team_rows(), 2025)
    model = _RecordingModel()
    strength = compute_strength(rows, model, ["seed", "seed_2", "x"])

    assert rows["Team"].tolist() == ["Top", "Bubble"]
    # The 2025 rows, not each team's latest (2026) row.
    assert model.seen["x"].tolist() == [0.9, 0.4]
    # Bracket seeds, never 0 for a team the data has no seed for, and seed_2 rebuilt from them.
    assert model.seen["seed"].tolist() == [1.0, 16.0]
    assert model.seen["seed_2"].tolist() == [1.0, 256.0]
    assert rows["data_seed"].tolist()[0] == 1.0 and np.isnan(rows["data_seed"].tolist()[1])
    assert strength.tolist() == pytest.approx([0.9 - 0.1, 0.4 - 1.6])


def test_bracket_season_rows_refuse_a_team_with_no_row_for_the_season() -> None:
    # "Late" has only a 2026 row; the old latest-season lookup would have used it for a 2025 bracket.
    bracket = pd.DataFrame(
        {"Team": ["Top", "Late", "Ghost"], "Seed": [1, 12, 16], "Region": ["East", "East", "West"]}
    )
    with pytest.raises(ValueError, match="2 of 3 bracket teams have no 2025 row: Late, Ghost"):
        bracket_season_rows(bracket, _team_rows(), 2025)


def test_bracket_season_rows_refuse_duplicate_season_rows() -> None:
    rows = pd.concat([_team_rows(), _team_rows().iloc[[0]]], ignore_index=True)
    bracket = pd.DataFrame({"Team": ["Top"], "Seed": [1], "Region": ["East"]})
    with pytest.raises(ValueError, match="More than one 2025 row for bracket team\\(s\\): Top"):
        bracket_season_rows(bracket, rows, 2025)


def test_fit_bradley_terry_k_recovers_scale() -> None:
    from src.models.win_probability import fit_bradley_terry_k

    rng = np.random.default_rng(0)
    diff = rng.normal(0, 1.5, 4000)
    a_won = rng.random(4000) < 1 / (1 + np.exp(-0.8 * diff))
    winner_minus_loser = np.where(a_won, diff, -diff)
    assert abs(fit_bradley_terry_k(winner_minus_loser) - 0.8) < 0.1
