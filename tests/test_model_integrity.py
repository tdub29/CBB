from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from scripts.run_analysis import OUTCOME_COLUMNS, assert_no_target_leakage


def test_outcome_features_are_excluded_and_leakage_guard_fails_closed() -> None:
    rng = np.random.default_rng(42)
    seed = np.tile(np.arange(1, 17), 8)
    expected = np.log2(17 - seed)
    round_wins = np.clip(np.rint(expected + rng.normal(0, 0.7, len(seed))), 0, 6)
    frame = pd.DataFrame(
        {
            "seed": seed,
            "round": round_wins,
            "PASE": round_wins - expected,
            "PAKE": round_wins - expected + rng.normal(0, 0.03, len(seed)),
            "AdjOE": rng.normal(110, 8, len(seed)),
        }
    )

    assert {"PAKE", "PASE"} <= OUTCOME_COLUMNS
    assert_no_target_leakage(frame, ["AdjOE"], "round")
    for leaked_feature in ("PAKE", "PASE"):
        with pytest.raises(ValueError, match=leaked_feature):
            assert_no_target_leakage(frame, [leaked_feature], "round")


def _season_frame(seed: int = 0) -> pd.DataFrame:
    rng = np.random.default_rng(seed)
    n = 80
    frame = pd.DataFrame(
        {
            "Team": [f"T{i}" for i in range(n)],
            "year": 2030,
            "AdjOE": rng.normal(110, 8, n),
            "AdjDE": rng.normal(100, 8, n),
            "Barthag": rng.uniform(0.2, 0.98, n),
            "seed": np.r_[np.tile(np.arange(1, 17), 4), [np.nan] * (n - 64)],
        }
    )
    frame["round"] = np.where(frame["seed"].notna(), rng.integers(0, 7, n), np.nan)
    frame["PASE"] = rng.normal(0, 1, n)
    frame["PAKE"] = rng.normal(0, 1, n)
    frame["result"] = 64 / 2 ** frame["round"]  # monotone recode of round, like the real column
    return frame


def test_score_field_ignores_outcome_columns() -> None:
    from sklearn.impute import SimpleImputer
    from sklearn.linear_model import Ridge
    from sklearn.pipeline import Pipeline

    from scripts.run_analysis import prepare_scoring_frame, score_field

    frame = _season_frame()
    features = ["AdjOE", "AdjDE", "seed", "Barthag_rank", "seed_2"]
    train = prepare_scoring_frame(frame).assign(seed_2=lambda d: d["seed"] ** 2)
    model = Pipeline([("imp", SimpleImputer()), ("m", Ridge())]).fit(train[features], frame["seed"].fillna(17))
    artifact = {"model": model, "features": features, "poly": {"seed": 2}, "feature_set": "t"}

    with_outcomes = score_field(artifact, frame)
    unplayed = score_field(artifact, frame.drop(columns=["round", "PASE", "PAKE", "result"]))
    shuffled = frame.assign(PASE=frame["PASE"].sample(frac=1, random_state=1).to_numpy(), round=0)
    np.testing.assert_array_equal(with_outcomes.to_numpy(), unplayed.to_numpy())
    np.testing.assert_array_equal(with_outcomes.to_numpy(), score_field(artifact, shuffled).to_numpy())


def test_prediction_time_contract_rejects_outcomes_and_unmapped_columns() -> None:
    from scripts.run_analysis import assert_prediction_time_contract

    assert_prediction_time_contract(["AdjOE", "seed_2", "Barthag_rank", "coach_tourn_wins_5y"])
    for bad in (["AdjOE", "PASE"], ["PAKE"], ["result"], ["mystery_column"]):
        with pytest.raises(ValueError):
            assert_prediction_time_contract(bad)


def test_feature_sets_never_contain_outcome_columns() -> None:
    from scripts.run_analysis import add_rank_features, build_feature_specs, choose_rank_base_columns

    frame = _season_frame().assign(Round2=1, S16=0, E8=0, F4=0, Ship=0, Champ=0)
    frame, rank_cols = add_rank_features(frame, choose_rank_base_columns(frame))
    for spec in build_feature_specs(frame, rank_cols):
        assert not set(spec.features) & OUTCOME_COLUMNS, spec.name


def test_leakage_guard_catches_target_copies() -> None:
    frame = _season_frame().dropna(subset=["seed"])
    for copy in ("round", "result"):
        with pytest.raises(ValueError):
            assert_no_target_leakage(frame.assign(feat=frame[copy]), ["feat"], "round")
