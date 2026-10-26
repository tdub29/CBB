"""Season-grouped validation and out-of-fold predround stacking."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from src.pipelines import madness

REPO_ROOT = Path(__file__).resolve().parents[1]


def _load_run_analysis():
    if "run_analysis" in sys.modules:
        return sys.modules["run_analysis"]
    spec = importlib.util.spec_from_file_location("run_analysis", REPO_ROOT / "scripts" / "run_analysis.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules["run_analysis"] = module  # dataclasses look their module up here
    spec.loader.exec_module(module)
    return module


def _synthetic_seasons(n_seasons: int = 6, per_seed: int = 2, signal: bool = True, rng_seed: int = 0) -> pd.DataFrame:
    """Small tournament-shaped frame: every season has each seed `per_seed` times."""
    rng = np.random.default_rng(rng_seed)
    rows = []
    for year in range(2010, 2010 + n_seasons):
        for seed in range(1, 17):
            for _ in range(per_seed):
                row = {"Team": f"T{year}_{seed}_{len(rows)}", "year": year, "seed": float(seed)}
                for feat in madness.GEN_FEATURES:
                    if feat != "seed":
                        row[feat] = float(rng.normal())
                if signal:
                    strength = (17 - seed) / 5.0 + 0.5 * row["Barthag"] + rng.normal(scale=0.6)
                    row["round"] = float(np.clip(np.round(strength), 0, 6))
                else:
                    row["round"] = float(rng.integers(0, 4))
                rows.append(row)
    return pd.DataFrame(rows)


def test_season_group_splits_never_put_a_season_on_both_sides() -> None:
    years = np.repeat(np.arange(2010, 2018), 8)
    for n_splits in (None, 3, 5, 100):
        splits = madness.season_group_splits(years, n_splits)
        seen_test = np.zeros(len(years), dtype=int)
        for train_idx, test_idx in splits:
            assert not set(years[train_idx]) & set(years[test_idx])
            seen_test[test_idx] += 1
        assert (seen_test == 1).all()  # every row is tested exactly once
    assert len(madness.season_group_splits(years, None)) == 8  # leave-one-season-out
    assert len(madness.season_group_splits(years, 3)) == 3


def test_walk_forward_trains_only_on_earlier_seasons() -> None:
    years = np.array([y for y in range(2008, 2026) if y != 2020 for _ in range(4)])
    splits = madness.walk_forward_splits(years, first_test_year=2013)
    assert [y for y, _, _ in splits] == [2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024, 2025]
    for test_year, train_idx, test_idx in splits:
        assert (years[test_idx] == test_year).all()
        assert (years[train_idx] < test_year).all()
        assert len(train_idx) == int((years < test_year).sum())  # expanding window


def test_season_years_requires_a_season() -> None:
    with pytest.raises(ValueError):
        madness.season_years(pd.DataFrame({"seed": [1, 2]}))
    with pytest.raises(ValueError):
        madness.season_years(pd.DataFrame({"year": [2010, None]}))


def test_regression_model_keeps_an_intercept() -> None:
    # After StandardScaler, fit_intercept=False forced predictions to average 0.
    rng = np.random.default_rng(1)
    X = pd.DataFrame(rng.normal(size=(200, 3)), columns=["a", "b", "c"])
    y = 3.0 + X["a"] + rng.normal(scale=0.1, size=200)
    for alpha in (0.0, 1.0):
        model = madness._build_regression_model(alpha).fit(X, y)
        assert model.predict(X).mean() == pytest.approx(y.mean(), abs=1e-8)
        assert y.mean() > 2.5


def test_seed_baseline_uses_training_seasons_only() -> None:
    train = pd.DataFrame({"year": [2010, 2010, 2011, 2011], "seed": [1.0, 2.0, 1.0, 2.0], "round": [4.0, 1.0, 2.0, 3.0]})
    test = pd.DataFrame({"year": [2012, 2012, 2012], "seed": [1.0, 2.0, 5.0], "round": [6.0, 6.0, 6.0]})
    baseline = madness.fit_seed_baseline(train)
    pred = madness.predict_seed_baseline(baseline, test)
    assert pred.tolist() == [3.0, 2.0, 2.5]  # seed 5 unseen -> training mean


def test_oof_predround_never_sees_its_own_season() -> None:
    df = _synthetic_seasons()
    oof = madness.oof_predround(df, ridge_alpha=1.0)

    # Scramble the outcomes of one season. Its out-of-fold predround must not move,
    # because the model that produced it was fit without that season.
    target_year = 2012
    scrambled = df.copy()
    in_season = scrambled["year"] == target_year
    scrambled.loc[in_season, "round"] = scrambled.loc[in_season, "round"].to_numpy()[::-1] + 2.0
    oof_scrambled = madness.oof_predround(scrambled, ridge_alpha=1.0)
    np.testing.assert_allclose(oof[in_season], oof_scrambled[in_season])
    assert not np.allclose(oof[~in_season], oof_scrambled[~in_season])  # other seasons did see it

    # The in-sample fit does move: that is the leak the OOF column removes.
    X, y = madness._get_Xy(df, madness.GEN_FEATURES, poly=madness.GEN_POLY)
    Xs, ys = madness._get_Xy(scrambled, madness.GEN_FEATURES, poly=madness.GEN_POLY)
    in_sample = madness._build_regression_model(1.0).fit(X, y).predict(X)
    in_sample_scrambled = madness._build_regression_model(1.0).fit(Xs, ys).predict(Xs)
    assert not np.allclose(in_sample[in_season.to_numpy()], in_sample_scrambled[in_season.to_numpy()])


def test_in_sample_predround_is_over_optimistic_on_noise() -> None:
    # round is pure noise here, so any skill predround shows is overfit. One noise draw of
    # 192 rows swings the OOF correlation by about +/-0.2, so average five draws.
    corr_in, corr_oof = [], []
    for rng_seed in range(5):
        df = _synthetic_seasons(n_seasons=6, per_seed=2, signal=False, rng_seed=rng_seed)
        X, y = madness._get_Xy(df, madness.GEN_FEATURES, poly=madness.GEN_POLY)
        in_sample = madness._build_regression_model(0.0).fit(X, y).predict(X)
        oof = madness.oof_predround(df, ridge_alpha=0.0)
        corr_in.append(float(np.corrcoef(in_sample, df["round"])[0, 1]))
        corr_oof.append(float(np.corrcoef(oof, df["round"])[0, 1]))
    assert np.mean(corr_in) > 0.2  # the in-sample fit "predicts" noise
    assert np.mean(corr_oof) < 0.1  # the out-of-fold one does not
    assert np.mean(corr_in) - np.mean(corr_oof) > 0.2


def test_train_seed_models_refuses_in_sample_predround() -> None:
    df = _synthetic_seasons()
    df["predround"] = 1.0  # an in-sample column alone is not accepted
    with pytest.raises(ValueError, match=madness.PREDROUND_OOF_COL):
        madness.train_seed_models(df)


def test_stack_predictions_ignore_test_outcomes() -> None:
    df = _synthetic_seasons()
    train = df[df["year"] < 2015].reset_index(drop=True)
    test = df[df["year"] == 2015].reset_index(drop=True)
    pred = madness.fit_predict_stack(train, test, ridge_alpha_grid=[0.1, 1.0, 10.0])
    shuffled = test.copy()
    shuffled["round"] = shuffled["round"].to_numpy()[::-1]
    pred_shuffled = madness.fit_predict_stack(train, shuffled, ridge_alpha_grid=[0.1, 1.0, 10.0])
    np.testing.assert_allclose(pred, pred_shuffled)
    assert len(pred) == len(test)
    assert ((pred >= madness.ROUND_MIN) & (pred <= madness.ROUND_MAX)).all()


def test_run_analysis_recipes_only_see_earlier_or_other_seasons() -> None:
    run_analysis = _load_run_analysis()
    df = _synthetic_seasons(n_seasons=8)
    years = madness.season_years(df)
    calls: list[tuple[set[int], set[int]]] = []

    def spy_recipe(train_df: pd.DataFrame, test_df: pd.DataFrame) -> np.ndarray:
        calls.append((set(train_df["year"]), set(test_df["year"])))
        return run_analysis.seed_baseline_recipe(train_df, test_df)

    cv = [(i, tr, te) for i, (tr, te) in enumerate(madness.season_group_splits(years, 4), start=1)]
    wf = madness.walk_forward_splits(years, first_test_year=2013)
    run_analysis.evaluate_recipe(spy_recipe, df, cv)
    n_cv = len(calls)
    result = run_analysis.evaluate_recipe(spy_recipe, df, wf)

    assert n_cv == 4
    for train_years, test_years in calls:
        assert not train_years & test_years
    for train_years, test_years in calls[n_cv:]:
        assert max(train_years) < min(test_years)
    assert result["n_seasons"] == 5  # 2013-2017
    assert set(result["per_split"]["split"]) == {2013, 2014, 2015, 2016, 2017}
