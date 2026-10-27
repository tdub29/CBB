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


def test_seed_median_baseline_is_the_mae_reference() -> None:
    train = pd.DataFrame(
        {"year": [2010] * 3 + [2011] * 3, "seed": [16.0, 16.0, 1.0, 16.0, 1.0, 1.0], "round": [0, 0, 6, 1, 2, 4]}
    )
    test = pd.DataFrame({"seed": [1.0, 16.0]})
    mean = madness.predict_seed_baseline(madness.fit_seed_baseline(train, stat="mean"), test)
    median = madness.predict_seed_baseline(madness.fit_seed_baseline(train, stat="median"), test)
    assert mean.tolist() == pytest.approx([4.0, 1 / 3])
    assert median.tolist() == [4.0, 0.0]  # mostly-zero seeds: the median says 0, the mean does not
    with pytest.raises(ValueError):
        madness.fit_seed_baseline(train, stat="mode")


def test_paired_season_test_uses_a_t_interval_over_seasons() -> None:
    from scipy import stats

    run_analysis = _load_run_analysis()
    seasons = list(range(2013, 2021))
    ref = pd.DataFrame({"split": seasons, "mse": [1.0] * 8, "mae": [0.7] * 8})
    gaps = np.array([-0.10, -0.05, 0.02, -0.08, -0.01, 0.03, -0.06, -0.04])
    model = ref.assign(mse=ref["mse"] + gaps)
    out = run_analysis.paired_season_test(model, ref, "mse")
    ci = stats.ttest_1samp(gaps, 0.0).confidence_interval(0.95)
    assert out["d"] == pytest.approx(gaps.mean())
    assert (out["lo"], out["hi"]) == (pytest.approx(ci.low), pytest.approx(ci.high))
    assert out["p_t"] == pytest.approx(stats.ttest_1samp(gaps, 0.0).pvalue)
    assert (out["seasons_better"], out["n_seasons"]) == (6, 8)
    assert out["p_sign"] == pytest.approx(stats.binomtest(6, 8, 0.5).pvalue)


def test_holm_adjust_matches_the_step_down_rule() -> None:
    run_analysis = _load_run_analysis()
    adjusted = run_analysis.holm_adjust([0.01, 0.04, 0.03, 0.5])
    # sorted: 0.01*4=0.04, 0.03*3=0.09, 0.04*2=0.08 -> 0.09 (monotone), 0.5*1=0.5
    assert adjusted == pytest.approx([0.04, 0.09, 0.09, 0.5])


def test_kitchen_sink_drops_outcomes_leaks_and_parse_artifacts() -> None:
    run_analysis = _load_run_analysis()
    df = _synthetic_seasons(n_seasons=3)
    df["PAKE"] = 0.0
    df["blank"] = np.nan
    df["_extra_42"] = df["seed"]
    df["Champ"] = 0.0
    specs = {s.name: s for s in run_analysis.build_feature_specs(df, rank_cols=[])}
    sink = set(specs["kitchen_sink_numeric_no_leakage"].features)
    assert {"Barthag", "seed"} <= sink
    assert not sink & {"round", "Champ", "PAKE", "blank", "_extra_42", "year"}


def test_tournament_model_frame_keeps_scored_tournament_rows_only() -> None:
    df = pd.DataFrame(
        {
            "year": [2019, 2019, 2020, 2021, 2021],
            "seed": [1.0, np.nan, 2.0, 3.0, 4.0],
            "round": [2.0, 0.0, 1.0, np.nan, 0.0],
        }
    )
    out = madness.tournament_model_frame(df, exclude_years=(2020,))
    assert out[["year", "seed"]].values.tolist() == [[2019, 1.0], [2021, 4.0]]


@pytest.mark.parametrize("use_seed_models", [False, True])
def test_train_pipeline_models_fits_on_tournament_rows(use_seed_models: bool, monkeypatch) -> None:
    # Smoke test for run_pipeline's modeling step, which used to end on an unfitted model:
    # tournament_model_frame -> general model -> (OOF predround -> seed models) or nothing.
    df = _synthetic_seasons(n_seasons=5)
    df = pd.concat([df, df.head(4).assign(seed=np.nan, round=np.nan)], ignore_index=True)  # non-tournament rows
    cfg = madness._validate_runtime_config(
        {"model": {"use_seed_models": use_seed_models, "ridge_alpha_grid": [0.1, 1.0, 10.0]}}
    )
    model_data = madness.tournament_model_frame(df)
    assert len(model_data) == len(df) - 4

    seen: dict[str, pd.DataFrame] = {}
    real_train_seed_models = madness.train_seed_models

    def spy(frame: pd.DataFrame, **kwargs):
        seen["frame"] = frame.copy()
        return real_train_seed_models(frame, **kwargs)

    monkeypatch.setattr(madness, "train_seed_models", spy)
    columns_before = list(model_data.columns)
    gen_model, gen_feats, gen_meta, seed_models, seed_meta = madness.train_pipeline_models(model_data, cfg)

    assert list(model_data.columns) == columns_before  # the caller's frame is not modified
    strength = madness.compute_strength(model_data, gen_model, gen_feats)
    assert strength.notna().all() and abs(strength.mean() - model_data["round"].mean()) < 0.5
    assert np.isfinite(gen_meta["mse"])
    if use_seed_models:
        # Seed models saw out-of-fold predround (general model refit without each season), not the in-sample fit.
        expected = madness.oof_predround(model_data, ridge_alpha=gen_meta["ridge_alpha"])
        np.testing.assert_allclose(seen["frame"][madness.PREDROUND_OOF_COL].to_numpy(), expected.to_numpy())
        in_sample = madness.compute_strength(model_data, gen_model, gen_feats).to_numpy()
        assert not np.allclose(seen["frame"][madness.PREDROUND_OOF_COL].to_numpy(), in_sample)
        assert len(seed_models) == len(madness.SEED_MODELS)
        assert all(np.isfinite(m["season_cv_mse"]) for m in seed_meta.values())
    else:
        assert "frame" not in seen  # seed models are never trained
        assert seed_models == {} and seed_meta == {}


def test_run_pipeline_end_to_end_writes_models_and_report(monkeypatch, tmp_path: Path) -> None:
    # run_pipeline from training data to model artifacts and the optimization report, with the
    # seed models off (the default). Data loading is replaced by a synthetic frame, so nothing
    # is read from data/ or fetched; the report write sits in a try/except, so check the file.
    full = _synthetic_seasons(n_seasons=5)
    full = pd.concat([full, full.head(4).assign(seed=np.nan, round=np.nan)], ignore_index=True)
    processed = tmp_path / "processed"

    def _mock_cfg():
        cfg = madness._deep_merge(madness.DEFAULT_CONFIG, {"model": {"use_seed_models": False}})
        cfg["ingestion"] = {"year_range": [2010, 2014], "exclude_years": [2020]}
        cfg["paths"] = {
            "data_raw": str(tmp_path / "raw"),
            "data_external": str(tmp_path / "external"),
            "data_processed": str(processed),
            "output_training_csv": str(processed / "cbbtrainingdata"),
            "output_training_clean": str(processed / "cbb_training_clean"),
            "output_training_research": str(processed / "cbbtrainingdata_last2seasons"),
            "models": str(tmp_path / "models"),
            "reports": str(tmp_path / "reports"),
            "bracket_csv": "",
        }
        return cfg

    def _mock_build(**kwargs):
        return full.copy(), full.copy()

    monkeypatch.setattr(madness, "load_config", _mock_cfg)
    monkeypatch.setattr(madness, "build_training_from_all_tables", _mock_build)
    madness.run_pipeline()

    assert (processed / "cbb_training_clean.csv").exists()
    assert len(list((tmp_path / "models").glob("general_model_*.joblib"))) == 1
    reports = list((tmp_path / "reports").glob("model_optimization_*.md"))
    assert len(reports) == 1
    text = reports[0].read_text(encoding="utf-8")
    assert "Disabled (`model.use_seed_models: false`)" in text
    assert "Leave-one-season-out MSE at the selected `alpha` (grid minimum)" in text
    assert "(selected)" in text  # the alpha tuning table was written


def test_general_model_features_override_drops_columns() -> None:
    df = _synthetic_seasons(n_seasons=4)
    no_coach = [f for f in madness.GEN_FEATURES if not f.startswith("coach_")]
    assert len(no_coach) < len(madness.GEN_FEATURES)
    _, default_feats, _ = madness.train_general_model(df, ridge_alpha_grid=[1.0])
    _, ablated_feats, _ = madness.train_general_model(df, ridge_alpha_grid=[1.0], features=no_coach)
    assert any(f.startswith("coach_") for f in default_feats)
    assert not any(f.startswith("coach_") for f in ablated_feats)
    assert {"Barthag_2", "AdjDE_2", "seed_2", "seed_3"} <= set(ablated_feats)  # GEN_POLY still applies
    test = df[df["year"] == df["year"].max()]
    pred = madness.fit_predict_general(df, test, ridge_alpha_grid=[1.0], features=no_coach)
    assert pred.shape == (len(test),) and np.isfinite(pred).all()
