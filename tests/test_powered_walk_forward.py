"""The walk-forward must never use year-Y data when predicting year Y.

Every candidate family is fit for Y on a synthetic history, then refit after flipping every
year-Y (and later) game result, scrambling every year-Y outcome column and every later season's
inputs. Year-Y predictions must not move by a single bit.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from scripts.run_bracket_evaluation import fit_year_model, prepare_model_frame
from scripts.run_powered_comparison import (
    TOPIC_COLS,
    game_frame,
    madness_fit_year,
    predict_games,
    results_frame,
    team_features,
)

YEARS = range(2008, 2013)
Y = 2010
R64 = [(1, 16), (8, 9), (5, 12), (4, 13), (6, 11), (3, 14), (7, 10), (2, 15)]
FEATURES = ["AdjOE", "AdjDE", "Barthag", "Adj T."] + [c for c in TOPIC_COLS if c != "Adj T."]


def _synthetic(seed: int = 0) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    rng = np.random.default_rng(seed)
    rows, games = [], []
    for y in YEARS:
        seeds = np.r_[np.tile(np.arange(1, 17), 4), [np.nan] * 6]
        strength = np.where(np.isnan(seeds), -1.5, -0.25 * np.nan_to_num(seeds)) + rng.normal(0, 0.6, len(seeds))
        teams = [f"T{y}_{i}" for i in range(len(seeds))]
        frame = pd.DataFrame({"Team": teams, "year": y, "seed": seeds,
                              "AdjOE": 105 + 6 * strength + rng.normal(0, 2, len(seeds)),
                              "AdjDE": 100 - 5 * strength + rng.normal(0, 2, len(seeds)),
                              "Barthag": 1 / (1 + np.exp(-(strength + rng.normal(0, 0.3, len(seeds))))),
                              "Adj T.": rng.normal(68, 3, len(seeds))})
        for c in FEATURES[4:]:
            frame[c] = rng.normal(0, 1, len(seeds))
        frame["round"] = np.where(np.isnan(seeds), np.nan, 0.0)
        for region in range(4):
            idx = {int(s): region * 16 + int(s) - 1 for s in range(1, 17)}
            for sa, sb in R64:
                a, b = idx[sa], idx[sb]
                a_wins = rng.random() < 1 / (1 + np.exp(-(strength[a] - strength[b])))
                w, lo = (a, b) if a_wins else (b, a)
                frame.loc[w, "round"] = 1 + rng.integers(0, 3)
                games.append({"year": y, "round": 1, "winner": teams[w], "loser": teams[lo],
                              "winner_seed": int(seeds[w]), "loser_seed": int(seeds[lo]), "no_contest": False})
        frame["PASE"] = rng.normal(0, 1, len(seeds))
        frame["PAKE"] = rng.normal(0, 1, len(seeds))
        frame["result"] = 64 / 2 ** frame["round"]
        rows.append(frame)
    history = pd.DataFrame([{"year": 2000 + i % 8, "round": 1, "winner": "h", "loser": "g",
                             "winner_seed": sa if i % 4 else sb, "loser_seed": sb if i % 4 else sa,
                             "no_contest": False} for i, (sa, sb) in enumerate(R64 * 8)])
    return pd.concat(rows, ignore_index=True), pd.DataFrame(games), history


def _poison(raw: pd.DataFrame, games: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    rng = np.random.default_rng(99)
    raw, games = raw.copy(), games.copy()
    now_or_later = raw["year"] >= Y
    raw.loc[now_or_later & raw["round"].notna(), "round"] = rng.integers(0, 7, int((now_or_later & raw["round"].notna()).sum()))
    for c in ("PASE", "PAKE", "result"):
        raw.loc[now_or_later, c] = rng.normal(0, 5, int(now_or_later.sum()))
    later = raw["year"] > Y
    for c in FEATURES:
        raw.loc[later, c] = raw.loc[later, c].to_numpy() * rng.uniform(0.5, 1.5, int(later.sum()))
    flip = games["year"] >= Y
    games.loc[flip, ["winner", "loser"]] = games.loc[flip, ["loser", "winner"]].to_numpy()
    games.loc[flip, ["winner_seed", "loser_seed"]] = games.loc[flip, ["loser_seed", "winner_seed"]].to_numpy()
    return raw, games


def _predict_all(raw: pd.DataFrame, games: pd.DataFrame, history: pd.DataFrame) -> dict[str, np.ndarray]:
    model_df, rank_cols = prepare_model_frame(raw)
    fits = {"ridge_bt": {Y: fit_year_model(Y, raw, model_df, rank_cols, games, 3, 42)},
            "madness_bt": {Y: madness_fit_year(Y, raw, games)}}
    gl, results = game_frame(games, team_features(raw)), results_frame(history, games)
    specs = [{"base": "seed_pair"}, {"base": "log5"}, {"base": "log5_scaled"}, {"base": "em_logistic", "blend": True},
             {"base": "tempo_margin"}, {"base": "logit_diffs", "C": 1.0}, {"base": "logit_topics", "C": 0.1, "blend": True},
             {"base": "ridge_bt"}, {"base": "ridge_bt", "blend": True}, {"base": "madness_bt", "blend": True}]
    out = {}
    for spec in specs:
        p, _, _ = predict_games(Y, spec, gl, results, fits)
        out[str(spec)] = np.asarray(p)
    return out


def test_walk_forward_never_uses_year_y_data(monkeypatch) -> None:
    from src.pipelines import madness as mad

    # ponytail: one alpha keeps madness.py's LOO tuning fast; the leak property does not depend on the grid
    monkeypatch.setitem(mad.DEFAULT_CONFIG["model"], "ridge_alpha_grid", [1.0])
    raw, games, history = _synthetic()
    clean = _predict_all(raw, games, history)
    poisoned = _predict_all(*_poison(raw, games), history)
    assert len(clean[str({"base": "seed_pair"})]) == 32
    for name, p in clean.items():
        np.testing.assert_array_equal(p, poisoned[name], err_msg=name)
        assert np.all((p > 0) & (p < 1)), name
