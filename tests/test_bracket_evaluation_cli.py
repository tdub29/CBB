"""run_bracket_evaluation.py's per-year walk-forward runs and reproduces the saved p_ridge.

walk_forward_year used to raise NameError (`hist` lived in fit_year_model after the extraction).
This drives it on the committed 2021 inputs: the model probabilities must equal the powered
comparison's saved p_ridge, and a rerun must write tagged files, never the 2026-03 evidence.
"""

from __future__ import annotations

import argparse
import sys

import numpy as np
import pandas as pd

from scripts.run_bracket_evaluation import REPO_ROOT, parse_args, prepare_model_frame, walk_forward_year
from src.models.tournament import seed_pair_rates


def test_walk_forward_year_runs_and_matches_saved_p_ridge():
    raw = pd.read_csv(REPO_ROOT / "data/processed/cbb_training_clean.csv")
    games = pd.read_csv(REPO_ROOT / "data/raw/tournament_games.csv")
    games["no_contest"] = games["no_contest"].astype(bool)
    model_df, rank_cols = prepare_model_frame(raw)
    r = walk_forward_year(2021, raw, model_df, rank_cols, games,
                          argparse.Namespace(cv_folds=10, random_state=42, n_sims=200, seed=123))

    saved = pd.read_csv(REPO_ROOT / "data/processed/powered_game_predictions_2010_2026.csv")
    m = r["games"].merge(saved[saved["year"] == 2021], on=["year", "round", "team_a", "team_b"])
    assert len(m) == len(r["games"]) == 62  # 2021 VCU-Oregon was a no-contest
    assert np.abs(m["p_model"] - m["p_ridge"]).max() <= 1e-12

    pair = seed_pair_rates(games[(games["year"] < 2021) & ~games["no_contest"]])
    want = [pair(a, b) for a, b in zip(r["games"]["seed_a"], r["games"]["seed_b"])]
    assert np.allclose(r["games"]["p_seed_pair"], want, rtol=0, atol=1e-15)


def test_outputs_are_tagged(monkeypatch):
    monkeypatch.setattr(sys, "argv", ["run_bracket_evaluation.py", "--out-tag", "t1"])
    assert parse_args().out_tag == "t1"
    monkeypatch.setattr(sys, "argv", ["run_bracket_evaluation.py"])
    assert parse_args().out_tag.isdigit()
