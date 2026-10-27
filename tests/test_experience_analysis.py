from __future__ import annotations

from pathlib import Path
import sys

import numpy as np
import pandas as pd
import pytest
from scipy.stats import spearmanr

SCRIPTS_DIR = Path(__file__).resolve().parents[1] / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

import experience_analysis_02_04 as exp  # noqa: E402

COACH_METRIC = "coach_tourn_games_5y"


def _frames_with_gappy_index(seed: int = 0) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Tournament-like rows whose index labels never fall in 0..n-1, as after filtering a larger frame."""
    rng = np.random.default_rng(seed)
    seeds = np.repeat(np.arange(1, 17), 12)
    n = len(seeds)
    finish_depth = np.clip(np.round(4.5 - 0.3 * seeds + rng.normal(scale=1.0, size=n)), 0, 6)
    exp_rank = rng.uniform(1, 360, size=n)
    finish = np.select(
        [finish_depth >= 6, finish_depth <= 0], ["CHAMPS", "R64"], default="MID"
    )
    index = rng.permutation(1000 + 7 * np.arange(n))
    tournament = pd.DataFrame(
        {
            "seed_int": seeds,
            "finish_depth": finish_depth,
            "finish": finish,
            "exp_raw": rng.uniform(0.5, 3.0, size=n),
            "exp_rank_season": exp_rank,
        },
        index=index,
    )
    over = pd.DataFrame(
        {
            "seed_int": seeds,
            "seed_group": pd.Series(seeds, index=index).map(exp.SEED_GROUP_MAP).to_numpy(),
            "overperformance": rng.normal(size=n) - 0.004 * exp_rank,
            "exp_rank_season_vs_seed": exp_rank - rng.uniform(100, 200, size=n),
            f"{COACH_METRIC}_vs_seed": rng.normal(size=n),
        },
        index=index,
    )
    return tournament, over


def test_seed_group_player_rho_pairs_each_team_with_its_own_edge() -> None:
    tournament, over = _frames_with_gappy_index()
    table, _ = exp.build_seed_group_table(tournament, over, over, COACH_METRIC)

    for row in table.itertuples(index=False):
        band = over[over["seed_group"] == row.seed_group]
        expected = spearmanr(band["overperformance"], -band["exp_rank_season_vs_seed"])[0]
        assert row.player_exp_rho == pytest.approx(expected, abs=1e-12)
        coach_expected = spearmanr(band["overperformance"], band[f"{COACH_METRIC}_vs_seed"])[0]
        assert row.coach_exp_rho == pytest.approx(coach_expected, abs=1e-12)


def test_pooled_player_rhos_pair_each_team_with_its_own_edge() -> None:
    tournament, over = _frames_with_gappy_index(seed=1)
    row = exp.build_analysis_02_table(tournament, over).iloc[0]

    # Both rhos use the negated Exp rank, so positive = more experienced.
    raw_expected = spearmanr(tournament["finish_depth"], -tournament["exp_rank_season"])[0]
    seed_expected = spearmanr(over["overperformance"], -over["exp_rank_season_vs_seed"])[0]
    assert row["raw_spearman"] == pytest.approx(raw_expected, abs=1e-12)
    assert row["seed_adjusted_spearman"] == pytest.approx(seed_expected, abs=1e-12)
    # The synthetic overperformance falls with rank, so the aligned estimate must be clearly positive.
    assert row["seed_adjusted_spearman"] > 0.2
