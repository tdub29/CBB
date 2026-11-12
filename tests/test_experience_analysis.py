"""Experience 02-04 rank correlations pair each team with its own experience edge.

The tournament subset keeps the season table's row labels, so its index is never 0..n-1. A Series
built from a bare numpy array gets a fresh 0..n-1 index, and `.corr()` aligns on labels: before the
fix every player-experience rho paired one team's finish with another team's experience.
"""

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


def _frames_with_gappy_index(seed: int = 0, overlapping: bool = False) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Tournament-like rows indexed as after filtering a larger frame.

    With overlapping=False no label falls in 0..n-1; with overlapping=True about half do, as in the
    tournament subset of the season table, so a misaligned .corr() returns a wrong number, not NaN.
    """
    rng = np.random.default_rng(seed)
    seeds = np.repeat(np.arange(1, 17), 12)
    n = len(seeds)
    finish_depth = np.clip(np.round(4.5 - 0.3 * seeds + rng.normal(scale=1.0, size=n)), 0, 6)
    exp_rank = rng.uniform(1, 360, size=n)
    finish = np.select([finish_depth >= 6, finish_depth <= 0], ["CHAMPS", "R64"], default="R32")
    if overlapping:
        index = np.sort(rng.choice(2 * n, size=n, replace=False))
    else:
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
            "seed_group": pd.Series(seeds).map(exp.SEED_GROUP_MAP).to_numpy(),
            "overperformance": rng.normal(size=n) - 0.004 * exp_rank,
            "exp_rank_season_vs_seed": exp_rank - rng.uniform(100, 200, size=n),
            f"{COACH_METRIC}_vs_seed": rng.normal(size=n),
        },
        index=index,
    )
    return tournament, over


@pytest.mark.parametrize("overlapping", [False, True])
def test_pooled_player_rhos_match_spearman_on_aligned_arrays(overlapping: bool) -> None:
    tournament, over = _frames_with_gappy_index(seed=1, overlapping=overlapping)
    row = exp.build_analysis_02_table(tournament, over).iloc[0]

    # Both rhos use the negated Exp rank, so positive = more experienced.
    raw_expected = spearmanr(tournament["finish_depth"].to_numpy(), -tournament["exp_rank_season"].to_numpy())[0]
    seed_expected = spearmanr(over["overperformance"].to_numpy(), -over["exp_rank_season_vs_seed"].to_numpy())[0]
    assert row["raw_spearman"] == pytest.approx(raw_expected, abs=1e-12)
    assert row["seed_adjusted_spearman"] == pytest.approx(seed_expected, abs=1e-12)
    # The synthetic overperformance falls with rank, so the aligned estimate is clearly positive.
    assert row["seed_adjusted_spearman"] > 0.2


@pytest.mark.parametrize("overlapping", [False, True])
def test_seed_band_rhos_match_spearman_on_aligned_arrays(overlapping: bool) -> None:
    tournament, over = _frames_with_gappy_index(overlapping=overlapping)
    table, _ = exp.build_seed_group_table(tournament, over, over, COACH_METRIC)

    assert len(table) == len(exp.SEED_GROUP_ORDER)
    for row in table.itertuples(index=False):
        band = over[over["seed_group"] == row.seed_group]
        expected = spearmanr(band["overperformance"].to_numpy(), -band["exp_rank_season_vs_seed"].to_numpy())[0]
        assert row.player_exp_rho == pytest.approx(expected, abs=1e-12)
        coach_expected = spearmanr(band["overperformance"].to_numpy(), band[f"{COACH_METRIC}_vs_seed"].to_numpy())[0]
        assert row.coach_exp_rho == pytest.approx(coach_expected, abs=1e-12)
        assert row.coach_team_count == len(band)
