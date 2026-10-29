from __future__ import annotations

from pathlib import Path
import sys

import numpy as np
import pandas as pd
import pytest

SCRIPTS_DIR = Path(__file__).resolve().parents[1] / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

import robustness_checks as rc  # noqa: E402


def _seed_confounded(n_per_seed: int = 60, seed: int = 0) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """x and y both move with seed line but are independent inside every seed line."""
    rng = np.random.default_rng(seed)
    seeds = np.repeat(np.arange(1, 5), n_per_seed)
    x = -2.0 * seeds + rng.normal(size=len(seeds))
    y = -3.0 * seeds + rng.normal(size=len(seeds))
    return x, y, seeds


def test_stratified_permutation_keeps_every_value_in_its_stratum() -> None:
    strata = np.array([3, 1, 2, 1, 3, 3, 2, 1, 2, 2])
    perms = rc.stratified_permutation_indices(strata, 500, np.random.default_rng(1))

    assert perms.shape == (500, len(strata))
    assert np.all(strata[perms] == strata)
    assert np.all(np.sort(perms, axis=1) == np.arange(len(strata)))
    assert np.any(perms != np.arange(len(strata)))


def test_stratified_bootstrap_and_cluster_bootstrap_respect_their_units() -> None:
    strata = np.array([1, 1, 1, 2, 2, 3, 3, 3, 3])
    boot = rc.stratified_bootstrap_indices(strata, 400, np.random.default_rng(2))
    assert np.all(strata[boot] == strata)

    clusters = np.repeat([2019, 2021, 2022], 4)
    idx = rc.cluster_bootstrap_indices(clusters, 300, np.random.default_rng(3))
    assert idx.shape == (300, len(clusters))
    blocks = clusters[idx].reshape(300, 3, 4)
    assert np.all(blocks == blocks[:, :, :1])

    with pytest.raises(ValueError):
        rc.cluster_bootstrap_indices(np.array([1, 1, 2]), 10, np.random.default_rng(4))


def test_spearman_matches_pandas_with_ties() -> None:
    rng = np.random.default_rng(5)
    x = rng.integers(0, 5, size=80).astype(float)
    y = rng.integers(1, 4, size=80).astype(float) + 0.1 * x
    expected = pd.Series(x).corr(pd.Series(y), method="spearman")
    assert rc.spearman(x, y) == pytest.approx(expected, abs=1e-12)

    rows = np.vstack([x, x[::-1]])
    np.testing.assert_allclose(
        rc.rowwise_pearson(rc.rankdata(rows, axis=1), rc.rankdata(y)),
        [expected, pd.Series(x[::-1]).corr(pd.Series(y), method="spearman")],
    )


def test_center_within_removes_each_stratum_mean() -> None:
    values = np.array([1.0, 3.0, 10.0, 14.0, 5.0])
    codes = np.array([1, 1, 2, 2, 3])
    np.testing.assert_allclose(rc.center_within(values, codes), [-1.0, 1.0, -2.0, 2.0, 0.0])

    batch = np.vstack([values, values * 2])
    np.testing.assert_allclose(rc.center_within(batch, codes)[1], [-2.0, 2.0, -4.0, 4.0, 0.0])


def test_modal_share_within_ignores_per_seed_shifts() -> None:
    finish = np.array([1.0, 1.0, 2.0, 1.0, 1.0, 3.0])
    seeds = np.array([1, 1, 1, 2, 2, 2])
    shifted = finish - np.array([1.3, 1.3, 1.3, 1.7, 1.7, 1.7])
    assert rc.modal_share_within(finish, seeds) == pytest.approx(4 / 6)
    assert rc.modal_share_within(shifted, seeds) == pytest.approx(4 / 6)


def test_within_seed_permutation_does_not_flag_seed_confounding() -> None:
    x, y, seeds = _seed_confounded()
    observed = rc.spearman(x, y)
    assert observed > 0.5

    within_null = rc.permutation_spearman_null(x, y, seeds, 2000, np.random.default_rng(6))
    pooled_null = rc.permutation_spearman_null(x, y, np.zeros_like(seeds), 2000, np.random.default_rng(7))

    # The within-seed null keeps the seed-driven part of rho, so the raw correlation is unremarkable.
    assert within_null.mean() > 0.5
    assert rc.permutation_pvalue(observed, within_null) > 0.05
    assert rc.permutation_pvalue(observed, pooled_null) < 0.001


def test_within_seed_permutation_detects_a_real_within_seed_signal() -> None:
    rng = np.random.default_rng(8)
    seeds = np.repeat(np.arange(1, 5), 60)
    x = rng.normal(size=len(seeds))
    y = -seeds + 0.6 * x + rng.normal(scale=0.5, size=len(seeds))
    xc, yc = rc.center_within(x, seeds), rc.center_within(y, seeds)

    null = rc.permutation_spearman_null(xc, yc, seeds, 2000, np.random.default_rng(9))
    assert rc.permutation_pvalue(rc.spearman(xc, yc), null) <= 2 / 2001 + 1e-12

    boot = rc.bootstrap_spearman(
        x, y, seeds, rc.stratified_bootstrap_indices(seeds, 1000, np.random.default_rng(10)), recenter=True
    )
    lo, hi = rc.percentile_ci(boot)
    assert lo > 0.3 and lo < rc.spearman(xc, yc) < hi


def test_permutation_pvalue_is_two_sided_with_plus_one_correction() -> None:
    null = np.linspace(-1, 1, 999)
    assert rc.permutation_pvalue(0.0, null) == pytest.approx(1.0)
    assert rc.permutation_pvalue(5.0, null) == pytest.approx(2 / 1000)
    assert rc.permutation_pvalue(-5.0, null) == pytest.approx(2 / 1000)
    # Off-centre nulls are handled by the tails, not by |rho|.
    shifted = null + 0.5
    assert rc.permutation_pvalue(0.5, shifted) == pytest.approx(1.0)


def test_benjamini_hochberg_matches_hand_computed_values() -> None:
    p = np.array([0.01, 0.04, 0.03, 0.005])
    np.testing.assert_allclose(rc.benjamini_hochberg(p), [0.02, 0.04, 0.04, 0.02])

    with_nan = rc.benjamini_hochberg([0.2, np.nan, 0.9])
    assert np.isnan(with_nan[1])
    np.testing.assert_allclose(with_nan[[0, 2]], [0.4, 0.9])
    assert np.all(rc.benjamini_hochberg(np.array([0.9, 0.95, 1.0])) <= 1.0)


def test_benjamini_hochberg_q_values_are_monotone_in_p() -> None:
    p = np.random.default_rng(11).uniform(size=200)
    q = rc.benjamini_hochberg(p)
    order = np.argsort(p)
    assert np.all(np.diff(q[order]) >= -1e-15)
    assert np.all(q >= p)


def test_permutation_group_means_preserve_seed_composition() -> None:
    seeds = np.repeat(np.arange(1, 5), 25)
    values = seeds.astype(float) * 1.5
    rng = np.random.default_rng(12)
    membership = np.column_stack([rng.uniform(size=len(seeds)) < 0.3, seeds % 2 == 0]).astype(float)

    # Values that are constant inside a seed line cannot move under a within-seed shuffle.
    null = rc.permutation_group_means(values, membership, seeds, 300, np.random.default_rng(13))
    observed = (values @ membership) / membership.sum(axis=0)
    np.testing.assert_allclose(null, np.broadcast_to(observed, null.shape))


def test_cluster_bootstrap_means_handle_unequal_clusters() -> None:
    values = np.array([1.0, 1.0, 1.0, 1.0, 1.0])
    means = rc.cluster_bootstrap_means(values, np.array(["a", "a", "a", "b", "c"]), 200, np.random.default_rng(14))
    np.testing.assert_allclose(means, 1.0)

    values = np.array([10.0, 10.0, 10.0, 0.0])
    means = rc.cluster_bootstrap_means(values, np.array(["a", "a", "a", "b"]), 2000, np.random.default_rng(15))
    assert set(np.round(np.unique(means), 6)) <= {0.0, 7.5, 10.0}


def test_dersimonian_laird_shrinks_to_grand_mean_without_heterogeneity() -> None:
    flat = rc.dersimonian_laird(np.array([0.1, 0.1, 0.1, 0.1]), np.array([0.05, 0.02, 0.1, 0.04]))
    assert flat["tau2"] == 0.0
    np.testing.assert_allclose(flat["shrunk"], 0.1)

    spread = rc.dersimonian_laird(np.array([-2.0, 0.0, 2.0, 4.0]), np.full(4, 1e-4))
    assert spread["tau2"] > 1.0
    np.testing.assert_allclose(spread["shrunk"], [-2.0, 0.0, 2.0, 4.0], atol=1e-3)


def test_misaligned_player_rho_reproduces_the_pandas_index_pitfall() -> None:
    frame = pd.DataFrame({"overperformance": [3.0, 1.0, 2.0, 0.0, 5.0]}, index=[0, 2, 4, 7, 9])
    edge = frame["overperformance"].to_numpy() * 2

    rho, paired = rc.misaligned_player_rho(frame, edge)
    # Only labels 0, 2 and 4 exist in both indexes, pairing (3, 6), (1, 4) and (2, 10).
    assert paired == 3
    assert rho == pytest.approx(0.5)
    aligned = frame["overperformance"].corr(pd.Series(edge, index=frame.index), method="spearman")
    assert aligned == pytest.approx(1.0)


def test_benjamini_hochberg_within_matches_per_group_bh_and_is_never_stricter_than_pooled() -> None:
    p = np.array([0.001, 0.02, 0.04, 0.3, 0.01, 0.5])
    groups = np.array(["a", "a", "a", "a", "b", "b"])
    within = rc.benjamini_hochberg_within(p, groups)

    np.testing.assert_allclose(within[:4], rc.benjamini_hochberg(p[:4]))
    np.testing.assert_allclose(within[4:], rc.benjamini_hochberg(p[4:]))
    assert np.all(within <= rc.benjamini_hochberg(p) + 1e-15)


def test_iid_bootstrap_collapses_without_upsets_while_the_within_seed_null_band_does_not() -> None:
    # 16-seed line: most lose at once (overperformance -0.03), a few win a game (+0.97).
    seeds = np.full(200, 16)
    values = np.where(np.arange(200) < 6, 0.97, -0.03)
    conference = np.arange(200) >= 190  # ten teams, none of which won a game
    assert np.all(values[conference] < 0)

    boot = rc.cluster_bootstrap_means(values[conference], np.arange(10), 2000, np.random.default_rng(16))
    lo, hi = rc.percentile_ci(boot)
    assert lo == hi < 0  # a degenerate interval entirely below zero

    null = rc.permutation_group_means(values, conference[:, None].astype(float), seeds, 4000, np.random.default_rng(17))
    observed = float(values[conference].mean())
    null_hi = float(np.percentile(null[:, 0], 97.5))
    assert null_hi > 0 and observed >= float(np.percentile(null[:, 0], 2.5)) - 1e-12
    assert rc.permutation_pvalue(observed, null[:, 0]) > 0.5


def test_conference_ci_note_flags_each_rule() -> None:
    assert rc.conference_ci_note(0.3, 1.2, 17, 12) == ""
    assert "cannot draw an upset" in rc.conference_ci_note(0.0, 0.2, 17, 12)
    assert "bootstrap SD" in rc.conference_ci_note(0.1, rc.BOOT_SPREAD_FLOOR - 0.01, 17, 12)
    note = rc.conference_ci_note(0.3, 1.0, rc.MIN_CLUSTERS - 1, rc.MIN_CLUSTERS - 5)
    assert "season CI" in note and "program CI" in note


def test_read_published_md_value_keeps_the_printed_precision(tmp_path: Path) -> None:
    report = tmp_path / "summary.md"
    report.write_text("- Result: after controlling for seed, the within-seed Spearman relationship is `0.061`.\n")
    value, tolerance = rc.read_published_md_value(report)
    assert value == pytest.approx(0.061)
    assert tolerance == pytest.approx(0.0005)
    assert np.isnan(rc.read_published_md_value(tmp_path / "missing.md")[0])


def test_sample_descriptions_are_derived_from_the_data() -> None:
    assert rc.describe_years(np.array([2019, 2018, 2021, 2021, 2022])) == "4 tournaments (2018-2022, no 2020)"
    assert rc.describe_years(np.array([2008, 2009])) == "2 tournaments (2008-2009)"
    assert rc.describe_seed_season_strata(pd.Series([4, 4, 4])) == "the 4 same-seed teams of one season"
    assert "3-5 per seed line" in rc.describe_seed_season_strata(pd.Series([3, 4, 5]))


def test_published_conference_check_reads_the_published_csv(tmp_path: Path) -> None:
    published = tmp_path / "conf.csv"
    pd.DataFrame(
        {"Conf": ["A", "B"], "team_count": [3, 2], "mean_overperformance": [0.5, -0.25], "median_overperformance": [0.1, -0.25]}
    ).to_csv(published, index=False)
    ours = pd.DataFrame(
        {
            "Conf": ["A", "B", "A excl. X"],
            "n": [3, 2, 2],
            "mean_overperformance": [0.5000004, -0.2, 0.0],
            "median_overperformance": [0.1, -0.25, 0.0],
        }
    )
    checked = rc.published_conference_check(ours, published)
    assert checked["matches_published"].iloc[:2].tolist() == [True, False]
    assert pd.isna(checked["matches_published"].iloc[2])
