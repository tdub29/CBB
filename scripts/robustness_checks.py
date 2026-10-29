#!/usr/bin/env python3
"""Uncertainty and multiple-comparison checks for the headline descriptive claims.

Each claim is first re-derived with the same loaders and table builders that produced the
published number, then stress-tested:

a) SOS within-seed Spearman (reports/sos_vs_finish_pooled_summary.md)
b) every seed-band Spearman in reports/*_vs_finish_by_seed_group_table.csv
c) conference overperformance vs exact-seed expectation (reports/sos_conf_vs_overperformance_summary.md)

For every estimate: n, point estimate, bootstrap 95% CI, and a permutation p-value that shuffles the
stat within seed line so seed structure is preserved. For (a) and (b) the bootstrap resamples teams
within their seed line (plus a season-cluster variant). For (c) it resamples the conference's own
team-seasons with seed baselines fixed (plus season and program clusters), and each conference also
gets the 95% band of its within-seed permutation null. Family (b) and the conference table get
Benjamini-Hochberg q-values.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import argparse
import importlib
import re
import sys
import zlib

import numpy as np
import pandas as pd
from scipy.stats import chi2, rankdata

REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPTS_DIR = Path(__file__).resolve().parent
for _path in (REPO_ROOT, SCRIPTS_DIR):
    if str(_path) not in sys.path:
        sys.path.insert(0, str(_path))

RANDOM_SEED = 20260308
N_RESAMPLES = 10_000
CHUNK_SIZE = 1_000
ALPHA = 0.05
SEED_GROUP_ORDER = ["1-2", "3-4", "5-6", "7-8", "9-10", "11-13", "14-16"]
SEED_GROUP_MAP = {
    1: "1-2",
    2: "1-2",
    3: "3-4",
    4: "3-4",
    5: "5-6",
    6: "5-6",
    7: "7-8",
    8: "7-8",
    9: "9-10",
    10: "9-10",
    11: "11-13",
    12: "11-13",
    13: "11-13",
    14: "14-16",
    15: "14-16",
    16: "14-16",
}
# Focus conferences for the detailed (c) breakdown, and the program the review asked about.
FOCUS_CONFERENCES = ["Horz", "MWC"]
HORIZON_DROP_PROGRAM = "Butler"
# Conference bootstrap diagnostics: a team-resampled CI whose spread is under this share of the
# seed-line spread, or a cluster CI with fewer clusters than MIN_CLUSTERS, is flagged in `ci_note`.
BOOT_SPREAD_FLOOR = 0.75
MIN_CLUSTERS = 10
SOS_PUBLISHED_PATTERN = r"within-seed Spearman relationship is `([-+]?\d*\.\d+)`"


def get_paths() -> dict[str, Path]:
    reports = REPO_ROOT / "reports"
    return {
        "reports": reports,
        "sos_sample": REPO_ROOT / "data" / "processed" / "sos_sample.csv",
        "published_sos_md": reports / "sos_vs_finish_pooled_summary.md",
        "published_conf_csv": reports / "sos_conf_vs_overperformance_table.csv",
        "md": reports / "robustness_summary.md",
        "csv_sos": reports / "robustness_sos_within_seed.csv",
        "csv_bands": reports / "robustness_seed_band_correlations.csv",
        "csv_conf": reports / "robustness_conference_overperformance.csv",
        "csv_conf_team_seasons": reports / "robustness_conference_team_seasons.csv",
        "csv_conf_loo": reports / "robustness_conference_leave_one_program_out.csv",
    }


# ---------------------------------------------------------------------------
# Core stat helpers (pure numpy; covered by tests/test_robustness_checks.py)
# ---------------------------------------------------------------------------


def rng_for(key: str, seed: int = RANDOM_SEED) -> np.random.Generator:
    """Deterministic generator per estimate, independent of the order estimates are run in."""
    return np.random.default_rng([seed, zlib.crc32(key.encode("utf-8"))])


def rowwise_pearson(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    a = np.asarray(a, dtype=float)
    b = np.asarray(b, dtype=float)
    a = a - a.mean(axis=-1, keepdims=True)
    b = b - b.mean(axis=-1, keepdims=True)
    num = (a * b).sum(axis=-1)
    den = np.sqrt((a * a).sum(axis=-1) * (b * b).sum(axis=-1))
    with np.errstate(invalid="ignore", divide="ignore"):
        return np.where(den > 0, num / np.where(den > 0, den, 1.0), np.nan)


def spearman(x: np.ndarray, y: np.ndarray) -> float:
    """Spearman rho with average ranks for ties (matches pandas .corr(method="spearman"))."""
    return float(rowwise_pearson(rankdata(x), rankdata(y)))


def center_within(values: np.ndarray, codes: np.ndarray) -> np.ndarray:
    """Subtract each stratum's mean along the last axis; codes broadcast against values."""
    values = np.asarray(values, dtype=float)
    codes = np.broadcast_to(np.asarray(codes), values.shape)
    out = values.copy()
    for code in np.unique(codes):
        mask = codes == code
        counts = mask.sum(axis=-1, keepdims=True)
        sums = np.where(mask, values, 0.0).sum(axis=-1, keepdims=True)
        means = sums / np.where(counts > 0, counts, 1)
        out = out - np.where(mask, means, 0.0)
    return out


def stratified_permutation_indices(strata: np.ndarray, n_perm: int, rng: np.random.Generator) -> np.ndarray:
    """(n_perm, n) indices; row b sends every position to a random position in the same stratum."""
    strata = np.asarray(strata)
    order = np.argsort(strata, kind="stable")
    _, codes = np.unique(strata[order], return_inverse=True)
    keys = codes[None, :] + rng.random((n_perm, len(strata)))
    shuffled = np.argsort(keys, axis=1, kind="stable")
    out = np.empty((n_perm, len(strata)), dtype=np.intp)
    out[:, order] = order[shuffled]
    return out


def stratified_bootstrap_indices(strata: np.ndarray, n_boot: int, rng: np.random.Generator) -> np.ndarray:
    """(n_boot, n) indices resampled with replacement inside each stratum (stratum sizes fixed)."""
    strata = np.asarray(strata)
    out = np.empty((n_boot, len(strata)), dtype=np.intp)
    for code in np.unique(strata):
        positions = np.flatnonzero(strata == code)
        out[:, positions] = positions[rng.integers(0, len(positions), size=(n_boot, len(positions)))]
    return out


def cluster_bootstrap_indices(clusters: np.ndarray, n_boot: int, rng: np.random.Generator) -> np.ndarray:
    """(n_boot, n) indices that resample whole clusters with replacement. Needs equal cluster sizes."""
    clusters = np.asarray(clusters)
    labels = np.unique(clusters)
    members = [np.flatnonzero(clusters == label) for label in labels]
    sizes = {len(m) for m in members}
    if len(sizes) != 1:
        raise ValueError(f"cluster_bootstrap_indices needs equal cluster sizes, got {sorted(sizes)}")
    member_matrix = np.vstack(members)
    draws = rng.integers(0, len(labels), size=(n_boot, len(labels)))
    return member_matrix[draws].reshape(n_boot, -1)


def bootstrap_spearman(
    x: np.ndarray,
    y: np.ndarray,
    strata: np.ndarray,
    indices: np.ndarray,
    recenter: bool,
) -> np.ndarray:
    """Spearman for each resample in `indices`; optionally re-centers x and y on stratum means per resample."""
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    strata = np.asarray(strata)
    out = np.empty(len(indices), dtype=float)
    for start in range(0, len(indices), CHUNK_SIZE):
        idx = indices[start : start + CHUNK_SIZE]
        xs = x[idx]
        ys = y[idx]
        if recenter:
            codes = strata[idx]
            xs = center_within(xs, codes)
            ys = center_within(ys, codes)
        out[start : start + len(idx)] = rowwise_pearson(rankdata(xs, axis=1), rankdata(ys, axis=1))
    return out


def permutation_spearman_null(
    x: np.ndarray,
    y: np.ndarray,
    strata: np.ndarray,
    n_perm: int,
    rng: np.random.Generator,
) -> np.ndarray:
    """Null distribution of Spearman(x, y) with x shuffled within strata (y and strata fixed)."""
    rank_x = rankdata(np.asarray(x, dtype=float))
    rank_y = rankdata(np.asarray(y, dtype=float))
    perms = stratified_permutation_indices(strata, n_perm, rng)
    out = np.empty(n_perm, dtype=float)
    for start in range(0, n_perm, CHUNK_SIZE):
        idx = perms[start : start + CHUNK_SIZE]
        out[start : start + len(idx)] = rowwise_pearson(rank_x[idx], rank_y)
    return out


def permutation_pvalue(observed: float, null: np.ndarray) -> float:
    """Two-sided p = twice the smaller tail, with the +1 correction. Works for off-center nulls."""
    null = np.asarray(null, dtype=float)
    null = null[np.isfinite(null)]
    tol = 1e-12
    upper = (1 + np.sum(null >= observed - tol)) / (len(null) + 1)
    lower = (1 + np.sum(null <= observed + tol)) / (len(null) + 1)
    return float(min(1.0, 2 * min(upper, lower)))


def percentile_ci(samples: np.ndarray, level: float = 0.95) -> tuple[float, float]:
    tail = (1 - level) / 2 * 100
    lo, hi = np.nanpercentile(np.asarray(samples, dtype=float), [tail, 100 - tail])
    return float(lo), float(hi)


def benjamini_hochberg(pvalues: np.ndarray | list[float]) -> np.ndarray:
    """BH step-up q-values; NaNs pass through and are excluded from the family size."""
    p = np.asarray(pvalues, dtype=float)
    q = np.full(p.shape, np.nan)
    valid = np.flatnonzero(np.isfinite(p))
    if len(valid) == 0:
        return q
    order = valid[np.argsort(p[valid], kind="stable")]
    m = len(order)
    scaled = p[order] * m / np.arange(1, m + 1)
    q[order] = np.minimum(np.minimum.accumulate(scaled[::-1])[::-1], 1.0)
    return q


def benjamini_hochberg_within(pvalues: np.ndarray | list[float], groups: np.ndarray | list[str]) -> np.ndarray:
    """BH q-values computed separately inside each group (a narrower family than pooling every test)."""
    p = np.asarray(pvalues, dtype=float)
    groups = np.asarray(groups)
    q = np.full(p.shape, np.nan)
    for group in pd.unique(groups):
        mask = groups == group
        q[mask] = benjamini_hochberg(p[mask])
    return q


def cluster_bootstrap_means(
    values: np.ndarray,
    clusters: np.ndarray,
    n_boot: int,
    rng: np.random.Generator,
) -> np.ndarray:
    """Bootstrap of a mean that resamples whole clusters (clusters may differ in size)."""
    values = np.asarray(values, dtype=float)
    _, codes = np.unique(np.asarray(clusters), return_inverse=True)
    sums = np.bincount(codes, weights=values)
    sizes = np.bincount(codes).astype(float)
    draws = rng.integers(0, len(sums), size=(n_boot, len(sums)))
    return sums[draws].sum(axis=1) / sizes[draws].sum(axis=1)


def permutation_group_means(
    values: np.ndarray,
    membership: np.ndarray,
    strata: np.ndarray,
    n_perm: int,
    rng: np.random.Generator,
) -> np.ndarray:
    """(n_perm, k) null group means with values shuffled within strata and group membership fixed."""
    values = np.asarray(values, dtype=float)
    membership = np.asarray(membership, dtype=float)
    sizes = membership.sum(axis=0)
    perms = stratified_permutation_indices(strata, n_perm, rng)
    out = np.empty((n_perm, membership.shape[1]), dtype=float)
    for start in range(0, n_perm, CHUNK_SIZE):
        idx = perms[start : start + CHUNK_SIZE]
        out[start : start + len(idx)] = (values[idx] @ membership) / sizes
    return out


def dersimonian_laird(means: np.ndarray, variances: np.ndarray) -> dict[str, object]:
    """Normal-normal empirical Bayes: DerSimonian-Laird tau^2, shrunken means, plug-in posterior SDs."""
    means = np.asarray(means, dtype=float)
    variances = np.asarray(variances, dtype=float)
    weights = 1 / variances
    fixed_mean = float(np.sum(weights * means) / np.sum(weights))
    q_stat = float(np.sum(weights * (means - fixed_mean) ** 2))
    dof = len(means) - 1
    scale = float(np.sum(weights) - np.sum(weights**2) / np.sum(weights))
    tau2 = max(0.0, (q_stat - dof) / scale)
    re_weights = 1 / (variances + tau2)
    grand_mean = float(np.sum(re_weights * means) / np.sum(re_weights))
    shrink = tau2 / (tau2 + variances)
    shrunk = grand_mean + shrink * (means - grand_mean)
    # Posterior variance given tau^2, plus the uncertainty in the grand mean it is shrunk towards.
    posterior_var = shrink * variances + (1 - shrink) ** 2 / np.sum(re_weights)
    return {
        "tau2": tau2,
        "grand_mean": grand_mean,
        "q_stat": q_stat,
        "dof": dof,
        "shrink": shrink,
        "shrunk": shrunk,
        "posterior_sd": np.sqrt(posterior_var),
    }


def cochran_q(means: np.ndarray, variances: np.ndarray) -> np.ndarray:
    """Cochran's Q for each row of `means` (k columns) with fixed per-group variances."""
    means = np.atleast_2d(np.asarray(means, dtype=float))
    weights = 1 / np.asarray(variances, dtype=float)
    fixed = (means * weights).sum(axis=1, keepdims=True) / weights.sum()
    return ((means - fixed) ** 2 * weights).sum(axis=1)


def conference_ci_note(share_positive: float, boot_sd_ratio: float, seasons: int, programs: int) -> str:
    """Why a conference's bootstrap CIs should not be read at face value ("" when no rule fires).

    The team bootstrap only sees the conference's own team-seasons. If none (or few) of them beat
    their seed line it cannot draw the rare deep run its seed lines produce, so it is too narrow.
    """
    notes = []
    if share_positive == 0:
        notes.append("no team beat its seed line, so the bootstrap cannot draw an upset")
    elif boot_sd_ratio < BOOT_SPREAD_FLOOR:
        notes.append(f"bootstrap SD < {BOOT_SPREAD_FLOOR:g} x seed-line SD")
    if seasons < MIN_CLUSTERS:
        notes.append(f"season CI: < {MIN_CLUSTERS} seasons")
    if programs < MIN_CLUSTERS:
        notes.append(f"program CI: < {MIN_CLUSTERS} programs")
    return "; ".join(notes)


def read_published_md_value(path: Path, pattern: str = SOS_PUBLISHED_PATTERN) -> tuple[float, float]:
    """Number captured by `pattern` in a published markdown report, and half a unit of its last
    printed decimal (the tolerance a match is held to). (NaN, NaN) if the file or number is absent."""
    if not path.exists():
        return np.nan, np.nan
    match = re.search(pattern, path.read_text(encoding="utf-8"))
    if not match:
        return np.nan, np.nan
    text = match.group(1)
    decimals = len(text.split(".")[1]) if "." in text else 0
    return float(text), 0.5 * 10.0**-decimals


# ---------------------------------------------------------------------------
# Claim (a) and family (b): Spearman estimates
# ---------------------------------------------------------------------------


@dataclass
class CorrelationCell:
    """One published Spearman estimate plus the arrays needed to resample it.

    `x_est` / `y_est` are the exact arrays the original report code correlated; they give the
    point estimate and the permutation null, so the published number is reproduced bit for bit.
    `x` / `y` are the pre-centering values used by the bootstrap. When `recenter` is True the
    published estimator centers both on exact-seed-line means of the same sample, so every
    resample re-estimates those means. When False `x` / `y` are used as-is (raw-within-band
    estimators, or centering offsets taken from a larger sample, as for tempo since 2014).
    """

    family: str
    topic: str
    statistic: str
    metric: str
    seed_group: str
    source: str
    published: float
    x_est: np.ndarray
    y_est: np.ndarray
    x: np.ndarray
    y: np.ndarray
    seed: np.ndarray
    year: np.ndarray
    recenter: bool
    estimator: str
    note: str = ""
    published_n: float = np.nan
    # Published CSVs carry 6 decimals; a value parsed from a markdown report carries fewer.
    published_tolerance: float = 5e-6

    @property
    def key(self) -> str:
        return f"{self.family}|{self.topic}|{self.statistic}|{self.seed_group}"


def modal_share_within(values: np.ndarray, strata: np.ndarray) -> float:
    """Share of rows equal to their stratum's most common value (outcomes shifted per seed still tie)."""
    frame = pd.DataFrame({"value": values, "stratum": strata})
    modal_counts = frame.groupby("stratum")["value"].agg(lambda column: column.value_counts().iloc[0])
    return float(modal_counts.sum() / len(frame))


def evaluate_cell(cell: CorrelationCell, n_resamples: int) -> dict[str, object]:
    rho = spearman(cell.x_est, cell.y_est)
    rho_seed_centered = spearman(center_within(cell.x, cell.seed), center_within(cell.y, cell.seed))

    rng = rng_for(cell.key)
    team_boot = bootstrap_spearman(
        cell.x, cell.y, cell.seed, stratified_bootstrap_indices(cell.seed, n_resamples, rng), cell.recenter
    )
    season_boot = bootstrap_spearman(
        cell.x, cell.y, cell.seed, cluster_bootstrap_indices(cell.year, n_resamples, rng), cell.recenter
    )
    null_seed = permutation_spearman_null(cell.x_est, cell.y_est, cell.seed, n_resamples, rng)
    null_seed_season = permutation_spearman_null(
        cell.x_est, cell.y_est, cell.seed * 10_000 + cell.year, n_resamples, rng
    )
    team_lo, team_hi = percentile_ci(team_boot)
    season_lo, season_hi = percentile_ci(season_boot)
    return {
        "family": cell.family,
        "topic": cell.topic,
        "statistic": cell.statistic,
        "metric": cell.metric,
        "seed_group": cell.seed_group,
        "estimator": cell.estimator,
        "n": int(len(cell.x)),
        "seasons": int(len(np.unique(cell.year))),
        "published_rho": cell.published,
        "published_team_count": cell.published_n,
        "rho": rho,
        "matches_published": bool(abs(rho - cell.published) <= cell.published_tolerance)
        if np.isfinite(cell.published)
        else False,
        "rho_exact_seed_centered": rho_seed_centered,
        "ci_lo": team_lo,
        "ci_hi": team_hi,
        "season_ci_lo": season_lo,
        "season_ci_hi": season_hi,
        "null_mean_within_seed": float(np.mean(null_seed)),
        "null_mean_seed_season": float(np.mean(null_seed_season)),
        "p_perm": permutation_pvalue(rho, null_seed),
        "p_perm_seed_season": permutation_pvalue(rho, null_seed_season),
        "modal_finish_share": modal_share_within(cell.y, cell.seed),
        "source": cell.source,
        "note": cell.note,
    }


def _published_value(table: pd.DataFrame, column: str, seed_group: str) -> tuple[float, float]:
    row = table.loc[table["seed_group"].astype(str) == seed_group]
    if row.empty:
        return np.nan, np.nan
    count_column = "team_count" if "team_count" in table.columns else "team_count_all"
    if column.endswith("since_2014"):
        count_column = "team_count_since_2014"
    if column == "coach_exp_rho" and "coach_team_count" in table.columns:
        count_column = "coach_team_count"
    return float(row[column].iloc[0]), float(row[count_column].iloc[0])


def build_sos_within_seed_cell(sample_path: Path, published_md: Path) -> CorrelationCell:
    sos = importlib.import_module("sos_analysis_02_04")
    df = sos.load_tournament_data(sample_path).reset_index(drop=True)
    adjusted, _ = sos.build_analysis_02_seed_controlled(df, "Elite SOS")
    # The published value is the 3-decimal number printed in the report, not a recomputation.
    published, tolerance = read_published_md_value(published_md)
    return CorrelationCell(
        family="a_sos_within_seed",
        topic="sos",
        statistic="within_seed_rho",
        metric="Elite SOS",
        seed_group="all (seed-centered)",
        source=f"reports/{published_md.name}",
        published=published,
        x_est=adjusted["metric_vs_seed"].to_numpy(float),
        y_est=adjusted["finish_vs_seed"].to_numpy(float),
        x=df["Elite SOS"].to_numpy(float),
        y=df["finish_depth"].to_numpy(float),
        seed=df["seed_int"].to_numpy(int),
        year=df["year"].to_numpy(int),
        recenter=True,
        estimator="exact-seed centered x and y",
        published_n=float(len(df)),
        published_tolerance=tolerance,
    )


def _band_cells(
    *,
    topic: str,
    frame: pd.DataFrame,
    table: pd.DataFrame,
    source: str,
    column: str,
    metric: str,
    x_est_column: str,
    y_est_column: str,
    x_column: str,
    y_column: str,
    recenter: bool,
    estimator: str,
    bands: list[str],
    note: str = "",
) -> list[CorrelationCell]:
    cells = []
    for seed_group in bands:
        subset = frame.loc[frame["seed_group"] == seed_group]
        published, published_n = _published_value(table, column, seed_group)
        cells.append(
            CorrelationCell(
                family="b_seed_band",
                topic=topic,
                statistic=column,
                metric=metric,
                seed_group=seed_group,
                source=source,
                published=published,
                x_est=subset[x_est_column].to_numpy(float),
                y_est=subset[y_est_column].to_numpy(float),
                x=subset[x_column].to_numpy(float),
                y=subset[y_column].to_numpy(float),
                seed=subset["seed_int"].to_numpy(int),
                year=subset["year"].to_numpy(int),
                recenter=recenter,
                estimator=estimator,
                note=note,
                published_n=published_n,
            )
        )
    return cells


# (topic, common module, loader, paths fn, published csv, [(column, metric, since_year)])
EDGE_TOPICS: list[tuple[str, str, str, str, str, list[tuple[str, str, int | None]]]] = [
    (
        "free_throw_rate",
        "free_throw_rate_analysis_common",
        "load_free_throw_rate_data",
        "get_free_throw_rate_base_paths",
        "free_throw_rate_vs_finish_by_seed_group_table.csv",
        [("ftr_rho", "FTR", None), ("ft_pct_rho", "FT%", None), ("ftrd_rho", "FTRD", None)],
    ),
    (
        "rebounding",
        "rebounding_analysis_common",
        "load_rebounding_data",
        "get_rebounding_base_paths",
        "rebounding_vs_finish_by_seed_group_table.csv",
        [("orb_seed_adjusted_rho", "ORB", None), ("drb_seed_adjusted_rho", "DRB", None)],
    ),
    (
        "tempo",
        "tempo_analysis_common",
        "load_tempo_data",
        "get_tempo_base_paths",
        "tempo_vs_finish_by_seed_group_table.csv",
        [("adjt_rho_all", "AdjT", None), ("adjt_rho_since_2014", "AdjT", 2014)],
    ),
    (
        "three_point",
        "three_point_analysis_common",
        "load_three_point_data",
        "get_three_point_base_paths",
        "three_point_vs_finish_by_seed_group_table.csv",
        [
            ("three_prate_seed_band_rho", "3PRATE", None),
            ("three_pct_seed_band_rho", "3P%", None),
            ("three_pct_d_seed_band_rho", "3P%D", None),
        ],
    ),
    (
        "turnovers",
        "turnovers_analysis_common",
        "load_turnovers_data",
        "get_turnovers_base_paths",
        "turnovers_vs_finish_by_seed_group_table.csv",
        [("tor_seed_band_rho", "TOR", None), ("tord_seed_band_rho", "TORD", None)],
    ),
    (
        "turnovers_blocks",
        "turnovers_analysis_common",
        "load_turnovers_data",
        "get_turnovers_base_paths",
        "turnovers_blocks_vs_finish_by_seed_group_table.csv",
        [("blk_pct_seed_band_rho", "BLK%", None), ("blked_pct_seed_band_rho", "BLKED%", None)],
    ),
]


def build_edge_topic_cells(reports_dir: Path) -> list[CorrelationCell]:
    """Topics whose band rho is Spearman(overperformance, metric edge vs exact-seed mean)."""
    cells: list[CorrelationCell] = []
    for topic, module_name, loader_name, paths_fn, csv_name, specs in EDGE_TOPICS:
        common = importlib.import_module(module_name)
        full = getattr(common, loader_name)(getattr(common, paths_fn)()["input_sample"])
        over = common.build_overperformance_data(common.load_tournament_data(full)).reset_index(drop=True)
        table = pd.read_csv(reports_dir / csv_name)
        for column, metric, since_year in specs:
            sign = -1.0 if metric in common.LOWER_BETTER else 1.0
            edge_column = f"{common.metric_slug(metric)}_edge"
            over[f"_x_{column}"] = sign * over[metric]
            # Guard: the published edge is exactly sign * (metric - exact-seed mean).
            rebuilt = center_within(over[f"_x_{column}"].to_numpy(float), over["seed_int"].to_numpy())
            if not np.allclose(rebuilt, over[edge_column].to_numpy(float)):
                raise ValueError(f"{topic}: could not rebuild {edge_column} from {metric}")
            if since_year is None:
                frame, x_column, y_column, recenter = over, f"_x_{column}", "finish_depth", True
                estimator = "exact-seed centered x and y"
            else:
                # Published code keeps the full-sample seed baselines for the post-2014 subset.
                frame = over.loc[over["year"] >= since_year]
                x_column, y_column, recenter = edge_column, "overperformance", False
                estimator = f"{since_year}+ subset, full-sample seed baselines"
            cells.extend(
                _band_cells(
                    topic=topic,
                    frame=frame,
                    table=table,
                    source=f"reports/{csv_name}",
                    column=column,
                    metric=metric,
                    x_est_column=edge_column,
                    y_est_column="overperformance",
                    x_column=x_column,
                    y_column=y_column,
                    recenter=recenter,
                    estimator=estimator,
                    bands=SEED_GROUP_ORDER,
                )
            )
    return cells


def build_size_cells(reports_dir: Path) -> list[CorrelationCell]:
    common = importlib.import_module("size_analysis_common")
    df = common.load_tournament_data(common.load_size_data(common.get_size_base_paths()["input_sample"]))
    csv_name = "size_vs_finish_by_seed_group_table.csv"
    return _band_cells(
        topic="size",
        frame=df.reset_index(drop=True),
        table=pd.read_csv(reports_dir / csv_name),
        source=f"reports/{csv_name}",
        column="effhgt_rho",
        metric="EFFHGT",
        x_est_column="EFFHGT",
        y_est_column="finish_depth",
        x_column="EFFHGT",
        y_column="finish_depth",
        recenter=False,
        estimator="raw metric vs finish inside band (not seed-line centered)",
        bands=SEED_GROUP_ORDER,
    )


def build_sos_band_cells(reports_dir: Path, sample_path: Path) -> list[CorrelationCell]:
    sos = importlib.import_module("sos_analysis_02_04")
    df = sos.load_tournament_data(sample_path).reset_index(drop=True)
    df = df.loc[df["seed_int"].between(1, 13)].copy()
    df["seed_group"] = df["seed_int"].map(SEED_GROUP_MAP)
    csv_name = "sos_vs_finish_by_seed_group_table.csv"
    table = pd.read_csv(reports_dir / csv_name)
    cells = []
    for column, metric in [("elite_sos_spearman", "Elite SOS"), ("conf_avg_spearman", "conf_avg_barthag")]:
        cells.extend(
            _band_cells(
                topic="sos",
                frame=df,
                table=table,
                source=f"reports/{csv_name}",
                column=column,
                metric=metric,
                x_est_column=metric,
                y_est_column="finish_depth",
                x_column=metric,
                y_column="finish_depth",
                recenter=False,
                estimator="raw metric vs finish inside band (not seed-line centered)",
                bands=SEED_GROUP_ORDER[:-1],
            )
        )
    return cells


def misaligned_player_rho(over_subset: pd.DataFrame, edge_values: np.ndarray) -> tuple[float, int]:
    """Replicates the pre-fix experience_analysis_02_04.build_seed_group_table: the edge Series got a
    fresh 0..n-1 index, so pandas paired it with whichever overperformance rows shared those labels."""
    edge = pd.Series(edge_values)
    paired = int(len(over_subset.index.intersection(edge.index)))
    return float(over_subset["overperformance"].corr(edge, method="spearman")), paired


def build_experience_cells(reports_dir: Path) -> tuple[list[CorrelationCell], pd.DataFrame]:
    common = importlib.import_module("experience_analysis_common")
    full = common.load_experience_data(common.get_experience_base_paths()["input_sample"])
    tournament_df = common.load_tournament_data(full)
    coach_tournament_df = common.filter_coach_analysis(tournament_df)
    coach_metric, _ = common.compute_coach_metric_selection(coach_tournament_df)
    over = common.build_overperformance_data(tournament_df)
    csv_name = "experience_vs_finish_by_seed_group_table.csv"
    table = pd.read_csv(reports_dir / csv_name)

    player_slug = common.metric_slug(common.PLAYER_SUPPORT_METRIC)
    audit_rows = []
    for seed_group in SEED_GROUP_ORDER:
        subset = over.loc[over["seed_group"] == seed_group]
        edge_values = -subset[f"{player_slug}_vs_seed"].to_numpy(float)
        published_like, paired = misaligned_player_rho(subset, edge_values)
        audit_rows.append(
            {
                "scope": seed_group,
                "rows": int(len(subset)),
                "rows_actually_paired": paired,
                "old_code_rho": published_like,
                "aligned_rho": float(subset["overperformance"].corr(pd.Series(edge_values, index=subset.index), method="spearman")),
            }
        )
    # The pooled experience numbers (reports/player_exp_vs_finish_pooled_summary.md) had the same pattern.
    pooled_edge = -over[f"{player_slug}_vs_seed"].to_numpy(float)
    pooled_like, pooled_paired = misaligned_player_rho(over, pooled_edge)
    audit_rows.append(
        {
            "scope": "pooled seed-adjusted",
            "rows": int(len(over)),
            "rows_actually_paired": pooled_paired,
            "old_code_rho": pooled_like,
            "aligned_rho": float(over["overperformance"].corr(pd.Series(pooled_edge, index=over.index), method="spearman")),
        }
    )
    raw_edge = -tournament_df[common.PLAYER_SUPPORT_METRIC].to_numpy(float)
    raw_like = float(tournament_df["finish_depth"].corr(pd.Series(raw_edge), method="spearman"))
    audit_rows.append(
        {
            "scope": "pooled raw",
            "rows": int(len(tournament_df)),
            "rows_actually_paired": int(len(tournament_df.index.intersection(pd.RangeIndex(len(raw_edge))))),
            "old_code_rho": raw_like,
            "aligned_rho": float(
                tournament_df["finish_depth"].corr(pd.Series(raw_edge, index=tournament_df.index), method="spearman")
            ),
        }
    )
    audit = pd.DataFrame(audit_rows)

    # player_visual_edge() is a sign flip: higher = more experienced than the seed line's average.
    frame = over.reset_index(drop=True).copy()
    frame["_x_player"] = -frame[common.PLAYER_SUPPORT_METRIC]
    frame["_x_player_edge"] = -frame[f"{player_slug}_vs_seed"]
    cells = _band_cells(
        topic="experience",
        frame=frame,
        table=table,
        source=f"reports/{csv_name}",
        column="player_exp_rho",
        metric=common.PLAYER_SUPPORT_METRIC,
        x_est_column="_x_player_edge",
        y_est_column="overperformance",
        x_column="_x_player",
        y_column="finish_depth",
        recenter=True,
        estimator="exact-seed centered x and y",
        bands=SEED_GROUP_ORDER,
        note="row-alignment bug fixed in experience_analysis_02_04.py; published and rho are both aligned",
    )
    coach_frame = common.build_overperformance_data(coach_tournament_df).reset_index(drop=True)
    cells.extend(
        _band_cells(
            topic="experience",
            frame=coach_frame,
            table=table,
            source=f"reports/{csv_name}",
            column="coach_exp_rho",
            metric=coach_metric,
            x_est_column=f"{common.metric_slug(coach_metric)}_vs_seed",
            y_est_column="overperformance",
            x_column=coach_metric,
            y_column="finish_depth",
            recenter=True,
            estimator=f"exact-seed centered x and y, {common.COACH_ANALYSIS_START_YEAR}+ only",
            bands=SEED_GROUP_ORDER,
            note=f"coach rho uses {common.COACH_ANALYSIS_START_YEAR}+ only; published n is the coach_team_count column",
        )
    )
    return cells, audit


def build_band_cells(paths: dict[str, Path]) -> tuple[list[CorrelationCell], pd.DataFrame]:
    experience_cells, experience_audit = build_experience_cells(paths["reports"])
    cells = (
        build_size_cells(paths["reports"])
        + build_sos_band_cells(paths["reports"], paths["sos_sample"])
        + experience_cells
        + build_edge_topic_cells(paths["reports"])
    )
    return cells, experience_audit


def run_band_family(cells: list[CorrelationCell], n_resamples: int) -> pd.DataFrame:
    table = pd.DataFrame([evaluate_cell(cell, n_resamples) for cell in cells])
    # Primary family: every published band cell pooled (the conservative choice). The narrower
    # per-table and per-statistic families are reported so the dependence on that choice is visible.
    table["q_bh"] = benjamini_hochberg(table["p_perm"].to_numpy())
    table["q_bh_within_table"] = benjamini_hochberg_within(table["p_perm"].to_numpy(), table["source"].to_numpy())
    table["q_bh_within_statistic"] = benjamini_hochberg_within(
        table["p_perm"].to_numpy(), (table["source"] + "|" + table["statistic"]).to_numpy()
    )
    table["ci_excludes_zero"] = (table["ci_lo"] > 0) | (table["ci_hi"] < 0)
    table["verdict"] = [
        family_verdict(p, q, q_table) for p, q, q_table in zip(table["p_perm"], table["q_bh"], table["q_bh_within_table"])
    ]
    return table


# ---------------------------------------------------------------------------
# Claim (c): conference overperformance
# ---------------------------------------------------------------------------


def load_conference_data(sample_path: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    sos57 = importlib.import_module("sos_analysis_05_07")
    full = sos57.load_data(sample_path)
    over = sos57.build_overperformance_data(sos57.load_tournament_data(full)).reset_index(drop=True)
    _, conference_table = sos57.build_analysis_05_tables(over)
    return over, conference_table.reset_index(drop=True)


def run_conference_checks(
    over: pd.DataFrame,
    conference_table: pd.DataFrame,
    n_resamples: int,
) -> dict[str, object]:
    conferences = conference_table["Conf"].tolist()
    y = over["overperformance"].to_numpy(float)
    seed = over["seed_int"].to_numpy(int)
    year = over["year"].to_numpy(int)
    conf = over["Conf"].to_numpy()
    team = over["Team"].astype(str).to_numpy()

    # Groups tested under the permutation null: every reported conference plus Horizon without Butler.
    group_masks = {name: conf == name for name in conferences}
    drop_label = f"{FOCUS_CONFERENCES[0]} excl. {HORIZON_DROP_PROGRAM}"
    group_masks[drop_label] = (conf == FOCUS_CONFERENCES[0]) & (team != HORIZON_DROP_PROGRAM)
    labels = list(group_masks)
    membership = np.column_stack([group_masks[label] for label in labels]).astype(float)
    observed = (y @ membership) / membership.sum(axis=0)

    null_seed = permutation_group_means(y, membership, seed, n_resamples, rng_for("c|perm|seed"))
    null_seed_season = permutation_group_means(
        y, membership, seed * 10_000 + year, n_resamples, rng_for("c|perm|seed_season")
    )

    # Under the within-seed null each team's variance is its seed line's finish variance.
    seed_var = over.groupby("seed_int")["finish_depth"].var(ddof=1)
    team_var = over["seed_int"].map(seed_var).to_numpy(float)
    null_var = (team_var @ membership) / membership.sum(axis=0) ** 2

    n_conf = len(conferences)
    eb = dersimonian_laird(observed[:n_conf], null_var[:n_conf])
    q_null = cochran_q(null_seed[:, :n_conf], null_var[:n_conf])
    max_null = null_seed[:, :n_conf].max(axis=1)
    min_null = null_seed[:, :n_conf].min(axis=1)
    # Where each group's mean lands 95% of the time if its teams were random draws from their seed lines.
    null_lo, null_hi = np.percentile(null_seed, [2.5, 97.5], axis=0)

    rows = []
    for k, label in enumerate(labels):
        mask = group_masks[label]
        values = y[mask]
        rng = rng_for(f"c|boot|{label}")
        # iid bootstrap of this conference's own team-seasons; seed baselines stay fixed.
        team_boot = cluster_bootstrap_means(values, np.arange(len(values)), n_resamples, rng)
        season_boot = cluster_bootstrap_means(values, year[mask], n_resamples, rng)
        program_boot = cluster_bootstrap_means(values, team[mask], n_resamples, rng)
        seasons = int(len(np.unique(year[mask])))
        programs = int(len(np.unique(team[mask])))
        share_positive = float(np.mean(values > 0))
        boot_sd_ratio = float(np.std(team_boot) / np.sqrt(null_var[k]))
        row = {
            "Conf": label,
            "n": int(mask.sum()),
            "seasons": seasons,
            "programs": programs,
            "mean_overperformance": float(observed[k]),
            "median_overperformance": float(np.median(values)),
            "share_positive": share_positive,
            "null_lo": float(null_lo[k]),
            "null_hi": float(null_hi[k]),
            "ci_lo": percentile_ci(team_boot)[0],
            "ci_hi": percentile_ci(team_boot)[1],
            "season_ci_lo": percentile_ci(season_boot)[0],
            "season_ci_hi": percentile_ci(season_boot)[1],
            "program_ci_lo": percentile_ci(program_boot)[0],
            "program_ci_hi": percentile_ci(program_boot)[1],
            "null_sd": float(np.sqrt(null_var[k])),
            "boot_sd_ratio": boot_sd_ratio,
            "ci_note": conference_ci_note(share_positive, boot_sd_ratio, seasons, programs),
            "p_perm": permutation_pvalue(observed[k], null_seed[:, k]),
            "p_perm_seed_season": permutation_pvalue(observed[k], null_seed_season[:, k]),
        }
        if k < n_conf:
            row["eb_shrink_weight"] = float(eb["shrink"][k])
            row["eb_mean"] = float(eb["shrunk"][k])
            row["eb_lo"] = float(eb["shrunk"][k] - 1.96 * eb["posterior_sd"][k])
            row["eb_hi"] = float(eb["shrunk"][k] + 1.96 * eb["posterior_sd"][k])
        rows.append(row)
    table = pd.DataFrame(rows)
    in_family = table["Conf"].isin(conferences)
    table.loc[in_family, "q_bh"] = benjamini_hochberg(table.loc[in_family, "p_perm"].to_numpy())

    loo = leave_one_program_out(over, conferences)
    loo_summary = (
        loo.groupby("Conf")
        .agg(loo_min_mean=("mean_without", "min"), loo_max_mean=("mean_without", "max"))
        .reset_index()
    )
    most_influential = loo.loc[loo.groupby("Conf")["abs_shift"].idxmax(), ["Conf", "program", "mean_without"]]
    most_influential = most_influential.rename(
        columns={"program": "most_influential_program", "mean_without": "mean_without_most_influential"}
    )
    table = table.merge(loo_summary, on="Conf", how="left").merge(most_influential, on="Conf", how="left")
    table = table.sort_values("mean_overperformance", ascending=False, kind="stable").reset_index(drop=True)

    best_k = int(np.argmax(observed[:n_conf]))
    worst_k = int(np.argmin(observed[:n_conf]))
    tolerance = 1e-12
    omnibus = {
        "conferences": n_conf,
        "best_conf": conferences[best_k],
        "best_mean": float(observed[best_k]),
        "p_best_is_extreme": float((1 + np.sum(max_null >= observed[best_k] - tolerance)) / (len(max_null) + 1)),
        "null_max_median": float(np.median(max_null)),
        "worst_conf": conferences[worst_k],
        "worst_mean": float(observed[worst_k]),
        "p_worst_is_extreme": float((1 + np.sum(min_null <= observed[worst_k] + tolerance)) / (len(min_null) + 1)),
        "null_min_median": float(np.median(min_null)),
        "q_stat": float(eb["q_stat"]),
        "q_dof": int(eb["dof"]),
        "q_p_chi2": float(chi2.sf(eb["q_stat"], eb["dof"])),
        "q_p_perm": float((1 + np.sum(q_null >= eb["q_stat"] - tolerance)) / (len(q_null) + 1)),
        "tau2": float(eb["tau2"]),
        "eb_grand_mean": float(eb["grand_mean"]),
    }
    team_seasons = focus_team_seasons(over)
    return {"table": table, "omnibus": omnibus, "loo": loo, "team_seasons": team_seasons}


def leave_one_program_out(over: pd.DataFrame, conferences: list[str]) -> pd.DataFrame:
    rows = []
    for conf_name in conferences:
        subset = over.loc[over["Conf"] == conf_name]
        full_mean = float(subset["overperformance"].mean())
        for program, group in subset.groupby("Team", sort=True):
            rest = subset.loc[subset["Team"] != program, "overperformance"]
            mean_without = float(rest.mean()) if len(rest) else np.nan
            rows.append(
                {
                    "Conf": conf_name,
                    "program": program,
                    "program_seasons": int(len(group)),
                    "program_overperformance_sum": float(group["overperformance"].sum()),
                    "conf_mean": full_mean,
                    "n_without": int(len(rest)),
                    "mean_without": mean_without,
                    "abs_shift": abs(mean_without - full_mean),
                }
            )
    return pd.DataFrame(rows)


def focus_team_seasons(over: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for conf_name in FOCUS_CONFERENCES:
        subset = over.loc[over["Conf"] == conf_name, ["Conf", "Team", "year", "seed_int", "finish", "overperformance"]]
        ascending = conf_name != FOCUS_CONFERENCES[0]
        subset = subset.sort_values("overperformance", ascending=ascending, kind="stable").copy()
        total = float(subset["overperformance"].sum())
        subset["conf_total_overperformance"] = total
        subset["cumulative_overperformance"] = subset["overperformance"].cumsum()
        subset["rank_in_conf"] = np.arange(1, len(subset) + 1)
        rows.append(subset)
    return pd.concat(rows, ignore_index=True)


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------


def fmt_p(p: float, n_resamples: int) -> str:
    floor = 2 / (n_resamples + 1)
    if p <= floor + 1e-12:
        return f"<={floor:.4f}"
    return f"{p:.4f}" if p < 0.01 else f"{p:.3f}"


def fmt_ci(lo: float, hi: float) -> str:
    return f"[{lo:+.3f}, {hi:+.3f}]"


def markdown_table(df: pd.DataFrame, digits: int = 3) -> str:
    """Small to_markdown stand-in so the report does not need the optional tabulate dependency."""

    def cell(value: object) -> str:
        if isinstance(value, (bool, np.bool_)):
            return str(bool(value))
        if isinstance(value, (int, np.integer)):
            return str(int(value))
        if isinstance(value, (float, np.floating)):
            return "" if np.isnan(value) else f"{value:.{digits}f}"
        return str(value)

    header = "| " + " | ".join(str(column) for column in df.columns) + " |"
    align = "|" + "|".join("--:" if pd.api.types.is_numeric_dtype(df[column]) else ":--" for column in df.columns) + "|"
    body = ["| " + " | ".join(cell(value) for value in row) + " |" for row in df.itertuples(index=False)]
    return "\n".join([header, align, *body])


def ci_excludes_zero(lo: float, hi: float) -> bool:
    return bool(lo > 0 or hi < 0)


def single_test_verdict(p_value: float, lo: float, hi: float) -> str:
    significant = p_value < ALPHA
    excludes = ci_excludes_zero(lo, hi)
    if significant and excludes:
        return "distinguishable from zero, but small"
    if significant or excludes:
        return "borderline (permutation and bootstrap disagree at 5%)"
    return "not distinguishable from zero"


def family_verdict(p_value: float, q_value: float, q_within_table: float = np.nan) -> str:
    if np.isfinite(q_value) and q_value < ALPHA:
        return "survives BH"
    if np.isfinite(q_within_table) and q_within_table < ALPHA:
        return "survives per-table BH only (fails pooled BH)"
    if p_value < ALPHA:
        return "nominal only (fails BH)" if np.isfinite(q_value) else "nominal only"
    return "not distinguishable from noise"


def band_row(bands: pd.DataFrame, topic: str, statistic: str, seed_group: str) -> pd.Series:
    return bands.loc[
        (bands["topic"] == topic) & (bands["statistic"] == statistic) & (bands["seed_group"] == seed_group)
    ].iloc[0]


def topic_rollup(bands: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for (topic, statistic), group in bands.groupby(["topic", "statistic"], sort=False):
        strongest = group.loc[group["rho"].abs().idxmax()]
        rows.append(
            {
                "topic": topic,
                "statistic": statistic,
                "bands": int(len(group)),
                "max_abs_rho": float(abs(strongest["rho"])),
                "at_band": strongest["seed_group"],
                "nominal_p_lt_05": int((group["p_perm"] < ALPHA).sum()),
                "min_p": float(group["p_perm"].min()),
                "min_q": float(group["q_bh"].min()),
                "min_q_within_table": float(group["q_bh_within_table"].min()),
            }
        )
    return pd.DataFrame(rows)


def describe_years(years: np.ndarray) -> str:
    """e.g. '17 tournaments (2008-2025, no 2020)'."""
    years = np.sort(np.unique(np.asarray(years, dtype=int)))
    missing = sorted(set(range(int(years[0]), int(years[-1]) + 1)) - set(years.tolist()))
    gap = f", no {', '.join(str(year) for year in missing)}" if missing else ""
    return f"{len(years)} tournaments ({years[0]}-{years[-1]}{gap})"


def describe_seed_season_strata(sizes: pd.Series) -> str:
    """How many teams the seed-and-season permutation shuffles among, e.g. 'the 4 same-seed teams of one season'."""
    low, high = int(sizes.min()), int(sizes.max())
    if low == high:
        return f"the {low} same-seed teams of one season"
    return f"the same-seed teams of one season ({low}-{high} per seed line and season)"


def band_cell_label(row: pd.Series) -> str:
    return f"{row['topic']} {row['metric']} seeds {row['seed_group']}"


def write_summary(
    path: Path,
    sos_row: pd.Series,
    bands: pd.DataFrame,
    experience_audit: pd.DataFrame,
    conf: dict[str, object],
    n_resamples: int,
    sample: dict[str, object],
) -> None:
    conf_table: pd.DataFrame = conf["table"]
    omnibus: dict[str, object] = conf["omnibus"]
    team_seasons: pd.DataFrame = conf["team_seasons"]
    loo: pd.DataFrame = conf["loo"]

    def p(value: float) -> str:
        return fmt_p(value, n_resamples)

    years_text = describe_years(sample["years"])
    strata_text = describe_seed_season_strata(sample["seed_season_sizes"])
    lead_conf, trail_conf = FOCUS_CONFERENCES
    horizon = conf_table.loc[conf_table["Conf"] == lead_conf].iloc[0]
    mwc = conf_table.loc[conf_table["Conf"] == trail_conf].iloc[0]
    horizon_drop = conf_table.loc[conf_table["Conf"] == f"{lead_conf} excl. {HORIZON_DROP_PROGRAM}"].iloc[0]
    butler = loo.loc[(loo["Conf"] == lead_conf) & (loo["program"] == HORIZON_DROP_PROGRAM)].iloc[0]
    horizon_total = float(butler["conf_mean"] * horizon["n"])
    size_top = band_row(bands, "size", "effhgt_rho", "1-2")
    size_78 = band_row(bands, "size", "effhgt_rho", "7-8")
    sos_1113 = band_row(bands, "sos", "elite_sos_spearman", "11-13")
    exp_top = band_row(bands, "experience", "player_exp_rho", "1-2")
    exp_player = bands.loc[(bands["topic"] == "experience") & (bands["statistic"] == "player_exp_rho")]
    old_band_rhos = experience_audit.loc[experience_audit["scope"].isin(SEED_GROUP_ORDER), "old_code_rho"]
    old_exp_top = float(experience_audit.loc[experience_audit["scope"] == "1-2", "old_code_rho"].iloc[0])
    n_cells = len(bands)
    nominal = int((bands["p_perm"] < ALPHA).sum())
    bh_05 = bands.loc[bands["q_bh"] < ALPHA]
    bh_10 = bands.loc[bands["q_bh"] < 0.10]
    table_05 = bands.loc[bands["q_bh_within_table"] < ALPHA].sort_values("q_bh_within_table", kind="stable")
    statistic_05 = bands.loc[bands["q_bh_within_statistic"] < ALPHA].sort_values("q_bh_within_statistic", kind="stable")
    best_cell = bands.loc[bands["q_bh"].idxmin()]
    in_family = conf_table["q_bh"].notna()
    conf_bh = conf_table.loc[in_family & (conf_table["q_bh"] < ALPHA), "Conf"].tolist()
    conf_nominal = conf_table.loc[in_family & (conf_table["p_perm"] < ALPHA), "Conf"].tolist()
    max_abs_eb = float(conf_table.loc[in_family, "eb_mean"].abs().max())
    horizon_top = team_seasons.loc[team_seasons["Conf"] == lead_conf].head(3)
    mwc_top = team_seasons.loc[team_seasons["Conf"] == trail_conf].head(3)
    mwc_loo = loo.loc[loo["Conf"] == trail_conf].sort_values("abs_shift", ascending=False).iloc[0]
    verdict_a = single_test_verdict(sos_row["p_perm"], sos_row["ci_lo"], sos_row["ci_hi"])
    season_share = sos_row["null_mean_seed_season"] / sos_row["rho"] if sos_row["rho"] else np.nan
    team_width = sos_row["ci_hi"] - sos_row["ci_lo"]
    season_width = sos_row["season_ci_hi"] - sos_row["season_ci_lo"]
    low_seed = bands.loc[bands["seed_group"] == SEED_GROUP_ORDER[-1]]
    # The low-seed cell whose estimate sits furthest off-centre in its own bootstrap interval.
    off_centre = (low_seed["rho"] - (low_seed["ci_lo"] + low_seed["ci_hi"]) / 2).abs() / (
        low_seed["ci_hi"] - low_seed["ci_lo"]
    )
    lopsided = low_seed.loc[off_centre.idxmax()]
    # Conferences whose bootstrap CIs should not be read at face value (rules in conference_ci_note).
    family_rows = conf_table.loc[in_family]
    no_upset = family_rows.loc[family_rows["share_positive"] == 0, "Conf"].tolist()
    narrow = family_rows.loc[
        (family_rows["share_positive"] > 0) & (family_rows["boot_sd_ratio"] < BOOT_SPREAD_FLOOR), "Conf"
    ].tolist()
    few_programs = family_rows.loc[family_rows["programs"] < MIN_CLUSTERS, "Conf"].tolist()
    few_seasons = family_rows.loc[family_rows["seasons"] < MIN_CLUSTERS, "Conf"].tolist()
    band_seasons = bands.groupby(["topic", "statistic"], sort=False)["seasons"].max().reset_index()
    subset_bands = band_seasons.loc[band_seasons["seasons"] < len(sample["years"])]

    if butler["program_overperformance_sum"] >= horizon_total:
        butler_text = (
            f"{HORIZON_DROP_PROGRAM}'s {int(butler['program_seasons'])} appearances sum to "
            f"`{butler['program_overperformance_sum']:+.3f}`, more than the conference's entire `{horizon_total:+.3f}` total"
        )
    else:
        butler_text = (
            f"{HORIZON_DROP_PROGRAM}'s {int(butler['program_seasons'])} appearances sum to "
            f"`{butler['program_overperformance_sum']:+.3f}` of the conference's `{horizon_total:+.3f}` total"
        )
    conference_signal = (
        "no detectable conference effect beyond seed"
        if omnibus["q_p_perm"] >= ALPHA
        else "some conference heterogeneity beyond seed"
    )
    if season_width <= team_width * 1.05:
        season_ci_text = (
            f"The season-cluster CI {fmt_ci(sos_row['season_ci_lo'], sos_row['season_ci_hi'])} is no wider than the team CI,"
            " so resampling whole tournaments adds no visible uncertainty."
        )
    else:
        season_ci_text = (
            f"The season-cluster CI {fmt_ci(sos_row['season_ci_lo'], sos_row['season_ci_hi'])} is wider than the team CI;"
            " quote that one."
        )
    if len(table_05):
        table_cells_text = "; ".join(
            f"{band_cell_label(row)} (rho `{row['rho']:+.3f}`, per-table q = {row['q_bh_within_table']:.3f}, "
            f"seed-and-season p = {p(row['p_perm_seed_season'])}, within-seed null mean `{row['null_mean_within_seed']:+.3f}`, "
            f"exact-seed-centered rho `{row['rho_exact_seed_centered']:+.3f}`)"
            for _, row in table_05.iterrows()
        )
        family_text = (
            f"  The count depends on the family. With BH applied within each published table, {len(table_05)} survive: "
            f"{table_cells_text}. Within each statistic's bands, {len(statistic_05)} survive"
            + (f" ({', '.join(band_cell_label(row) for _, row in statistic_05.iterrows())})" if len(statistic_05) else "")
            + "."
        )
    else:
        family_text = "  Applying BH within each published table instead of across all cells leaves none surviving either."

    lines = [
        "# Robustness Checks: Uncertainty on the Headline Descriptive Claims",
        "",
        f"Generated by `scripts/robustness_checks.py` ({n_resamples:,} resamples per test, fixed seeds). Every point",
        "estimate was first re-derived with the same loaders and table builders that wrote the original reports, then",
        "compared with the published file: (b) band values and (c) conference means and medians against the published",
        "CSVs to 6 decimals, (a) against the 3-decimal value printed in",
        f"`{sos_row['source']}`. The `matches_published` column in each CSV records the result.",
        "",
        "## Bottom line",
        "",
        f"- **(a) SOS within seed line:** rho `{sos_row['rho']:+.3f}` (n = {sos_row['n']}), 95% CI "
        f"{fmt_ci(sos_row['ci_lo'], sos_row['ci_hi'])}, permutation p = {p(sos_row['p_perm'])}: {verdict_a}.",
        f"  About {season_share:.0%} of the estimate sits at the seed-line x season level: a null that shuffles only among",
        f"  {strata_text} has mean `{sos_row['null_mean_seed_season']:+.3f}`, and against it the association is not",
        f"  detectable (p = {p(sos_row['p_perm_seed_season'])}). That is a narrower, lower-power test, so it does not show the",
        f"  estimate is spurious. {season_ci_text}",
        f"- **(b) Seed-band correlations:** pooled across all {n_cells} cells (the conservative family), {len(bh_05)} survive "
        f"Benjamini-Hochberg at q < {ALPHA} ({len(bh_10)} at q < 0.10). {nominal} have nominal p < {ALPHA}, against "
        f"~{ALPHA * n_cells:.1f} expected by chance. Smallest pooled q = {best_cell['q_bh']:.3f} ({band_cell_label(best_cell)}).",
        family_text,
        f"  The quoted size numbers (EFFHGT `{size_top['rho']:+.3f}` seeds 1-2, `{size_78['rho']:+.3f}` seeds 7-8) have "
        f"p = {p(size_top['p_perm'])} / {p(size_78['p_perm'])} and pooled q = {size_top['q_bh']:.3f} / {size_78['q_bh']:.3f}.",
        f"  The player-experience band values were corrected for a row-misalignment bug (seeds 1-2 was published as",
        f"  `{old_exp_top:+.3f}`, now `{exp_top['published_rho']:+.3f}`); the largest |rho| is now `{exp_player['rho'].abs().max():.3f}`.",
        f"- **(c) Conference overperformance:** {lead_conf} `{horizon['mean_overperformance']:+.3f}` has median "
        f"`{horizon['median_overperformance']:+.3f}`; {butler_text}. Without {HORIZON_DROP_PROGRAM} the mean is "
        f"`{horizon_drop['mean_overperformance']:+.3f}` (p = {p(horizon_drop['p_perm'])}).",
        f"  A conference with {lead_conf}'s seed mix lands in {fmt_ci(horizon['null_lo'], horizon['null_hi'])} 95% of the time "
        f"under the within-seed null. {lead_conf} (p = {p(horizon['p_perm'])}, seed-and-season p = "
        f"{p(horizon['p_perm_seed_season'])}, q = {horizon['q_bh']:.3f}) and {trail_conf} "
        f"(`{mwc['mean_overperformance']:+.3f}`, null band {fmt_ci(mwc['null_lo'], mwc['null_hi'])}, p = {p(mwc['p_perm'])}, "
        f"q = {mwc['q_bh']:.3f}) are {'not ' if not conf_bh else ''}significant after BH across {omnibus['conferences']} "
        "conferences,",
        f"  and as the most extreme of {omnibus['conferences']} means they are unremarkable (P = {p(omnibus['p_best_is_extreme'])} / "
        f"{p(omnibus['p_worst_is_extreme'])}). Cochran's Q permutation p = {p(omnibus['q_p_perm'])}: {conference_signal}.",
        f"  Empirical-Bayes shrinkage puts every conference within `{max_abs_eb:.3f}` of zero.",
        "",
        "## Method",
        "",
        "- **Bootstrap CI (teams), claims (a) and (b):** 95% percentile interval, resampling team-seasons with replacement",
        "  *within their exact seed line* (seed lines keep their size), re-estimating any seed-line centering inside each",
        "  resample.",
        "- **Bootstrap CI (teams), claim (c):** 95% percentile interval from an iid bootstrap of that conference's own",
        "  team-seasons, with each team's seed baseline (its seed line's full-sample mean finish) held fixed. It only sees the",
        "  spread the conference's own sample shows, so when its teams rarely or never beat their seed line it cannot draw",
        "  an upset and comes out too narrow. `boot_sd_ratio` (bootstrap SD / seed-line SD of the mean) and `ci_note`",
        f"  flag this: no team above its seed line, or a ratio below {BOOT_SPREAD_FLOOR:g}.",
        "- **Bootstrap CI (seasons):** same estimator, resampling whole tournaments. Outcomes inside one bracket are coupled,",
        "  so prefer this interval when it is wider. For conferences a third variant resamples programs. Cluster intervals",
        f"  with fewer than {MIN_CLUSTERS} clusters are too narrow and are flagged in `ci_note`.",
        "- **Within-seed null band (c):** the 2.5%-97.5% range of the conference mean under the permutation null below, i.e.",
        "  where the mean lands 95% of the time if the conference's teams were random draws from their seed lines. Compare a",
        "  conference's mean with this band, not with 0.",
        "- **Permutation p:** the stat is shuffled *within exact seed line* (outcomes and seeds fixed), so the null keeps all",
        f"  seed structure. Two-sided (`2 x` smaller tail, +1 correction). `p_perm_seed_season` shuffles only among {strata_text},",
        "  which also holds seed-line x season shifts fixed; it is a narrower, lower-power test.",
        f"- **Multiple comparisons:** Benjamini-Hochberg across all {n_cells} seed-band cells in (b) (`q_bh`, primary), and across "
        f"the {omnibus['conferences']} conferences in (c). For (b) the CSV also gives BH within each published table",
        "  (`q_bh_within_table`) and within each statistic's bands (`q_bh_within_statistic`).",
        "- **Conference shrinkage:** normal-normal empirical Bayes (DerSimonian-Laird). A conference mean's sampling variance is",
        "  the sum of its teams' seed-line finish variances over n^2, i.e. its variance under the within-seed null.",
        "",
        "## Verdicts",
        "",
        "| Claim | n | Estimate | 95% CI (teams) | 95% CI (seasons) | p (perm) | q (pooled BH) | q (per-table BH) | Verdict |",
        "|:--|--:|--:|:--|:--|--:|--:|--:|:--|",
        f"| (a) SOS within-seed Spearman | {sos_row['n']} | {sos_row['rho']:+.3f} | {fmt_ci(sos_row['ci_lo'], sos_row['ci_hi'])} "
        f"| {fmt_ci(sos_row['season_ci_lo'], sos_row['season_ci_hi'])} | {p(sos_row['p_perm'])} | single test | single test "
        f"| {verdict_a} |",
    ]
    band_claims = [
        ("(b) Size EFFHGT, seeds 1-2", size_top),
        ("(b) Size EFFHGT, seeds 7-8", size_78),
        ("(b) SOS Elite SOS, seeds 11-13 (largest published band rho)", sos_1113),
        (f"(b) Player experience, seeds 1-2 (was {old_exp_top:+.3f} before the row-alignment fix)", exp_top),
    ]
    for label, row in band_claims:
        lines.append(
            f"| {label} | {row['n']} | {row['rho']:+.3f} | {fmt_ci(row['ci_lo'], row['ci_hi'])} "
            f"| {fmt_ci(row['season_ci_lo'], row['season_ci_hi'])} | {p(row['p_perm'])} | {row['q_bh']:.3f} "
            f"| {row['q_bh_within_table']:.3f} | {row['verdict']} |"
        )
    for label, row in [
        (f"(c) {lead_conf} mean overperformance", horizon),
        (f"(c) {lead_conf} without {HORIZON_DROP_PROGRAM}", horizon_drop),
        (f"(c) {trail_conf} mean overperformance", mwc),
    ]:
        q_value = float(row["q_bh"]) if pd.notna(row["q_bh"]) else np.nan
        q_text = "n/a" if np.isnan(q_value) else f"{q_value:.3f}"
        lines.append(
            f"| {label} | {row['n']} | {row['mean_overperformance']:+.3f} | {fmt_ci(row['ci_lo'], row['ci_hi'])} "
            f"| {fmt_ci(row['season_ci_lo'], row['season_ci_hi'])} | {p(row['p_perm'])} "
            f"| {q_text} | {q_text} | {family_verdict(row['p_perm'], q_value)} |"
        )
    focus_notes = [
        f"{row['Conf']}: {row['ci_note']}" for row in (horizon, horizon_drop, mwc) if isinstance(row["ci_note"], str) and row["ci_note"]
    ]
    lines += [
        "",
        "For (c) the CIs are iid bootstraps of the conference's own team-seasons (see Method), and the conference table is a",
        "single published table, so its pooled and per-table q are the same. Within-seed null 95% bands: "
        + "; ".join(
            f"{row['Conf']} {fmt_ci(row['null_lo'], row['null_hi'])}" for row in (horizon, horizon_drop, mwc)
        )
        + "."
        + (f" Flags: {'; '.join(focus_notes)}." if focus_notes else ""),
        "",
        "## (a) SOS within seed line",
        "",
        f"- Published `{sos_row['published_rho']:.3f}`; reproduced `{sos_row['rho']:.4f}` on n = {sos_row['n']} team-seasons "
        f"({sos_row['seasons']} tournaments).",
        f"- 95% CI {fmt_ci(sos_row['ci_lo'], sos_row['ci_hi'])} (teams), {fmt_ci(sos_row['season_ci_lo'], sos_row['season_ci_hi'])} "
        f"(seasons). Permutation p = {p(sos_row['p_perm'])} within seed line (null mean `{sos_row['null_mean_within_seed']:+.3f}`);",
        f"  p = {p(sos_row['p_perm_seed_season'])} within seed line and season (null mean `{sos_row['null_mean_seed_season']:+.3f}`).",
        f"- Read: {verdict_a}. About {season_share:.0%} of the estimate sits at the seed-line x season level, and within a",
        "  seed line and season the association is not detectable. Quote it with its interval, and not as evidence of an SOS",
        "  effect inside a season.",
        "",
        "## (b) Seed-band correlations (all topics)",
        "",
        f"- Family: {n_cells} band-level Spearman estimates from {bands['source'].nunique()} published tables "
        f"({', '.join(sorted(bands['topic'].unique()))}).",
        f"- {nominal} of {n_cells} have permutation p < {ALPHA} (about {ALPHA * n_cells:.1f} expected by chance); "
        f"{len(bh_05)} have pooled q < {ALPHA}; {len(bh_10)} have pooled q < 0.10.",
        "- **Family definition matters.** Pooling every published band cell into one family is the conservative choice, and",
        "  the one the verdicts use. A reader who treats each published table as its own family gets "
        f"{len(table_05)} survivor{'s' if len(table_05) != 1 else ''} at q < {ALPHA}"
        + (f" ({', '.join(band_cell_label(row) for _, row in table_05.iterrows())})" if len(table_05) else "")
        + "; treating each statistic's bands as a family gives "
        f"{len(statistic_05)}"
        + (f" ({', '.join(band_cell_label(row) for _, row in statistic_05.iterrows())})" if len(statistic_05) else "")
        + ". "
        + (
            "None of them is significant under the seed-and-season null (p = "
            + ", ".join(p(value) for value in table_05["p_perm_seed_season"])
            + "), and each is a raw-within-band estimator, part of which is seed mix (see below)."
            if len(table_05) and bool((table_05["p_perm_seed_season"] >= ALPHA).all())
            else ""
        ),
        "",
        "Per topic and statistic (`max_abs_rho` uses aligned experience values):",
        "",
        markdown_table(topic_rollup(bands)),
        "",
        "Every cell with nominal p < 0.05:",
        "",
        markdown_table(
            bands.loc[
                bands["p_perm"] < ALPHA,
                [
                    "topic",
                    "metric",
                    "seed_group",
                    "n",
                    "rho",
                    "ci_lo",
                    "ci_hi",
                    "season_ci_lo",
                    "season_ci_hi",
                    "p_perm",
                    "p_perm_seed_season",
                    "q_bh",
                    "q_bh_within_table",
                ],
            ]
        ),
        "",
        f"- **Size, EFFHGT:** seeds 1-2 `{size_top['rho']:+.3f}` (n = {size_top['n']}, CI {fmt_ci(size_top['ci_lo'], size_top['ci_hi'])}, "
        f"p = {p(size_top['p_perm'])}, q = {size_top['q_bh']:.3f}); seeds 7-8 `{size_78['rho']:+.3f}` "
        f"(CI {fmt_ci(size_78['ci_lo'], size_78['ci_hi'])}, p = {p(size_78['p_perm'])}, q = {size_78['q_bh']:.3f}). "
        + (
            "Both intervals include zero."
            if not ci_excludes_zero(size_top["ci_lo"], size_top["ci_hi"]) and not ci_excludes_zero(size_78["ci_lo"], size_78["ci_hi"])
            else "At least one interval excludes zero; see q."
        ),
        "- **Size and SOS band estimators mix seed lines.** They correlate the raw metric with finish inside a band that spans",
        "  2-3 seed lines, so part of the number is just \"the better seed in the band also has more of the metric and goes",
        "  further\". The within-seed null keeps that part, so its mean shows how much is seed mix. SOS Elite SOS, seeds 11-13:",
        f"  published `{sos_1113['published_rho']:+.3f}`, within-seed null mean `{sos_1113['null_mean_within_seed']:+.3f}`, "
        f"exact-seed-centered rho `{sos_1113['rho_exact_seed_centered']:+.3f}` (p = {p(sos_1113['p_perm'])}, pooled q = "
        f"{sos_1113['q_bh']:.3f}, per-table q = {sos_1113['q_bh_within_table']:.3f}, seed-and-season p = "
        f"{p(sos_1113['p_perm_seed_season'])}).",
        "",
        "### Fixed bug: the player-experience correlations used to pair the wrong rows",
        "",
        "`scripts/experience_analysis_02_04.py` used to wrap the experience edge in `pd.Series(numpy_array)`, which gets a",
        "fresh `0..n-1` index, then call `overperformance.corr(edge)`. pandas aligns on index labels, so each correlation was",
        "computed only on the few rows whose original labels fall in `0..n-1`, paired with the wrong teams. The script now",
        "keeps the row index, and the regenerated reports carry the aligned values. For the record, `old_code_rho` replays",
        "the old call and `aligned_rho` is the corrected value. Both use the negated `Exp` rank, so a positive rho means",
        "more-experienced teams tended to finish deeper (raw) or further above their seed line (bands, pooled seed-adjusted):",
        "",
        markdown_table(experience_audit, digits=4),
        "",
        f"Aligned, the band estimates run from `{exp_player['rho'].min():+.3f}` to `{exp_player['rho'].max():+.3f}` "
        f"(old code: `{old_band_rhos.min():+.3f}` to `{old_band_rhos.max():+.3f}`). The pooled numbers in",
        "`reports/player_exp_vs_finish_pooled_summary.md` had the same bug (last two rows) and are corrected too.",
        "",
        "## (c) Conference overperformance vs exact-seed expectation",
        "",
        f"- **{lead_conf}:** mean `{horizon['mean_overperformance']:+.3f}`, median `{horizon['median_overperformance']:+.3f}` "
        f"(n = {horizon['n']}, {horizon['programs']} programs; {horizon['share_positive']:.0%} of team-seasons beat their seed line).",
        f"  Within-seed null band {fmt_ci(horizon['null_lo'], horizon['null_hi'])}; p = {p(horizon['p_perm'])} within seed line, "
        f"p = {p(horizon['p_perm_seed_season'])} within seed line and season; q = {horizon['q_bh']:.3f}.",
        f"  Bootstrap CI {fmt_ci(horizon['ci_lo'], horizon['ci_hi'])} teams, {fmt_ci(horizon['season_ci_lo'], horizon['season_ci_hi'])} "
        f"seasons, {fmt_ci(horizon['program_ci_lo'], horizon['program_ci_hi'])} programs; shrunken "
        f"`{horizon['eb_mean']:+.3f}` [{horizon['eb_lo']:+.3f}, {horizon['eb_hi']:+.3f}].",
        f"- Top team-seasons (conference total `{horizon_total:+.3f}`):",
    ]
    for row in horizon_top.itertuples(index=False):
        lines.append(f"  - {row.Team} {row.year} ({row.seed_int}-seed, {row.finish}): `{row.overperformance:+.3f}`")
    lines += [
        f"- **Leave one program out:** without {HORIZON_DROP_PROGRAM} the mean is `{horizon_drop['mean_overperformance']:+.3f}` "
        f"(n = {horizon_drop['n']}, CI {fmt_ci(horizon_drop['ci_lo'], horizon_drop['ci_hi'])}, null band "
        f"{fmt_ci(horizon_drop['null_lo'], horizon_drop['null_hi'])}, p = {p(horizon_drop['p_perm'])}). "
        f"Across all programs the {lead_conf} mean ranges `{horizon['loo_min_mean']:+.3f}` to `{horizon['loo_max_mean']:+.3f}`.",
        f"- **{trail_conf}:** mean `{mwc['mean_overperformance']:+.3f}`, median `{mwc['median_overperformance']:+.3f}` (n = {mwc['n']}, "
        f"{mwc['programs']} programs). Within-seed null band {fmt_ci(mwc['null_lo'], mwc['null_hi'])}; p = {p(mwc['p_perm'])} "
        f"within seed line, p = {p(mwc['p_perm_seed_season'])} within seed line and season; q = {mwc['q_bh']:.3f}.",
        f"  Bootstrap CI {fmt_ci(mwc['ci_lo'], mwc['ci_hi'])} teams, {fmt_ci(mwc['season_ci_lo'], mwc['season_ci_hi'])} seasons, "
        f"{fmt_ci(mwc['program_ci_lo'], mwc['program_ci_hi'])} programs; shrunken "
        f"`{mwc['eb_mean']:+.3f}` [{mwc['eb_lo']:+.3f}, {mwc['eb_hi']:+.3f}].",
        f"  Most influential program: {mwc_loo['program']} (dropping it moves the mean to `{mwc_loo['mean_without']:+.3f}`); "
        f"leave-one-program-out range `{mwc['loo_min_mean']:+.3f}` to `{mwc['loo_max_mean']:+.3f}`. Worst team-seasons: "
        + "; ".join(f"{r.Team} {r.year} ({r.seed_int}-seed, {r.finish}, `{r.overperformance:+.3f}`)" for r in mwc_top.itertuples(index=False))
        + ".",
        f"- **Selection:** \"best\" and \"worst\" are the extremes of {omnibus['conferences']} means. Under the within-seed null the",
        f"  best conference mean has median `{omnibus['null_max_median']:+.3f}`, and P(best >= {omnibus['best_mean']:+.3f}) = "
        f"{p(omnibus['p_best_is_extreme'])}; the worst has median `{omnibus['null_min_median']:+.3f}`, and "
        f"P(worst <= {omnibus['worst_mean']:+.3f}) = {p(omnibus['p_worst_is_extreme'])}.",
        f"- **Any conference effect beyond seed?** Cochran's Q = {omnibus['q_stat']:.1f} on {omnibus['q_dof']} df "
        f"(chi-square p = {omnibus['q_p_chi2']:.3f}, permutation p = {p(omnibus['q_p_perm'])}); DerSimonian-Laird "
        f"tau^2 = {omnibus['tau2']:.4f}, grand mean `{omnibus['eb_grand_mean']:+.3f}`.",
        f"- BH across conferences: {len(conf_bh)} with q < {ALPHA}" + (f" ({', '.join(conf_bh)})" if conf_bh else "")
        + f"; nominal p < {ALPHA}: {', '.join(conf_nominal) if conf_nominal else 'none'}.",
        "",
        "Conference table, sorted by raw mean. `null_lo`/`null_hi` = within-seed null 95% band (compare the mean with this,",
        "not with 0); `eb_*` = shrunken estimate and interval; `loo_*` = leave-one-program-out range. Bootstrap CIs per",
        "conference are in `reports/robustness_conference_overperformance.csv` only, because for many conferences they",
        "are not trustworthy (see `ci_note` and Caveats).",
        "",
        markdown_table(
            conf_table[
                [
                    "Conf",
                    "n",
                    "programs",
                    "mean_overperformance",
                    "median_overperformance",
                    "null_lo",
                    "null_hi",
                    "p_perm",
                    "q_bh",
                    "eb_mean",
                    "eb_lo",
                    "eb_hi",
                    "loo_min_mean",
                    "loo_max_mean",
                ]
            ]
        ),
        "",
        "## Caveats",
        "",
        f"- These are descriptive checks on {years_text}"
        + (
            "; some band cells use a subset ("
            + ", ".join(f"{row.topic} {row.statistic}: {row.seasons} seasons" for row in subset_bands.itertuples(index=False))
            + ")"
            if len(subset_bands)
            else ""
        )
        + ". They ask whether a published number stands",
        "  out from seed-structure noise, not whether it would predict future brackets.",
        "- Percentile bootstrap intervals can under-cover in small, skewed samples (e.g. Horizon: n = "
        f"{horizon['n']} with two extreme values). The permutation p-values do not rely on that approximation.",
        "- **Per-conference bootstrap CIs (in the CSV) are unreliable for many conferences.** No team-season beat its seed",
        f"  line in {', '.join(no_upset) if no_upset else 'none'}, so the bootstrap cannot draw an upset and the interval collapses "
        "below zero even where the",
        f"  permutation test finds nothing (e.g. {', '.join(no_upset[:1]) or 'n/a'}). The bootstrap SD is under "
        f"{BOOT_SPREAD_FLOOR:g}x the seed-line SD in {', '.join(narrow) if narrow else 'none'}.",
        f"  {len(few_programs)} of {omnibus['conferences']} conferences have fewer than {MIN_CLUSTERS} programs "
        f"({', '.join(few_programs) if few_programs else 'none'}), so their program-cluster CI is too narrow"
        + (f"; {', '.join(few_seasons)} also ha{'s' if len(few_seasons) == 1 else 've'} fewer than {MIN_CLUSTERS} seasons" if few_seasons else "")
        + ". For these rows use `null_lo`/`null_hi`, `p_perm`/`q_bh` and the EB interval.",
        f"- In seeds {SEED_GROUP_ORDER[-1]}, {low_seed['modal_finish_share'].min():.0%}-{low_seed['modal_finish_share'].max():.0%} "
        "of teams share their seed line's most common finish, so a single deep run moves the",
        f"  estimate and bootstrap distributions can be bimodal (e.g. {lopsided['topic']} {lopsided['metric']}: "
        f"`{lopsided['rho']:+.3f}`, CI {fmt_ci(lopsided['ci_lo'], lopsided['ci_hi'])}). Trust the permutation p there.",
        "- Cells overlap (tempo since 2014 is a subset of tempo all years; offense/defense metrics share teams), so the",
        "  tests are dependent. BH controls FDR under positive dependence"
        + (
            "; since nothing survives pooled BH, nothing would survive the stricter Benjamini-Yekutieli correction either."
            if len(bh_05) == 0
            else "; Benjamini-Yekutieli would be the conservative alternative."
        ),
        "- The empirical-Bayes interval treats tau^2 as known (plug-in), so it is approximate.",
        "",
        "## Files",
        "",
        "- `reports/robustness_sos_within_seed.csv` - claim (a)",
        "- `reports/robustness_seed_band_correlations.csv` - family (b), one row per topic x statistic x seed band",
        "- `reports/robustness_conference_overperformance.csv` - claim (c), one row per conference (+ Horizon without Butler),",
        "  including the bootstrap CIs, `boot_sd_ratio` and `ci_note`",
        "- `reports/robustness_conference_team_seasons.csv` - Horizon and MWC team-seasons ranked by contribution",
        "- `reports/robustness_conference_leave_one_program_out.csv` - conference mean after dropping each program",
    ]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def published_conference_check(conf_table: pd.DataFrame, published_csv: Path) -> pd.DataFrame:
    """Compare each conference's n, mean and median with the published CSV (6 decimals).
    Rows with no published counterpart (Horizon without Butler) get NA rather than False."""
    published = pd.read_csv(published_csv).set_index("Conf")
    out = conf_table.copy()
    out["published_n"] = out["Conf"].map(published["team_count"])
    out["published_mean"] = out["Conf"].map(published["mean_overperformance"])
    out["published_median"] = out["Conf"].map(published["median_overperformance"])
    out["matches_published"] = (
        (out["n"] == out["published_n"])
        & ((out["mean_overperformance"] - out["published_mean"]).abs() < 1e-6)
        & ((out["median_overperformance"] - out["published_median"]).abs() < 1e-6)
    ).astype("boolean")
    out.loc[out["published_mean"].isna(), "matches_published"] = pd.NA
    return out


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--n-resamples", type=int, default=N_RESAMPLES)
    args = parser.parse_args(argv)
    paths = get_paths()

    sos_cell = build_sos_within_seed_cell(paths["sos_sample"], paths["published_sos_md"])
    sos_row = pd.Series(evaluate_cell(sos_cell, args.n_resamples))
    pd.DataFrame([sos_row]).round(6).to_csv(paths["csv_sos"], index=False)

    band_cells, experience_audit = build_band_cells(paths)
    bands = run_band_family(band_cells, args.n_resamples)
    bands.round(6).to_csv(paths["csv_bands"], index=False)

    over, conference_table = load_conference_data(paths["sos_sample"])
    conf = run_conference_checks(over, conference_table, args.n_resamples)
    conf["table"] = published_conference_check(conf["table"], paths["published_conf_csv"])
    conf["table"].round(6).to_csv(paths["csv_conf"], index=False)
    conf["team_seasons"].round(6).to_csv(paths["csv_conf_team_seasons"], index=False)
    conf["loo"].round(6).to_csv(paths["csv_conf_loo"], index=False)

    sample = {
        "years": np.sort(over["year"].unique()).astype(int),
        "seed_season_sizes": over.groupby(["year", "seed_int"]).size(),
    }
    write_summary(paths["md"], sos_row, bands, experience_audit, conf, args.n_resamples, sample)

    mismatched = bands.loc[~bands["matches_published"], ["topic", "statistic", "seed_group", "published_rho", "rho"]]
    in_family = conf["table"]["q_bh"].notna()
    conf_mismatched = int((~conf["table"].loc[in_family, "matches_published"]).sum())
    print(
        f"(a) SOS within-seed rho: {sos_row['rho']:.4f} (published {sos_row['published_rho']:.3f}; "
        f"matches: {sos_row['matches_published']})"
    )
    print(f"(b) {len(bands)} seed-band cells; {len(mismatched)} differ from the published value")
    print(f"(c) {len(conference_table)} conferences; {conf_mismatched} differ from the published table; omnibus: {conf['omnibus']}")
    for key in ["md", "csv_sos", "csv_bands", "csv_conf", "csv_conf_team_seasons", "csv_conf_loo"]:
        print(f"Wrote: {paths[key]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
