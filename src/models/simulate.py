"""Monte Carlo bracket simulation."""

from __future__ import annotations

from collections.abc import Callable

import numpy as np
import pandas as pd

from .bracket import FIRST_ROUND_PAIRINGS, advance_round, validate_bracket
from .win_probability import p_win, seed_fallback_strength

ROUND_COLUMNS = ("p_R64", "p_R32", "p_S16", "p_E8", "p_F4", "p_Finals", "p_Champ")


def _region_pairings(regions: list[str]) -> list[tuple[str, str]]:
    """Define Final Four pairing order from regions."""
    ordered = sorted(regions)
    if len(ordered) != 4:
        raise ValueError(f"Simulation requires exactly 4 regions; found {len(ordered)}")
    return [(ordered[0], ordered[1]), (ordered[2], ordered[3])]


def _team_strength(team: str, seed: int, strength_lookup: dict[str, float]) -> float:
    """Get team strength with seed fallback for missing teams."""
    if team in strength_lookup:
        return float(strength_lookup[team])
    return seed_fallback_strength(seed)


def _simulate_game(
    team_a: str,
    seed_a: int,
    team_b: str,
    seed_b: int,
    strength_lookup: dict[str, float],
    rng: np.random.Generator,
    k: float,
    p_win_fn: Callable[[float, float, float], float],
) -> str:
    """Draw one game winner."""
    strength_a = _team_strength(team_a, seed_a, strength_lookup)
    strength_b = _team_strength(team_b, seed_b, strength_lookup)
    prob_a = p_win_fn(strength_a, strength_b, k)
    return team_a if rng.random() < prob_a else team_b


def _build_seed_lookup(bracket_df: pd.DataFrame) -> dict[str, int]:
    """Map team names to their bracket seed."""
    return dict(zip(bracket_df["Team"], bracket_df["Seed"], strict=False))


def _compute_expected_round(sim_probs: pd.DataFrame) -> pd.Series:
    """
    Expected round index (0..6) based on exit probabilities.

    Round index aligns to:
    0=R64, 1=R32, 2=S16, 3=E8, 4=F4, 5=Finals, 6=Champion
    """
    p_r64 = sim_probs["p_R64"]
    p_r32 = sim_probs["p_R32"]
    p_s16 = sim_probs["p_S16"]
    p_e8 = sim_probs["p_E8"]
    p_f4 = sim_probs["p_F4"]
    p_finals = sim_probs["p_Finals"]
    p_champ = sim_probs["p_Champ"]

    exit_r0 = p_r64 - p_r32
    exit_r1 = p_r32 - p_s16
    exit_r2 = p_s16 - p_e8
    exit_r3 = p_e8 - p_f4
    exit_r4 = p_f4 - p_finals
    exit_r5 = p_finals - p_champ
    exit_r6 = p_champ

    return (
        0 * exit_r0
        + 1 * exit_r1
        + 2 * exit_r2
        + 3 * exit_r3
        + 4 * exit_r4
        + 5 * exit_r5
        + 6 * exit_r6
    )


def simulate_bracket(
    bracket_df: pd.DataFrame,
    strength_lookup: dict[str, float],
    p_win_fn: Callable[[float, float, float], float] = p_win,
    n_sims: int = 10000,
    k: float = 1.0,
    random_state: int | None = None,
    progress_every: int = 0,
) -> pd.DataFrame:
    """Run Monte Carlo simulations and return round probabilities by team."""
    if n_sims <= 0:
        raise ValueError(f"n_sims must be > 0, got {n_sims}")
    if k <= 0:
        raise ValueError(f"k must be > 0, got {k}")

    errors, warnings = validate_bracket(bracket_df)
    if errors:
        raise ValueError(f"Cannot simulate invalid bracket: {'; '.join(errors)}")
    if warnings:
        print("Simulation warnings:")
        for warning in warnings:
            print(f"- {warning}")

    work = bracket_df.copy()
    work["Team"] = work["Team"].astype(str).str.strip()
    work["Region"] = work["Region"].astype(str).str.strip()
    work["Seed"] = pd.to_numeric(work["Seed"], errors="coerce").astype(int)

    regions = sorted(work["Region"].unique().tolist())
    semifinal_regions = _region_pairings(regions)
    seed_lookup = _build_seed_lookup(work)
    rng = np.random.default_rng(random_state)

    teams = sorted(work["Team"].tolist())
    team_idx = {team: idx for idx, team in enumerate(teams)}
    n_teams = len(teams)

    reached_r32 = np.zeros(n_teams, dtype=np.int64)
    reached_s16 = np.zeros(n_teams, dtype=np.int64)
    reached_e8 = np.zeros(n_teams, dtype=np.int64)
    reached_f4 = np.zeros(n_teams, dtype=np.int64)
    reached_finals = np.zeros(n_teams, dtype=np.int64)
    champs = np.zeros(n_teams, dtype=np.int64)

    # ponytail: region seed maps built once, not per simulation; RNG draw order is unchanged.
    region_seed_maps = {
        region: {int(row.Seed): row.Team for row in work[work["Region"] == region].itertuples(index=False)}
        for region in regions
    }

    for sim_num in range(n_sims):
        region_winners: dict[str, str] = {}

        for region in regions:
            seed_to_team = region_seed_maps[region]

            winners_r32: list[str] = []
            for seed_a, seed_b in FIRST_ROUND_PAIRINGS:
                team_a = seed_to_team[seed_a]
                team_b = seed_to_team[seed_b]
                winner = _simulate_game(
                    team_a,
                    seed_a,
                    team_b,
                    seed_b,
                    strength_lookup,
                    rng,
                    k,
                    p_win_fn,
                )
                winners_r32.append(winner)
                reached_r32[team_idx[winner]] += 1

            winners_s16: list[str] = []
            for team_a, team_b in advance_round(winners_r32):
                winner = _simulate_game(
                    team_a,
                    seed_lookup[team_a],
                    team_b,
                    seed_lookup[team_b],
                    strength_lookup,
                    rng,
                    k,
                    p_win_fn,
                )
                winners_s16.append(winner)
                reached_s16[team_idx[winner]] += 1

            winners_e8: list[str] = []
            for team_a, team_b in advance_round(winners_s16):
                winner = _simulate_game(
                    team_a,
                    seed_lookup[team_a],
                    team_b,
                    seed_lookup[team_b],
                    strength_lookup,
                    rng,
                    k,
                    p_win_fn,
                )
                winners_e8.append(winner)
                reached_e8[team_idx[winner]] += 1

            region_champ = _simulate_game(
                winners_e8[0],
                seed_lookup[winners_e8[0]],
                winners_e8[1],
                seed_lookup[winners_e8[1]],
                strength_lookup,
                rng,
                k,
                p_win_fn,
            )
            region_winners[region] = region_champ
            reached_f4[team_idx[region_champ]] += 1

        finalists: list[str] = []
        for region_a, region_b in semifinal_regions:
            team_a = region_winners[region_a]
            team_b = region_winners[region_b]
            finalist = _simulate_game(
                team_a,
                seed_lookup[team_a],
                team_b,
                seed_lookup[team_b],
                strength_lookup,
                rng,
                k,
                p_win_fn,
            )
            finalists.append(finalist)
            reached_finals[team_idx[finalist]] += 1

        champion = _simulate_game(
            finalists[0],
            seed_lookup[finalists[0]],
            finalists[1],
            seed_lookup[finalists[1]],
            strength_lookup,
            rng,
            k,
            p_win_fn,
        )
        champs[team_idx[champion]] += 1

        if progress_every and (sim_num + 1) % progress_every == 0:
            print(f"Simulated {sim_num + 1:,}/{n_sims:,} brackets...")

    sim_probs = pd.DataFrame(
        {
            "Team": teams,
            "p_R64": np.ones(n_teams, dtype=float),
            "p_R32": reached_r32 / n_sims,
            "p_S16": reached_s16 / n_sims,
            "p_E8": reached_e8 / n_sims,
            "p_F4": reached_f4 / n_sims,
            "p_Finals": reached_finals / n_sims,
            "p_Champ": champs / n_sims,
        }
    )
    sim_probs["expected_round"] = _compute_expected_round(sim_probs)
    return sim_probs
