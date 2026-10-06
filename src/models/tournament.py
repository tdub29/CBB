"""Tournament structure and scoring for walked-forward bracket evaluation."""

from __future__ import annotations

from collections.abc import Callable

import numpy as np
import pandas as pd

from .bracket import FIRST_ROUND_PAIRINGS

# Seed positions down one region of the bracket; two seeds meet in the round given by the
# highest differing bit of their positions (1 vs 16 in R64, 1 vs 8 in R32, 1 vs 5 in S16...).
BRACKET_ORDER = (1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
ROUND_NAMES = {1: "R64", 2: "R32", 3: "S16", 4: "E8", 5: "F4", 6: "Final"}
# p_<name> in simulate_bracket output = probability of winning a game in that round.
REACH_COLUMNS = {1: "p_R32", 2: "p_S16", 3: "p_E8", 4: "p_F4", 5: "p_Finals", 6: "p_Champ"}
PICK_POINTS = {1: 10, 2: 20, 3: 40, 4: 80, 5: 160, 6: 320}
CHALK_CONFIDENCE = 0.99  # only used to give chalk a finite log loss; Brier uses a hard 0/1 pick

MatchupFn = Callable[[str, str], float]


def bracket_from_games(games: pd.DataFrame) -> pd.DataFrame:
    """Team/Seed/Region for one season, rebuilt from its 63 games.

    Regions are the connected components of rounds 1-4. They are labelled A-D so that
    simulate_bracket's sorted pairing (A-B, C-D) reproduces the real national semifinals.
    """
    parent: dict[str, str] = {}

    def find(team: str) -> str:
        parent.setdefault(team, team)
        while parent[team] != team:
            team = parent[team]
        return team

    for g in games[games["round"] <= 4].itertuples(index=False):
        a, b = find(g.winner), find(g.loser)
        if a != b:
            parent[a] = b

    seeds = {**dict(zip(games["loser"], games["loser_seed"])), **dict(zip(games["winner"], games["winner_seed"]))}
    label: dict[str, str] = {}
    for i, g in enumerate(games[games["round"] == 5].sort_values("winner").itertuples(index=False)):
        label[find(g.winner)] = "AC"[i]
        label[find(g.loser)] = "BD"[i]

    out = pd.DataFrame(
        [{"Team": t, "Seed": int(s), "Region": label[find(t)]} for t, s in seeds.items()]
    ).sort_values(["Region", "Seed"]).reset_index(drop=True)
    for region, block in out.groupby("Region"):
        if sorted(block["Seed"]) != list(range(1, 17)):
            raise ValueError(f"region {region} does not hold seeds 1-16: {sorted(block['Seed'])}")
    meet = meeting_round_fn(out)
    wrong = [(g.winner, g.loser) for g in games.itertuples(index=False) if meet(g.winner, g.loser) != g.round]
    if wrong:
        raise ValueError(f"rebuilt bracket disagrees with the games played: {wrong[:3]}")
    return out


def meeting_round_fn(bracket: pd.DataFrame) -> Callable[[str, str], int]:
    pos = {t: BRACKET_ORDER.index(int(s)) for t, s in zip(bracket["Team"], bracket["Seed"])}
    region = dict(zip(bracket["Team"], bracket["Region"]))

    def meeting_round(a: str, b: str) -> int:
        if region[a] == region[b]:
            return (pos[a] ^ pos[b]).bit_length()
        return 5 if {region[a], region[b]} in ({"A", "B"}, {"C", "D"}) else 6

    return meeting_round


def oriented_games(games: pd.DataFrame) -> pd.DataFrame:
    """One row per played game from the better seed's side (ties: alphabetical). No-contests dropped."""
    g = games[~games["no_contest"].astype(bool)].copy()
    winner_first = (g["winner_seed"] < g["loser_seed"]) | (
        (g["winner_seed"] == g["loser_seed"]) & (g["winner"] < g["loser"])
    )
    return pd.DataFrame({
        "year": g["year"], "round": g["round"],
        "team_a": np.where(winner_first, g["winner"], g["loser"]),
        "team_b": np.where(winner_first, g["loser"], g["winner"]),
        "seed_a": np.where(winner_first, g["winner_seed"], g["loser_seed"]),
        "seed_b": np.where(winner_first, g["loser_seed"], g["winner_seed"]),
        "a_won": winner_first.astype(int),
    }).reset_index(drop=True)


def seed_pair_rates(history: pd.DataFrame) -> Callable[[float, float], float]:
    """P(seed_a beats seed_b) from historical games, Laplace-smoothed: (wins + 1) / (games + 2)."""
    g = oriented_games(history)
    g = g[g["seed_a"] != g["seed_b"]]
    table = g.groupby(["seed_a", "seed_b"])["a_won"].agg(["sum", "count"])
    rates = {k: (row["sum"] + 1.0) / (row["count"] + 2.0) for k, row in table.iterrows()}

    def p(seed_a: float, seed_b: float) -> float:
        lo, hi = sorted((int(seed_a), int(seed_b)))
        if lo == hi:
            return 0.5
        p_lo = rates.get((lo, hi), 0.5)
        return p_lo if int(seed_a) == lo else 1.0 - p_lo

    return p


def chalk_probability(seed_a: float, seed_b: float) -> float:
    """Deterministic seed chalk: the better seed always wins; equal seeds are a coin flip."""
    return 1.0 if seed_a < seed_b else (0.0 if seed_a > seed_b else 0.5)


def brier_logloss(p: np.ndarray, y: np.ndarray, eps: float = 1e-15) -> tuple[float, float]:
    p = np.asarray(p, dtype=float)
    y = np.asarray(y, dtype=float)
    q = np.clip(p, eps, 1 - eps)
    return float(np.mean((p - y) ** 2)), float(-np.mean(y * np.log(q) + (1 - y) * np.log(1 - q)))


def committed_bracket(bracket: pd.DataFrame, p_match: MatchupFn) -> dict[int, list[dict[str, float]]]:
    """Pick the likelier winner of every game, round by round, from the Round of 64.

    Returns {round: [slot, ...]} where a slot maps team -> pick weight. Weights are 1.0 except
    when p_match is exactly 0.5, which splits the pick evenly (seed chalk between two 1 seeds),
    so hit counts are the expected value under a coin-flip tiebreak rather than name order.
    """
    slots: list[dict[str, float]] = []
    for region in sorted(bracket["Region"].unique()):
        block = bracket[bracket["Region"] == region]
        seed_to_team = dict(zip(block["Seed"].astype(int), block["Team"]))
        for sa, sb in FIRST_ROUND_PAIRINGS:
            slots += [{seed_to_team[sa]: 1.0}, {seed_to_team[sb]: 1.0}]

    picks: dict[int, list[dict[str, float]]] = {}
    for rnd in range(1, 7):
        nxt = []
        for left, right in zip(slots[0::2], slots[1::2]):
            won: dict[str, float] = {}
            for ta, wa in left.items():
                for tb, wb in right.items():
                    p = p_match(ta, tb)
                    share_a = 1.0 if p > 0.5 else (0.0 if p < 0.5 else 0.5)
                    if share_a > 0:
                        won[ta] = won.get(ta, 0.0) + wa * wb * share_a
                    if share_a < 1:
                        won[tb] = won.get(tb, 0.0) + wa * wb * (1 - share_a)
            nxt.append(won)
        picks[rnd] = nxt
        slots = nxt
    return picks


def score_committed_bracket(picks: dict[int, list[dict[str, float]]], wins: dict[str, int]) -> dict[str, float]:
    """Correct picks per round (a round-r pick is right if that team won >= r games)."""
    out: dict[str, float] = {}
    points = 0.0
    for rnd, slots in picks.items():
        hits = sum(w for slot in slots for t, w in slot.items() if wins.get(t, 0) >= rnd)
        out[f"correct_{ROUND_NAMES[rnd]}"] = hits
        points += hits * PICK_POINTS[rnd]
    out["final_four_hits"] = out["correct_E8"]
    out["champion_hit"] = out["correct_Final"]
    out["espn_points"] = points
    return out
