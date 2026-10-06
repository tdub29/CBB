from __future__ import annotations

import pandas as pd
import pytest

from src.models.bracket import (
    FIRST_ROUND_PAIRINGS,
    advance_round,
    build_first_round_matchups,
    load_bracket,
    validate_bracket,
)


def _sample_bracket() -> pd.DataFrame:
    rows = []
    for region in ["East", "West", "South", "Midwest"]:
        for seed in range(1, 17):
            rows.append({"Team": f"{region}_{seed}", "Seed": seed, "Region": region})
    return pd.DataFrame(rows)


def test_validate_bracket_standard_64_team_shape() -> None:
    df = _sample_bracket()
    errors, warnings = validate_bracket(df)
    assert errors == []
    assert warnings == []


def test_build_first_round_matchups_has_32_games() -> None:
    df = _sample_bracket()
    matchups = build_first_round_matchups(df)
    assert len(matchups) == 32

    east_games = [m for m in matchups if m["region"] == "East"]
    east_pairs = {(m["seed_a"], m["seed_b"]) for m in east_games}
    assert set(FIRST_ROUND_PAIRINGS).issubset(east_pairs)


def test_load_bracket_roundtrip(tmp_path) -> None:
    df = _sample_bracket()
    csv_path = tmp_path / "bracket.csv"
    df.to_csv(csv_path, index=False)
    loaded = load_bracket(csv_path)
    assert len(loaded) == 64
    assert set(loaded.columns) == {"Team", "Seed", "Region"}


def test_advance_round_pairs_neighbors() -> None:
    winners = ["A", "B", "C", "D"]
    assert advance_round(winners) == [("A", "B"), ("C", "D")]


def _play_out(bracket: pd.DataFrame, upsets: set[str] = frozenset()) -> pd.DataFrame:
    """Games of a bracket where the better seed wins unless the underdog is in `upsets`."""
    seed = dict(zip(bracket["Team"], bracket["Seed"]))
    slots = []
    for region in sorted(bracket["Region"].unique()):
        block = bracket[bracket["Region"] == region]
        s2t = dict(zip(block["Seed"], block["Team"]))
        slots += [s2t[s] for pair in FIRST_ROUND_PAIRINGS for s in pair]
    games, rnd = [], 1
    while len(slots) > 1:
        nxt = []
        for a, b in zip(slots[0::2], slots[1::2]):
            fav, dog = sorted((a, b), key=lambda t: (seed[t], t))
            w, lo = (dog, fav) if dog in upsets else (fav, dog)
            games.append({"year": 2030, "round": rnd, "winner": w, "loser": lo, "winner_seed": seed[w],
                          "loser_seed": seed[lo], "no_contest": False})
            nxt.append(w)
        slots, rnd = nxt, rnd + 1
    return pd.DataFrame(games)


def test_bracket_from_games_rebuilds_regions_and_semifinal_pairs() -> None:
    from src.models.tournament import bracket_from_games

    truth = _sample_bracket()
    games = _play_out(truth, upsets={"East_12", "West_9"})
    rebuilt = bracket_from_games(games)
    assert len(rebuilt) == 64
    # same 16-team groups, and sorted labels pair the regions that met in the semifinals
    groups = {frozenset(g["Team"]) for _, g in rebuilt.groupby("Region")}
    assert groups == {frozenset(g["Team"]) for _, g in truth.groupby("Region")}
    region = dict(zip(rebuilt["Team"], rebuilt["Region"]))
    for semi in games[games["round"] == 5].itertuples():
        assert {region[semi.winner], region[semi.loser]} in ({"A", "B"}, {"C", "D"})

    broken = games.copy()
    broken.loc[broken.index[0], "round"] = 2
    with pytest.raises(ValueError):
        bracket_from_games(broken)


def test_committed_bracket_splits_exact_ties_and_scores_hits() -> None:
    from src.models.tournament import chalk_probability, committed_bracket, score_committed_bracket

    bracket = _sample_bracket()
    seed = dict(zip(bracket["Team"], bracket["Seed"]))
    picks = committed_bracket(bracket, lambda a, b: chalk_probability(seed[a], seed[b]))
    assert [set(s) for s in picks[4]] == [{"East_1"}, {"Midwest_1"}, {"South_1"}, {"West_1"}]
    assert picks[6][0] == {t: 0.25 for t in ("East_1", "Midwest_1", "South_1", "West_1")}

    wins = {t: 0 for t in bracket["Team"]} | {"East_1": 6, "West_1": 4, "Midwest_1": 5, "South_1": 4}
    score = score_committed_bracket(picks, wins)
    assert score["final_four_hits"] == 4
    assert score["champion_hit"] == 0.25


def test_seed_pair_rates_are_laplace_smoothed_and_symmetric() -> None:
    from src.models.tournament import seed_pair_rates

    history = pd.DataFrame({
        "year": 2020, "round": 1, "winner": ["a", "b", "c"], "loser": ["x", "y", "z"],
        "winner_seed": [1, 1, 1], "loser_seed": [16, 16, 16], "no_contest": False,
    })
    p = seed_pair_rates(history)
    assert p(1, 16) == 4 / 5
    assert abs(p(16, 1) - 1 / 5) < 1e-12
    assert p(5, 12) == 0.5
    assert p(3, 3) == 0.5
