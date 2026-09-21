from __future__ import annotations

import pandas as pd

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
