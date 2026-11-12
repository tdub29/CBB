"""Coach tournament history: every finish label in the team-history cache maps to its exact W-L.

The old lookup matched substrings in dict order, so "Sweet Sixteen" matched nothing (0-0) and "R68"
matched the "8" key (an Elite Eight's 3-1).
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from scripts import enrich_team_history_cache as enrich
from src.frozen_inputs import REPO_ROOT

EXPECTED_WL = {
    "R68": (0, 1),
    "R64": (0, 1),
    "R32": (1, 1),
    "Sweet Sixteen": (2, 1),
    "Elite Eight": (3, 1),
    "Final Four": (4, 1),
    "Finals": (5, 1),
    "CHAMPS": (6, 0),
    "Proj.": (0, 0),
}


def _observed_finish_labels() -> list[str]:
    cache = pd.read_csv(REPO_ROOT / "data" / "raw" / "team_history_cache.csv", usecols=["finish"])
    return sorted(cache["finish"].dropna().astype(str).str.strip().unique())


def test_every_finish_label_in_the_cache_maps_exactly() -> None:
    observed = _observed_finish_labels()
    assert observed, "team_history_cache.csv has no finish labels"
    assert set(observed) == set(EXPECTED_WL), "a finish label was added or dropped; extend FINISH_TO_WL and this test"
    for label in observed:
        assert enrich._finish_to_wl(label) == EXPECTED_WL[label], label


@pytest.mark.parametrize("blank", [None, np.nan, "", "  "])
def test_blank_finish_is_not_a_tournament(blank) -> None:
    assert enrich._finish_to_wl(blank) == (0, 0)


def test_labels_the_substring_lookup_got_wrong() -> None:
    assert enrich._finish_to_wl("Sweet Sixteen") == (2, 1)
    assert enrich._finish_to_wl("R68") == (0, 1)
    assert enrich._finish_to_wl(" Final Four ") == (4, 1)


def test_unknown_label_raises() -> None:
    with pytest.raises(ValueError, match="Unknown team-history finish label"):
        enrich._finish_to_wl("Sweet 16")


def test_coach_window_counts_the_prior_five_seasons_only(tmp_path, monkeypatch) -> None:
    rows = [
        ("Alpha", 2019, "Coach A", "R64"),
        ("Alpha", 2020, "Coach A", "Proj."),
        ("Alpha", 2021, "Coach A", "R68"),
        ("Alpha", 2022, "Coach A", np.nan),
        ("Alpha", 2023, "Coach A", "Sweet Sixteen"),
        ("Alpha", 2024, "Coach A", "CHAMPS"),
        ("Beta", 2023, "Coach B", "R32"),
        ("Beta", 2024, "Coach A", "R64"),
    ]
    cache = pd.DataFrame(rows, columns=["team", "year", "coach", "finish"])
    src, out = tmp_path / "cache.csv", tmp_path / "enriched.csv"
    cache.to_csv(src, index=False)
    monkeypatch.setattr(enrich, "CACHE_PATH", src)
    monkeypatch.setattr(enrich, "OUT_PATH", out)
    assert enrich.main() == 0

    got = pd.read_csv(out).set_index(["team", "year"])
    cols = ["coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y"]
    # The 2024 window is 2019-2023 without 2020: R64 (0-1), R68 (0-1), Sweet Sixteen (2-1). The 2024 title is excluded.
    assert tuple(got.loc[("Alpha", 2024), cols]) == (3, 2, 3)
    # The window follows the coach, not the team: Coach A at Beta in 2024 has the same history.
    assert tuple(got.loc[("Beta", 2024), cols]) == (3, 2, 3)
    assert got.loc[("Beta", 2024), "coach_years_with_program"] == 1
    assert got.loc[("Alpha", 2024), "coach_years_with_program"] == 6


def test_committed_enriched_cache_is_current_and_agrees_with_the_model_table(tmp_path, monkeypatch) -> None:
    out = tmp_path / "enriched.csv"
    monkeypatch.setattr(enrich, "OUT_PATH", out)
    assert enrich.main() == 0
    committed = REPO_ROOT / "data" / "raw" / "team_history_cache_enriched.csv"
    assert out.read_bytes() == committed.read_bytes(), "rerun scripts/enrich_team_history_cache.py"

    # The model table (src/pipelines/madness.py) maps R68 to no tournament, so on tournament rows the two
    # agree on wins and differ on appearances and losses by exactly the First Four losses in the window.
    cols = ["coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y"]
    model = pd.read_csv(REPO_ROOT / "data" / "processed" / "cbb_training_clean.csv", usecols=["Team", "year", "seed", "coach"] + cols)
    model = model[model["seed"].notna()]
    enriched = pd.read_csv(committed).rename(columns={"team": "Team"})
    merged = model.merge(enriched[["Team", "year"] + cols], on=["Team", "year"], suffixes=("_model", ""))
    assert len(merged) == len(model)

    cache = pd.read_csv(REPO_ROOT / "data" / "raw" / "team_history_cache.csv", usecols=["year", "coach", "finish"])
    first_four = cache.loc[cache["finish"] == "R68"].assign(coach=lambda d: d["coach"].astype(str).str.strip())
    first_four_seasons = first_four.groupby("coach")["year"].apply(set).to_dict()

    def first_four_losses(row) -> int:
        window = {y for y in range(row.year - 5, row.year) if y != enrich.EXCLUDE_YEAR}
        return len(first_four_seasons.get(str(row.coach).strip(), set()) & window)

    r68 = merged.apply(first_four_losses, axis=1)
    assert (merged["coach_tourn_wins_5y"] == merged["coach_tourn_wins_5y_model"]).all()
    assert (merged["coach_tourn_appearances_5y"] - merged["coach_tourn_appearances_5y_model"]).eq(r68).all()
    assert (merged["coach_tourn_losses_5y"] - merged["coach_tourn_losses_5y_model"]).eq(r68).all()
    assert r68.sum() > 0
