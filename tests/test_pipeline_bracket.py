from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from src.pipelines import madness


class _SeedAwareModel:
    """Predicts 4 * Barthag - seed / 4 and keeps the matrix it was given."""

    def __init__(self) -> None:
        self.seen: pd.DataFrame | None = None

    def predict(self, X: pd.DataFrame) -> np.ndarray:
        self.seen = X.copy()
        return 4.0 * X["Barthag"].to_numpy(dtype=float) - X["seed"].to_numpy(dtype=float) / 4.0


def _training_data_full() -> pd.DataFrame:
    # Every D1 team has a row each season; seed is NaN for a team that missed that tournament.
    return pd.DataFrame(
        {
            "Team": ["Duke", "Duke", "Seton Hall", "Seton Hall"],
            "year": [2024, 2025, 2024, 2025],
            "seed": [4.0, 1.0, 13.0, np.nan],
            "Barthag": [0.90, 0.98, 0.70, 0.39],
            "round": [2.0, 4.0, 0.0, np.nan],
        }
    )


def _patch_pipeline(monkeypatch, tmp_path: Path, bracket_rows: str) -> dict:
    """run_pipeline on the four rows above: build, fit and simulate stubbed, the bracket feed real."""
    bracket = tmp_path / "bracket.csv"
    bracket.write_text("Team,Seed,Region\n" + bracket_rows, encoding="utf-8")
    model = _SeedAwareModel()
    seen: dict = {"model": model, "fits": 0}

    def _cfg():
        cfg = madness._deep_merge(madness.DEFAULT_CONFIG, {})
        cfg["ingestion"] = {"year_range": [2024, 2025], "exclude_years": [2020]}
        cfg["paths"] = {
            "data_processed": str(tmp_path / "processed"),
            "data_raw": str(tmp_path / "raw"),
            "output_training_csv": str(tmp_path / "processed" / "cbbtrainingdata"),
            "output_training_clean": str(tmp_path / "processed" / "cbb_training_clean"),
            "output_training_research": str(tmp_path / "processed" / "cbbtrainingdata_last2seasons"),
            "models": str(tmp_path / "models"),
            "reports": str(tmp_path / "reports"),
            "bracket_csv": str(bracket),
        }
        return cfg

    def _build(**kwargs):
        full = _training_data_full()
        return full[full["seed"].notna()].copy(), full

    def _fit_general(*args, **kwargs):
        seen["fits"] += 1
        return model, ["Barthag", "seed"], {"mse": 0.0, "ridge_alpha": 1.0, "tuning": {}}

    def _simulate(bracket_df, strength_lookup, **kwargs):
        seen["lookup"] = dict(strength_lookup)
        teams = bracket_df["Team"].tolist()
        out = pd.DataFrame({"Team": teams})
        for col in ["p_R64", "p_R32", "p_S16", "p_E8", "p_F4", "p_Finals", "p_Champ", "expected_round"]:
            out[col] = 1.0 / len(teams)
        return out

    monkeypatch.setattr(madness, "load_config", _cfg)
    monkeypatch.setattr(madness, "build_training_from_all_tables", _build)
    monkeypatch.setattr(madness, "build_clean_training_dataset", lambda df: df)
    monkeypatch.setattr(madness, "train_general_model", _fit_general)
    monkeypatch.setattr(madness, "train_seed_models", lambda *args, **kwargs: ({}, {}))
    monkeypatch.setattr(madness, "_save_model_artifacts", lambda *args, **kwargs: None)
    monkeypatch.setattr(madness, "simulate_bracket", _simulate)
    return seen


def test_run_pipeline_scores_the_bracket_season_with_bracket_seeds(monkeypatch, tmp_path: Path) -> None:
    # Old feed: each team's latest (2025) row, with Seton Hall's missing 2025 seed filled as 0, so it
    # scored 4 * 0.39 - 0 = 1.56 whatever season or seed the bracket gave it.
    seen = _patch_pipeline(monkeypatch, tmp_path, "Duke,4,East\nSeton Hall,13,East\n")
    madness.run_pipeline(bracket_season=2024)

    model = seen["model"]
    assert model.seen["Barthag"].tolist() == [0.90, 0.70]  # the 2024 rows
    assert model.seen["seed"].tolist() == [4.0, 13.0]  # bracket seeds
    assert seen["lookup"]["Duke"] == pytest.approx(4 * 0.90 - 1.0)
    assert seen["lookup"]["Seton Hall"] == pytest.approx(0.0)  # 2.8 - 3.25 < 0, clipped like every predround
    out = pd.read_csv(next((tmp_path / "processed").glob("round_predictions_*.csv")))
    assert dict(zip(out["Team"], out["predround"])) == pytest.approx(seen["lookup"])


def test_run_pipeline_stops_before_fitting_when_a_bracket_team_has_no_season_row(
    monkeypatch, tmp_path: Path
) -> None:
    # The old feed fell back to 6 - log2(seed) for a name it did not know and printed model-style odds.
    seen = _patch_pipeline(monkeypatch, tmp_path, "Duke,1,East\nTeam_East_16,16,East\n")
    with pytest.raises(ValueError, match="1 of 2 bracket teams have no 2025 row: Team_East_16"):
        madness.run_pipeline(bracket_season=2025)
    assert seen["fits"] == 0


def test_run_pipeline_needs_the_bracket_season(monkeypatch, tmp_path: Path) -> None:
    _patch_pipeline(monkeypatch, tmp_path, "Duke,1,East\n")
    with pytest.raises(ValueError, match="A bracket needs its season"):
        madness.run_pipeline()


def test_run_pipeline_warns_when_bracket_seeds_disagree_with_the_season(monkeypatch, tmp_path: Path, capsys) -> None:
    _patch_pipeline(monkeypatch, tmp_path, "Duke,4,East\nSeton Hall,13,East\n")
    madness.run_pipeline(bracket_season=2025)
    out = capsys.readouterr().out
    assert "1 of 2 bracket teams have another seed in the 2025 data: Duke (1 in data, 4 here)" in out
    assert "the 2025 tournament is in the training data" in out
