from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd

from src.pipelines import madness


def _history_html(rows: list[list[str]]) -> str:
    header = [
        "Year",
        "T-Rk",
        "Coach",
        "Conf",
        "Rec.",
        "AdjO",
        "AdjD",
        "efgO",
        "efgD",
        "TO%",
        "TOD%",
        "OR%",
        "DR%",
        "FTR",
        "FTRD",
        "FT%",
        "2P%",
        "2P%D",
        "3P%",
        "Adj T",
    ]
    th = "".join(f"<th>{c}</th>" for c in header)
    trs = []
    for row in rows:
        tds = "".join(f"<td>{v}</td>" for v in row)
        trs.append(f"<tr>{tds}</tr>")
    return f"<html><body><table><tr>{th}</tr>{''.join(trs)}</table></body></html>"


def _row(
    *,
    year: int,
    t_rk: str,
    coach: str,
    conf: str,
    rec: str,
    adj_o: str,
) -> list[str]:
    return [
        str(year),
        t_rk,
        coach,
        conf,
        rec,
        adj_o,
        "95.0 20",
        "52.0 40",
        "48.0 30",
        "16.0 100",
        "17.0 90",
        "31.0 70",
        "28.0 80",
        "35.0 55",
        "29.0 60",
        "74.0 80",
        "54.0 70",
        "46.0 65",
        "35.0 90",
        "66.0 250",
    ]


def _make_all_tables_csv(path: Path, rows: list[dict]) -> None:
    cols = madness.TEAM_TABLE_COL_NAMES
    out_rows: list[list[object]] = []
    for r in rows:
        base = {c: 0 for c in cols}
        base["TEAM"] = r["TEAM"]
        base["ADJOE"] = r.get("ADJOE", 110.0)
        base["ADJDE"] = r.get("ADJDE", 100.0)
        base["BARTHAG"] = r.get("BARTHAG", 0.8)
        base["RECORD"] = r.get("RECORD", "20-10")
        base["WINS"] = r.get("WINS", 20)
        base["GAMES"] = r.get("GAMES", 30)
        base["YEAR"] = r["YEAR"]
        out_rows.append([base[c] for c in cols])
    pd.DataFrame(out_rows).to_csv(path, index=False, header=False)


def test_parse_team_history_to_schema_coach_patterns() -> None:
    html = _history_html(
        [
            _row(
                year=2025,
                t_rk="2",
                coach="Jon Scheyer 1 seed, Final Four",
                conf="ACC",
                rec="35-4 (19-1)",
                adj_o="130.6 1",
            ),
            _row(
                year=2024,
                t_rk="8",
                coach="Jon Scheyer",
                conf="ACC",
                rec="27-9 (15-5)",
                adj_o="121.3 9",
            ),
        ]
    )
    out = madness.parse_team_history_to_schema(html, team="Duke")
    assert set(madness.TEAM_HISTORY_CANONICAL_COLS).issubset(set(out.columns))
    assert len(out) == 2

    r2025 = out[out["year"] == 2025].iloc[0]
    assert r2025["team"] == "Duke"
    assert r2025["coach"] == "Jon Scheyer"
    assert r2025["seed"] == 1
    assert r2025["finish"] == "Final Four"
    assert r2025["ov_rec"] == "35-4"
    assert r2025["conf_rec"] == "19-1"
    assert np.isclose(float(r2025["adj_o"]), 130.6)

    r2024 = out[out["year"] == 2024].iloc[0]
    assert r2024["coach"] == "Jon Scheyer"
    assert pd.isna(r2024["seed"])
    assert r2024["finish"] == ""


def test_finish_mappings() -> None:
    assert madness._finish_to_result("R64") == 64
    assert madness._finish_to_result("R32") == 32
    assert madness._finish_to_result("Sweet Sixteen") == 16
    assert madness._finish_to_result("Elite Eight") == 8
    assert madness._finish_to_result("Final Four") == 4
    assert madness._finish_to_result("Finals") == 2
    assert madness._finish_to_result("CHAMPS") == 1
    assert pd.isna(madness._finish_to_result("Proj."))
    assert madness._finish_to_wl("R32") == (1, 1)
    assert madness._finish_to_wl("CHAMPS") == (6, 0)
    assert madness._result_to_round(64) == 0
    assert madness._result_to_round(1) == 6


def test_compute_coach_stats_skips_2020_and_counts_tournament_only() -> None:
    history = pd.DataFrame(
        [
            {"team": "Alpha", "year": 2024, "coach": "Coach A", "finish": "R32"},
            {"team": "Alpha", "year": 2023, "coach": "Coach A", "finish": "R64"},
            {"team": "Alpha", "year": 2022, "coach": "Coach B", "finish": "R64"},
            {"team": "Beta", "year": 2021, "coach": "Coach A", "finish": "R64"},
            {"team": "Beta", "year": 2020, "coach": "Coach A", "finish": "Final Four"},
            {"team": "Beta", "year": 2019, "coach": "Coach A", "finish": "Elite Eight"},
            {"team": "Beta", "year": 2022, "coach": "Coach A", "finish": ""},
        ]
    )
    team_year = pd.DataFrame([{"Team": "Alpha", "year": 2024}])
    out = madness.compute_coach_stats_from_history(team_year, history, exclude_years=(2020,))
    row = out.iloc[0]
    assert row["coach"] == "Coach A"
    # Window is 2019, 2021-2023: 2019 E8 (3-1), 2021 R64 (0-1), 2023 R64 (0-1). The 2024 R32 is the
    # season being predicted, 2020 is excluded, and the empty 2022 finish is not a tournament.
    assert row["coach_tourn_appearances_5y"] == 3
    assert row["coach_tourn_wins_5y"] == 3
    assert row["coach_tourn_losses_5y"] == 3
    assert row["coach_years_with_program"] == 2


def test_years_last_5_tournaments_excludes_current_and_2020() -> None:
    assert madness._years_last_5_tournaments(2024, (2020,)) == [2019, 2021, 2022, 2023]
    assert madness._years_last_5_tournaments(2021, (2020,)) == [2016, 2017, 2018, 2019]
    assert madness._years_last_5_tournaments(2026, (2020,)) == [2021, 2022, 2023, 2024, 2025]


def test_compute_coach_stats_ignores_current_season_finish() -> None:
    # Coach features feed models that predict the current season's round, so the
    # current finish must not move them (a deep run would otherwise leak the target).
    def _stats(current_finish: str) -> pd.Series:
        history = pd.DataFrame(
            [
                {"team": "Alpha", "year": 2024, "coach": "Coach A", "finish": current_finish},
                {"team": "Alpha", "year": 2023, "coach": "Coach A", "finish": "Sweet Sixteen"},
                {"team": "Alpha", "year": 2018, "coach": "Coach A", "finish": "CHAMPS"},
            ]
        )
        team_year = pd.DataFrame([{"Team": "Alpha", "year": 2024}])
        return madness.compute_coach_stats_from_history(team_year, history, exclude_years=(2020,)).iloc[0]

    lost_early = _stats("R64")
    champion = _stats("CHAMPS")
    for c in ["coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y", "coach_years_with_program"]:
        assert lost_early[c] == champion[c]
    # Only 2023 is inside the window; 2018 is six seasons back.
    assert champion["coach_tourn_appearances_5y"] == 1
    assert champion["coach_tourn_wins_5y"] == 2
    assert champion["coach_tourn_losses_5y"] == 1


def test_compute_coach_stats_tenure_spans_excluded_2020() -> None:
    # 2020 lost only its tournament; the season was played, so it counts toward tenure.
    def _tenure(rows: list[tuple[int, str]]) -> int:
        history = pd.DataFrame([{"team": "Alpha", "year": y, "coach": c, "finish": ""} for y, c in rows])
        team_year = pd.DataFrame([{"Team": "Alpha", "year": 2021}])
        out = madness.compute_coach_stats_from_history(team_year, history, exclude_years=(2020,))
        return int(out.iloc[0]["coach_years_with_program"])

    # Same coach 2018-2021: 4 seasons whether or not history has a 2020 row.
    assert _tenure([(2018, "Coach A"), (2019, "Coach A"), (2020, "Coach A"), (2021, "Coach A")]) == 4
    assert _tenure([(2018, "Coach A"), (2019, "Coach A"), (2021, "Coach A")]) == 4
    # A missing 2020 row is only bridged when the same coach is on both sides.
    assert _tenure([(2019, "Coach Z"), (2021, "Coach A")]) == 1
    assert _tenure([(2019, "Coach Z"), (2020, "Coach A"), (2021, "Coach A")]) == 2


def test_build_training_from_all_tables_keeps_2020_for_coach_tenure(monkeypatch, tmp_path: Path) -> None:
    all_tables = tmp_path / "all_trank_team_tables.csv"
    _make_all_tables_csv(all_tables, [{"TEAM": "Alpha", "YEAR": 2021}, {"TEAM": "Beta", "YEAR": 2021}])

    def _mock_history(**kwargs):
        hist = pd.DataFrame(
            [
                {"team": "Alpha", "year": 2021, "coach": "Coach A", "seed": 3, "finish": "R64", "conf": "ACC"},
                {"team": "Alpha", "year": 2020, "coach": "Coach A", "finish": "", "conf": "ACC"},
                {"team": "Alpha", "year": 2019, "coach": "Coach A", "seed": 5, "finish": "R32", "conf": "ACC"},
                {"team": "Beta", "year": 2021, "coach": "Coach B", "seed": 9, "finish": "R64", "conf": "SEC"},
                {"team": "Beta", "year": 2020, "coach": "Coach B", "finish": "", "conf": "SEC"},
                {"team": "Beta", "year": 2019, "coach": "Coach Z", "finish": "", "conf": "SEC"},
            ]
        )
        for c in madness.TEAM_HISTORY_CANONICAL_COLS:
            if c not in hist.columns:
                hist[c] = ""
        return hist[madness.TEAM_HISTORY_CANONICAL_COLS], []

    monkeypatch.setattr(madness, "build_team_history_all", _mock_history)
    _, full = madness.build_training_from_all_tables(
        all_tables_path=all_tables,
        team_history_cache_path=tmp_path / "team_history_cache.csv",
        ext_data_dir=tmp_path,
        data_raw_dir=tmp_path,
        run_tag="20260101",
        year_start=2021,
        year_end=2021,
        exclude_years=(2020,),
        refresh_team_history=False,
        team_history_delay_sec=0.0,
        team_history_force_playwright=False,
    )

    # Training rows stay 2021-only, but tenure sees the 2019-2020 history (Beta's coach was hired for 2020).
    assert set(full["year"]) == {2021}
    tenure = full.set_index("Team")["coach_years_with_program"]
    assert tenure["Alpha"] == 3
    assert tenure["Beta"] == 2
    appearances = full.set_index("Team")["coach_tourn_appearances_5y"]
    assert appearances["Alpha"] == 1
    assert appearances["Beta"] == 0


def test_build_training_from_all_tables_integration(monkeypatch, tmp_path: Path) -> None:
    all_tables = tmp_path / "all_trank_team_tables.csv"
    _make_all_tables_csv(
        all_tables,
        [
            {"TEAM": "Alpha", "YEAR": 2024, "ADJOE": 120.0, "ADJDE": 95.0, "BARTHAG": 0.95},
            {"TEAM": "Beta", "YEAR": 2024, "ADJOE": 112.0, "ADJDE": 100.0, "BARTHAG": 0.80},
        ],
    )

    def _mock_history(**kwargs):
        hist = pd.DataFrame(
            [
                {"team": "Alpha", "year": 2024, "coach": "Coach A", "seed": 1, "finish": "Elite Eight", "conf": "ACC"},
                {"team": "Alpha", "year": 2023, "coach": "Coach A", "seed": 2, "finish": "R64", "conf": "ACC"},
                {"team": "Beta", "year": 2024, "coach": "Coach B", "seed": 7, "finish": "R32", "conf": "SEC"},
                {"team": "Beta", "year": 2023, "coach": "Coach B", "seed": 8, "finish": "R64", "conf": "SEC"},
            ]
        )
        for c in madness.TEAM_HISTORY_CANONICAL_COLS:
            if c not in hist.columns:
                hist[c] = ""
        hist["ov_rec"] = "25-8"
        hist["conf_rec"] = "14-4"
        return hist[madness.TEAM_HISTORY_CANONICAL_COLS], []

    monkeypatch.setattr(madness, "build_team_history_all", _mock_history)
    train, full = madness.build_training_from_all_tables(
        all_tables_path=all_tables,
        team_history_cache_path=tmp_path / "team_history_cache.csv",
        ext_data_dir=tmp_path,
        data_raw_dir=tmp_path,
        run_tag="20260101",
        year_start=2024,
        year_end=2024,
        exclude_years=(2020,),
        refresh_team_history=False,
        team_history_delay_sec=0.0,
        team_history_force_playwright=False,
    )

    assert full.duplicated(["Team", "year"]).sum() == 0
    for c in ["coach", "seed", "finish", "Conf", "coach_tourn_appearances_5y"]:
        assert c in full.columns
    assert set(full["Team"]) == {"Alpha", "Beta"}
    assert len(train) > 0


def test_run_pipeline_data_only_avoids_trank_scrape(monkeypatch, tmp_path: Path) -> None:
    data_raw = tmp_path / "raw"
    data_processed = tmp_path / "processed"
    data_external = tmp_path / "external"
    data_raw.mkdir(parents=True, exist_ok=True)
    data_processed.mkdir(parents=True, exist_ok=True)
    data_external.mkdir(parents=True, exist_ok=True)

    _make_all_tables_csv(
        data_raw / "all_trank_team_tables.csv",
        [
            {"TEAM": "Alpha", "YEAR": 2024, "ADJOE": 120.0, "ADJDE": 95.0, "BARTHAG": 0.95},
            {"TEAM": "Beta", "YEAR": 2024, "ADJOE": 112.0, "ADJDE": 100.0, "BARTHAG": 0.80},
        ],
    )

    def _mock_cfg():
        cfg = madness._deep_merge(madness.DEFAULT_CONFIG, {})
        cfg["ingestion"] = {
            "year_range": [2024, 2024],
            "exclude_years": [2020],
            "team_history_delay_sec": 0.0,
            "team_history_force_playwright": False,
        }
        cfg["paths"] = {
            "data_raw": str(data_raw),
            "data_external": str(data_external),
            "data_processed": str(data_processed),
            "all_trank_team_tables": str(data_raw / "all_trank_team_tables.csv"),
            "team_history_cache": str(data_raw / "team_history_cache.csv"),
            "output_training_csv": str(data_processed / "cbbtrainingdata"),
            "output_training_clean": str(data_processed / "cbb_training_clean"),
            "output_training_research": str(data_processed / "cbbtrainingdata_last2seasons"),
            "models": str(tmp_path / "models"),
            "reports": str(tmp_path / "reports"),
            "bracket_csv": "",
        }
        cfg["research_last_n_seasons"] = 1
        return cfg

    def _mock_history(**kwargs):
        hist = pd.DataFrame(
            [
                {"team": "Alpha", "year": 2024, "coach": "Coach A", "seed": 1, "finish": "Elite Eight", "conf": "ACC"},
                {"team": "Beta", "year": 2024, "coach": "Coach B", "seed": 7, "finish": "R32", "conf": "SEC"},
            ]
        )
        for c in madness.TEAM_HISTORY_CANONICAL_COLS:
            if c not in hist.columns:
                hist[c] = ""
        return hist[madness.TEAM_HISTORY_CANONICAL_COLS], []

    monkeypatch.setattr(madness, "load_config", _mock_cfg)
    monkeypatch.setattr(madness, "build_team_history_all", _mock_history)
    monkeypatch.setattr(
        madness,
        "scrape_bart_torvik",
        lambda *args, **kwargs: (_ for _ in ()).throw(AssertionError("scrape path should not be used")),
    )

    madness.run_pipeline(data_only=True)
    out = data_processed / "cbb_training_clean.csv"
    assert out.exists()
    clean = pd.read_csv(out)
    assert clean.duplicated(["Team", "year"]).sum() == 0


def test_model_features_drop_wab() -> None:
    assert "WAB" not in madness.GEN_FEATURES
    assert all("WAB" not in feats for _, _, feats in madness.SEED_MODELS)

