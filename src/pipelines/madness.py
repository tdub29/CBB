"""
March Madness pipeline (Python): the orchestration entry point.

Replicates and optimizes madness.R with vectorized pandas and config-driven modeling.
Each step lives in its own package; this module wires them together:

- src/config.py      defaults, configs/default.yaml loading, runtime validation
- src/ingestion/     Torvik fetch and parse: T-Rank tables, team-table CSVs, team history
- src/features/      outcome encodings, coach stats, training-frame assembly and cleaning
- src/modeling/      feature lists, general ridge model, seed-stratified stack, seed baseline
- src/evaluation/    season-grouped splits, target-leakage guard, run reports

Every name this module defined before the split is re-exported below, so
`from src.pipelines.madness import X` keeps working. build_training_from_all_tables,
train_pipeline_models and run_pipeline look their steps up in this module's namespace,
so monkeypatching `madness.<name>` still reaches them.
"""

from __future__ import annotations

import logging
import time
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
from sklearn.pipeline import Pipeline

# Re-exports: scripts/ and tests/ import these from src.pipelines.madness.
from src.config import (  # noqa: F401
    DEFAULT_CONFIG,
    _deep_merge,
    _get_cfg_model,
    _get_cfg_path,
    _validate_runtime_config,
    load_config,
)
from src.evaluation.leakage import assert_no_target_leakage  # noqa: F401
from src.evaluation.reporting import (  # noqa: F401
    _build_narrative_summary,
    _md_table,
    _write_optimization_report,
)
from src.evaluation.splits import (  # noqa: F401
    WALK_FORWARD_FIRST_TEST_YEAR,
    season_cv_label,
    season_group_splits,
    season_years,
    walk_forward_splits,
)
from src.features.coach import (  # noqa: F401
    _years_last_5_tournaments,
    compute_coach_stats_from_history,
)
from src.features.outcomes import (  # noqa: F401
    _extract_seed,
    _finish_to_result,
    _finish_to_wl,
    _result_to_round,
)
from src.features.training_frame import (  # noqa: F401
    _REGULAR_TABLE_FILL_COLS,
    _clean_wab,
    _finalize_training_frame,
    _get_rec_column,
    _parse_rec_l,
    _parse_rec_w,
    _tourn_team_clean,
    _truncate_numeric_col,
    _validate_training_data,
    build_clean_training_dataset,
    build_features,
    merge_regular_into_base,
    merge_seed_result_from_tourn,
)
from src.ingestion.team_history import (  # noqa: F401
    TEAM_HISTORY_CANONICAL_COLS,
    TEAM_HISTORY_HEADER_MAP,
    TEAM_HISTORY_NUMERIC_COLS,
    TEAM_HISTORY_URL,
    _parse_team_history_numeric,
    _split_coach_seed_finish,
    _split_record_cell,
    _team_history_empty,
    _write_failed_teams_report,
    build_team_history_all,
    fetch_team_history,
    parse_team_history_to_schema,
)
from src.ingestion.team_tables import (  # noqa: F401
    TEAM_TABLE_COL_NAMES,
    TEAM_TABLE_HEADER_TO_CANONICAL,
    TEAM_TABLE_TO_CANONICAL,
    _normalize_header,
    load_all_trank_base,
    load_team_tables_as_primary,
)
from src.ingestion.torvik import (  # noqa: F401
    _TRANK_CANONICAL_COLS,
    _TRANK_COL_MAP,
    BASE_URL,
    USER_AGENT,
    _fetch_table,
    _get_trank_html,
    _normalize_columns,
    _regular_table_has_data,
    _strip_rank_suffix,
    fetch_regular_table_for_enrichment,
    fetch_tournament_table_only,
    save_raw_trank_tables,
    scrape_bart_torvik,
)
from src.modeling.artifacts import _save_model_artifacts  # noqa: F401
from src.modeling.baseline import fit_seed_baseline, predict_seed_baseline  # noqa: F401
from src.modeling.design import (  # noqa: F401
    COACH_FEATURES,
    GEN_FEATURES,
    GEN_POLY,
    ROUND_MAX,
    ROUND_MIN,
    SEED_MODELS,
    _add_poly_features,
    _get_Xy,
    _resolve_features,
    general_features,
    tournament_model_frame,
)
from src.modeling.ridge import (  # noqa: F401
    _build_regression_model,
    fit_predict_general,
    season_cv_predict,
    train_general_model,
    tune_ridge_alpha,
)
from src.modeling.stack import (  # noqa: F401
    PREDROUND_OOF_COL,
    _ensure_derived_features,
    _predict_with_seed_models,
    fit_predict_stack,
    oof_predround,
    train_seed_models,
)
from src.models.bracket import load_bracket
from src.models.simulate import simulate_bracket
from src.models.win_probability import build_strength_lookup, compute_strength

LOGGER = logging.getLogger(__name__)
if not LOGGER.handlers:
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")


def build_training_from_all_tables(
    *,
    all_tables_path: Path,
    team_history_cache_path: Path,
    ext_data_dir: Path,
    data_raw_dir: Path,
    run_tag: str,
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
    refresh_team_history: bool = False,
    team_history_delay_sec: float = 0.8,
    team_history_force_playwright: bool = False,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Build training frame from all-trank base + Torvik team-history enrichment."""
    base = load_all_trank_base(all_tables_path, year_start, year_end, exclude_years)
    if base.empty:
        raise ValueError(f"No rows loaded from base file: {all_tables_path}")

    teams = sorted(base["Team"].dropna().astype(str).str.strip().unique().tolist())
    history_all, failed = build_team_history_all(
        teams=teams,
        cache_path=team_history_cache_path,
        refresh=refresh_team_history,
        delay_sec=team_history_delay_sec,
        force_playwright=team_history_force_playwright,
        user_agent=USER_AGENT,
    )

    skip = set(exclude_years)
    history_all = history_all.copy()
    # Coach stats only look back from each season, so they get the untrimmed history: excluded seasons (2020)
    # still count toward tenure, and _years_last_5_tournaments drops them from the tournament window.
    coach_history = history_all
    if not history_all.empty:
        history_all["year"] = pd.to_numeric(history_all["year"], errors="coerce")
        history_all = history_all.dropna(subset=["year"])
        history_all["year"] = history_all["year"].astype(int)
        coach_history = history_all
        history_all = history_all[
            (history_all["year"] >= year_start)
            & (history_all["year"] <= year_end)
            & (~history_all["year"].isin(skip))
        ]
        history_all["result"] = history_all["finish"].apply(_finish_to_result)
        history_all["round"] = history_all["result"].apply(
            lambda x: _result_to_round(x) if pd.notna(x) else np.nan
        )

    history_merge_cols = ["team", "year", "coach", "seed", "finish", "conf", "ov_rec", "conf_rec", "result", "round"]
    hist_for_merge = history_all[history_merge_cols].rename(
        columns={"team": "Team", "conf": "Conf_hist"}
    ) if not history_all.empty else pd.DataFrame(columns=["Team", "year"])
    merged = base.merge(hist_for_merge, on=["Team", "year"], how="left")
    if "Conf_hist" in merged.columns:
        merged["Conf"] = merged["Conf_hist"].fillna(merged.get("Conf", ""))
        merged = merged.drop(columns=["Conf_hist"], errors="ignore")

    coach_stats = compute_coach_stats_from_history(
        merged[["Team", "year"]].drop_duplicates(),
        coach_history,
        exclude_years=exclude_years,
    )
    merged = merged.merge(
        coach_stats,
        on=["Team", "year"],
        how="left",
        suffixes=("", "_coachcalc"),
    )
    if "coach_coachcalc" in merged.columns:
        merged["coach"] = merged["coach"].replace("", np.nan).fillna(merged["coach_coachcalc"])
        merged = merged.drop(columns=["coach_coachcalc"], errors="ignore")

    # Preserve one row per Team/year before finalization.
    merged = merged.drop_duplicates(subset=["Team", "year"], keep="first").reset_index(drop=True)

    for c in [
        "coach_tourn_appearances_5y",
        "coach_tourn_wins_5y",
        "coach_tourn_losses_5y",
        "coach_years_with_program",
    ]:
        if c not in merged.columns:
            merged[c] = 0
        merged[c] = pd.to_numeric(merged[c], errors="coerce").fillna(0).astype(int)

    report_path = _write_failed_teams_report(failed, data_raw_dir=data_raw_dir, run_tag=run_tag)
    if report_path is not None:
        LOGGER.warning(
            {
                "event": "team_history_missing_teams",
                "missing_count": len(failed),
                "report_path": str(report_path),
            }
        )

    return _finalize_training_frame(
        merged,
        ext_data_dir,
        skip_ext_merge=True,
        skip_coach_merge=True,
    )


def train_pipeline_models(
    model_data: pd.DataFrame,
    runtime_cfg: dict[str, Any],
) -> tuple[Pipeline, list[str], dict, dict[int, tuple[Pipeline, list[str]]], dict[int, dict[str, Any]]]:
    """
    Fit the production models on every row of model_data (see tournament_model_frame).

    Returns (general model, its features, its meta, seed models, seed meta). The general
    model uses the coach columns only when runtime_cfg["use_coach_features"] is true. The
    seed models are only trained when runtime_cfg["use_seed_models"] is true, and then on
    out-of-fold predround from that same general feature list, grouped by season.
    """
    features = general_features(runtime_cfg["use_coach_features"])
    if not runtime_cfg["use_coach_features"]:
        print("Coach features disabled (model.use_coach_features: false); the general model leaves them out.")
    print(f"Training general model on {len(model_data)} tournament rows...")
    gen_model, gen_feats, gen_meta = train_general_model(
        model_data,
        ridge_alpha=runtime_cfg["ridge_alpha"],
        optimize_ridge_alpha=runtime_cfg["optimize_ridge_alpha"],
        ridge_alpha_grid=runtime_cfg["ridge_alpha_grid"],
        features=features,
    )
    seed_models: dict[int, tuple[Pipeline, list[str]]] = {}
    seed_meta: dict[int, dict[str, Any]] = {}
    if not runtime_cfg["use_seed_models"]:
        print("Seed-stratified models disabled (model.use_seed_models: false); using the general model.")
        return gen_model, gen_feats, gen_meta, seed_models, seed_meta

    # Seed models train on out-of-fold predround, grouped by season. The in-sample fit has
    # already seen each row's result, so seed models trained on it over-trust it.
    data = model_data.copy()
    data[PREDROUND_OOF_COL] = oof_predround(data, ridge_alpha=gen_meta["ridge_alpha"], features=features)
    print("Training seed-stratified models...")
    seed_models, seed_meta = train_seed_models(
        data,
        ridge_alpha=runtime_cfg["ridge_alpha"],
        optimize_ridge_alpha=runtime_cfg["optimize_ridge_alpha"],
        ridge_alpha_grid=runtime_cfg["ridge_alpha_grid"],
    )
    return gen_model, gen_feats, gen_meta, seed_models, seed_meta


def run_pipeline(
    bracket_csv: str | None = None,
    n_sims: int | None = None,
    k_bradley_terry: float | None = None,
    ridge_alpha: float | None = None,
    optimize_ridge_alpha: bool | None = None,
    ridge_alpha_grid: list[float] | None = None,
    data_only: bool = False,
    year_start: int | None = None,
    year_end: int | None = None,
    refresh_team_history: bool = False,
    use_trank_only: bool = False,
) -> None:
    """Run full March Madness pipeline. If data_only=True, stop after compiling and validating training data (no models)."""
    cfg = load_config()
    if year_start is not None or year_end is not None:
        cfg.setdefault("ingestion", {})
        start = year_start if year_start is not None else cfg.get("ingestion", {}).get("year_range", [2008, 2025])[0]
        end = year_end if year_end is not None else cfg.get("ingestion", {}).get("year_range", [2008, 2025])[1]
        cfg["ingestion"]["year_range"] = [start, end]
    if bracket_csv is not None:
        cfg.setdefault("paths", {})["bracket_csv"] = bracket_csv
    if n_sims is not None:
        cfg.setdefault("model", {})["n_sims"] = int(n_sims)
    if k_bradley_terry is not None:
        cfg.setdefault("model", {})["k_bradley_terry"] = float(k_bradley_terry)
    if ridge_alpha is not None:
        cfg.setdefault("model", {})["ridge_alpha"] = float(ridge_alpha)
    if optimize_ridge_alpha is not None:
        cfg.setdefault("model", {})["optimize_ridge_alpha"] = bool(optimize_ridge_alpha)
    if ridge_alpha_grid is not None:
        cfg.setdefault("model", {})["ridge_alpha_grid"] = ridge_alpha_grid

    runtime_cfg = _validate_runtime_config(cfg)
    out_prefix = _get_cfg_path(cfg, "output_training_csv", "data/processed/cbbtrainingdata")
    ext_dir = Path(_get_cfg_path(cfg, "data_external", "data/external"))
    data_raw_dir = Path(_get_cfg_path(cfg, "data_raw", "data/raw"))
    data_processed_dir = Path(_get_cfg_path(cfg, "data_processed", "data/processed"))
    all_tables_path = Path(_get_cfg_path(cfg, "all_trank_team_tables", "data/raw/assembled/all_trank_team_tables.csv"))
    team_history_cache_path = Path(_get_cfg_path(cfg, "team_history_cache", "data/raw/team_history_cache.csv"))
    models_dir = Path(_get_cfg_path(cfg, "models", "models"))
    bracket_path = _get_cfg_path(cfg, "bracket_csv", "")
    year_range = cfg.get("ingestion", {}).get("year_range", [2008, 2025])
    year_start, year_end = year_range[0], year_range[1]
    exclude_years = tuple(cfg.get("ingestion", {}).get("exclude_years", [2020]))
    team_history_delay_sec = float(cfg.get("ingestion", {}).get("team_history_delay_sec", 0.8))
    team_history_force_playwright = bool(
        cfg.get("ingestion", {}).get("team_history_force_playwright", False)
    )
    run_tag = pd.Timestamp.now().strftime("%Y%m%d")
    start_time = time.time()
    LOGGER.info(
        {
            "event": "pipeline_start",
            "year_start": year_start,
            "year_end": year_end,
            "exclude_years": exclude_years,
            "n_sims": runtime_cfg["n_sims"],
            "k_bradley_terry": runtime_cfg["k_bradley_terry"],
            "ridge_alpha": runtime_cfg["ridge_alpha"],
        }
    )

    if use_trank_only:
        print("Note: --trank-only is deprecated and ignored. Using all_trank_team_tables + team-history mode.")

    print("Building training data from all_trank_team_tables.csv + team-history.php...")
    training_data, training_data_full = build_training_from_all_tables(
        all_tables_path=all_tables_path,
        team_history_cache_path=team_history_cache_path,
        ext_data_dir=ext_dir,
        data_raw_dir=data_raw_dir,
        run_tag=run_tag,
        year_start=year_start,
        year_end=year_end,
        exclude_years=exclude_years,
        refresh_team_history=refresh_team_history,
        team_history_delay_sec=team_history_delay_sec,
        team_history_force_playwright=team_history_force_playwright,
    )

    out_path = Path(f"{out_prefix}_{run_tag}.csv")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    training_data_full.to_csv(out_path, index=False)
    print(f"Training data written to {out_path}")

    # One clean, prepped dataset: no duplicate vars, no duplicate rows, all outcomes
    clean_prefix = _get_cfg_path(cfg, "output_training_clean", "data/processed/cbb_training_clean")
    clean_path = Path(f"{clean_prefix}.csv")
    clean_path.parent.mkdir(parents=True, exist_ok=True)
    clean_df = build_clean_training_dataset(training_data_full)
    clean_df.to_csv(clean_path, index=False)
    print(f"Clean training dataset written to {clean_path} ({len(clean_df)} rows, {len(clean_df.columns)} columns)")

    # Research/feature-engineering cut: last N seasons only
    research_seasons = cfg.get("research_last_n_seasons", 2)
    if research_seasons and research_seasons > 0:
        year_col = training_data_full["year"]
        year_numeric = pd.to_numeric(year_col, errors="coerce")
        min_year = int(year_numeric.max()) - research_seasons + 1
        mask = year_numeric >= min_year
        research_full = training_data_full.loc[mask]
        if not research_full.empty:
            research_prefix = _get_cfg_path(cfg, "output_training_research", "data/processed/cbbtrainingdata_last2seasons")
            research_path = Path(f"{research_prefix}_{run_tag}.csv")
            research_full.to_csv(research_path, index=False)
            print(f"Research cut (last {research_seasons} seasons) written to {research_path} ({len(research_full)} rows)")

    if data_only:
        _validate_training_data(clean_df)
        print("Data-only run complete. No models trained.")
        return

    # Models train on the clean tournament rows, the same rows scripts/run_analysis.py
    # evaluates. The `training_data` frame drops its first three columns (Team, AdjOE,
    # AdjDE) and then every row with any NaN, which left 0 rows and an unfitted model.
    model_data = tournament_model_frame(clean_df, exclude_years)
    gen_model, gen_feats, gen_meta, seed_models, seed_meta = train_pipeline_models(model_data, runtime_cfg)

    # Create latest-team strength table used for bracket simulation. The general model is
    # fit on every training season here, which is what a new season should be scored with.
    full_predictions = training_data_full.copy()
    full_predictions["predround"] = compute_strength(full_predictions, gen_model, gen_feats)
    if seed_models:
        full_predictions = _predict_with_seed_models(full_predictions, seed_models)
    strength_lookup = build_strength_lookup(full_predictions)
    _save_model_artifacts(
        gen_model,
        seed_models,
        models_dir,
        run_tag,
        meta={"general": gen_meta, "seed_meta": seed_meta},
    )

    # Every season in year_range is in training here, so nothing in this run is a holdout.
    print("Out-of-sample evaluation: python scripts/run_analysis.py (walk-forward + held-out season).")

    round_out_path: Path | None = None
    if bracket_path:
        try:
            bracket_df = load_bracket(bracket_path)
            progress_every = runtime_cfg["n_sims"] // 10 if runtime_cfg["n_sims"] >= 2000 else 0
            round_probs = simulate_bracket(
                bracket_df=bracket_df,
                strength_lookup=strength_lookup,
                n_sims=runtime_cfg["n_sims"],
                k=runtime_cfg["k_bradley_terry"],
                progress_every=progress_every,
            )

            latest_predround = (
                full_predictions[["Team", "year", "predround"]]
                .copy()
                .assign(year=lambda d: pd.to_numeric(d["year"], errors="coerce").fillna(-1))
                .sort_values(["Team", "year"])
                .groupby("Team", as_index=False)
                .tail(1)[["Team", "predround"]]
            )
            round_output = (
                bracket_df.merge(round_probs, on="Team", how="left")
                .merge(latest_predround, on="Team", how="left")
                .loc[
                    :,
                    [
                        "Team",
                        "Seed",
                        "Region",
                        "p_R64",
                        "p_R32",
                        "p_S16",
                        "p_E8",
                        "p_F4",
                        "p_Finals",
                        "p_Champ",
                        "expected_round",
                        "predround",
                    ],
                ]
            )
            data_processed_dir.mkdir(parents=True, exist_ok=True)
            round_out_path = data_processed_dir / f"round_predictions_{run_tag}.csv"
            round_output.to_csv(round_out_path, index=False)
            print(f"Round-probability predictions written to {round_out_path}")
            print(
                _build_narrative_summary(
                    round_output,
                    n_sims=runtime_cfg["n_sims"],
                    k_bradley_terry=runtime_cfg["k_bradley_terry"],
                    bracket_path=bracket_path,
                )
            )
        except Exception as exc:
            LOGGER.warning({"event": "simulation_skipped", "reason": str(exc)})
            print(f"Bracket simulation skipped: {exc}")
    else:
        print("No bracket CSV configured; skipping round-probability simulation.")

    try:
        reports_dir = Path(_get_cfg_path(cfg, "reports", "reports"))
        report_path = reports_dir / f"model_optimization_{run_tag}.md"
        _write_optimization_report(
            report_path,
            run_tag=run_tag,
            year_start=year_start,
            year_end=year_end,
            exclude_years=exclude_years,
            runtime_cfg=runtime_cfg,
            gen_feats=gen_feats,
            gen_meta=gen_meta,
            seed_meta=seed_meta,
            round_predictions_path=str(round_out_path) if round_out_path else None,
            bracket_path=bracket_path,
        )
        print(f"Model optimization report written to {report_path}")
    except Exception as exc:
        LOGGER.warning({"event": "report_write_failed", "reason": str(exc)})

    training_data["sweetsixteen"] = (training_data["round"] >= 2).astype(int)
    elapsed = round(time.time() - start_time, 2)
    LOGGER.info(
        {
            "event": "pipeline_complete",
            "status": "success",
            "elapsed_seconds": elapsed,
            "rows_training": int(len(model_data)),
            "rows_full": int(len(training_data_full)),
            "general_season_cv_mse": float(gen_meta["mse"]),
            "seed_models_trained": int(len(seed_models)),
        }
    )
    print("Pipeline complete.")


if __name__ == "__main__":
    run_pipeline()
