#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import re

import numpy as np
import pandas as pd

from size_analysis_common import (
    ACCENT,
    CURRENT_FIELD_YEAR,
    HIGHLIGHT,
    LABEL_TEXT,
    NEGATIVE,
    NEUTRAL,
    POSITIVE,
    PROJECTED_FIELD_ALIASES,
    SECONDARY,
    SEED_GROUP_MAP,
    SEED_GROUP_ORDER,
    TEXT_DARK,
    TEXT_LIGHT,
    TRANSFER_PORTAL_YEAR,
    apply_report_background,
    cache_logo_images,
    clean_projected_team_name,
    format_year_tick,
    load_logo_image,
    normalize_team_key,
    repo_relative_logo_paths,
    set_project_theme,
    styled_box,
)
from topic_analysis_common import (
    FINISH_DEPTH,
    FINISH_DISPLAY,
    FINISH_ORDER,
)

PLAYER_PRIMARY_METRIC = "exp_raw"
PLAYER_SUPPORT_METRIC = "exp_rank_season"
COACH_CANDIDATES = ["coach_tourn_games_5y", "coach_years_with_program"]
COACH_SUPPORT_METRIC = "coach_tourn_win_pct_5y"
COACH_ANALYSIS_START_YEAR = 2013


def get_experience_base_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input_raw": project_root / "data" / "raw" / "assembled" / "all_trank_team_tables.csv",
        "input_sample": project_root / "data" / "processed" / "experience_sample.csv",
        "metric_selection": project_root / "reports" / "experience_metric_selection.csv",
        "seeds_2026": project_root / "data" / "raw" / "2026_expected_seeds_update.csv",
        "logo_cache": project_root / "data" / "external" / "team_logos" / "espn_ncaam",
    }


def metric_slug(metric: str) -> str:
    return {
        "exp_raw": "exp_raw",
        "exp_rank_season": "exp_rank_season",
        "exp_pct_season": "exp_pct_season",
        "coach_tourn_games_5y": "coach_tourn_games_5y",
        "coach_years_with_program": "coach_years_with_program",
        "coach_tourn_win_pct_5y": "coach_tourn_win_pct_5y",
    }.get(metric, re.sub(r"[^a-z0-9]+", "_", metric.lower()).strip("_"))


def metric_label(metric: str) -> str:
    return {
        "exp_raw": "Exp.",
        "exp_rank_season": "Season-wide Exp rank",
        "exp_pct_season": "Season-wide Exp percentile",
        "coach_tourn_games_5y": "Coach tourney games (5y)",
        "coach_years_with_program": "Coach years at program",
        "coach_tourn_win_pct_5y": "Coach tourney win pct (5y)",
    }.get(metric, metric)


def load_experience_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    rename_map = {
        "Year": "year",
        "conf": "Conf",
        "Exp.": "exp_raw",
    }
    applicable = {old: new for old, new in rename_map.items() if old in df.columns and new not in df.columns}
    if applicable:
        df = df.rename(columns=applicable)

    required = {
        "Team",
        "year",
        "Conf",
        "Barthag",
        "exp_raw",
        "coach_tourn_wins_5y",
        "coach_tourn_losses_5y",
        "coach_years_with_program",
    }
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required experience columns: {missing}")

    out = df.copy()
    numeric_columns = [
        "year",
        "seed",
        "Barthag",
        "exp_raw",
        "coach_tourn_wins_5y",
        "coach_tourn_losses_5y",
        "coach_years_with_program",
    ]
    for column in numeric_columns:
        if column in out.columns:
            out[column] = pd.to_numeric(out[column], errors="coerce")

    out["coach"] = out["coach"].fillna("").astype(str).str.strip()
    prior_history = (
        out.loc[out["year"] < CURRENT_FIELD_YEAR, ["coach", "year", "coach_tourn_wins_5y", "coach_tourn_losses_5y"]]
        .sort_values(["coach", "year"], kind="stable")
        .groupby("coach", as_index=False)
        .tail(1)
        .drop(columns=["year"])
        .rename(
            columns={
                "coach_tourn_wins_5y": "coach_tourn_wins_5y_prior",
                "coach_tourn_losses_5y": "coach_tourn_losses_5y_prior",
            }
        )
    )
    out = out.merge(prior_history, on="coach", how="left")
    current_mask = out["year"] == CURRENT_FIELD_YEAR
    for column in ["coach_tourn_wins_5y", "coach_tourn_losses_5y"]:
        prior_column = f"{column}_prior"
        fill_mask = current_mask & out[column].fillna(0).eq(0) & out[prior_column].notna()
        out.loc[fill_mask, column] = out.loc[fill_mask, prior_column]
    out = out.drop(columns=["coach_tourn_wins_5y_prior", "coach_tourn_losses_5y_prior"])

    out["exp_raw"] = out["exp_raw"].fillna(0.0)
    out["coach_tourn_wins_5y"] = out["coach_tourn_wins_5y"].fillna(0.0)
    out["coach_tourn_losses_5y"] = out["coach_tourn_losses_5y"].fillna(0.0)
    out["coach_years_with_program"] = out["coach_years_with_program"].fillna(0.0)
    out["coach_tourn_games_5y"] = out["coach_tourn_wins_5y"] + out["coach_tourn_losses_5y"]
    out["coach_tourn_win_pct_5y"] = np.where(
        out["coach_tourn_games_5y"] > 0,
        out["coach_tourn_wins_5y"] / out["coach_tourn_games_5y"],
        0.0,
    )
    out["exp_rank_season"] = (
        out.groupby("year")["exp_raw"]
        .rank(method="min", ascending=False)
        .astype(float)
    )
    season_counts = out.groupby("year")["Team"].transform("size").astype(float)
    out["exp_pct_season"] = np.where(
        season_counts > 1,
        1 - ((out["exp_rank_season"] - 1) / (season_counts - 1)),
        1.0,
    )
    out["Conf"] = out["Conf"].astype(str).str.strip()
    if "finish" in out.columns:
        out["finish"] = out["finish"].astype(str).str.strip()
    return out


def load_tournament_data(df: pd.DataFrame) -> pd.DataFrame:
    required = {"Team", "year", "seed", "finish", PLAYER_PRIMARY_METRIC, PLAYER_SUPPORT_METRIC, "Conf"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required tournament columns: {missing}")

    out = df.copy()
    out = out[out["year"] != 2020].copy()
    out = out[out["seed"].notna() & out["finish"].notna()].copy()
    out = out[out["finish"] != "R68"].copy()
    out = out[out["finish"].isin(FINISH_ORDER)].copy()
    out["seed_int"] = out["seed"].astype(int)
    out["finish_depth"] = out["finish"].map(FINISH_DEPTH)
    out["finish_label"] = out["finish"].map(FINISH_DISPLAY)
    out["seed_group"] = out["seed_int"].map(SEED_GROUP_MAP)
    return out


def build_overperformance_data(tournament_df: pd.DataFrame) -> pd.DataFrame:
    out = tournament_df.copy()
    out["expected_finish_seed"] = out.groupby("seed_int")["finish_depth"].transform("mean")
    out["overperformance"] = out["finish_depth"] - out["expected_finish_seed"]
    metrics = [
        PLAYER_PRIMARY_METRIC,
        PLAYER_SUPPORT_METRIC,
        "coach_tourn_games_5y",
        "coach_years_with_program",
        COACH_SUPPORT_METRIC,
    ]
    for metric in metrics:
        if metric in out.columns:
            slug = metric_slug(metric)
            out[f"{slug}_vs_seed"] = out[metric] - out.groupby("seed_int")[metric].transform("mean")
    return out


def compute_coach_metric_selection(tournament_df: pd.DataFrame) -> tuple[str, pd.DataFrame]:
    over_df = build_overperformance_data(tournament_df)
    rows = []
    for metric in COACH_CANDIDATES:
        slug = metric_slug(metric)
        rows.append(
            {
                "metric": metric,
                "raw_spearman": float(over_df["finish_depth"].corr(over_df[metric], method="spearman")),
                "seed_adjusted_spearman": float(
                    over_df["overperformance"].corr(over_df[f"{slug}_vs_seed"], method="spearman")
                ),
            }
        )
    metric_df = (
        pd.DataFrame(rows)
        .sort_values("seed_adjusted_spearman", ascending=False, kind="stable")
        .reset_index(drop=True)
    )
    default_metric = "coach_tourn_games_5y"
    margin = float(metric_df.iloc[0]["seed_adjusted_spearman"] - metric_df.iloc[1]["seed_adjusted_spearman"])
    lead_metric = str(metric_df.iloc[0]["metric"])
    primary_metric = lead_metric if margin >= 0.05 else default_metric
    note = (
        f"{metric_label(primary_metric)} led by {margin:.3f} and becomes the lead coach metric."
        if margin >= 0.05
        else f"{lead_metric} led by only {margin:.3f}, below the 0.05 switch threshold, so coach_tourn_games_5y remains the default."
    )
    metric_df["lead_margin"] = margin
    metric_df["coach_primary_metric"] = primary_metric
    metric_df["selection_note"] = note
    return primary_metric, metric_df


def load_or_compute_coach_metric_selection(
    tournament_df: pd.DataFrame,
    out_path: Path | None = None,
) -> tuple[str, pd.DataFrame]:
    primary_metric, metric_df = compute_coach_metric_selection(tournament_df)
    if out_path is not None:
        out_path.parent.mkdir(parents=True, exist_ok=True)
        metric_df.round(6).to_csv(out_path, index=False)
    return primary_metric, metric_df


def filter_coach_analysis(df: pd.DataFrame) -> pd.DataFrame:
    return df[df["year"] >= COACH_ANALYSIS_START_YEAR].copy()


def build_seed_reference_tables(
    tournament_df: pd.DataFrame,
    metrics: list[str],
) -> tuple[pd.DataFrame, pd.DataFrame]:
    def summarize(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
        rows: list[dict[str, object]] = []
        for seed_int, group in df.groupby("seed_int", as_index=False):
            summary_row: dict[str, object] = {
                "projected_seed": int(seed_int),
                f"{prefix}_seed_team_count": int(len(group)),
            }
            for metric in metrics:
                slug = metric_slug(metric)
                summary_row[f"{prefix}_seed_avg_{slug}"] = float(group[metric].mean())
                summary_row[f"{prefix}_seed_std_{slug}"] = float(group[metric].std(ddof=0))
            rows.append(summary_row)
        summary = pd.DataFrame(rows)
        std_columns = [column for column in summary.columns if column.startswith(f"{prefix}_seed_std_")]
        for column in std_columns:
            summary[column] = summary[column].replace(0, np.nan)
        return summary

    full_reference = summarize(tournament_df.copy(), "full")
    recent_reference = summarize(
        tournament_df[tournament_df["year"].between(TRANSFER_PORTAL_YEAR, 2025)].copy(),
        "recent",
    )
    return full_reference, recent_reference


def load_projected_field(path: Path, full_df: pd.DataFrame) -> pd.DataFrame:
    if path.suffix.lower() == ".csv":
        field = pd.read_csv(path)
    else:
        field = pd.read_excel(path)
    field = field[field["Team"].notna()].copy()
    field["projected_seed"] = pd.to_numeric(field["Seed"], errors="coerce")
    field = field[field["projected_seed"].notna()].copy()
    field["projected_seed"] = field["projected_seed"].astype(int)
    field["team_clean"] = field["Team"].map(clean_projected_team_name)

    current = full_df[full_df["year"] == CURRENT_FIELD_YEAR].copy()
    current["team_clean"] = current["Team"].astype(str).str.strip()
    keep_columns = [
        "team_clean",
        "year",
        "Team",
        "Conf",
        "Barthag",
        PLAYER_PRIMARY_METRIC,
        PLAYER_SUPPORT_METRIC,
        "exp_pct_season",
        "coach",
        "coach_tourn_games_5y",
        "coach_years_with_program",
        COACH_SUPPORT_METRIC,
    ]
    current = current[keep_columns].drop_duplicates("team_clean", keep="first")

    merged = field.merge(current, on="team_clean", how="left", suffixes=("_seed", ""))
    missing = merged.loc[merged["Team"].isna(), "team_clean"].tolist()
    if missing:
        raise ValueError(f"Projected 2026 field did not match current experience data for: {missing}")

    merged = merged.rename(
        columns={
            "Team": "team",
            "Conf": "conf",
            "Barthag": "barthag",
            "Team_seed": "seed_source_team",
            "Conf_seed": "projected_conf",
        }
    )
    merged["team_display"] = merged["team"].replace(PROJECTED_FIELD_ALIASES)
    merged["team_key"] = merged["team"].map(normalize_team_key)
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)
    return merged


def coach_context_bucket(z_score: float, threshold: float = 0.35) -> str:
    if pd.isna(z_score):
        return "neutral"
    if z_score >= threshold:
        return "positive"
    if z_score <= -threshold:
        return "negative"
    return "neutral"


__all__ = [
    "ACCENT",
    "COACH_CANDIDATES",
    "COACH_ANALYSIS_START_YEAR",
    "COACH_SUPPORT_METRIC",
    "CURRENT_FIELD_YEAR",
    "FINISH_DEPTH",
    "FINISH_DISPLAY",
    "FINISH_ORDER",
    "HIGHLIGHT",
    "LABEL_TEXT",
    "NEGATIVE",
    "NEUTRAL",
    "PLAYER_PRIMARY_METRIC",
    "PLAYER_SUPPORT_METRIC",
    "POSITIVE",
    "SECONDARY",
    "SEED_GROUP_MAP",
    "SEED_GROUP_ORDER",
    "TEXT_DARK",
    "TEXT_LIGHT",
    "TRANSFER_PORTAL_YEAR",
    "apply_report_background",
    "build_overperformance_data",
    "build_seed_reference_tables",
    "cache_logo_images",
    "clean_projected_team_name",
    "coach_context_bucket",
    "compute_coach_metric_selection",
    "format_year_tick",
    "filter_coach_analysis",
    "get_experience_base_paths",
    "load_experience_data",
    "load_logo_image",
    "repo_relative_logo_paths",
    "load_or_compute_coach_metric_selection",
    "load_projected_field",
    "load_tournament_data",
    "metric_label",
    "metric_slug",
    "normalize_team_key",
    "set_project_theme",
    "styled_box",
]
