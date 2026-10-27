#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import re
import sys

import numpy as np
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.visualization.palette import PROJECT_PALETTE, apply_report_background, set_project_theme
from topic_analysis_common import (  # noqa: F401  (re-exported for the topic scripts)
    ACCENT,
    CURRENT_FIELD_YEAR,
    ERAS,
    FINISH_DEPTH,
    FINISH_DISPLAY,
    FINISH_ORDER,
    HIGHLIGHT,
    NEGATIVE,
    POSITIVE,
    PROJECTED_FIELD_ALIASES,
    SECONDARY,
    SEED_GROUP_MAP,
    SEED_GROUP_ORDER,
    TEXT_DARK,
    TRANSFER_PORTAL_YEAR,
    clean_projected_team_name,
    format_year_tick,
    load_tournament_data,
    normalize_team_key,
    styled_box,
)

LABEL_TEXT = PROJECT_PALETTE["fog"]

LOWER_BETTER = {"3P%D"}
METRIC_COLUMNS = ["3PRATE", "3P%", "3P%D"]


def metric_slug(metric: str) -> str:
    return {
        "3PRATE": "three_prate",
        "3P%": "three_pct",
        "3P%D": "three_pct_d",
    }.get(metric, re.sub(r"[^a-z0-9]+", "_", metric.lower()).strip("_"))


def metric_label(metric: str) -> str:
    return {
        "3PRATE": "Three-point attempt rate",
        "3P%": "Three-point percentage",
        "3P%D": "Opponent three-point percentage",
    }.get(metric, metric)


def three_point_edge(series: pd.Series, metric: str) -> pd.Series:
    if metric in LOWER_BETTER:
        return -series
    return series


def get_three_point_base_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input_sample": project_root / "data" / "processed" / "three_point_sample.csv",
        "metric_selection": project_root / "reports" / "three_point_metric_selection.csv",
        "seeds_2026": project_root / "data" / "raw" / "2026_expected_seeds_update.csv",
    }


def load_three_point_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    required = {"Team", "year", "Barthag", "Conf", "3PRATE", "3P%", "3P%D"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required three-point columns: {missing}")

    out = df.copy()
    numeric_columns = ["year", "seed", "Barthag"] + METRIC_COLUMNS
    for column in numeric_columns:
        if column in out.columns:
            out[column] = pd.to_numeric(out[column], errors="coerce")
    if "finish" in out.columns:
        out["finish"] = out["finish"].astype(str).str.strip()
    out["Conf"] = out["Conf"].astype(str).str.strip()
    return out


def build_overperformance_data(tournament_df: pd.DataFrame) -> pd.DataFrame:
    out = tournament_df.copy()
    out["expected_finish_seed"] = out.groupby("seed_int")["finish_depth"].transform("mean")
    out["overperformance"] = out["finish_depth"] - out["expected_finish_seed"]
    for metric in METRIC_COLUMNS:
        slug = metric_slug(metric)
        out[f"{slug}_vs_seed"] = out[metric] - out.groupby("seed_int")[metric].transform("mean")
        out[f"{slug}_edge"] = three_point_edge(out[f"{slug}_vs_seed"], metric)
    out["volume_bucket"] = np.where(out["three_prate_edge"] >= 0, "high volume", "low volume")
    out["success_bucket"] = np.where(out["three_pct_edge"] >= 0, "high success", "low success")
    out["defense_bucket"] = np.where(out["three_pct_d_edge"] >= 0, "strong defense", "weak defense")
    out["shot_profile_score"] = (
        0.55 * out["three_pct_edge"]
        + 0.45 * out["three_prate_edge"]
        + 0.35 * out["three_pct_d_edge"]
    )
    return out


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
    keep_columns = ["team_clean", "Team", "Conf", "Barthag"] + METRIC_COLUMNS
    current = current[keep_columns].drop_duplicates("team_clean", keep="first")

    merged = field.merge(current, on="team_clean", how="left", suffixes=("_seed", ""))
    missing = merged.loc[merged["Team"].isna(), "team_clean"].tolist()
    if missing:
        raise ValueError(f"Projected 2026 field did not match current three-point data for: {missing}")

    merged = merged.rename(
        columns={
            "Team": "team",
            "Conf": "conf",
            "Team_seed": "seed_source_team",
            "Conf_seed": "projected_conf",
            "Barthag": "barthag",
        }
    )
    merged["team_display"] = merged["team"].replace(PROJECTED_FIELD_ALIASES)
    merged["team_key"] = merged["team"].map(normalize_team_key)
    return merged


def choose_primary_metric(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for metric in ["3P%", "3PRATE", "3P%D"]:
        slug = metric_slug(metric)
        rows.append(
            {
                "metric": metric,
                "label": metric_label(metric),
                "seed_adjusted_spearman": float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman")),
            }
        )
    metric_df = pd.DataFrame(rows)
    offense_df = metric_df[metric_df["metric"].isin(["3P%", "3PRATE"])].sort_values("seed_adjusted_spearman", ascending=False, kind="stable")
    lead_margin = float(offense_df.iloc[0]["seed_adjusted_spearman"] - offense_df.iloc[1]["seed_adjusted_spearman"])
    metric_df["offense_primary_metric"] = "3P%" if lead_margin < 0.05 else str(offense_df.iloc[0]["metric"])
    metric_df["selection_note"] = np.where(
        metric_df["metric"].isin(["3P%", "3PRATE"]),
        f"3P% remains the default lead offense metric unless another metric leads by >= 0.05; observed lead margin = {lead_margin:.3f}.",
        "3P%D is the defensive companion metric.",
    )
    return metric_df
