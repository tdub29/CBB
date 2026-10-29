#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import re
import sys
import unicodedata

import numpy as np
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.visualization.palette import PROJECT_PALETTE, apply_report_background, set_project_theme

FINISH_ORDER = [
    "R64",
    "R32",
    "Sweet Sixteen",
    "Elite Eight",
    "Final Four",
    "Finals",
    "CHAMPS",
]
FINISH_DEPTH = {
    "R64": 1,
    "R32": 2,
    "Sweet Sixteen": 3,
    "Elite Eight": 4,
    "Final Four": 5,
    "Finals": 6,
    "CHAMPS": 7,
}
FINISH_DISPLAY = {
    "R64": "R64",
    "R32": "R32",
    "Sweet Sixteen": "Sweet 16",
    "Elite Eight": "Elite 8",
    "Final Four": "Final 4",
    "Finals": "Finals",
    "CHAMPS": "Champ",
}
SEED_GROUP_MAP = {
    1: "1-2",
    2: "1-2",
    3: "3-4",
    4: "3-4",
    5: "5-6",
    6: "5-6",
    7: "7-8",
    8: "7-8",
    9: "9-10",
    10: "9-10",
    11: "11-13",
    12: "11-13",
    13: "11-13",
    14: "14-16",
    15: "14-16",
    16: "14-16",
}
SEED_GROUP_ORDER = ["1-2", "3-4", "5-6", "7-8", "9-10", "11-13", "14-16"]
CURRENT_FIELD_YEAR = 2026
TRANSFER_PORTAL_YEAR = 2022
ERAS = [
    ("2008-2011", 2008, 2011),
    ("2012-2021", 2012, 2021),
    ("2022-2025", 2022, 2025),
]
PROJECTED_FIELD_ALIASES = {
    "Connecticut": "UConn",
    "Miami FL": "Miami",
    "McNeese St.": "McNeese",
}

ACCENT = PROJECT_PALETTE["signal_blue"]
SECONDARY = PROJECT_PALETTE["steel"]
HIGHLIGHT = PROJECT_PALETTE["sage"]
TEXT_DARK = PROJECT_PALETTE["fog"]
LABEL_TEXT = PROJECT_PALETTE["fog"]
POSITIVE = PROJECT_PALETTE["sage"]
NEGATIVE = "#B26B67"

LOWER_BETTER: set[str] = set()
METRIC_COLUMNS = ["ORB", "DRB"]


def styled_box(
    facecolor: str | None = None,
    edgecolor: str | None = None,
    linewidth: float = 1.15,
    alpha: float = 0.97,
) -> dict[str, object]:
    return {
        "facecolor": facecolor or PROJECT_PALETTE["midnight"],
        "edgecolor": edgecolor or PROJECT_PALETTE["sky_blue"],
        "linewidth": linewidth,
        "alpha": alpha,
        "boxstyle": "round,pad=0.24",
    }


def metric_slug(metric: str) -> str:
    return {
        "ORB": "orb",
        "DRB": "drb",
    }.get(metric, re.sub(r"[^a-z0-9]+", "_", metric.lower()).strip("_"))


def metric_label(metric: str) -> str:
    return {
        "ORB": "Offensive rebounding rate",
        "DRB": "Defensive rebounding rate",
    }.get(metric, metric)


def format_year_tick(value: float, _: object) -> str:
    return str(int(value)) if float(value).is_integer() else ""


def clean_projected_team_name(value: object) -> str:
    text = str(value).replace("\xa0", " ").strip()
    text = re.sub(r"\*+", "", text)
    text = re.sub(r"\s+\d+$", "", text).strip()
    return text


def normalize_team_key(value: object) -> str:
    text = unicodedata.normalize("NFKD", str(value)).encode("ascii", "ignore").decode("ascii")
    text = text.lower().replace("&", "and")
    text = re.sub(r"\bsaint\b", "st", text)
    text = re.sub(r"\bstate\b", "st", text)
    text = re.sub(r"\bmount\b", "mt", text)
    text = re.sub(r"\buniversity\b", "", text)
    text = re.sub(r"\bof\b", "", text)
    text = re.sub(r"[^a-z0-9]+", "", text)
    alias_map = {
        "connecticut": "uconn",
        "uconn": "uconn",
        "miamifl": "miami",
        "miami": "miami",
        "mcneesest": "mcneese",
        "mcneese": "mcneese",
    }
    return alias_map.get(text, text)


def rebounding_edge(series: pd.Series, metric: str) -> pd.Series:
    if metric in LOWER_BETTER:
        return -series
    return series


def get_rebounding_base_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input_sample": project_root / "data" / "processed" / "rebounding_sample.csv",
        "metric_selection": project_root / "reports" / "rebounding_metric_selection.csv",
        # The March 2026 bracketology projection the 2026-field reports describe. ingest_2026.py replaced
        # data/raw/2026_expected_seeds_update.csv with the official bracket and archived the projection here.
        "seeds_2026": project_root / "data" / "raw" / "torvik_2026" / "bracketology_archive" / "2026_expected_seeds_update.csv",
        "thread_dir": project_root / "reports" / "figures" / "rebounding_thread",
    }


def load_rebounding_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    required = {"Team", "year", "Barthag", "Conf", "ORB", "DRB"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required rebounding columns: {missing}")

    out = df.copy()
    numeric_columns = ["year", "seed", "Barthag"] + METRIC_COLUMNS
    for column in numeric_columns:
        if column in out.columns:
            out[column] = pd.to_numeric(out[column], errors="coerce")
    if "finish" in out.columns:
        out["finish"] = out["finish"].astype(str).str.strip()
    out["Conf"] = out["Conf"].astype(str).str.strip()
    return out


def load_tournament_data(df: pd.DataFrame) -> pd.DataFrame:
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
    for metric in METRIC_COLUMNS:
        slug = metric_slug(metric)
        out[f"{slug}_vs_seed"] = out[metric] - out.groupby("seed_int")[metric].transform("mean")
        out[f"{slug}_edge"] = rebounding_edge(out[f"{slug}_vs_seed"], metric)
    out["glass_control_score"] = 0.55 * out["drb_edge"] + 0.45 * out["orb_edge"]
    out["offense_bucket"] = np.where(out["orb_edge"] >= 0, "strong O-board work", "weak O-board work")
    out["defense_bucket"] = np.where(out["drb_edge"] >= 0, "strong D-board work", "weak D-board work")
    return out


def build_seed_reference_tables(
    tournament_df: pd.DataFrame,
    metrics: list[str],
) -> tuple[pd.DataFrame, pd.DataFrame]:
    def summarize(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
        rows: list[dict[str, object]] = []
        for seed_int, group in df.groupby("seed_int", as_index=False):
            row: dict[str, object] = {
                "projected_seed": int(seed_int),
                f"{prefix}_seed_team_count": int(len(group)),
            }
            for metric in metrics:
                slug = metric_slug(metric)
                row[f"{prefix}_seed_avg_{slug}"] = float(group[metric].mean())
                row[f"{prefix}_seed_std_{slug}"] = float(group[metric].std(ddof=0))
            rows.append(row)
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
        raise ValueError(f"Projected 2026 field did not match current rebounding data for: {missing}")

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
    for metric in METRIC_COLUMNS:
        slug = metric_slug(metric)
        rows.append(
            {
                "metric": metric,
                "label": metric_label(metric),
                "seed_adjusted_spearman": float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman")),
            }
        )
    metric_df = pd.DataFrame(rows).sort_values("seed_adjusted_spearman", ascending=False, kind="stable").reset_index(drop=True)
    lead_margin = float(metric_df.iloc[0]["seed_adjusted_spearman"] - metric_df.iloc[1]["seed_adjusted_spearman"])
    metric_df["primary_metric"] = "DRB" if lead_margin < 0.05 else str(metric_df.iloc[0]["metric"])
    metric_df["selection_note"] = (
        f"DRB remains the default lead rebounding metric unless another metric leads by >= 0.05; observed lead margin = {lead_margin:.3f}."
    )
    return metric_df
