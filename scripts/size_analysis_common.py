#!/usr/bin/env python3
from __future__ import annotations

from io import BytesIO
from pathlib import Path
import re
import sys
import unicodedata

import numpy as np
import pandas as pd
import requests
from PIL import Image

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
CONFERENCE_RENAMES = {"P10": "P12"}
POWER_CONFERENCES = {"ACC", "B10", "B12", "BE", "SEC", "P12"}
TRANSFER_PORTAL_YEAR = 2022
ERAS = [
    ("2008-2011", 2008, 2011),
    ("2012-2021", 2012, 2021),
    ("2022-2025", 2022, 2025),
]
CURRENT_FIELD_YEAR = 2026
PRIMARY_CANDIDATES = ["EFFHGT", "AVGHGT"]
SUPPORT_METRICS = ["BLK%", "BLKED%"]
POSITION_GROUP_SUPPORT_METRICS = ["max_c_porpag"]
PROJECTED_FIELD_ALIASES = {
    "Connecticut": "UConn",
    "Miami FL": "Miami",
    "McNeese St.": "McNeese",
}
ACCENT = PROJECT_PALETTE["signal_blue"]
SECONDARY = PROJECT_PALETTE["steel"]
HIGHLIGHT = PROJECT_PALETTE["sage"]
TEXT_DARK = PROJECT_PALETTE["fog"]
TEXT_LIGHT = PROJECT_PALETTE["steel"]
LABEL_TEXT = PROJECT_PALETTE["deep_ocean"]
POSITIVE = PROJECT_PALETTE["sage"]
NEUTRAL = PROJECT_PALETTE["steel"]
NEGATIVE = "#B26B67"


def get_size_base_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input_raw": project_root / "data" / "raw" / "assembled" / "all_trank_team_tables.csv",
        "input_sample": project_root / "data" / "processed" / "size_sample.csv",
        "metric_selection": project_root / "reports" / "size_metric_selection.csv",
        # The March 2026 bracketology projection the 2026-field reports describe. ingest_2026.py replaced
        # data/raw/2026_expected_seeds_update.csv with the official bracket and archived the projection here.
        "seeds_2026": project_root / "data" / "raw" / "torvik_2026" / "bracketology_archive" / "2026_expected_seeds_update.csv",
        "logo_cache": project_root / "data" / "external" / "team_logos" / "espn_ncaam",
        "player_stats_advanced": project_root / "data" / "raw" / "player_stats" / "player_advanced_all.csv",
    }


def styled_box(
    facecolor: str | None = None,
    edgecolor: str | None = None,
    linewidth: float = 1.15,
    alpha: float = 0.97,
) -> dict[str, object]:
    return {
        "facecolor": facecolor or PROJECT_PALETTE["paper"],
        "edgecolor": edgecolor or PROJECT_PALETTE["sky_blue"],
        "linewidth": linewidth,
        "alpha": alpha,
        "boxstyle": "round,pad=0.24",
    }


def mean_label_box() -> dict[str, object]:
    return styled_box(
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=PROJECT_PALETTE["signal_blue"],
        linewidth=1.15,
        alpha=0.96,
    )


def format_year_tick(value: float, _: object) -> str:
    return str(int(value)) if float(value).is_integer() else ""


def metric_slug(metric: str) -> str:
    return {
        "EFFHGT": "effhgt",
        "AVGHGT": "avghgt",
        "BLK%": "blk_pct",
        "BLKED%": "blked_pct",
        "max_c_porpag": "max_c_porpag",
    }.get(metric, re.sub(r"[^a-z0-9]+", "_", metric.lower()).strip("_"))


def metric_label(metric: str) -> str:
    return {
        "EFFHGT": "Effective height",
        "AVGHGT": "Average height",
        "BLK%": "Block %",
        "BLKED%": "Blocked %",
        "max_c_porpag": "Max big-man PORPAG",
    }.get(metric, metric)


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
    return text


def choose_logo_url(logos: list[dict[str, object]]) -> str | None:
    if not logos:
        return None
    for preferred_rel in ["default", "primary_logo_on_white_color", "dark"]:
        for logo in logos:
            rels = [str(rel) for rel in logo.get("rel", [])]
            if preferred_rel in rels:
                href = logo.get("href")
                return str(href) if href else None
    href = logos[0].get("href")
    return str(href) if href else None


def load_size_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    rename_map = {
        "Year": "year",
        "conf": "Conf",
        "Avg Hgt.": "AVGHGT",
        "Eff. Hgt.": "EFFHGT",
        "Blk %": "BLK%",
        "Blked %": "BLKED%",
        "Talent": "TALENT",
    }
    applicable = {old: new for old, new in rename_map.items() if old in df.columns and new not in df.columns}
    if applicable:
        df = df.rename(columns=applicable)

    required = {"Team", "year", "Conf", "Barthag", "EFFHGT", "AVGHGT"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required size columns: {missing}")

    out = df.copy()
    numeric_columns = ["year", "seed", "Barthag", "EFFHGT", "AVGHGT", "BLK%", "BLKED%", "max_c_porpag"]
    for column in numeric_columns:
        if column in out.columns:
            out[column] = pd.to_numeric(out[column], errors="coerce")
    out["Conf"] = out["Conf"].astype(str).str.strip().replace(CONFERENCE_RENAMES)
    if "finish" in out.columns:
        out["finish"] = out["finish"].astype(str).str.strip()
    return out


def load_tournament_data(df: pd.DataFrame) -> pd.DataFrame:
    required = {"Team", "year", "seed", "finish", "EFFHGT", "AVGHGT", "Conf"}
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


def compute_metric_selection(tournament_df: pd.DataFrame) -> tuple[str, str, pd.DataFrame]:
    rows = []
    for metric in PRIMARY_CANDIDATES:
        rows.append(
            {
                "metric": metric,
                "spearman_rho": float(tournament_df["finish_depth"].corr(tournament_df[metric], method="spearman")),
            }
        )
    metric_df = pd.DataFrame(rows).sort_values("spearman_rho", ascending=False, kind="stable").reset_index(drop=True)
    eff_rho = float(metric_df.loc[metric_df["metric"] == "EFFHGT", "spearman_rho"].iloc[0])
    avg_rho = float(metric_df.loc[metric_df["metric"] == "AVGHGT", "spearman_rho"].iloc[0])
    lead = abs(eff_rho - avg_rho)
    leading_metric = str(metric_df.iloc[0]["metric"])
    if lead >= 0.05:
        primary_metric = leading_metric
        selection_note = f"{metric_label(primary_metric)} led by {lead:.3f}, so it becomes the primary metric."
    else:
        primary_metric = "EFFHGT"
        selection_note = f"{leading_metric} led by only {lead:.3f}, below the 0.05 switch threshold, so EFFHGT remains the default."
    secondary_metric = [metric for metric in PRIMARY_CANDIDATES if metric != primary_metric][0]
    metric_df["lead_margin"] = lead
    metric_df["is_primary_metric"] = metric_df["metric"].eq(primary_metric)
    metric_df["selection_note"] = selection_note
    metric_df["primary_metric"] = primary_metric
    metric_df["secondary_metric"] = secondary_metric
    return primary_metric, secondary_metric, metric_df


def load_or_compute_metric_selection(
    tournament_df: pd.DataFrame,
    out_path: Path | None = None,
) -> tuple[str, str, pd.DataFrame]:
    primary_metric, secondary_metric, metric_df = compute_metric_selection(tournament_df)
    if out_path is not None:
        out_path.parent.mkdir(parents=True, exist_ok=True)
        metric_df.round(6).to_csv(out_path, index=False)
    return primary_metric, secondary_metric, metric_df


def build_overperformance_data(tournament_df: pd.DataFrame) -> pd.DataFrame:
    out = tournament_df.copy()
    out["expected_finish_seed"] = out.groupby("seed_int")["finish_depth"].transform("mean")
    out["overperformance"] = out["finish_depth"] - out["expected_finish_seed"]
    for metric in PRIMARY_CANDIDATES + [metric for metric in POSITION_GROUP_SUPPORT_METRICS if metric in out.columns]:
        slug = metric_slug(metric)
        out[f"{slug}_vs_seed"] = out[metric] - out.groupby("seed_int")[metric].transform("mean")
    return out


def build_seed_reference_tables(
    tournament_df: pd.DataFrame,
    metrics: list[str],
) -> tuple[pd.DataFrame, pd.DataFrame]:
    def summarize(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
        grouped = df.groupby("seed_int", as_index=False)
        rows = []
        for row in grouped:
            seed_int, group = row
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


def build_seed_group_carryover_table(tournament_df: pd.DataFrame, metric: str) -> pd.DataFrame:
    def summarize(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
        work = df.copy()
        work["seed_group"] = work["seed_int"].map(SEED_GROUP_MAP)
        rows: list[dict[str, object]] = []
        for seed_group, group in work.groupby("seed_group", sort=False):
            expected_finish = float(group["finish_depth"].mean())
            finish_vs_expected = group["finish_depth"] - expected_finish
            rows.append(
                {
                    "seed_group": seed_group,
                    f"{prefix}_group_rho": float(finish_vs_expected.corr(group[metric], method="spearman")),
                    f"{prefix}_group_team_count": int(len(group)),
                }
            )
        return pd.DataFrame(rows)

    full_group = summarize(tournament_df.copy(), "full")
    recent_group = summarize(
        tournament_df[tournament_df["year"].between(TRANSFER_PORTAL_YEAR, 2025)].copy(),
        "recent",
    )
    group_table = full_group.merge(recent_group, on="seed_group", how="outer")

    def classify_action_tier(row: pd.Series) -> str:
        full_rho = float(row.get("full_group_rho", np.nan))
        recent_rho = float(row.get("recent_group_rho", np.nan))
        if pd.notna(full_rho) and full_rho >= 0.25 and pd.notna(recent_rho) and recent_rho > 0:
            return "High-use pocket"
        if (pd.notna(full_rho) and full_rho >= 0.10) or (pd.notna(recent_rho) and recent_rho >= 0.15):
            return "Secondary-use pocket"
        return "Low-use pocket"

    group_table["seed_action_tier"] = group_table.apply(classify_action_tier, axis=1)
    return group_table


def assign_era(year: int) -> str:
    for label, start, end in ERAS:
        if start <= int(year) <= end:
            return label
    raise ValueError(f"Year {year} does not fit the fixed size eras")


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
        "Team",
        "Conf",
        "Barthag",
        "EFFHGT",
        "AVGHGT",
    ]
    for optional in SUPPORT_METRICS:
        if optional in current.columns:
            keep_columns.append(optional)
    for optional in POSITION_GROUP_SUPPORT_METRICS:
        if optional in current.columns:
            keep_columns.append(optional)
    current = current[keep_columns].drop_duplicates("team_clean", keep="first")

    merged = field.merge(current, on="team_clean", how="left", suffixes=("_seed", ""))
    missing = merged.loc[merged["Team"].isna(), "team_clean"].tolist()
    if missing:
        raise ValueError(f"Projected 2026 field did not match current size data for: {missing}")

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
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)
    return merged


def fetch_espn_logo_catalog() -> pd.DataFrame:
    response = requests.get(
        "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/teams?limit=2000",
        timeout=30,
    )
    response.raise_for_status()
    payload = response.json()
    teams = payload["sports"][0]["leagues"][0]["teams"]

    rows: list[dict[str, object]] = []
    for entry in teams:
        team = entry.get("team", {})
        logo_url = choose_logo_url(team.get("logos", []))
        if not logo_url:
            continue
        candidate_names = {
            team.get("displayName", ""),
            team.get("shortDisplayName", ""),
            team.get("location", ""),
            team.get("abbreviation", ""),
        }
        for candidate in candidate_names:
            if not candidate:
                continue
            rows.append(
                {
                    "logo_lookup_key": normalize_team_key(candidate),
                    "espn_team_id": str(team.get("id", "")),
                    "logo_url": logo_url,
                    "espn_display_name": str(team.get("displayName", "")),
                }
            )
    return pd.DataFrame(rows).drop_duplicates("logo_lookup_key", keep="first")


def cache_logo_images(field_table: pd.DataFrame, cache_dir: Path) -> pd.DataFrame:
    cache_dir.mkdir(parents=True, exist_ok=True)
    logo_catalog = fetch_espn_logo_catalog()
    merged = field_table.merge(logo_catalog, on="logo_lookup_key", how="left")
    cache_paths: list[str | None] = []
    for row in merged.itertuples(index=False):
        team_id = str(getattr(row, "espn_team_id", ""))
        logo_url = getattr(row, "logo_url", None)
        if not team_id or pd.isna(team_id) or pd.isna(logo_url) or not logo_url:
            cache_paths.append(None)
            continue
        logo_path = cache_dir / f"{team_id}.png"
        if not logo_path.exists():
            try:
                response = requests.get(str(logo_url), timeout=30)
                response.raise_for_status()
                image = Image.open(BytesIO(response.content)).convert("RGBA")
                image.save(logo_path)
            except Exception:
                cache_paths.append(None)
                continue
        cache_paths.append(str(logo_path))
    merged["logo_path"] = cache_paths
    return merged


def load_logo_image(logo_path: str | Path) -> Image.Image:
    return Image.open(logo_path).convert("RGBA")
