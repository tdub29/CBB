#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import sys
from io import BytesIO
import re
import unicodedata

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import requests
from matplotlib.lines import Line2D
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
from PIL import Image

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.visualization.palette import PROJECT_PALETTE, apply_report_background, set_project_theme

FINISH_DEPTH = {
    "R64": 1,
    "R32": 2,
    "Sweet Sixteen": 3,
    "Elite Eight": 4,
    "Final Four": 5,
    "Finals": 6,
    "CHAMPS": 7,
}
CONFERENCE_RENAMES = {
    "P10": "P12",
}
POWER_CONFERENCES = {"ACC", "B10", "B12", "BE", "SEC", "P12"}
TRANSFER_PORTAL_YEAR = 2022
ERAS = [
    ("2008-2011", 2008, 2011),
    ("2012-2021", 2012, 2021),
    ("2022-2025", 2022, 2025),
]
CURRENT_FIELD_YEAR = 2026
ACCENT = PROJECT_PALETTE["signal_blue"]
SECONDARY = PROJECT_PALETTE["steel"]
HIGHLIGHT = PROJECT_PALETTE["sage"]
TEXT_DARK = PROJECT_PALETTE["fog"]
TEXT_LIGHT = PROJECT_PALETTE["steel"]
LABEL_TEXT = PROJECT_PALETTE["deep_ocean"]
NEGATIVE = "#B26B67"
POSITIVE = PROJECT_PALETTE["sage"]
NEUTRAL = PROJECT_PALETTE["steel"]
SEED_GROUP_MAP_07 = {
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
PROJECTED_FIELD_ALIASES = {
    "Connecticut": "UConn",
    "Miami FL": "Miami",
    "McNeese St.": "McNeese",
}


def styled_box() -> dict[str, object]:
    return {
        "facecolor": PROJECT_PALETTE["paper"],
        "edgecolor": PROJECT_PALETTE["sky_blue"],
        "linewidth": 1.2,
        "alpha": 0.98,
        "boxstyle": "round,pad=0.25",
    }


def get_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input": project_root / "data" / "processed" / "sos_sample.csv",
        "seeds_2026": project_root / "data" / "raw" / "2026_expected_seeds_update.csv",
        "logo_cache": project_root / "data" / "external" / "team_logos" / "espn_ncaam",
        "fig_05a": project_root / "reports" / "figures" / "sos_vs_overperformance.png",
        "fig_05b": project_root / "reports" / "figures" / "sos_conf_vs_overperformance.png",
        "fig_05b_last_10": project_root / "reports" / "figures" / "sos_conf_vs_overperformance_last_10.png",
        "fig_05b_post_transfer": project_root / "reports" / "figures" / "sos_conf_vs_overperformance_post_transfer.png",
        "fig_06": project_root / "reports" / "figures" / "sos_era_comparison.png",
        "fig_07": project_root / "reports" / "figures" / "sos_2026_field.png",
        "md_05a": project_root / "reports" / "sos_vs_overperformance_summary.md",
        "md_05b": project_root / "reports" / "sos_conf_vs_overperformance_summary.md",
        "md_06": project_root / "reports" / "sos_era_comparison_summary.md",
        "md_07": project_root / "reports" / "sos_2026_field_summary.md",
        "csv_05a": project_root / "reports" / "sos_vs_overperformance_table.csv",
        "csv_05b": project_root / "reports" / "sos_conf_vs_overperformance_table.csv",
        "csv_06": project_root / "reports" / "sos_era_comparison_table.csv",
        "csv_07": project_root / "reports" / "sos_2026_field_table.csv",
    }


def pretty_metric(metric: str) -> str:
    return "Elite SOS" if metric == "Elite SOS" else "Conference avg Barthag"


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
                return str(logo.get("href"))
    href = logos[0].get("href")
    return str(href) if href else None


def load_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    rename_map = {}
    if "Year" in df.columns and "year" not in df.columns:
        rename_map["Year"] = "year"
    if "conf" in df.columns and "Conf" not in df.columns:
        rename_map["conf"] = "Conf"
    if rename_map:
        df = df.rename(columns=rename_map)

    out = df.copy()
    out["year"] = pd.to_numeric(out["year"], errors="coerce")
    out["seed"] = pd.to_numeric(out["seed"], errors="coerce")
    out["Elite SOS"] = pd.to_numeric(out["Elite SOS"], errors="coerce")
    out["conf_avg_barthag"] = pd.to_numeric(out["conf_avg_barthag"], errors="coerce")
    out["Barthag"] = pd.to_numeric(out["Barthag"], errors="coerce")
    out["Conf"] = out["Conf"].astype(str).str.strip().replace(CONFERENCE_RENAMES)
    out["finish"] = out["finish"].astype(str).str.strip()
    out = out[out["year"] != 2020].copy()
    return out


def load_tournament_data(df: pd.DataFrame) -> pd.DataFrame:
    out = df[df["seed"].notna() & df["finish"].notna() & (df["finish"] != "R68")].copy()
    out = out[out["finish"].isin(FINISH_DEPTH)].copy()
    out["seed_int"] = out["seed"].astype(int)
    out["finish_depth"] = out["finish"].map(FINISH_DEPTH)
    return out


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
    current = current[
        ["team_clean", "Team", "Conf", "Barthag", "Elite SOS", "conf_avg_barthag"]
    ].drop_duplicates("team_clean", keep="first")

    merged = field.merge(current, on="team_clean", how="left", suffixes=("_seed", ""))
    missing = merged.loc[merged["Team"].isna(), "team_clean"].tolist()
    if missing:
        raise ValueError(f"Projected 2026 field did not match current SOS data for: {missing}")

    merged = merged.rename(
        columns={
            "Team": "team",
            "Conf": "conf",
            "Conf_seed": "projected_conf",
            "Team_seed": "seed_source_team",
        }
    )
    merged["team_display"] = merged["team"].replace(PROJECTED_FIELD_ALIASES)
    merged["team_key"] = merged["team"].map(normalize_team_key)
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)
    return merged


def build_seed_reference_tables(tournament_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    def summarize(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
        summary = (
            df.groupby("seed_int", as_index=False)
            .agg(
                mean_elite_sos=("Elite SOS", "mean"),
                std_elite_sos=("Elite SOS", lambda series: float(series.std(ddof=0))),
                team_count=("Team", "size"),
            )
            .rename(columns={"seed_int": "projected_seed"})
        )
        summary["std_elite_sos"] = summary["std_elite_sos"].replace(0, np.nan)
        return summary.rename(
            columns={
                "mean_elite_sos": f"{prefix}_seed_avg_elite_sos",
                "std_elite_sos": f"{prefix}_seed_std_elite_sos",
                "team_count": f"{prefix}_seed_team_count",
            }
        )

    full_reference = summarize(tournament_df.copy(), "full")
    recent_reference = summarize(
        tournament_df[tournament_df["year"].between(TRANSFER_PORTAL_YEAR, 2025)].copy(),
        "recent",
    )
    return full_reference, recent_reference


def build_seed_group_carryover_table(tournament_df: pd.DataFrame) -> pd.DataFrame:
    def summarize(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
        work = df.copy()
        work["seed_group"] = work["seed_int"].map(SEED_GROUP_MAP_07)
        rows: list[dict[str, object]] = []
        for seed_group, group in work.groupby("seed_group", sort=False):
            expected_finish = float(group["finish_depth"].mean())
            finish_vs_expected = group["finish_depth"] - expected_finish
            rho = float(finish_vs_expected.corr(group["Elite SOS"], method="spearman"))
            rows.append(
                {
                    "seed_group": seed_group,
                    f"{prefix}_group_rho": rho,
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


def build_conference_signal_tables(over_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    full_conf = (
        over_df.groupby("Conf", as_index=False)
        .agg(
            conference_full_team_count=("Team", "size"),
            conference_full_overperformance=("overperformance", "mean"),
        )
        .rename(columns={"Conf": "conf"})
    )
    recent_conf = (
        over_df[over_df["year"].between(TRANSFER_PORTAL_YEAR, 2025)]
        .groupby("Conf", as_index=False)
        .agg(
            conference_recent_team_count=("Team", "size"),
            conference_recent_overperformance=("overperformance", "mean"),
        )
        .rename(columns={"Conf": "conf"})
    )

    def classify_signal(value: float, threshold: float) -> str:
        if pd.isna(value):
            return "unclear"
        if value >= threshold:
            return "positive"
        if value <= -threshold:
            return "negative"
        return "neutral"

    full_conf["conference_full_signal"] = full_conf["conference_full_overperformance"].apply(
        lambda value: classify_signal(float(value), 0.05)
    )
    recent_conf["conference_recent_signal"] = recent_conf["conference_recent_overperformance"].apply(
        lambda value: classify_signal(float(value), 0.10)
    )
    return full_conf, recent_conf


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


def repo_relative_path(value: object) -> object:
    """Express a local file path relative to the repo root (POSIX separators); other values pass through."""
    if not isinstance(value, (str, Path)) or not str(value):
        return value
    path = Path(value)
    try:
        return path.resolve().relative_to(REPO_ROOT).as_posix()
    except ValueError:
        return path.as_posix()


def choose_primary_metric(tournament_df: pd.DataFrame) -> str:
    elite_rho = float(tournament_df["finish_depth"].corr(tournament_df["Elite SOS"], method="spearman"))
    conf_rho = float(tournament_df["finish_depth"].corr(tournament_df["conf_avg_barthag"], method="spearman"))
    return "Elite SOS" if abs(elite_rho - conf_rho) < 0.05 or elite_rho >= conf_rho else "conf_avg_barthag"


def build_overperformance_data(tournament_df: pd.DataFrame) -> pd.DataFrame:
    out = tournament_df.copy()
    out["expected_finish_seed"] = out.groupby("seed_int")["finish_depth"].transform("mean")
    out["overperformance"] = out["finish_depth"] - out["expected_finish_seed"]
    out["elite_sos_vs_seed"] = out["Elite SOS"] - out.groupby("seed_int")["Elite SOS"].transform("mean")
    out["conf_avg_vs_seed"] = out["conf_avg_barthag"] - out.groupby("seed_int")["conf_avg_barthag"].transform("mean")
    return out


def build_analysis_05_tables(over_df: pd.DataFrame, min_team_count: int = 5) -> tuple[pd.DataFrame, pd.DataFrame]:
    metric_summary = pd.DataFrame(
        [
            {
                "metric": "Elite SOS",
                "raw_spearman": float(over_df["overperformance"].corr(over_df["Elite SOS"], method="spearman")),
                "seed_adjusted_spearman": float(
                    over_df["overperformance"].corr(over_df["elite_sos_vs_seed"], method="spearman")
                ),
            },
            {
                "metric": "conf_avg_barthag",
                "raw_spearman": float(over_df["overperformance"].corr(over_df["conf_avg_barthag"], method="spearman")),
                "seed_adjusted_spearman": float(
                    over_df["overperformance"].corr(over_df["conf_avg_vs_seed"], method="spearman")
                ),
            },
        ]
    )
    conference_table = (
        over_df.groupby("Conf", as_index=False)
        .agg(
            team_count=("Team", "size"),
            mean_overperformance=("overperformance", "mean"),
            median_overperformance=("overperformance", "median"),
            elite_sos_mean=("Elite SOS", "mean"),
            conf_avg_mean=("conf_avg_barthag", "mean"),
        )
        .sort_values("mean_overperformance", ascending=False, kind="stable")
        .reset_index(drop=True)
    )
    conference_table["conference_group"] = np.where(
        conference_table["Conf"].isin(POWER_CONFERENCES), "Power", "Mid-major"
    )
    conference_table = conference_table[conference_table["team_count"] >= min_team_count].copy()
    return metric_summary, conference_table


def format_year_span(years: list[int]) -> str:
    if not years:
        return ""
    if years[0] == years[-1]:
        return str(years[0])
    return f"{years[0]}-{years[-1]}"


def choose_clear_corner(x_values: np.ndarray, point_count: int) -> tuple[float, float, str, str]:
    candidates = [
        (0.03, 0.95, "left", "top"),
        (0.97, 0.95, "right", "top"),
        (0.03, 0.08, "left", "bottom"),
        (0.97, 0.08, "right", "bottom"),
    ]
    box_width = 0.23
    box_height = 0.11
    x_min = float(np.min(x_values))
    x_max = float(np.max(x_values))
    x_span = max(x_max - x_min, 1e-9)
    x_norm = (x_values - x_min) / x_span
    y_norm = (np.arange(point_count, dtype=float) + 0.5) / max(point_count, 1)

    best_choice: tuple[float, float, str, str] | None = None
    best_hits: int | None = None
    best_distance = -1.0

    for x_anchor, y_anchor, ha, va in candidates:
        left = x_anchor if ha == "left" else x_anchor - box_width
        right = left + box_width
        bottom = y_anchor - box_height if va == "top" else y_anchor
        top = bottom + box_height

        hits = int(((x_norm >= left) & (x_norm <= right) & (y_norm >= bottom) & (y_norm <= top)).sum())
        dx = np.where(x_norm < left, left - x_norm, np.where(x_norm > right, x_norm - right, 0.0))
        dy = np.where(y_norm < bottom, bottom - y_norm, np.where(y_norm > top, y_norm - top, 0.0))
        min_distance = float(np.sqrt(dx**2 + dy**2).min()) if point_count else 1.0

        if best_hits is None or hits < best_hits or (hits == best_hits and min_distance > best_distance):
            best_choice = (x_anchor, y_anchor, ha, va)
            best_hits = hits
            best_distance = min_distance

    if best_choice is None:
        return 0.03, 0.95, "left", "top"
    return best_choice


def build_05b_views(over_df: pd.DataFrame) -> list[dict[str, object]]:
    years = sorted(int(year) for year in over_df["year"].unique())
    last_10_years = years[-10:]
    post_transfer_years = [year for year in years if year >= TRANSFER_PORTAL_YEAR]
    return [
        {
            "path_key": "fig_05b",
            "label": "All tournament years",
            "years": years,
            "data": over_df.copy(),
        },
        {
            "path_key": "fig_05b_last_10",
            "label": "Last 10 tournaments",
            "years": last_10_years,
            "data": over_df[over_df["year"].isin(last_10_years)].copy(),
        },
        {
            "path_key": "fig_05b_post_transfer",
            "label": "Post-transfer era",
            "years": post_transfer_years,
            "data": over_df[over_df["year"] >= TRANSFER_PORTAL_YEAR].copy(),
        },
    ]


def plot_analysis_05a(over_df: pd.DataFrame, primary_metric: str, metric_summary: pd.DataFrame, out_path: Path) -> None:
    metric_column = "Elite SOS" if primary_metric == "Elite SOS" else "conf_avg_barthag"
    summary_row = metric_summary.loc[metric_summary["metric"] == primary_metric].iloc[0]
    raw_rho = float(summary_row["raw_spearman"])
    adjusted_rho = float(summary_row["seed_adjusted_spearman"])
    slope, intercept = np.polyfit(over_df[metric_column], over_df["overperformance"], 1)
    line_x = np.linspace(float(over_df[metric_column].min()), float(over_df[metric_column].max()), 200)
    line_y = intercept + slope * line_x

    set_project_theme()
    fig, ax = plt.subplots(figsize=(13.8, 8.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.10, right=0.95)
    ax.scatter(
        over_df[metric_column],
        over_df["overperformance"],
        s=26,
        color=PROJECT_PALETTE["fog"],
        alpha=0.42,
        linewidths=0,
        zorder=1,
    )
    ax.plot(line_x, line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.4, zorder=2)
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3, zorder=0)
    ax.text(
        0.03,
        0.95,
        f"Raw rho = {raw_rho:+.3f}\nSeed-adjusted rho = {adjusted_rho:+.3f}",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.5,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax.text(
        0.98,
        0.95,
        "Dashed line = raw SOS trend",
        transform=ax.transAxes,
        ha="right",
        va="top",
        fontsize=10,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax.set_xlabel(pretty_metric(primary_metric), fontsize=11, fontweight="semibold")
    ax.set_ylabel("Finish - exact-seed expectation", fontsize=11, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
    ax.tick_params(axis="y", labelsize=10, length=0, pad=6)
    fig.suptitle(
        "Raw SOS barely explains who beats seed expectation",
        fontsize=21,
        fontweight="bold",
        y=0.97,
    )
    ax.text(
        0.5,
        1.03,
        "Each point is a tournament team; the outcome is already adjusted against exact seed-line expectation",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_05b(views: list[dict[str, object]], out_path: Path) -> None:
    plot_views: list[dict[str, object]] = []
    all_values: list[float] = []
    for view in views:
        conference_table = view["conference_table"]
        plot_df = conference_table.sort_values("mean_overperformance", ascending=True, kind="stable").reset_index(drop=True)
        if plot_df.empty:
            raise ValueError(f"No conferences available to plot for {view['label']}.")
        plot_views.append(
            {
                "label": view["label"],
                "year_span": view["year_span"],
                "plot_df": plot_df,
            }
        )
        all_values.extend(plot_df["mean_overperformance"].tolist())

    x_min = min(all_values)
    x_max = max(all_values)
    x_pad = max(0.04, (x_max - x_min) * 0.08)

    set_project_theme()
    fig, axes = plt.subplots(1, 3, figsize=(23.5, 8.8), sharex=True, constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.08, right=0.98, wspace=0.24)

    for idx, (ax, view) in enumerate(zip(axes, plot_views)):
        plot_df = view["plot_df"]
        colors = np.where(
            plot_df["conference_group"].eq("Power"),
            PROJECT_PALETTE["signal_blue"],
            PROJECT_PALETTE["sage"],
        )
        sizes = 40 + (plot_df["team_count"] - plot_df["team_count"].min()) * 3.5
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)
        ax.scatter(plot_df["mean_overperformance"], plot_df["Conf"], s=sizes, c=colors, alpha=0.95, zorder=2)
        ax.set_xlim(x_min - x_pad, x_max + x_pad)
        ax.set_title(str(view["label"]), fontsize=12.5, fontweight="bold", color=TEXT_DARK, pad=20)
        ax.text(
            0.5,
            1.02,
            str(view["year_span"]),
            transform=ax.transAxes,
            ha="center",
            va="bottom",
            fontsize=9.5,
            color=TEXT_LIGHT,
        )
        ax.set_xlabel("Mean finish - exact-seed expectation", fontsize=11, fontweight="semibold")
        ax.set_ylabel("Conference" if idx == 0 else "", fontsize=11, fontweight="semibold")
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
        ax.tick_params(axis="y", labelsize=9.5, length=0, pad=6)

        top_conf = plot_df.iloc[-1]
        bottom_conf = plot_df.iloc[0]
        label_x, label_y, label_ha, label_va = choose_clear_corner(
            plot_df["mean_overperformance"].to_numpy(dtype=float),
            len(plot_df),
        )
        ax.text(
            label_x,
            label_y,
            f"Best: {top_conf['Conf']} ({top_conf['mean_overperformance']:+.2f})\nWorst: {bottom_conf['Conf']} ({bottom_conf['mean_overperformance']:+.2f})",
            transform=ax.transAxes,
            ha=label_ha,
            va=label_va,
            fontsize=10,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    fig.suptitle(
        "Conference overperformance relative to exact-seed expectation",
        fontsize=21,
        fontweight="bold",
        y=0.97,
    )
    fig.text(
        0.5,
        0.91,
        "Three slices of the same conference-overperformance view; conferences need at least 5 tournament teams in each sample, and point size reflects sample size",
        ha="center",
        va="center",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def assign_era(year: int) -> str:
    for label, start, end in ERAS:
        if start <= int(year) <= end:
            return label
    raise ValueError(f"Year {year} does not fit an SOS era")


def build_analysis_06_table(over_df: pd.DataFrame, primary_metric: str) -> pd.DataFrame:
    metric_column = "Elite SOS" if primary_metric == "Elite SOS" else "conf_avg_barthag"
    metric_vs_seed = "elite_sos_vs_seed" if primary_metric == "Elite SOS" else "conf_avg_vs_seed"
    seed_group_map = {1: "1-2", 2: "1-2", 3: "3-4", 4: "3-4", 5: "5-6", 6: "5-6", 7: "7-8", 8: "7-8", 9: "9-10", 10: "9-10", 11: "11-13", 12: "11-13", 13: "11-13"}

    era_df = over_df.copy()
    era_df["era"] = era_df["year"].apply(assign_era)
    era_df["seed_group"] = era_df["seed_int"].map(seed_group_map)

    rows = []
    for era, group in era_df.groupby("era", sort=False):
        seed_11_13 = group[group["seed_group"] == "11-13"].copy()
        seed_11_13["finish_vs_group"] = seed_11_13["finish_depth"] - seed_11_13["finish_depth"].mean()
        rows.append(
            {
                "era": era,
                "team_count": int(len(group)),
                "pooled_raw_rho": float(group["finish_depth"].corr(group[metric_column], method="spearman")),
                "seed_adjusted_rho": float(group["overperformance"].corr(group[metric_vs_seed], method="spearman")),
                "seed_11_13_rho": float(seed_11_13["finish_vs_group"].corr(seed_11_13[metric_column], method="spearman")),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_06(era_table: pd.DataFrame, out_path: Path) -> None:
    metric_specs = [
        ("pooled_raw_rho", "Pooled raw SOS vs finish"),
        ("seed_adjusted_rho", "Exact-seed adjusted SOS vs overperformance"),
        ("seed_11_13_rho", "Seeds 11-13: SOS vs finish within the band"),
    ]
    x = np.arange(len(era_table))

    set_project_theme()
    fig, axes = plt.subplots(3, 1, figsize=(14.2, 11.0), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.86, bottom=0.08, left=0.12, right=0.95, hspace=0.34)

    for ax, (column, title) in zip(axes, metric_specs):
        values = era_table[column].to_numpy(dtype=float)
        ax.plot(x, values, color=ACCENT, linewidth=2.4, marker="o", markersize=7)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        low = min(float(np.nanmin(values)), 0.0)
        high = max(float(np.nanmax(values)), 0.0)
        ax.set_ylim(low - 0.12 * (high - low), high + 0.32 * (high - low))
        for idx, value in enumerate(values):
            ax.annotate(
                f"{value:+.3f}",
                (idx, value),
                xytext=(0, 9),
                textcoords="offset points",
                ha="center",
                va="bottom",
                fontsize=10,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )
        ax.set_title(title, fontsize=12, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.set_xticks(x)
        ax.set_xticklabels(era_table["era"].tolist(), fontsize=10.5, fontweight="semibold")
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=6)
        ax.tick_params(axis="y", length=0, pad=6, labelsize=10)

    axes[-1].set_xlabel("Era", fontsize=11, fontweight="semibold")
    axes[1].set_ylabel("Spearman rho", fontsize=11, fontweight="semibold")
    fig.suptitle(
        "SOS vs tournament finish by era",
        fontsize=21,
        fontweight="bold",
        y=0.97,
    )
    fig.text(
        0.5,
        0.92,
        "Eras use the 2012 structural shift and the 2022 transfer-rules breakpoint established earlier",
        ha="center",
        va="center",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_table(
    full_df: pd.DataFrame,
    tournament_df: pd.DataFrame,
    over_df: pd.DataFrame,
    seeds_path: Path,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df).rename(
        columns={
            "Barthag": "barthag",
            "Elite SOS": "elite_sos",
        }
    )
    if len(field) != 68:
        raise ValueError(f"Expected 68 projected 2026 teams after join; found {len(field)}")

    full_reference, recent_reference = build_seed_reference_tables(tournament_df)
    group_table = build_seed_group_carryover_table(tournament_df)
    full_conf, recent_conf = build_conference_signal_tables(over_df)
    full_conf.loc[full_conf["conference_full_team_count"] < 5, "conference_full_signal"] = "unclear"
    recent_conf.loc[recent_conf["conference_recent_team_count"] < 3, "conference_recent_signal"] = "unclear"

    field["seed_group"] = field["projected_seed"].map(SEED_GROUP_MAP_07)
    merged = (
        field.merge(full_reference, on="projected_seed", how="left")
        .merge(recent_reference, on="projected_seed", how="left")
        .merge(group_table, on="seed_group", how="left")
        .merge(full_conf, on="conf", how="left")
        .merge(recent_conf, on="conf", how="left")
    )

    merged["full_sos_delta"] = merged["elite_sos"] - merged["full_seed_avg_elite_sos"]
    merged["recent_sos_delta"] = merged["elite_sos"] - merged["recent_seed_avg_elite_sos"]
    merged["full_sos_z"] = merged["full_sos_delta"] / merged["full_seed_std_elite_sos"].replace(0, np.nan)
    merged["recent_sos_z"] = merged["recent_sos_delta"] / merged["recent_seed_std_elite_sos"].replace(0, np.nan)

    def classify_position(z_score: float) -> str:
        if pd.isna(z_score):
            return "near seed baseline"
        if z_score >= 0.5:
            return "above seed baseline"
        if z_score <= -0.5:
            return "below seed baseline"
        return "near seed baseline"

    def describe_conference(row: pd.Series) -> str:
        full_signal = row.get("conference_full_signal", "unclear")
        recent_signal = row.get("conference_recent_signal", "unclear")
        if full_signal == "positive" and recent_signal == "positive":
            return "full-sample tailwind with recent reinforcement"
        if full_signal == "negative" and recent_signal == "negative":
            return "full-sample headwind with recent reinforcement"
        if full_signal == "positive":
            return "full-sample tailwind"
        if full_signal == "negative":
            return "full-sample headwind"
        if recent_signal == "positive":
            return "recent-only tailwind"
        if recent_signal == "negative":
            return "recent-only headwind"
        return "mixed or unclear conference signal"

    def classify_team(row: pd.Series) -> tuple[str, str]:
        full_position = row["full_sample_position"]
        recent_position = row["recent_position"]
        seed_action_tier = row.get("seed_action_tier", "Low-use pocket")
        conference_full_signal = row.get("conference_full_signal", "unclear")
        conference_recent_signal = row.get("conference_recent_signal", "unclear")

        high_use = seed_action_tier == "High-use pocket"
        secondary_use = seed_action_tier == "Secondary-use pocket"
        full_positive = full_position == "above seed baseline"
        full_negative = full_position == "below seed baseline"
        recent_positive = recent_position == "above seed baseline"
        recent_negative = recent_position == "below seed baseline"
        conference_tailwind = conference_full_signal == "positive"
        conference_headwind = conference_full_signal == "negative"
        recent_tailwind = conference_recent_signal == "positive"
        recent_headwind = conference_recent_signal == "negative"

        if full_positive:
            if high_use and not conference_headwind and (recent_positive or conference_tailwind or recent_tailwind):
                return "positive carryover", "stronger carryover case"
            if secondary_use and not conference_headwind and (recent_positive or conference_tailwind or recent_tailwind):
                return "positive carryover", "some support from recent trends"
            return "cautious positive", "mixed evidence"

        if full_negative:
            if high_use and (recent_negative or conference_headwind or recent_headwind):
                return "negative carryover", "weak carryover case"
            if secondary_use and (recent_negative or conference_headwind):
                return "negative carryover", "weak carryover case"
            if conference_headwind and recent_negative:
                return "negative carryover", "weak carryover case"
            return "cautious negative", "mixed evidence"

        if recent_positive:
            return "cautious positive", "some support from recent trends"
        if recent_negative:
            return "cautious negative", "some support from recent trends"
        return "mixed", "mixed evidence"

    merged["full_sample_position"] = merged["full_sos_z"].apply(classify_position)
    merged["recent_position"] = merged["recent_sos_z"].apply(classify_position)
    merged["conference_carryover_note"] = merged.apply(describe_conference, axis=1)
    merged[["carryover_direction", "carryover_confidence_note"]] = merged.apply(
        lambda row: pd.Series(classify_team(row)),
        axis=1,
    )
    merged["conference_halo"] = merged["conference_full_signal"].map(
        {
            "positive": POSITIVE,
            "negative": NEGATIVE,
            "neutral": NEUTRAL,
            "unclear": NEUTRAL,
        }
    ).fillna(NEUTRAL)

    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(["projected_seed", "elite_sos", "barthag"], ascending=[True, False, False], kind="stable")

    ordered_columns = [
        "projected_seed",
        "team",
        "team_display",
        "conf",
        "barthag",
        "elite_sos",
        "conf_avg_barthag",
        "seed_group",
        "seed_action_tier",
        "full_group_rho",
        "recent_group_rho",
        "full_seed_avg_elite_sos",
        "recent_seed_avg_elite_sos",
        "full_sos_delta",
        "recent_sos_delta",
        "full_sos_z",
        "recent_sos_z",
        "full_sample_position",
        "recent_position",
        "conference_full_overperformance",
        "conference_recent_overperformance",
        "conference_full_signal",
        "conference_recent_signal",
        "conference_carryover_note",
        "carryover_direction",
        "carryover_confidence_note",
        "conference_halo",
        "logo_path",
        "logo_fallback",
    ]
    return merged[ordered_columns].reset_index(drop=True)


def _legacy_plot_analysis_07(field_table: pd.DataFrame, out_path: Path) -> None:
    plot_df = field_table.sort_values("elite_sos_pct", ascending=True, kind="stable").reset_index(drop=True)
    y = np.arange(len(plot_df))

    set_project_theme()
    fig, ax = plt.subplots(figsize=(14.2, 10.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.85, bottom=0.08, left=0.22, right=0.95)
    ax.hlines(y, plot_df["conf_avg_pct"], plot_df["elite_sos_pct"], color=PROJECT_PALETTE["storm"], linewidth=2.0)
    ax.scatter(plot_df["conf_avg_pct"], y, s=80, color=SECONDARY, zorder=2)
    ax.scatter(plot_df["elite_sos_pct"], y, s=80, color=ACCENT, zorder=3)
    ax.set_yticks(y)
    ax.set_yticklabels(plot_df["Team"] + " (" + plot_df["Conf"] + ")", fontsize=10, fontweight="semibold")
    ax.set_xlabel("Historical percentile vs 2008-2025 tournament teams", fontsize=11, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
    ax.tick_params(axis="y", length=0, pad=6)
    ax.text(0.98, 0.96, "● Team SOS percentile", transform=ax.transAxes, ha="right", va="top", fontsize=10.5, fontweight="bold", color=ACCENT)
    ax.text(0.98, 0.92, "● Conference percentile", transform=ax.transAxes, ha="right", va="top", fontsize=10.5, fontweight="bold", color=SECONDARY)
    fig.suptitle(
        f"{CURRENT_FIELD_YEAR} contenders with the strongest historical SOS profiles",
        fontsize=21,
        fontweight="bold",
        y=0.97,
    )
    ax.text(
        0.5,
        1.03,
        f"Top {CURRENT_FIELD_TOP_N} teams by current Barthag; percentiles benchmarked against completed tournament fields",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)

def plot_analysis_07(field_table: pd.DataFrame, out_path: Path) -> None:
    plot_df = field_table.copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    seed_reference = (
        plot_df.groupby("projected_seed", as_index=False)[["full_seed_avg_elite_sos", "recent_seed_avg_elite_sos"]]
        .first()
        .sort_values("projected_seed", kind="stable")
    )

    set_project_theme()
    fig, ax = plt.subplots(figsize=(23.2, 12.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.1, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=PROJECT_PALETTE["steel"], linewidth=0.7, alpha=0.18, zorder=0)

    for row in seed_reference.itertuples(index=False):
        seed = float(row.projected_seed)
        ax.plot(
            [row.full_seed_avg_elite_sos, row.full_seed_avg_elite_sos],
            [seed - 0.26, seed + 0.26],
            color=ACCENT,
            linewidth=3.0,
            solid_capstyle="round",
            zorder=1,
        )
        ax.plot(
            [row.recent_seed_avg_elite_sos, row.recent_seed_avg_elite_sos],
            [seed - 0.18, seed + 0.18],
            color=SECONDARY,
            linewidth=2.2,
            alpha=0.75,
            solid_capstyle="round",
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(row.elite_sos)
        y_value = float(row.plot_y)
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            image = Image.open(logo_path).convert("RGBA")
            artist = AnnotationBbox(
                OffsetImage(np.asarray(image), zoom=0.11),
                (x_value, y_value),
                frameon=True,
                bboxprops={
                    "edgecolor": str(row.conference_halo),
                    "facecolor": PROJECT_PALETTE["paper"],
                    "linewidth": 2.0,
                    "alpha": 0.98,
                    "boxstyle": "circle,pad=0.16",
                },
                zorder=3,
            )
            ax.add_artist(artist)
        else:
            ax.text(
                x_value,
                y_value,
                str(row.team_display),
                ha="center",
                va="center",
                fontsize=7.2,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox={
                    "facecolor": PROJECT_PALETTE["paper"],
                    "edgecolor": str(row.conference_halo),
                    "linewidth": 1.2,
                    "alpha": 0.97,
                    "boxstyle": "round,pad=0.2",
                },
                zorder=4,
            )

    x_values = np.concatenate(
        [
            plot_df["elite_sos"].to_numpy(dtype=float),
            seed_reference["full_seed_avg_elite_sos"].to_numpy(dtype=float),
            seed_reference["recent_seed_avg_elite_sos"].to_numpy(dtype=float),
        ]
    )
    x_min = float(np.nanmin(x_values))
    x_max = float(np.nanmax(x_values))
    x_padding = max((x_max - x_min) * 0.05, 1.1)
    ax.set_xlim(x_min - x_padding, x_max + x_padding)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(seed) for seed in range(1, 17)], fontsize=11, fontweight="semibold")
    ax.set_xlabel("Elite SOS", fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        Line2D([], [], color=ACCENT, marker="|", linestyle="None", markersize=16, markeredgewidth=3, label="Full-sample seed avg"),
        Line2D([], [], color=SECONDARY, marker="|", linestyle="None", markersize=14, markeredgewidth=2.3, label="Recent seed avg"),
        Line2D([], [], marker="o", linestyle="None", markersize=10, markerfacecolor=PROJECT_PALETTE["paper"], markeredgecolor=POSITIVE, markeredgewidth=2, label="Positive conf context"),
        Line2D([], [], marker="o", linestyle="None", markersize=10, markerfacecolor=PROJECT_PALETTE["paper"], markeredgecolor=NEUTRAL, markeredgewidth=2, label="Mixed / neutral conf"),
        Line2D([], [], marker="o", linestyle="None", markersize=10, markerfacecolor=PROJECT_PALETTE["paper"], markeredgecolor=NEGATIVE, markeredgewidth=2, label="Negative conf context"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=PROJECT_PALETTE["sky_blue"],
        fontsize=9.5,
        title="Series label",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.2)
    legend_text_color = PROJECT_PALETTE["paper"]
    legend.get_title().set_color(legend_text_color)
    for text in legend.get_texts():
        text.set_color(legend_text_color)

    fig.suptitle(
        f"Projected {CURRENT_FIELD_YEAR} seed lines vs. strength of schedule",
        fontsize=22,
        fontweight="bold",
        y=0.965,
    )
    fig.text(
        0.5,
        0.92,
        "Logos to the right of their seed markers faced a tougher schedule than that seed line usually carries; halo color shows full-sample conference context",
        ha="center",
        va="center",
        fontsize=10.3,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_analysis_07_md(
    path: Path,
    field_table: pd.DataFrame,
    tournament_df: pd.DataFrame,
    over_df: pd.DataFrame,
) -> None:
    yearly_rho = (
        tournament_df.groupby("year")
        .apply(lambda group: float(group["finish_depth"].corr(group["Elite SOS"], method="spearman")))
        .reset_index(name="yearly_rho")
    )
    positive_years = int((yearly_rho["yearly_rho"] > 0).sum())
    total_years = int(len(yearly_rho))
    seed_adjusted_rho = float(over_df["overperformance"].corr(over_df["elite_sos_vs_seed"], method="spearman"))

    seed_group_view = (
        field_table[
            ["seed_group", "full_group_rho", "recent_group_rho", "seed_action_tier"]
        ]
        .drop_duplicates()
        .sort_values("seed_group", kind="stable")
    )
    strongest_full_group = seed_group_view.loc[seed_group_view["full_group_rho"].idxmax()]
    recent_11_13 = seed_group_view.loc[seed_group_view["seed_group"] == "11-13", "recent_group_rho"]
    recent_1_2 = seed_group_view.loc[seed_group_view["seed_group"] == "1-2", "recent_group_rho"]

    represented_conferences = (
        field_table[
            [
                "conf",
                "conference_full_overperformance",
                "conference_recent_overperformance",
                "conference_full_signal",
                "conference_recent_signal",
                "conference_carryover_note",
            ]
        ]
        .drop_duplicates()
        .sort_values("conference_full_overperformance", ascending=False, kind="stable")
    )
    field_counts = field_table.groupby("conf", as_index=False).agg(conference_field_team_count=("team", "size"))
    represented_conferences = represented_conferences.merge(field_counts, on="conf", how="left")
    applicable_conferences = represented_conferences[represented_conferences["conference_field_team_count"] >= 2].copy()
    tailwinds = applicable_conferences[
        applicable_conferences["conference_full_signal"] == "positive"
    ].sort_values(["conference_field_team_count", "conference_full_overperformance"], ascending=[False, False], kind="stable")
    headwinds = applicable_conferences[
        applicable_conferences["conference_full_signal"] == "negative"
    ].sort_values(["conference_field_team_count", "conference_full_overperformance"], ascending=[False, True], kind="stable")
    mixed_conferences = represented_conferences[
        ~represented_conferences["conf"].isin(tailwinds["conf"]) & ~represented_conferences["conf"].isin(headwinds["conf"])
    ].sort_values(["conference_field_team_count", "conference_full_overperformance"], ascending=[False, False], kind="stable")

    positive_teams = field_table[field_table["carryover_direction"] == "positive carryover"].copy()
    negative_teams = field_table[field_table["carryover_direction"] == "negative carryover"].copy()
    cautious_teams = field_table[
        field_table["carryover_direction"].isin(["cautious positive", "cautious negative", "mixed"])
    ].copy()

    if positive_teams.empty:
        positive_teams = field_table[field_table["full_sample_position"] == "above seed baseline"].copy()
    if negative_teams.empty:
        negative_teams = field_table[field_table["full_sample_position"] == "below seed baseline"].copy()
    if cautious_teams.empty:
        cautious_teams = field_table[field_table["recent_position"] != "near seed baseline"].copy()

    positive_teams = positive_teams.sort_values(["full_sos_delta", "barthag"], ascending=[False, False], kind="stable").head(5)
    negative_teams = negative_teams.sort_values(["full_sos_delta", "barthag"], ascending=[True, False], kind="stable").head(5)
    cautious_teams = cautious_teams.assign(abs_full_sos_delta=lambda df: df["full_sos_delta"].abs()).sort_values(
        ["abs_full_sos_delta", "barthag"], ascending=[False, False], kind="stable"
    ).head(5)

    def format_team_list(table: pd.DataFrame) -> str:
        parts: list[str] = []
        for row in table.itertuples(index=False):
            parts.append(
                f"`{row.team}` ({int(row.projected_seed)}) — {row.full_sos_delta:+.2f} vs full seed baseline; {row.conference_carryover_note}"
            )
        return "; ".join(parts)

    display_table = field_table[
        [
            "projected_seed",
            "team",
            "conf",
            "elite_sos",
            "full_seed_avg_elite_sos",
            "recent_seed_avg_elite_sos",
            "full_sample_position",
            "recent_position",
            "conference_full_signal",
            "conference_recent_signal",
            "carryover_confidence_note",
        ]
    ].copy()

    recent_11_13_value = float(recent_11_13.iloc[0]) if not recent_11_13.empty else np.nan
    recent_1_2_value = float(recent_1_2.iloc[0]) if not recent_1_2.empty else np.nan

    tailwind_text = (
        "; ".join(
            f"`{row.conf}` ({int(row.conference_field_team_count)} projected teams, {row.conference_carryover_note}, {row.conference_full_overperformance:+.3f} full)"
            for row in tailwinds.head(4).itertuples(index=False)
        )
        if not tailwinds.empty
        else "No represented conference shows a strong full-sample tailwind."
    )
    headwind_text = (
        "; ".join(
            f"`{row.conf}` ({int(row.conference_field_team_count)} projected teams, {row.conference_carryover_note}, {row.conference_full_overperformance:+.3f} full)"
            for row in headwinds.head(4).itertuples(index=False)
        )
        if not headwinds.empty
        else "No represented conference shows a strong full-sample headwind."
    )
    mixed_text = (
        "; ".join(
            f"`{row.conf}` ({int(row.conference_field_team_count)} projected teams, {row.conference_carryover_note})"
            for row in mixed_conferences.head(5).itertuples(index=False)
        )
        if not mixed_conferences.empty
        else "No represented conference falls in the mixed bucket."
    )

    lines = [
        f"# SOS Analysis 7: {CURRENT_FIELD_YEAR} Field Application",
        "",
        f"Projected seeds from `data/raw/2026_expected_seeds_update.csv` are joined to the `{CURRENT_FIELD_YEAR}` SOS sample. The scatter is contextual: logos to the right of their seed markers faced a tougher schedule than that seed line usually carries. Baselines (`full_*`) average each seed line over all `2008-2025` tournaments; the `recent_*` columns use `{TRANSFER_PORTAL_YEAR}-2025` only and are shown for comparison.",
        "",
        "## Historical relationship (2008-2025)",
        "",
        f"- Across the full tournament sample, the exact-seed-adjusted relationship between `Elite SOS` and overperformance is `{seed_adjusted_rho:+.3f}` (uncertainty: `reports/robustness_summary.md`, claim (a)).",
        f"- Raw (not seed-adjusted) `Elite SOS` was positively correlated with finish depth in `{positive_years}/{total_years}` completed tournament seasons; that raw relationship carries seed strength with it.",
        f"- Largest full-sample seed-band rho: `{strongest_full_group['seed_group']}` seeds (`rho = {float(strongest_full_group['full_group_rho']):+.3f}`). Band rhos are picked from the same sample and are not corrected for multiple testing here; see `reports/robustness_summary.md`.",
        "",
        f"## Recent seasons ({TRANSFER_PORTAL_YEAR}-2025)",
        "",
        f"- `11-13` seed-band rho in `{TRANSFER_PORTAL_YEAR}-2025`: `{recent_11_13_value:+.3f}`.",
        f"- `1-2` seed-band rho in `{TRANSFER_PORTAL_YEAR}-2025`: `{recent_1_2_value:+.3f}`.",
        f"- Recent-only bands draw on `{2025 - TRANSFER_PORTAL_YEAR + 1}` tournaments, so these estimates are noisier than the full-sample ones.",
        "",
        f"## Conference groupings in the projected {CURRENT_FIELD_YEAR} field",
        "",
        "- Conference labels (`conference_full_signal`) come from each conference's 2008-2025 mean overperformance against exact-seed expectation; `reports/robustness_summary.md` (claim (c)) gives their uncertainty.",
        f"- Above expectation historically: {tailwind_text}",
        f"- Below expectation historically: {headwind_text}",
        f"- Mixed: {mixed_text}",
        "",
        f"## {CURRENT_FIELD_YEAR} teams furthest from their seed baseline",
        "",
        f"- `carryover_direction` = positive: {format_team_list(positive_teams)}",
        f"- `carryover_direction` = negative: {format_team_list(negative_teams)}",
        f"- Cautious or mixed: {format_team_list(cautious_teams)}",
        "",
        "## Table",
        "",
        display_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_md(path: Path, title: str, bullets: list[str], table: pd.DataFrame | None = None) -> None:
    lines = [f"# {title}", ""]
    lines.extend([f"- {bullet}" for bullet in bullets])
    if table is not None:
        lines.extend(["", "## Table", "", table.round(3).to_markdown(index=False)])
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    primary_metric = choose_primary_metric(tournament_df)

    over_df = build_overperformance_data(tournament_df)
    metric_summary, conference_table = build_analysis_05_tables(over_df)
    plot_analysis_05a(over_df, primary_metric, metric_summary, paths["fig_05a"])
    analysis_05b_views: list[dict[str, object]] = []
    for view in build_05b_views(over_df):
        _, view_conference_table = build_analysis_05_tables(view["data"])
        analysis_05b_views.append(
            {
                "label": view["label"],
                "year_span": format_year_span(list(view["years"])),
                "conference_table": view_conference_table,
            }
        )
    plot_analysis_05b(analysis_05b_views, paths["fig_05b"])
    metric_summary.round(6).to_csv(paths["csv_05a"], index=False)
    conference_table.round(6).to_csv(paths["csv_05b"], index=False)
    primary_row = metric_summary.loc[metric_summary["metric"] == primary_metric].iloc[0]
    write_md(
        paths["md_05a"],
        "SOS Analysis 5A: SOS vs Overperformance",
        [
            f"Primary metric: `{pretty_metric(primary_metric)}`",
            f"Raw SOS vs overperformance rho: `{float(primary_row['raw_spearman']):+.3f}`",
            f"Seed-adjusted SOS vs overperformance rho: `{float(primary_row['seed_adjusted_spearman']):+.3f}`",
            "Interpretation: once the outcome is adjusted to exact seed expectation, raw SOS alone has little standalone explanatory power.",
        ],
        metric_summary,
    )
    best_conf = conference_table.iloc[0]
    worst_conf = conference_table.iloc[-1]
    write_md(
        paths["md_05b"],
        "SOS Analysis 5B: Conference Overperformance",
        [
            f"Best conference vs seed expectation: `{best_conf['Conf']}` at `{best_conf['mean_overperformance']:+.3f}`",
            f"Worst conference vs seed expectation: `{worst_conf['Conf']}` at `{worst_conf['mean_overperformance']:+.3f}`",
            "Interpretation: these conference means are descriptive; `reports/robustness_summary.md` (claim (c)) gives per-conference intervals and a test of whether conferences differ at all.",
        ],
        conference_table,
    )

    era_table = build_analysis_06_table(over_df, primary_metric)
    plot_analysis_06(era_table, paths["fig_06"])
    era_table.round(6).to_csv(paths["csv_06"], index=False)
    strongest_era = era_table.loc[era_table["seed_adjusted_rho"].idxmax()]
    weakest_era = era_table.loc[era_table["seed_adjusted_rho"].idxmin()]
    write_md(
        paths["md_06"],
        "SOS Analysis 6: Era Comparison",
        [
            f"Primary metric: `{pretty_metric(primary_metric)}`",
            f"Strongest exact-seed-adjusted era: `{strongest_era['era']}` at `{strongest_era['seed_adjusted_rho']:+.3f}`",
            f"Weakest exact-seed-adjusted era: `{weakest_era['era']}` at `{weakest_era['seed_adjusted_rho']:+.3f}`",
            f"Interpretation: the exact-seed-adjusted rho ranges from `{weakest_era['seed_adjusted_rho']:+.3f}` to `{strongest_era['seed_adjusted_rho']:+.3f}` across eras of `{int(era_table['team_count'].min())}`-`{int(era_table['team_count'].max())}` teams. The difference between eras is not tested here, so treat it as descriptive.",
        ],
        era_table,
    )

    field_table = build_analysis_07_table(
        full_df,
        tournament_df,
        over_df,
        paths["seeds_2026"],
        paths["logo_cache"],
    )
    plot_analysis_07(field_table, paths["fig_07"])
    field_table.assign(logo_path=field_table["logo_path"].map(repo_relative_path)).round(6).to_csv(paths["csv_07"], index=False)
    write_analysis_07_md(paths["md_07"], field_table, tournament_df, over_df)

    print(f"Primary metric: {primary_metric}")
    for key in [
        "fig_05a",
        "fig_05b",
        "fig_06",
        "fig_07",
        "md_05a",
        "md_05b",
        "md_06",
        "md_07",
        "csv_05a",
        "csv_05b",
        "csv_06",
        "csv_07",
    ]:
        print(f"Wrote {key}: {paths[key]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
