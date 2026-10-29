#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import shutil

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
import numpy as np
import pandas as pd

from size_analysis_common import cache_logo_images, load_logo_image, with_repo_relative_logo_paths
from tempo_analysis_common import (
    ACCENT,
    CURRENT_FIELD_YEAR,
    ERAS,
    HIGHLIGHT,
    LABEL_TEXT,
    PROJECT_PALETTE,
    SECONDARY,
    TEXT_DARK,
    apply_report_background,
    build_overperformance_data,
    build_seed_reference_tables,
    get_tempo_base_paths,
    load_projected_field,
    load_tempo_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    normalize_team_key,
    set_project_theme,
    styled_box,
)

RELATIONSHIP_SPECS = [
    ("AdjOE", HIGHLIGHT),
    ("AdjDE", PROJECT_PALETTE["lake_blue"]),
    ("Barthag", PROJECT_PALETTE["steel"]),
]
UPSET_BADGE_FACE = "#E8B5BF"
UPSET_BADGE_EDGE = "#9A4D5E"
OVERPERFORMER_BADGE_FACE = "#B6DCCB"
OVERPERFORMER_BADGE_EDGE = "#2F6B57"


def get_paths() -> dict[str, Path]:
    base = get_tempo_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": root / "data" / "external" / "team_logos" / "espn_ncaam",
        "thread_dir": base["thread_dir"],
        "fig_05": root / "reports" / "figures" / "tempo_relationships.png",
        "fig_06": root / "reports" / "figures" / "tempo_era_comparison.png",
        "fig_07_barthag": root / "reports" / "figures" / "tempo_2026_profile_map_barthag.png",
        "fig_07_adjoe": root / "reports" / "figures" / "tempo_2026_profile_map_adjoe.png",
        "fig_07_adjde": root / "reports" / "figures" / "tempo_2026_profile_map_adjde.png",
        "fig_07_seed": root / "reports" / "figures" / "tempo_2026_seed_vs_adjt.png",
        "csv_05": root / "reports" / "tempo_relationships_table.csv",
        "csv_06": root / "reports" / "tempo_era_comparison_table.csv",
        "csv_07": root / "reports" / "tempo_2026_field_table.csv",
        "md_05": root / "reports" / "tempo_relationships_summary.md",
        "md_06": root / "reports" / "tempo_era_comparison_summary.md",
        "md_07": root / "reports" / "tempo_2026_profile_map_summary.md",
    }


def build_analysis_05_table(tournament_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for metric, _ in RELATIONSHIP_SPECS:
        rows.append(
            {
                "metric": metric,
                "tempo_relationship_rho": float(tournament_df["AdjT"].corr(tournament_df[metric], method="spearman")),
                "tempo_relationship_pearson": float(tournament_df["AdjT"].corr(tournament_df[metric], method="pearson")),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_05(table: pd.DataFrame, tournament_df: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, axes = plt.subplots(1, 3, figsize=(20.0, 7.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.13, left=0.05, right=0.99, wspace=0.14)

    x = tournament_df["AdjT"].to_numpy(dtype=float)
    for ax, (metric, color) in zip(axes, RELATIONSHIP_SPECS):
        y = tournament_df[metric].to_numpy(dtype=float)
        slope, intercept = np.polyfit(x, y, 1)
        x_line = np.linspace(float(np.nanmin(x)), float(np.nanmax(x)), 160)
        y_line = slope * x_line + intercept
        rho = float(table.loc[table["metric"] == metric, "tempo_relationship_rho"].iloc[0])

        ax.scatter(x, y, s=13, color=color, alpha=0.24, edgecolors="none")
        ax.plot(x_line, y_line, color=color, linewidth=2.0, linestyle="--", alpha=0.95)
        ax.set_xlabel("Adjusted tempo (AdjT)", fontsize=11.0, fontweight="semibold")
        ax.set_ylabel(metric_label(metric), fontsize=11.0, fontweight="semibold")
        ax.set_title(metric_label(metric), fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)
        ax.text(
            0.03,
            0.94,
            f"rho = {rho:+.3f}",
            transform=ax.transAxes,
            ha="left",
            va="top",
            fontsize=8.8,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    fig.suptitle("How tempo relates to offense, defense, and overall quality", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "AdjT is the lead Day 8 lens here, but these panels check whether faster or slower teams are just proxies for efficiency and overall strength.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def build_analysis_06_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    all_group = {"era": "2008-2025", "team_count": int(len(over_df))}
    for metric in ["AdjT", "AdjOE", "AdjDE", "Barthag"]:
        slug = metric_slug(metric)
        all_group[f"{slug}_rho"] = float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman"))
    rows.append(all_group)
    for label, start, end in ERAS:
        group = over_df[over_df["year"].between(start, end)].copy()
        row = {"era": label, "team_count": int(len(group))}
        for metric in ["AdjT", "AdjOE", "AdjDE", "Barthag"]:
            slug = metric_slug(metric)
            row[f"{slug}_rho"] = float(group["overperformance"].corr(group[f"{slug}_edge"], method="spearman"))
        rows.append(row)
    return pd.DataFrame(rows)


def plot_analysis_06(table: pd.DataFrame, out_path: Path) -> None:
    specs = [
        ("adjt_rho", "Adjusted tempo", ACCENT),
        ("adjoe_rho", "Adjusted offensive efficiency", HIGHLIGHT),
        ("adjde_rho", "Adjusted defensive efficiency", PROJECT_PALETTE["lake_blue"]),
        ("barthag_rho", "Barthag", PROJECT_PALETTE["steel"]),
    ]
    set_project_theme()
    fig, axes = plt.subplots(2, 2, figsize=(16.2, 10.6), constrained_layout=False)
    axes = axes.flatten()
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.86, bottom=0.12, left=0.08, right=0.97, hspace=0.28, wspace=0.16)

    eras = table["era"].tolist()
    for ax, (column, title, color) in zip(axes, specs):
        colors = [color] + [HIGHLIGHT if "2022" in era else PROJECT_PALETTE["steel"] for era in eras[1:]]
        ax.bar(np.arange(len(table)), table[column], color=colors, width=0.62)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xticks(np.arange(len(table)))
        ax.set_xticklabels(eras, fontsize=10.0, fontweight="semibold")
        ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_title(title, fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)

    fig.suptitle("Did the modern era change the tempo read?", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        "Tempo gets the lead panel, but offense, defense, and Barthag stay on the page so we can see whether the era shift belongs to pace or to team quality.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_upset_history(
    tournament_df: pd.DataFrame,
    full_ref: pd.DataFrame,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    upset_rows = (
        tournament_df.loc[
            tournament_df["seed_int"].isin([1, 2, 3]) & tournament_df["finish"].eq("R64"),
            ["Team", "year", "seed_int", "AdjT", "AdjOE", "AdjDE", "Barthag"],
        ]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["year", "seed_int", "Team"], kind="stable")
        .reset_index(drop=True)
    )
    if upset_rows.empty:
        return upset_rows

    upset_rows = upset_rows.merge(
        full_ref.rename(columns={"projected_seed": "seed_int"}),
        on="seed_int",
        how="left",
    )
    for metric in ["AdjT", "AdjOE", "AdjDE", "Barthag"]:
        slug = metric_slug(metric)
        delta = upset_rows[metric] - upset_rows[f"full_seed_avg_{slug}"]
        upset_rows[f"historical_{slug}_delta_raw"] = delta
        upset_rows[f"historical_{slug}_edge"] = delta if metric != "AdjDE" else -delta
    upset_rows["team_display"] = upset_rows["Team"].astype(str).str.strip()
    upset_rows["logo_lookup_key"] = upset_rows["team_key"]
    upset_rows["historical_label"] = upset_rows.apply(
        lambda row: f"{int(row['year'])} {row['team_display']} ({int(row['seed_int'])})",
        axis=1,
    )
    upset_rows = cache_logo_images(upset_rows, logo_cache_dir)
    upset_rows["logo_fallback"] = upset_rows["logo_path"].isna()
    return upset_rows


def build_analysis_07_overperformer_history(
    tournament_df: pd.DataFrame,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    over_df = build_overperformance_data(tournament_df)
    history = (
        over_df.loc[
            over_df["overperformance"] >= 3,
            ["Team", "year", "seed_int", "AdjT", "AdjOE", "AdjDE", "Barthag", "overperformance"],
        ]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["year", "seed_int", "Team"], kind="stable")
        .reset_index(drop=True)
    )
    if history.empty:
        return history
    history["team_display"] = history["Team"].astype(str).str.strip()
    history["logo_lookup_key"] = history["team_key"]
    history = cache_logo_images(history, logo_cache_dir)
    history["logo_fallback"] = history["logo_path"].isna()
    return history


def build_analysis_07_table(
    full_df: pd.DataFrame,
    tournament_df: pd.DataFrame,
    seeds_path: Path,
    logo_cache_dir: Path,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    field = load_projected_field(seeds_path, full_df)
    field["Barthag"] = field["barthag"]
    full_ref, _ = build_seed_reference_tables(tournament_df, ["AdjT", "AdjOE", "AdjDE", "Barthag"])
    merged = field.merge(full_ref, on="projected_seed", how="left")
    for metric in ["AdjT", "AdjOE", "AdjDE", "Barthag"]:
        slug = metric_slug(metric)
        delta = merged[metric] - merged[f"full_seed_avg_{slug}"]
        merged[f"full_{slug}_delta_raw"] = delta
        merged[f"full_{slug}_edge"] = delta if metric != "AdjDE" else -delta
    merged["tempo_quality_score"] = 0.40 * merged["full_barthag_edge"] + 0.35 * merged["full_adjoe_edge"] + 0.25 * merged["full_adjde_edge"]
    merged["tournament_adjt_avg"] = float(tournament_df["AdjT"].mean())
    merged["tournament_barthag_avg"] = float(tournament_df["Barthag"].mean())
    merged["team_key"] = merged["team_display"].map(normalize_team_key)
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)

    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", ["Team", "year", "seed_int", "AdjT", "AdjOE", "AdjDE", "Barthag"]]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )

    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_pairs = []
        for _, row in group.iterrows():
            title_pairs.append(
                {
                    "year": int(row["year"]),
                    "seed": int(row["seed_int"]),
                    "adjt": float(row["AdjT"]),
                    "adjoe": float(row["AdjOE"]),
                    "adjde": float(row["AdjDE"]),
                    "barthag": float(row["Barthag"]),
                }
            )
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_pairs)),
                "title_years": [item["year"] for item in title_pairs],
                "title_seeds": [item["seed"] for item in title_pairs],
                "title_adjt_values": [item["adjt"] for item in title_pairs],
                "title_adjoe_values": [item["adjoe"] for item in title_pairs],
                "title_adjde_values": [item["adjde"] for item in title_pairs],
                "title_barthag_values": [item["barthag"] for item in title_pairs],
                "title_history": "; ".join([f"{item['year']} ({item['seed']})" for item in title_pairs]),
            }
        )

    champion_history = pd.DataFrame(champion_history_rows)
    if not champion_history.empty:
        merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged.get("championship_count", 0).fillna(0).astype(int)
    merged["title_years"] = merged.get("title_years", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged.get("title_seeds", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_adjt_values"] = merged.get("title_adjt_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_adjoe_values"] = merged.get("title_adjoe_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_adjde_values"] = merged.get("title_adjde_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_barthag_values"] = merged.get("title_barthag_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged.get("title_history", "").fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(["projected_seed", "tempo_quality_score"], ascending=[True, False], kind="stable").reset_index(drop=True)
    upset_history = build_analysis_07_upset_history(tournament_df, full_ref, logo_cache_dir)
    overperformer_history = build_analysis_07_overperformer_history(tournament_df, logo_cache_dir)
    return merged, upset_history, overperformer_history


def validate_analysis_07_table(table: pd.DataFrame) -> None:
    champion_rows = table.loc[table["is_former_champion"]].copy()
    for row in champion_rows.itertuples(index=False):
        title_years = list(getattr(row, "title_years", []))
        if not title_years:
            raise ValueError(f"Former champion {row.team_display} is missing title years for badge plotting")
        expected = len(title_years)
        for column in ["title_seeds", "title_adjt_values", "title_adjoe_values", "title_adjde_values", "title_barthag_values"]:
            values = list(getattr(row, column, []))
            if len(values) != expected:
                raise ValueError(f"Former champion {row.team_display} has mismatched values in {column}")


def plot_analysis_07_metric(
    table: pd.DataFrame,
    upset_history: pd.DataFrame,
    overperformer_history: pd.DataFrame,
    y_metric: str,
    title: str,
    y_label: str,
    subtitle: str,
    out_path: Path,
    invert_y: bool = False,
    y_axis_min: float | None = None,
    y_axis_max: float | None = None,
) -> None:
    plot_df = table.copy()
    x_values = plot_df["AdjT"].to_numpy(dtype=float)
    y_values = plot_df[y_metric].to_numpy(dtype=float)
    if not upset_history.empty:
        x_values = np.concatenate([x_values, upset_history["AdjT"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, upset_history[y_metric].to_numpy(dtype=float)])
    if not overperformer_history.empty:
        x_values = np.concatenate([x_values, overperformer_history["AdjT"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, overperformer_history[y_metric].to_numpy(dtype=float)])
    x_pad = max((float(np.nanmax(x_values)) - float(np.nanmin(x_values))) * 0.14, 0.8)
    y_pad = max((float(np.nanmax(y_values)) - float(np.nanmin(y_values))) * 0.14, 0.05)
    adjt_avg = float(plot_df["tournament_adjt_avg"].iloc[0])
    y_avg = float(plot_df[y_metric].mean())

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.6, 11.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.86, bottom=0.11, left=0.08, right=0.96)

    ax.axhline(y_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)
    ax.axvline(adjt_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)

    for row in plot_df.itertuples(index=False):
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if bool(getattr(row, "is_former_champion", False)):
                point_counts: dict[tuple[float, float], int] = {}
                title_y_values = list(getattr(row, f"title_{metric_slug(y_metric)}_values", []))
                for title_x, title_y in zip(row.title_adjt_values, title_y_values):
                    point_key = (round(float(title_x), 3), round(float(title_y), 3))
                    point_counts[point_key] = point_counts.get(point_key, 0) + 1
                    offset_idx = point_counts[point_key] - 1
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.052),
                        (float(title_x), float(title_y)),
                        xybox=(-14 - (offset_idx * 12), -14 - (offset_idx * 10)),
                        boxcoords="offset points",
                        frameon=True,
                        bboxprops={
                            "boxstyle": "round,pad=0.18",
                            "facecolor": "#F5E7A1",
                            "edgecolor": "#D4A72C",
                            "linewidth": 1.35,
                            "alpha": 0.96,
                        },
                        zorder=2,
                    )
                    ax.add_artist(champ_artist)
            artist = AnnotationBbox(
                OffsetImage(logo_image, zoom=0.092),
                (float(row.AdjT), float(getattr(row, y_metric))),
                frameon=False,
                zorder=3,
            )
            ax.add_artist(artist)

    if not upset_history.empty:
        for _, row in upset_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            logo_image = np.asarray(load_logo_image(logo_path))
            upset_artist = AnnotationBbox(
                OffsetImage(logo_image, zoom=0.050),
                (float(row["AdjT"]), float(row[y_metric])),
                frameon=True,
                bboxprops={
                    "boxstyle": "circle,pad=0.28",
                    "facecolor": UPSET_BADGE_FACE,
                    "edgecolor": UPSET_BADGE_EDGE,
                    "linewidth": 1.3,
                    "alpha": 0.95,
                },
                zorder=2,
            )
            ax.add_artist(upset_artist)

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["AdjT"])
            y_value = float(row[y_metric])
            ax.scatter(
                [x_value],
                [y_value],
                s=920,
                marker="^",
                facecolor=OVERPERFORMER_BADGE_FACE,
                edgecolor=OVERPERFORMER_BADGE_EDGE,
                linewidth=1.2,
                zorder=1.85,
            )
            ax.add_artist(
                AnnotationBbox(
                    OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.056),
                    (x_value, y_value),
                    frameon=False,
                    zorder=2.15,
                )
            )

    legend_handles = [
        Line2D([], [], color=SECONDARY, linestyle=":", linewidth=1.5, label="Tournament-field average"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor="none", markersize=10, label="Projected 2026 team"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor=UPSET_BADGE_FACE, markeredgecolor=UPSET_BADGE_EDGE, markeredgewidth=1.3, markersize=10, label="Historical 1-3 seed R64 upset"),
        Line2D([], [], marker="^", linestyle="None", markerfacecolor=OVERPERFORMER_BADGE_FACE, markeredgecolor=OVERPERFORMER_BADGE_EDGE, markeredgewidth=1.3, markersize=15, label="Historical 3+ round overperformer"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=ACCENT,
        fontsize=9.6,
        title="Field guide",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.1)
    legend_text_color = PROJECT_PALETTE["paper"]
    legend.get_title().set_color(legend_text_color)
    for text in legend.get_texts():
        text.set_color(legend_text_color)

    ax.set_xlim(float(np.nanmin(x_values)) - x_pad, float(np.nanmax(x_values)) + x_pad)
    y_min = float(np.nanmin(y_values)) - y_pad if y_axis_min is None else y_axis_min
    y_max = float(np.nanmax(y_values)) + y_pad if y_axis_max is None else y_axis_max
    ax.set_ylim(y_min, y_max)
    if invert_y:
        ax.invert_yaxis()
    ax.set_xlabel("Adjusted tempo (AdjT)", fontsize=12, fontweight="semibold")
    ax.set_ylabel(y_label, fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", labelsize=11, length=0, pad=7)

    fig.suptitle(title, fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        subtitle,
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_07_seed_map(
    table: pd.DataFrame,
    upset_history: pd.DataFrame,
    overperformer_history: pd.DataFrame,
    out_path: Path,
) -> None:
    plot_df = table.copy()
    x_values = plot_df["AdjT"].to_numpy(dtype=float)
    y_values = plot_df["projected_seed"].to_numpy(dtype=float)
    if not upset_history.empty:
        x_values = np.concatenate([x_values, upset_history["AdjT"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, upset_history["seed_int"].to_numpy(dtype=float)])
    if not overperformer_history.empty:
        x_values = np.concatenate([x_values, overperformer_history["AdjT"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, overperformer_history["seed_int"].to_numpy(dtype=float)])
    x_pad = max((float(np.nanmax(x_values)) - float(np.nanmin(x_values))) * 0.14, 0.8)
    adjt_avg = float(plot_df["tournament_adjt_avg"].iloc[0])

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.6, 11.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.86, bottom=0.11, left=0.08, right=0.96)

    ax.axvline(adjt_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)

    for row in plot_df.itertuples(index=False):
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if bool(getattr(row, "is_former_champion", False)):
                point_counts: dict[tuple[float, float], int] = {}
                for title_x, title_seed in zip(row.title_adjt_values, row.title_seeds):
                    point_key = (round(float(title_x), 3), round(float(title_seed), 3))
                    point_counts[point_key] = point_counts.get(point_key, 0) + 1
                    offset_idx = point_counts[point_key] - 1
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.052),
                        (float(title_x), float(title_seed)),
                        xybox=(-14 - (offset_idx * 12), -14 - (offset_idx * 10)),
                        boxcoords="offset points",
                        frameon=True,
                        bboxprops={
                            "boxstyle": "round,pad=0.18",
                            "facecolor": "#F5E7A1",
                            "edgecolor": "#D4A72C",
                            "linewidth": 1.35,
                            "alpha": 0.96,
                        },
                        zorder=2,
                    )
                    ax.add_artist(champ_artist)
            artist = AnnotationBbox(
                OffsetImage(logo_image, zoom=0.092),
                (float(row.AdjT), float(row.projected_seed)),
                frameon=False,
                zorder=3,
            )
            ax.add_artist(artist)

    if not upset_history.empty:
        for _, row in upset_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            logo_image = np.asarray(load_logo_image(logo_path))
            upset_artist = AnnotationBbox(
                OffsetImage(logo_image, zoom=0.050),
                (float(row["AdjT"]), float(row["seed_int"])),
                frameon=True,
                bboxprops={
                    "boxstyle": "circle,pad=0.28",
                    "facecolor": UPSET_BADGE_FACE,
                    "edgecolor": UPSET_BADGE_EDGE,
                    "linewidth": 1.3,
                    "alpha": 0.95,
                },
                zorder=2,
            )
            ax.add_artist(upset_artist)

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["AdjT"])
            y_value = float(row["seed_int"])
            ax.scatter(
                [x_value],
                [y_value],
                s=920,
                marker="^",
                facecolor=OVERPERFORMER_BADGE_FACE,
                edgecolor=OVERPERFORMER_BADGE_EDGE,
                linewidth=1.2,
                zorder=1.85,
            )
            ax.add_artist(
                AnnotationBbox(
                    OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.056),
                    (x_value, y_value),
                    frameon=False,
                    zorder=2.15,
                )
            )

    legend_handles = [
        Line2D([], [], color=SECONDARY, linestyle=":", linewidth=1.5, label="Tournament-field AdjT average"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor="none", markersize=10, label="Projected 2026 team"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor=UPSET_BADGE_FACE, markeredgecolor=UPSET_BADGE_EDGE, markeredgewidth=1.3, markersize=10, label="Historical 1-3 seed R64 upset"),
        Line2D([], [], marker="^", linestyle="None", markerfacecolor=OVERPERFORMER_BADGE_FACE, markeredgecolor=OVERPERFORMER_BADGE_EDGE, markeredgewidth=1.3, markersize=15, label="Historical 3+ round overperformer"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=ACCENT,
        fontsize=9.6,
        title="Field guide",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.1)
    legend_text_color = PROJECT_PALETTE["paper"]
    legend.get_title().set_color(legend_text_color)
    for text in legend.get_texts():
        text.set_color(legend_text_color)

    ax.set_xlim(float(np.nanmin(x_values)) - x_pad, float(np.nanmax(x_values)) + x_pad)
    ax.set_ylim(16.5, 0.5)
    ax.set_yticks(np.arange(1, 17))
    ax.set_xlabel("Adjusted tempo (AdjT)", fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", labelsize=11, length=0, pad=7)

    fig.suptitle(f"Projected {CURRENT_FIELD_YEAR} seed lines vs. adjusted tempo", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        "This isolates pace itself in the current field: farther right means faster tempo, higher on the chart means a better projected seed.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def write_summary_05(table: pd.DataFrame, out_path: Path) -> None:
    strongest = table.loc[table["tempo_relationship_rho"].abs().idxmax()]
    lines = [
        "# Tempo Analysis 5: Tempo Relationships",
        "",
        f"- Strongest tempo relationship: `{strongest['metric']}` at Spearman rho `{strongest['tempo_relationship_rho']:+.3f}`.",
        "- These panels test whether pace is acting as a real style choice or just tagging along with stronger offense, defense, or overall quality.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_06(table: pd.DataFrame, out_path: Path) -> None:
    modern = table.loc[table["era"] == "2022-2025"].iloc[0]
    lines = [
        "# Tempo Analysis 6: Era Comparison",
        "",
        f"- Modern-era AdjT rho: `{modern['adjt_rho']:+.3f}`.",
        f"- Modern-era AdjOE rho: `{modern['adjoe_rho']:+.3f}`.",
        f"- Modern-era AdjDE rho: `{modern['adjde_rho']:+.3f}`.",
        f"- Modern-era Barthag rho: `{modern['barthag_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_07(table: pd.DataFrame, out_path: Path) -> None:
    strongest = table.nlargest(8, "tempo_quality_score")[["team_display", "projected_seed", "AdjT", "AdjOE", "AdjDE", "Barthag"]]
    weakest = table.nsmallest(8, "tempo_quality_score")[["team_display", "projected_seed", "AdjT", "AdjOE", "AdjDE", "Barthag"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Tempo Analysis 7: 2026 Profile Map",
        "",
        "- The 2026 tempo package now includes four logo maps: AdjT x seed, AdjT x Barthag, AdjT x AdjOE, and AdjT x AdjDE.",
        f"- Gold-backed logos mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        "- Rose-backed circles mark historical 1-3 seeds that lost in the first round.",
        "",
        "## Strongest current-field pace-plus-quality cases",
        "",
        strongest.round(3).to_markdown(index=False),
        "",
        "## Weakest current-field pace-plus-quality cases",
        "",
        weakest.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def create_thread_assets(paths: dict[str, Path]) -> None:
    thread_dir = paths["thread_dir"]
    thread_dir.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(paths["fig_05"], thread_dir / "1_tempo_hook.png")


def main() -> int:
    paths = get_paths()
    full_df = load_tempo_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    table05 = build_analysis_05_table(tournament_df)
    table06 = build_analysis_06_table(over_df)
    table07, upset_history, overperformer_history = build_analysis_07_table(full_df, tournament_df, paths["seeds_2026"], paths["logo_cache"])
    validate_analysis_07_table(table07)

    table05.round(6).to_csv(paths["csv_05"], index=False)
    table06.round(6).to_csv(paths["csv_06"], index=False)
    with_repo_relative_logo_paths(table07).round(6).to_csv(paths["csv_07"], index=False)

    plot_analysis_05(table05, tournament_df, paths["fig_05"])
    plot_analysis_06(table06, paths["fig_06"])
    plot_analysis_07_metric(
        table07,
        upset_history,
        overperformer_history,
        "Barthag",
        "Projected 2026 tempo vs overall quality",
        "Barthag",
        "Right means faster. Up means stronger overall quality. Dotted lines mark tournament-field averages.",
        paths["fig_07_barthag"],
        y_axis_min=0.7,
        y_axis_max=1.0,
    )
    plot_analysis_07_seed_map(table07, upset_history, overperformer_history, paths["fig_07_seed"])
    plot_analysis_07_metric(
        table07,
        upset_history,
        overperformer_history,
        "AdjOE",
        "Projected 2026 tempo vs offense",
        "Adjusted offensive efficiency (AdjOE)",
        "Right means faster. Up means better offense. Dotted lines mark tournament-field averages.",
        paths["fig_07_adjoe"],
    )
    plot_analysis_07_metric(
        table07,
        upset_history,
        overperformer_history,
        "AdjDE",
        "Projected 2026 tempo vs defense",
        "Adjusted defensive efficiency (AdjDE, lower is better)",
        "Right means faster. Higher on the chart means better defense because the axis is inverted. Dotted lines mark tournament-field averages.",
        paths["fig_07_adjde"],
        invert_y=True,
    )

    write_summary_05(table05, paths["md_05"])
    write_summary_06(table06, paths["md_06"])
    write_summary_07(table07, paths["md_07"])
    create_thread_assets(paths)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
