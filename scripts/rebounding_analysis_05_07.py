#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import shutil

import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm
from matplotlib.lines import Line2D
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
import numpy as np
import pandas as pd

from size_analysis_common import cache_logo_images, load_logo_image
from rebounding_analysis_common import (
    ACCENT,
    CURRENT_FIELD_YEAR,
    ERAS,
    HIGHLIGHT,
    LABEL_TEXT,
    NEGATIVE,
    POSITIVE,
    PROJECT_PALETTE,
    SECONDARY,
    TEXT_DARK,
    apply_report_background,
    build_overperformance_data,
    build_seed_reference_tables,
    get_rebounding_base_paths,
    load_projected_field,
    load_rebounding_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    normalize_team_key,
    set_project_theme,
    styled_box,
)

OVERPERFORMER_BADGE_FACE = "#B6DCCB"
OVERPERFORMER_BADGE_EDGE = "#2F6B57"


def get_paths() -> dict[str, Path]:
    base = get_rebounding_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": root / "data" / "external" / "team_logos" / "espn_ncaam",
        "thread_dir": base["thread_dir"],
        "fig_05": root / "reports" / "figures" / "rebounding_archetypes_vs_overperformance.png",
        "fig_06": root / "reports" / "figures" / "rebounding_era_comparison.png",
        "fig_07": root / "reports" / "figures" / "rebounding_2026_profile_map.png",
        "csv_05": root / "reports" / "rebounding_archetypes_vs_overperformance_table.csv",
        "csv_06": root / "reports" / "rebounding_era_comparison_table.csv",
        "csv_07": root / "reports" / "rebounding_2026_field_table.csv",
        "md_05": root / "reports" / "rebounding_archetypes_vs_overperformance_summary.md",
        "md_06": root / "reports" / "rebounding_era_comparison_summary.md",
        "md_07": root / "reports" / "rebounding_2026_profile_map_summary.md",
    }


def build_analysis_05_table(over_df: pd.DataFrame) -> pd.DataFrame:
    plot_df = over_df.copy()
    plot_df["archetype"] = np.select(
        [
            (plot_df["orb_edge"] >= 0) & (plot_df["drb_edge"] >= 0),
            (plot_df["orb_edge"] >= 0) & (plot_df["drb_edge"] < 0),
            (plot_df["orb_edge"] < 0) & (plot_df["drb_edge"] >= 0),
            (plot_df["orb_edge"] < 0) & (plot_df["drb_edge"] < 0),
        ],
        [
            "strong on both ends",
            "offensive-glass only",
            "defensive-glass only",
            "below both baselines",
        ],
        default="mixed",
    )
    order = ["strong on both ends", "defensive-glass only", "offensive-glass only", "below both baselines"]
    rows = []
    for archetype in order:
        group = plot_df[plot_df["archetype"] == archetype].copy()
        rows.append(
            {
                "archetype": archetype,
                "team_count": int(len(group)),
                "mean_overperformance": float(group["overperformance"].mean()),
                "median_overperformance": float(group["overperformance"].median()),
                "avg_orb_edge": float(group["orb_edge"].mean()),
                "avg_drb_edge": float(group["drb_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


def build_analysis_05_heatmap(over_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    work = over_df.copy()
    orb_bins = pd.qcut(work["ORB"], q=4, duplicates="drop")
    drb_bins = pd.qcut(work["DRB"], q=4, duplicates="drop")
    work["orb_bin"] = orb_bins
    work["drb_bin"] = drb_bins

    grouped = (
        work.groupby(["drb_bin", "orb_bin"], observed=False)
        .agg(
            mean_overperformance=("overperformance", "mean"),
            team_count=("overperformance", "size"),
        )
        .reset_index()
    )

    def format_interval(interval: pd.Interval) -> str:
        return f"{float(interval.left):.1f}-{float(interval.right):.1f}"

    drb_order = sorted(grouped["drb_bin"].dropna().unique(), key=lambda interval: interval.left)
    orb_order = sorted(grouped["orb_bin"].dropna().unique(), key=lambda interval: interval.left)
    value_grid = grouped.pivot(index="drb_bin", columns="orb_bin", values="mean_overperformance").reindex(index=drb_order, columns=orb_order)
    count_grid = grouped.pivot(index="drb_bin", columns="orb_bin", values="team_count").reindex(index=drb_order, columns=orb_order)
    value_grid.index = [format_interval(interval) for interval in value_grid.index]
    value_grid.columns = [format_interval(interval) for interval in value_grid.columns]
    count_grid.index = value_grid.index
    count_grid.columns = value_grid.columns
    value_grid = value_grid.iloc[::-1]
    count_grid = count_grid.iloc[::-1]
    return value_grid, count_grid


def plot_analysis_05(table: pd.DataFrame, over_df: pd.DataFrame, out_path: Path) -> None:
    plot_df = table.copy().sort_values("mean_overperformance", ascending=False, kind="stable").reset_index(drop=True)
    y = np.arange(len(plot_df))
    values = plot_df["mean_overperformance"].to_numpy(dtype=float)
    colors = [POSITIVE if value > 0 else NEGATIVE for value in values]
    heatmap_values, heatmap_counts = build_analysis_05_heatmap(over_df)
    heatmap_array = heatmap_values.to_numpy(dtype=float)
    cmap = LinearSegmentedColormap.from_list(
        "rebounding_overperformance_heatmap",
        [NEGATIVE, PROJECT_PALETTE["fog"], POSITIVE],
    )
    norm = TwoSlopeNorm(
        vcenter=0.0,
        vmin=float(np.nanmin(heatmap_array)),
        vmax=float(np.nanmax(heatmap_array)),
    )

    set_project_theme()
    fig, (ax_rank, ax_heat) = plt.subplots(
        1,
        2,
        figsize=(19.0, 8.0),
        constrained_layout=False,
        gridspec_kw={"width_ratios": [1.0, 1.14]},
    )
    apply_report_background(fig, [ax_rank, ax_heat])
    fig.subplots_adjust(top=0.82, bottom=0.11, left=0.12, right=0.94, wspace=0.22)

    ax_rank.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
    ax_rank.hlines(y, 0, values, color=colors, linewidth=2.8)
    ax_rank.scatter(values, y, color=colors, s=96, zorder=3)
    for y_pos, value in zip(y, values):
        ax_rank.text(
            value + (0.010 if value >= 0 else -0.010),
            y_pos,
            f"{value:+.3f}",
            ha="left" if value >= 0 else "right",
            va="center",
            fontsize=9.5,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )
    ax_rank.set_yticks(y)
    ax_rank.set_yticklabels(plot_df["archetype"].tolist(), fontsize=10.5, fontweight="semibold")
    ax_rank.set_xlabel("Mean overperformance vs exact seed", fontsize=11.0, fontweight="semibold")
    ax_rank.set_title("Ranked archetype view", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax_rank.grid(False)
    ax_rank.spines["top"].set_visible(False)
    ax_rank.spines["right"].set_visible(False)
    ax_rank.tick_params(axis="x", length=0, pad=7)
    ax_rank.tick_params(axis="y", length=0, pad=8)

    mesh = ax_heat.imshow(heatmap_array, cmap=cmap, norm=norm, aspect="auto")
    ax_heat.set_xticks(np.arange(len(heatmap_values.columns)))
    ax_heat.set_xticklabels(heatmap_values.columns.tolist(), fontsize=9.2, fontweight="semibold")
    ax_heat.set_yticks(np.arange(len(heatmap_values.index)))
    ax_heat.set_yticklabels(heatmap_values.index.tolist(), fontsize=9.2, fontweight="semibold")
    ax_heat.set_xlabel("ORB", fontsize=11.0, fontweight="semibold")
    ax_heat.set_ylabel("DRB", fontsize=11.0, fontweight="semibold")
    ax_heat.set_title("Overperformance heatmap", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax_heat.set_xticks(np.arange(-0.5, heatmap_array.shape[1], 1), minor=True)
    ax_heat.set_yticks(np.arange(-0.5, heatmap_array.shape[0], 1), minor=True)
    ax_heat.grid(which="minor", color=PROJECT_PALETTE["midnight"], linewidth=1.35)
    ax_heat.tick_params(which="minor", bottom=False, left=False)
    ax_heat.tick_params(axis="x", length=0, pad=7)
    ax_heat.tick_params(axis="y", length=0, pad=7)
    for row_idx in range(heatmap_array.shape[0]):
        for col_idx in range(heatmap_array.shape[1]):
            value = heatmap_array[row_idx, col_idx]
            count = int(heatmap_counts.iloc[row_idx, col_idx])
            ax_heat.text(
                col_idx,
                row_idx,
                f"{value:+.3f}\n(n={count})",
                ha="center",
                va="center",
                fontsize=8.1,
                fontweight="bold",
                color=PROJECT_PALETTE["ink"],
                bbox=styled_box(
                    facecolor=PROJECT_PALETTE["paper"],
                    edgecolor=PROJECT_PALETTE["steel"],
                    linewidth=1.3,
                    alpha=0.96,
                ),
            )
    cbar = fig.colorbar(mesh, ax=ax_heat, fraction=0.045, pad=0.03)
    cbar.set_label("Mean overperformance vs exact seed", fontsize=10.0, fontweight="semibold", color=TEXT_DARK)
    cbar.ax.tick_params(labelsize=9.0, colors=TEXT_DARK, length=0)

    fig.suptitle("Glass-control archetypes versus overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Left: ranked rebounding archetypes. Right: binned ORB x DRB cells colored by average seed-adjusted overperformance.",
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
    for metric in ["ORB", "DRB"]:
        slug = metric_slug(metric)
        all_group[f"{slug}_seed_adjusted_rho"] = float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman"))
    rows.append(all_group)
    for label, start, end in ERAS:
        group = over_df[over_df["year"].between(start, end)].copy()
        row = {"era": label, "team_count": int(len(group))}
        for metric in ["ORB", "DRB"]:
            slug = metric_slug(metric)
            row[f"{slug}_seed_adjusted_rho"] = float(group["overperformance"].corr(group[f"{slug}_edge"], method="spearman"))
        rows.append(row)
    return pd.DataFrame(rows)


def plot_analysis_06(table: pd.DataFrame, out_path: Path) -> None:
    specs = [
        ("orb_seed_adjusted_rho", "Offensive rebounding rate", ACCENT),
        ("drb_seed_adjusted_rho", "Defensive rebounding rate", HIGHLIGHT),
    ]
    set_project_theme()
    fig, axes = plt.subplots(1, 2, figsize=(15.6, 7.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.07, right=0.98, wspace=0.14)

    eras = table["era"].tolist()
    for ax, (column, title, color) in zip(axes, specs):
        colors = [color] + [HIGHLIGHT if "2022" in era else PROJECT_PALETTE["steel"] for era in eras[1:]]
        ax.bar(np.arange(len(table)), table[column], color=colors, width=0.62)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xticks(np.arange(len(table)))
        ax.set_xticklabels(eras, fontsize=10.0, fontweight="semibold")
        ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)

    fig.suptitle("Did the modern era change the rebounding read?", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Each panel keeps one side of the glass isolated so the era comparison stays readable.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_table(
    full_df: pd.DataFrame,
    tournament_df: pd.DataFrame,
    seeds_path: Path,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df)
    full_ref, recent_ref = build_seed_reference_tables(tournament_df, ["ORB", "DRB"])
    merged = field.merge(full_ref, on="projected_seed", how="left").merge(recent_ref, on="projected_seed", how="left")
    for metric in ["ORB", "DRB"]:
        slug = metric_slug(metric)
        merged[f"full_{slug}_delta_raw"] = merged[metric] - merged[f"full_seed_avg_{slug}"]
        merged[f"full_{slug}_edge"] = merged[f"full_{slug}_delta_raw"]
    merged["tournament_orb_avg"] = float(tournament_df["ORB"].mean())
    merged["tournament_drb_avg"] = float(tournament_df["DRB"].mean())
    merged["glass_control_score"] = 0.55 * merged["full_drb_edge"] + 0.45 * merged["full_orb_edge"]
    merged["team_key"] = merged["team_display"].map(normalize_team_key)
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)

    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", ["Team", "year", "seed_int", "ORB", "DRB"]]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )
    champion_rows = champion_rows.merge(
        full_ref.rename(columns={"projected_seed": "seed_int"}),
        on="seed_int",
        how="left",
    )
    for metric in ["ORB", "DRB"]:
        slug = metric_slug(metric)
        champion_rows[f"title_{slug}_edge"] = champion_rows[metric] - champion_rows[f"full_seed_avg_{slug}"]

    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_pairs = [
            {
                "year": int(row["year"]),
                "seed": int(row["seed_int"]),
                "orb": float(row["ORB"]),
                "drb": float(row["DRB"]),
                "orb_edge": float(row["title_orb_edge"]),
                "drb_edge": float(row["title_drb_edge"]),
            }
            for _, row in group.iterrows()
        ]
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_pairs)),
                "title_years": [item["year"] for item in title_pairs],
                "title_seeds": [item["seed"] for item in title_pairs],
                "title_orb_values": [item["orb"] for item in title_pairs],
                "title_drb_values": [item["drb"] for item in title_pairs],
                "title_orb_edge_values": [item["orb_edge"] for item in title_pairs],
                "title_drb_edge_values": [item["drb_edge"] for item in title_pairs],
                "title_history": "; ".join([f"{item['year']} ({item['seed']})" for item in title_pairs]),
            }
        )
    champion_history = pd.DataFrame(champion_history_rows)
    if not champion_history.empty:
        merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged.get("championship_count", 0).fillna(0).astype(int)
    merged["title_years"] = merged.get("title_years", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged.get("title_seeds", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_orb_values"] = merged.get("title_orb_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_drb_values"] = merged.get("title_drb_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_orb_edge_values"] = merged.get("title_orb_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_drb_edge_values"] = merged.get("title_drb_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged.get("title_history", "").fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    return merged.sort_values(["projected_seed", "glass_control_score"], ascending=[True, False], kind="stable").reset_index(drop=True)


def build_analysis_07_overperformer_history(tournament_df: pd.DataFrame, logo_cache_dir: Path) -> pd.DataFrame:
    over_df = build_overperformance_data(tournament_df)
    history = (
        over_df.loc[
            over_df["overperformance"] >= 3,
            ["Team", "year", "seed_int", "ORB", "DRB", "overperformance"],
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


def validate_analysis_07_table(table: pd.DataFrame) -> None:
    champion_rows = table.loc[table["is_former_champion"]].copy()
    for row in champion_rows.itertuples(index=False):
        title_years = list(getattr(row, "title_years", []))
        if not title_years:
            raise ValueError(f"Former champion {row.team_display} is missing title years for badge plotting")
        expected = len(title_years)
        for column in ["title_seeds", "title_orb_values", "title_drb_values"]:
            values = list(getattr(row, column, []))
            if len(values) != expected:
                raise ValueError(f"Former champion {row.team_display} has mismatched values in {column}")


def plot_analysis_07(table: pd.DataFrame, overperformer_history: pd.DataFrame, out_path: Path) -> None:
    plot_df = table.copy()
    x_values = plot_df["ORB"].to_numpy(dtype=float)
    y_values = plot_df["DRB"].to_numpy(dtype=float)
    if not overperformer_history.empty:
        x_values = np.concatenate([x_values, overperformer_history["ORB"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, overperformer_history["DRB"].to_numpy(dtype=float)])
    x_pad = max((float(np.nanmax(x_values)) - float(np.nanmin(x_values))) * 0.14, 0.8)
    y_pad = max((float(np.nanmax(y_values)) - float(np.nanmin(y_values))) * 0.14, 0.8)
    orb_avg = float(plot_df["tournament_orb_avg"].iloc[0])
    drb_avg = float(plot_df["tournament_drb_avg"].iloc[0])

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.6, 11.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.86, bottom=0.11, left=0.08, right=0.96)

    ax.axhline(drb_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)
    ax.axvline(orb_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)

    quadrant_positions = [
        (0.22, 0.90, "Below avg ORB\nAbove avg DRB"),
        (0.78, 0.90, "Above avg ORB\nAbove avg DRB"),
        (0.22, 0.10, "Below avg ORB\nBelow avg DRB"),
        (0.78, 0.10, "Above avg ORB\nBelow avg DRB"),
    ]
    for x_pos, y_pos, label in quadrant_positions:
        ax.text(
            x_pos,
            y_pos,
            label,
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=11.0,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(alpha=0.90),
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(row.ORB)
        y_value = float(row.DRB)
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if bool(getattr(row, "is_former_champion", False)):
                point_counts: dict[tuple[float, float], int] = {}
                for title_x, title_y in zip(row.title_orb_values, row.title_drb_values):
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
                (x_value, y_value),
                frameon=True,
                bboxprops={
                    "boxstyle": "round,pad=0.18",
                    "facecolor": "none",
                    "edgecolor": PROJECT_PALETTE["steel"],
                    "linewidth": 1.8,
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
                bbox=styled_box(),
                zorder=4,
            )

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["ORB"])
            y_value = float(row["DRB"])
            ax.scatter(
                [x_value],
                [y_value],
                s=980,
                marker="^",
                facecolor=OVERPERFORMER_BADGE_FACE,
                edgecolor=OVERPERFORMER_BADGE_EDGE,
                linewidth=1.25,
                zorder=1.8,
            )
            ax.add_artist(
                AnnotationBbox(
                    OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.058),
                    (x_value, y_value),
                    frameon=False,
                    zorder=2.2,
                )
            )

    legend_handles = [
        Line2D([], [], color=SECONDARY, linestyle=":", linewidth=1.5, label="Tournament-field average"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=PROJECT_PALETTE["steel"], markeredgewidth=2.0, markersize=10, label="Projected 2026 team"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge"),
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
    ax.set_ylim(float(np.nanmin(y_values)) - y_pad, float(np.nanmax(y_values)) + y_pad)
    ax.set_xlabel("Offensive rebounding rate (ORB)", fontsize=12, fontweight="semibold")
    ax.set_ylabel("Defensive rebounding rate (DRB)", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", labelsize=11, length=0, pad=7)

    fig.suptitle("Projected 2026 rebounding profiles", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        "Right means a higher offensive rebounding rate. Up means a higher defensive rebounding rate. Dotted lines mark full-sample tournament averages.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def write_summary_05(table: pd.DataFrame, out_path: Path) -> None:
    best = table.sort_values("mean_overperformance", ascending=False, kind="stable").iloc[0]
    lines = [
        "# Rebounding Analysis 5: Glass Archetypes",
        "",
        f"- Best archetype: `{best['archetype']}` at `{best['mean_overperformance']:+.3f}` overperformance points.",
        "- Day 6 should treat offensive and defensive rebounding as separate inputs first, then combine them in the archetype read.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_06(table: pd.DataFrame, out_path: Path) -> None:
    modern = table.loc[table["era"] == "2022-2025"].iloc[0]
    full = table.loc[table["era"] == "2008-2025"].iloc[0]
    lines = [
        "# Rebounding Analysis 6: Era Comparison",
        "",
        f"- Full-sample ORB rho: `{full['orb_seed_adjusted_rho']:+.3f}`. Modern-era ORB rho: `{modern['orb_seed_adjusted_rho']:+.3f}`.",
        f"- Full-sample DRB rho: `{full['drb_seed_adjusted_rho']:+.3f}`. Modern-era DRB rho: `{modern['drb_seed_adjusted_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_07(table: pd.DataFrame, out_path: Path) -> None:
    best = table.nlargest(8, "glass_control_score")[["team_display", "projected_seed", "ORB", "DRB"]]
    worst = table.nsmallest(8, "glass_control_score")[["team_display", "projected_seed", "ORB", "DRB"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Rebounding Analysis 7: 2026 Profile Map",
        "",
        "- This chart uses raw offensive and defensive rebounding rates on the axes so the 2026 field can be read in one glance.",
        "- Dotted guide lines mark the full-sample tournament averages for ORB and DRB.",
        "- Best and weak cases below are still ranked by the combined seed-relative glass-control score, but the table shows the actual rebounding rates used on the chart.",
        f"- Gold-backed logos mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        "",
        "## Best current-field cases",
        "",
        best.round(3).to_markdown(index=False),
        "",
        "## Weak current-field cases",
        "",
        worst.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def create_thread_assets(paths: dict[str, Path]) -> None:
    thread_dir = paths["thread_dir"]
    thread_dir.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(paths["fig_05"], thread_dir / "1_rebounding_hook.png")


def main() -> int:
    paths = get_paths()
    full_df = load_rebounding_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    table05 = build_analysis_05_table(over_df)
    table06 = build_analysis_06_table(over_df)
    table07 = build_analysis_07_table(full_df, tournament_df, paths["seeds_2026"], paths["logo_cache"])
    overperformer_history = build_analysis_07_overperformer_history(tournament_df, paths["logo_cache"])
    validate_analysis_07_table(table07)

    table05.round(6).to_csv(paths["csv_05"], index=False)
    table06.round(6).to_csv(paths["csv_06"], index=False)
    table07.round(6).to_csv(paths["csv_07"], index=False)

    plot_analysis_05(table05, over_df, paths["fig_05"])
    plot_analysis_06(table06, paths["fig_06"])
    plot_analysis_07(table07, overperformer_history, paths["fig_07"])

    write_summary_05(table05, paths["md_05"])
    write_summary_06(table06, paths["md_06"])
    write_summary_07(table07, paths["md_07"])
    create_thread_assets(paths)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
