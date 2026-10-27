#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap, TwoSlopeNorm
from matplotlib.lines import Line2D
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
import numpy as np
import pandas as pd

from size_analysis_common import cache_logo_images, load_logo_image, repo_relative_logo_paths
from free_throw_rate_analysis_common import (
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
    get_free_throw_rate_base_paths,
    load_free_throw_rate_data,
    load_projected_field,
    load_tournament_data,
    metric_label,
    metric_slug,
    normalize_team_key,
    set_project_theme,
    styled_box,
)

OVERPERFORMER_BADGE_FACE = "#B6DCCB"
OVERPERFORMER_BADGE_EDGE = "#2F6B57"
UPSET_BADGE_FACE = "#F6D7D4"
UPSET_BADGE_EDGE = "#B55C5A"


def get_paths() -> dict[str, Path]:
    base = get_free_throw_rate_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": root / "data" / "external" / "team_logos" / "espn_ncaam",
        "fig_05": root / "reports" / "figures" / "free_throw_rate_archetypes_vs_overperformance.png",
        "fig_06": root / "reports" / "figures" / "free_throw_rate_era_comparison.png",
        "fig_07": root / "reports" / "figures" / "free_throw_rate_2026_profile_map.png",
        "csv_05": root / "reports" / "free_throw_rate_archetypes_vs_overperformance_table.csv",
        "csv_06": root / "reports" / "free_throw_rate_era_comparison_table.csv",
        "csv_07": root / "reports" / "free_throw_rate_2026_field_table.csv",
        "md_05": root / "reports" / "free_throw_rate_archetypes_vs_overperformance_summary.md",
        "md_06": root / "reports" / "free_throw_rate_era_comparison_summary.md",
        "md_07": root / "reports" / "free_throw_rate_2026_profile_map_summary.md",
    }


def build_analysis_05_table(over_df: pd.DataFrame) -> pd.DataFrame:
    plot_df = over_df.copy()
    plot_df["archetype"] = np.select(
        [
            (plot_df["ftr_edge"] >= 0) & (plot_df["ft_pct_edge"] >= 0),
            (plot_df["ftr_edge"] >= 0) & (plot_df["ft_pct_edge"] < 0),
            (plot_df["ftr_edge"] < 0) & (plot_df["ft_pct_edge"] >= 0),
            (plot_df["ftr_edge"] < 0) & (plot_df["ft_pct_edge"] < 0),
        ],
        [
            "high rate + high make%",
            "high rate + low make%",
            "low rate + high make%",
            "low rate + low make%",
        ],
        default="mixed",
    )
    order = [
        "high rate + high make%",
        "low rate + high make%",
        "high rate + low make%",
        "low rate + low make%",
    ]
    rows = []
    for archetype in order:
        group = plot_df[plot_df["archetype"] == archetype].copy()
        rows.append(
            {
                "archetype": archetype,
                "team_count": int(len(group)),
                "mean_overperformance": float(group["overperformance"].mean()),
                "median_overperformance": float(group["overperformance"].median()),
                "avg_ftr_edge": float(group["ftr_edge"].mean()),
                "avg_ft_pct_edge": float(group["ft_pct_edge"].mean()),
                "avg_ftrd_edge": float(group["ftrd_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


def build_analysis_05_heatmap(over_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    work = over_df.copy()
    ftr_bins = pd.qcut(work["FTR"], q=4, duplicates="drop")
    ft_bins = pd.qcut(work["FT%"], q=4, duplicates="drop")
    work["ftr_bin"] = ftr_bins
    work["ft_bin"] = ft_bins

    grouped = (
        work.groupby(["ft_bin", "ftr_bin"], observed=False)
        .agg(mean_overperformance=("overperformance", "mean"), team_count=("overperformance", "size"))
        .reset_index()
    )

    def format_interval(interval: pd.Interval) -> str:
        return f"{float(interval.left):.1f}-{float(interval.right):.1f}"

    ft_order = sorted(grouped["ft_bin"].dropna().unique(), key=lambda interval: interval.left)
    ftr_order = sorted(grouped["ftr_bin"].dropna().unique(), key=lambda interval: interval.left)
    value_grid = grouped.pivot(index="ft_bin", columns="ftr_bin", values="mean_overperformance").reindex(index=ft_order, columns=ftr_order)
    count_grid = grouped.pivot(index="ft_bin", columns="ftr_bin", values="team_count").reindex(index=ft_order, columns=ftr_order)
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
    cmap = LinearSegmentedColormap.from_list("free_throw_overperformance_heatmap", [NEGATIVE, PROJECT_PALETTE["fog"], POSITIVE])
    norm = TwoSlopeNorm(vcenter=0.0, vmin=float(np.nanmin(heatmap_array)), vmax=float(np.nanmax(heatmap_array)))

    set_project_theme()
    fig, (ax_rank, ax_heat) = plt.subplots(
        1, 2, figsize=(19.0, 8.0), constrained_layout=False, gridspec_kw={"width_ratios": [1.0, 1.14]}
    )
    apply_report_background(fig, [ax_rank, ax_heat])
    fig.subplots_adjust(top=0.82, bottom=0.11, left=0.12, right=0.95, wspace=0.22)

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
    ax_rank.invert_yaxis()
    ax_rank.set_xlabel("Mean overperformance vs exact seed", fontsize=11.0, fontweight="semibold")
    ax_rank.set_title("Rate x make% archetypes", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
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
    ax_heat.set_xlabel("FTR", fontsize=11.0, fontweight="semibold")
    ax_heat.set_ylabel("FT%", fontsize=11.0, fontweight="semibold")
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
                bbox=styled_box(facecolor=PROJECT_PALETTE["paper"], edgecolor=PROJECT_PALETTE["steel"], linewidth=1.3, alpha=0.96),
            )
    cbar = fig.colorbar(mesh, ax=ax_heat, fraction=0.045, pad=0.03)
    cbar.set_label("Mean overperformance vs exact seed", fontsize=10.0, fontweight="semibold", color=TEXT_DARK)
    cbar.ax.tick_params(labelsize=9.0, colors=TEXT_DARK, length=0)

    fig.suptitle("Free throw dependence versus overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Left: ranked FTR x FT% archetypes. Right: binned rate-and-make cells colored by average seed-adjusted overperformance.",
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
    metrics = ["FTR", "FT%", "FTRD"]
    all_group = {"era": "2008-2025", "team_count": int(len(over_df))}
    for metric in metrics:
        slug = metric_slug(metric)
        all_group[f"{slug}_rho"] = float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman"))
    rows.append(all_group)
    for label, start, end in ERAS:
        group = over_df[over_df["year"].between(start, end)].copy()
        row = {"era": label, "team_count": int(len(group))}
        for metric in metrics:
            slug = metric_slug(metric)
            row[f"{slug}_rho"] = float(group["overperformance"].corr(group[f"{slug}_edge"], method="spearman"))
        rows.append(row)
    return pd.DataFrame(rows)


def plot_analysis_06(table: pd.DataFrame, out_path: Path) -> None:
    specs = [
        ("ftr_rho", "Free throw rate", ACCENT),
        ("ft_pct_rho", "Free throw percentage", HIGHLIGHT),
        ("ftrd_rho", "Opponent free throw rate allowed", PROJECT_PALETTE["lake_blue"]),
    ]
    set_project_theme()
    fig, axes = plt.subplots(1, 3, figsize=(19.0, 7.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.05, right=0.99, wspace=0.14)

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

    fig.suptitle("Free-throw metrics vs seed-adjusted finish by era", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Each panel isolates one free throw metric so line reliance, make%, and foul frequency can be read separately across eras.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_table(full_df: pd.DataFrame, tournament_df: pd.DataFrame, seeds_path: Path, logo_cache_dir: Path) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df)
    full_ref, _ = build_seed_reference_tables(tournament_df, ["FTR", "FT%", "FTRD"])
    merged = field.merge(full_ref, on="projected_seed", how="left")
    for metric in ["FTR", "FT%", "FTRD"]:
        slug = metric_slug(metric)
        merged[f"full_{slug}_delta_raw"] = merged[metric] - merged[f"full_seed_avg_{slug}"]
        merged[f"full_{slug}_edge"] = merged[f"full_{slug}_delta_raw"]
    merged["whistle_profile_score"] = 0.45 * merged["full_ftr_edge"] + 0.35 * merged["full_ft_pct_edge"] + 0.20 * merged["full_ftrd_edge"]
    merged["tournament_ftr_avg"] = float(tournament_df["FTR"].mean())
    merged["tournament_ft_pct_avg"] = float(tournament_df["FT%"].mean())
    merged["team_key"] = merged["team_display"].map(normalize_team_key)
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)

    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", ["Team", "year", "seed_int", "FTR", "FT%"]]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )

    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_pairs = [
            {"year": int(row["year"]), "seed": int(row["seed_int"]), "ftr": float(row["FTR"]), "ft_pct": float(row["FT%"])}
            for _, row in group.iterrows()
        ]
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_pairs)),
                "title_years": [item["year"] for item in title_pairs],
                "title_seeds": [item["seed"] for item in title_pairs],
                "title_ftr_values": [item["ftr"] for item in title_pairs],
                "title_ft_pct_values": [item["ft_pct"] for item in title_pairs],
                "title_history": "; ".join([f"{item['year']} ({item['seed']})" for item in title_pairs]),
            }
        )

    champion_history = pd.DataFrame(champion_history_rows)
    if not champion_history.empty:
        merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged.get("championship_count", 0).fillna(0).astype(int)
    merged["title_years"] = merged.get("title_years", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged.get("title_seeds", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_ftr_values"] = merged.get("title_ftr_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_ft_pct_values"] = merged.get("title_ft_pct_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged.get("title_history", "").fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    return merged.sort_values(["projected_seed", "whistle_profile_score"], ascending=[True, False], kind="stable").reset_index(drop=True)


def build_analysis_07_overperformer_history(tournament_df: pd.DataFrame, logo_cache_dir: Path) -> pd.DataFrame:
    over_df = build_overperformance_data(tournament_df)
    history = (
        over_df.loc[
            over_df["overperformance"] >= 3,
            ["Team", "year", "seed_int", "FTR", "FT%", "FTRD", "overperformance"],
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


def build_analysis_07_upset_history(
    tournament_df: pd.DataFrame,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    history = (
        tournament_df.loc[
            tournament_df["seed_int"].isin([1, 2, 3]) & tournament_df["finish"].eq("R64"),
            ["Team", "year", "seed_int", "FTR", "FT%", "FTRD"],
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
        for column in ["title_seeds", "title_ftr_values", "title_ft_pct_values"]:
            values = list(getattr(row, column, []))
            if len(values) != expected:
                raise ValueError(f"Former champion {row.team_display} has mismatched values in {column}")


def plot_analysis_07(
    table: pd.DataFrame,
    overperformer_history: pd.DataFrame,
    upset_history: pd.DataFrame,
    out_path: Path,
) -> None:
    plot_df = table.copy()
    x_values = plot_df["FTR"].to_numpy(dtype=float)
    y_values = plot_df["FT%"].to_numpy(dtype=float)
    if not overperformer_history.empty:
        x_values = np.concatenate([x_values, overperformer_history["FTR"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, overperformer_history["FT%"].to_numpy(dtype=float)])
    if not upset_history.empty:
        x_values = np.concatenate([x_values, upset_history["FTR"].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, upset_history["FT%"].to_numpy(dtype=float)])
    x_pad = max((float(np.nanmax(x_values)) - float(np.nanmin(x_values))) * 0.14, 0.8)
    y_pad = max((float(np.nanmax(y_values)) - float(np.nanmin(y_values))) * 0.14, 0.8)
    ftr_avg = float(plot_df["tournament_ftr_avg"].iloc[0])
    ft_avg = float(plot_df["tournament_ft_pct_avg"].iloc[0])

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.6, 11.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.86, bottom=0.11, left=0.08, right=0.96)

    ax.axhline(ft_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)
    ax.axvline(ftr_avg, color=SECONDARY, linestyle=":", linewidth=1.5, zorder=0)

    quadrant_positions = [
        (0.22, 0.90, "Lower FTR\nHigher FT%"),
        (0.78, 0.90, "Higher FTR\nHigher FT%"),
        (0.22, 0.10, "Lower FTR\nLower FT%"),
        (0.78, 0.10, "Higher FTR\nLower FT%"),
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

    for _, row in plot_df.iterrows():
        x_value = float(row["FTR"])
        y_value = float(row["FT%"])
        logo_path = row.get("logo_path")
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if bool(row.get("is_former_champion", False)):
                point_counts: dict[tuple[float, float], int] = {}
                for title_x, title_y in zip(row["title_ftr_values"], row["title_ft_pct_values"]):
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
                frameon=False,
                zorder=3,
            )
            ax.add_artist(artist)

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["FTR"])
            y_value = float(row["FT%"])
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

    if not upset_history.empty:
        for _, row in upset_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["FTR"])
            y_value = float(row["FT%"])
            ax.add_artist(
                AnnotationBbox(
                    OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.056),
                    (x_value, y_value),
                    frameon=True,
                    bboxprops={
                        "boxstyle": "circle,pad=0.30",
                        "facecolor": UPSET_BADGE_FACE,
                        "edgecolor": UPSET_BADGE_EDGE,
                        "linewidth": 1.25,
                        "alpha": 0.96,
                    },
                    zorder=2.15,
                )
            )

    legend_handles = [
        Line2D([], [], color=SECONDARY, linestyle=":", linewidth=1.5, label="Tournament-field average"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor="none", markersize=10, label="Projected 2026 team"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge"),
        Line2D([], [], marker="^", linestyle="None", markerfacecolor=OVERPERFORMER_BADGE_FACE, markeredgecolor=OVERPERFORMER_BADGE_EDGE, markeredgewidth=1.3, markersize=15, label="Historical 3+ round overperformer"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor=UPSET_BADGE_FACE, markeredgecolor=UPSET_BADGE_EDGE, markeredgewidth=1.3, markersize=10, label="Historical 1-3 seed R64 upset"),
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
    ax.set_xlabel("Free throw rate (FTR)", fontsize=12, fontweight="semibold")
    ax.set_ylabel("Free throw percentage (FT%)", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", labelsize=11, length=0, pad=7)

    fig.suptitle("Projected 2026 free throw profiles", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        "Right means more line reliance. Up means better free throw shooting. Dotted lines mark full-sample tournament averages.",
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
        "# Free Throw Analysis 5: Rate x Make% Archetypes",
        "",
        f"- Best archetype: `{best['archetype']}` at `{best['mean_overperformance']:+.3f}` overperformance points.",
        "- Getting to the line (FTR) and making free throws (FT%) are treated as separate inputs until the interaction view.",
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
        "# Free Throw Analysis 6: Era Comparison",
        "",
        f"- Full-sample FTR rho: `{full['ftr_rho']:+.3f}`. Modern-era FTR rho: `{modern['ftr_rho']:+.3f}`.",
        f"- Full-sample FT% rho: `{full['ft_pct_rho']:+.3f}`. Modern-era FT% rho: `{modern['ft_pct_rho']:+.3f}`.",
        f"- Full-sample FTRD rho: `{full['ftrd_rho']:+.3f}`. Modern-era FTRD rho: `{modern['ftrd_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_07(table: pd.DataFrame, out_path: Path) -> None:
    best = table.nlargest(8, "whistle_profile_score")[["team_display", "projected_seed", "FTR", "FT%", "FTRD"]]
    worst = table.nsmallest(8, "whistle_profile_score")[["team_display", "projected_seed", "FTR", "FT%", "FTRD"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Free Throw Analysis 7: 2026 Profile Map",
        "",
        "- This chart uses raw FTR and FT% on the axes so 2026 line reliance and line conversion can be read in one glance.",
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


def main() -> int:
    paths = get_paths()
    full_df = load_free_throw_rate_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    table05 = build_analysis_05_table(over_df)
    table06 = build_analysis_06_table(over_df)
    table07 = build_analysis_07_table(full_df, tournament_df, paths["seeds_2026"], paths["logo_cache"])
    overperformer_history = build_analysis_07_overperformer_history(tournament_df, paths["logo_cache"])
    upset_history = build_analysis_07_upset_history(tournament_df, paths["logo_cache"])
    validate_analysis_07_table(table07)

    table05.round(6).to_csv(paths["csv_05"], index=False)
    table06.round(6).to_csv(paths["csv_06"], index=False)
    repo_relative_logo_paths(table07).round(6).to_csv(paths["csv_07"], index=False)

    plot_analysis_05(table05, over_df, paths["fig_05"])
    plot_analysis_06(table06, paths["fig_06"])
    plot_analysis_07(table07, overperformer_history, upset_history, paths["fig_07"])

    write_summary_05(table05, paths["md_05"])
    write_summary_06(table06, paths["md_06"])
    write_summary_07(table07, paths["md_07"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
