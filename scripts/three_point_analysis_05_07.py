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
from three_point_analysis_common import (
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
    get_three_point_base_paths,
    load_projected_field,
    load_three_point_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    normalize_team_key,
    LOWER_BETTER,
    set_project_theme,
    styled_box,
)

UPSET_BADGE_FACE = "#F6D7D4"
UPSET_BADGE_EDGE = "#B55C5A"
OVERPERFORMER_BADGE_FACE = "#B6DCCB"
OVERPERFORMER_BADGE_EDGE = "#2F6B57"


def get_paths() -> dict[str, Path]:
    base = get_three_point_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": Path(__file__).resolve().parent.parent / "data" / "external" / "team_logos" / "espn_ncaam",
        "thread_dir": base["thread_dir"],
        "fig_05": root / "reports" / "figures" / "three_point_rate_success_interaction.png",
        "fig_07": root / "reports" / "figures" / "three_point_2026_profile_map.png",
        "fig_07_defense": root / "reports" / "figures" / "three_point_2026_defense_field.png",
        "csv_05": root / "reports" / "three_point_rate_success_interaction_table.csv",
        "csv_06": root / "reports" / "three_point_era_comparison_table.csv",
        "csv_07": root / "reports" / "three_point_2026_field_table.csv",
        "md_05": root / "reports" / "three_point_rate_success_interaction_summary.md",
        "md_06": root / "reports" / "three_point_era_comparison_summary.md",
        "md_07": root / "reports" / "three_point_2026_profile_map_summary.md",
        "md_07_defense": root / "reports" / "three_point_2026_defense_field_summary.md",
    }


def build_analysis_05_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    order = [
        ("high success", "high volume"),
        ("high success", "low volume"),
        ("low success", "high volume"),
        ("low success", "low volume"),
    ]
    for success_bucket, volume_bucket in order:
        group = over_df[
            (over_df["success_bucket"] == success_bucket)
            & (over_df["volume_bucket"] == volume_bucket)
        ].copy()
        rows.append(
            {
                "success_bucket": success_bucket,
                "volume_bucket": volume_bucket,
                "profile": f"{success_bucket} + {volume_bucket}",
                "team_count": int(len(group)),
                "mean_overperformance": float(group["overperformance"].mean()),
                "median_overperformance": float(group["overperformance"].median()),
                "avg_three_prate_edge": float(group["three_prate_edge"].mean()),
                "avg_three_pct_edge": float(group["three_pct_edge"].mean()),
                "avg_three_pct_d_edge": float(group["three_pct_d_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


def build_analysis_05_heatmap(over_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    work = over_df.copy()
    prate_bins = pd.qcut(work["3PRATE"], q=4, duplicates="drop")
    pct_bins = pd.qcut(work["3P%"], q=4, duplicates="drop")
    work["three_prate_bin"] = prate_bins
    work["three_pct_bin"] = pct_bins

    grouped = (
        work.groupby(["three_pct_bin", "three_prate_bin"], observed=False)
        .agg(
            mean_overperformance=("overperformance", "mean"),
            team_count=("overperformance", "size"),
        )
        .reset_index()
    )

    def format_interval(interval: pd.Interval) -> str:
        left = float(interval.left)
        right = float(interval.right)
        return f"{left:.1f}-{right:.1f}"

    pct_order = sorted(grouped["three_pct_bin"].dropna().unique(), key=lambda interval: interval.left)
    prate_order = sorted(grouped["three_prate_bin"].dropna().unique(), key=lambda interval: interval.left)
    value_grid = grouped.pivot(index="three_pct_bin", columns="three_prate_bin", values="mean_overperformance").reindex(index=pct_order, columns=prate_order)
    count_grid = grouped.pivot(index="three_pct_bin", columns="three_prate_bin", values="team_count").reindex(index=pct_order, columns=prate_order)
    value_grid.index = [format_interval(interval) for interval in value_grid.index]
    value_grid.columns = [format_interval(interval) for interval in value_grid.columns]
    count_grid.index = value_grid.index
    count_grid.columns = value_grid.columns
    value_grid = value_grid.iloc[::-1]
    count_grid = count_grid.iloc[::-1]
    return value_grid, count_grid


def plot_analysis_05(table: pd.DataFrame, over_df: pd.DataFrame, out_path: Path) -> None:
    label_map = {
        "high success + high volume": "High volume, high success",
        "high success + low volume": "Low volume, high success",
        "low success + high volume": "High volume, low success",
        "low success + low volume": "Low volume, low success",
    }
    recent_start = 2019
    recent_over_df = over_df[over_df["year"] >= recent_start].copy()
    recent_table = build_analysis_05_table(recent_over_df)
    display = recent_table.copy()
    display["profile_label"] = display["profile"].map(label_map)
    display = display.sort_values("mean_overperformance", ascending=False, kind="stable").reset_index(drop=True)

    y = np.arange(len(display))
    values = display["mean_overperformance"].to_numpy(dtype=float)
    colors = [POSITIVE if value > 0 else NEGATIVE for value in values]
    heatmap_values, heatmap_counts = build_analysis_05_heatmap(recent_over_df)
    heatmap_array = heatmap_values.to_numpy(dtype=float)
    cmap = LinearSegmentedColormap.from_list(
        "three_point_overperformance_heatmap",
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
        figsize=(16.2, 10.2),
        constrained_layout=False,
        gridspec_kw={"width_ratios": [1.0, 1.0]},
    )
    apply_report_background(fig, [ax_rank, ax_heat])
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.10, right=0.94, wspace=0.18)

    ax_rank.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
    ax_rank.hlines(y, 0, values, color=colors, linewidth=3.1)
    ax_rank.scatter(values, y, color=colors, s=112, zorder=3)

    for y_pos, value, team_count in zip(y, values, display["team_count"].to_numpy(dtype=int)):
        offset = 0.004 if value >= 0 else -0.004
        ax_rank.text(
            value + offset,
            y_pos,
            f"{value:+.3f}\n(n={team_count})",
            ha="left" if value >= 0 else "right",
            va="center",
            fontsize=9.8,
            fontweight="bold",
            color=PROJECT_PALETTE["deep_ocean"],
            bbox=styled_box(facecolor="white", edgecolor=PROJECT_PALETTE["sky_blue"], alpha=0.9),
        )

    ax_rank.set_yticks(y)
    ax_rank.set_yticklabels(display["profile_label"].tolist(), fontsize=11.2, fontweight="semibold")
    ax_rank.invert_yaxis()
    ax_rank.set_xlabel("Mean overperformance vs exact seed", fontsize=11.2, fontweight="semibold")
    ax_rank.set_ylabel("Three-point profile", fontsize=11.2, fontweight="semibold")
    x_min = float(np.nanmin(values))
    x_max = float(np.nanmax(values))
    ax_rank.set_xlim(x_min - 0.018, x_max + 0.014)
    ax_rank.set_title("Ranked profile view", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax_rank.set_box_aspect(1)
    ax_rank.grid(False)
    ax_rank.spines["top"].set_visible(False)
    ax_rank.spines["right"].set_visible(False)
    ax_rank.tick_params(axis="x", length=0, pad=7)
    ax_rank.tick_params(axis="y", length=0, pad=8)
    ax_rank.text(
        0.03,
        0.95,
        f"{recent_start}+ only\n(n={len(recent_over_df)})",
        transform=ax_rank.transAxes,
        ha="left",
        va="top",
        fontsize=10.0,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    mesh = ax_heat.imshow(heatmap_array, cmap=cmap, norm=norm, aspect="auto")
    ax_heat.set_xticks(np.arange(len(heatmap_values.columns)))
    ax_heat.set_xticklabels(heatmap_values.columns.tolist(), fontsize=9.2, fontweight="semibold")
    ax_heat.set_yticks(np.arange(len(heatmap_values.index)))
    ax_heat.set_yticklabels(heatmap_values.index.tolist(), fontsize=9.2, fontweight="semibold")
    ax_heat.set_xlabel("3PRATE", fontsize=11.2, fontweight="semibold")
    ax_heat.set_ylabel("3P%", fontsize=11.2, fontweight="semibold")
    ax_heat.set_title("Overperformance heatmap", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax_heat.set_box_aspect(1)
    ax_heat.set_xticks(np.arange(-0.5, heatmap_array.shape[1], 1), minor=True)
    ax_heat.set_yticks(np.arange(-0.5, heatmap_array.shape[0], 1), minor=True)
    ax_heat.grid(which="minor", color=PROJECT_PALETTE["midnight"], linewidth=1.4)
    ax_heat.tick_params(which="minor", bottom=False, left=False)
    ax_heat.tick_params(axis="x", length=0, pad=7)
    ax_heat.tick_params(axis="y", length=0, pad=7)
    for heat_row_idx in range(heatmap_array.shape[0]):
        for col_idx in range(heatmap_array.shape[1]):
            value = heatmap_array[heat_row_idx, col_idx]
            count = int(heatmap_counts.iloc[heat_row_idx, col_idx])
            ax_heat.text(
                col_idx,
                heat_row_idx,
                f"{value:+.3f}\n(n={count})",
                ha="center",
                va="center",
                fontsize=8.3,
                fontweight="bold",
                color=PROJECT_PALETTE["deep_ocean"],
                bbox=styled_box(facecolor="white", edgecolor=PROJECT_PALETTE["sky_blue"], alpha=0.88),
            )

    cbar = fig.colorbar(mesh, ax=ax_heat, fraction=0.045, pad=0.03)
    cbar.set_label("Mean overperformance vs exact seed", fontsize=10.2, fontweight="semibold", color=TEXT_DARK)
    cbar.ax.tick_params(labelsize=9.2, colors=TEXT_DARK, length=0)

    fig.suptitle("Three-point profiles versus March overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        f"Modern-era view only: ranked profiles and the 3PRATE x 3P% heatmap use tournament teams from {recent_start}+.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_06_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    metrics = ["3PRATE", "3P%", "3P%D"]
    all_group = {"era": "2008-2025", "team_count": int(len(over_df))}
    for metric in metrics:
        slug = metric_slug(metric)
        all_group[f"{slug}_seed_adjusted_rho"] = float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman"))
    rows.append(all_group)
    for label, start, end in ERAS:
        group = over_df[over_df["year"].between(start, end)].copy()
        row = {"era": label, "team_count": int(len(group))}
        for metric in metrics:
            slug = metric_slug(metric)
            row[f"{slug}_seed_adjusted_rho"] = float(group["overperformance"].corr(group[f"{slug}_edge"], method="spearman"))
        rows.append(row)
    return pd.DataFrame(rows)


def plot_analysis_06(table: pd.DataFrame, out_path: Path) -> None:
    panel_specs = [
        ("three_prate_seed_adjusted_rho", "Three-point attempt rate", ACCENT),
        ("three_pct_seed_adjusted_rho", "Three-point percentage", PROJECT_PALETTE["lake_blue"]),
        ("three_pct_d_seed_adjusted_rho", "Opponent three-point percentage", HIGHLIGHT),
    ]

    set_project_theme()
    fig, axes = plt.subplots(1, 3, figsize=(20.8, 7.6), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.83, bottom=0.14, left=0.05, right=0.98, wspace=0.16)

    eras = table["era"].tolist()
    for ax, (column, title, color) in zip(axes, panel_specs):
        colors = [color] + [HIGHLIGHT if "2022" in era else PROJECT_PALETTE["steel"] for era in eras[1:]]
        ax.bar(np.arange(len(table)), table[column], color=colors, width=0.62)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
        ax.set_xticks(np.arange(len(table)))
        ax.set_xticklabels(eras, fontsize=10.0, fontweight="semibold")
        ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=6)

    fig.suptitle("Did the modern three-point era change the Day 5 read?", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.905, "Each panel keeps one metric isolated so the modern-era comparison stays readable.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_upset_history(
    tournament_df: pd.DataFrame,
    full_ref: pd.DataFrame,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    upset_rows = (
        tournament_df.loc[
            tournament_df["seed_int"].isin([1, 2, 3]) & tournament_df["finish"].eq("R64"),
            ["Team", "year", "seed_int", "3PRATE", "3P%", "3P%D"],
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
    for metric in ["3PRATE", "3P%", "3P%D"]:
        slug = metric_slug(metric)
        upset_rows[f"historical_{slug}_delta_raw"] = upset_rows[metric] - upset_rows[f"full_seed_avg_{slug}"]
        upset_rows[f"historical_{slug}_edge"] = (
            -upset_rows[f"historical_{slug}_delta_raw"] if metric in LOWER_BETTER else upset_rows[f"historical_{slug}_delta_raw"]
        )
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
            ["Team", "year", "seed_int", "3PRATE", "3P%", "3P%D", "overperformance"],
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
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    field = load_projected_field(seeds_path, full_df)
    full_ref, recent_ref = build_seed_reference_tables(tournament_df, ["3PRATE", "3P%", "3P%D"])
    merged = field.merge(full_ref, on="projected_seed", how="left").merge(recent_ref, on="projected_seed", how="left")
    for metric in ["3PRATE", "3P%", "3P%D"]:
        slug = metric_slug(metric)
        merged[f"full_{slug}_delta_raw"] = merged[metric] - merged[f"full_seed_avg_{slug}"]
        merged[f"full_{slug}_edge"] = -merged[f"full_{slug}_delta_raw"] if metric in LOWER_BETTER else merged[f"full_{slug}_delta_raw"]
    merged["offense_profile_score"] = 0.60 * merged["full_three_pct_edge"] + 0.50 * merged["full_three_prate_edge"]
    merged["defense_context"] = np.where(
        merged["full_three_pct_d_edge"] >= 0.75,
        "positive",
        np.where(merged["full_three_pct_d_edge"] <= -0.75, "negative", "neutral"),
    )
    merged["offense_context"] = np.where(
        merged["offense_profile_score"] >= 0.75,
        "positive",
        np.where(merged["offense_profile_score"] <= -0.75, "negative", "neutral"),
    )
    merged["day5_score"] = merged["offense_profile_score"] + 0.35 * merged["full_three_pct_d_edge"]
    merged["team_key"] = merged["team_display"].map(normalize_team_key)
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)

    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", ["Team", "year", "seed_int", "3PRATE", "3P%", "3P%D"]]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )
    champion_rows = champion_rows.merge(
        full_ref.rename(columns={"projected_seed": "seed_int"}),
        on="seed_int",
        how="left",
    )
    for metric in ["3PRATE", "3P%", "3P%D"]:
        slug = metric_slug(metric)
        champion_rows[f"title_{slug}_delta_raw"] = champion_rows[metric] - champion_rows[f"full_seed_avg_{slug}"]
        champion_rows[f"title_{slug}_edge"] = -champion_rows[f"title_{slug}_delta_raw"] if metric in LOWER_BETTER else champion_rows[f"title_{slug}_delta_raw"]
    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_pairs = [
            {
                "year": int(row["year"]),
                "seed": int(row["seed_int"]),
                "three_prate": float(row["3PRATE"]),
                "three_pct": float(row["3P%"]),
                "three_pct_d": float(row["3P%D"]),
                "three_prate_edge": float(row["title_three_prate_edge"]),
                "three_pct_edge": float(row["title_three_pct_edge"]),
                "three_pct_d_edge": float(row["title_three_pct_d_edge"]),
            }
            for _, row in group.iterrows()
        ]
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_pairs)),
                "last_title_year": int(title_pairs[-1]["year"]),
                "last_title_seed": int(title_pairs[-1]["seed"]),
                "title_years": [item["year"] for item in title_pairs],
                "title_history": "; ".join([f"{item['year']} ({item['seed']})" for item in title_pairs]),
                "title_three_prate_edge_values": [item["three_prate_edge"] for item in title_pairs],
                "title_three_pct_edge_values": [item["three_pct_edge"] for item in title_pairs],
                "title_three_pct_d_edge_values": [item["three_pct_d_edge"] for item in title_pairs],
                "title_three_prate_values": [item["three_prate"] for item in title_pairs],
                "title_three_pct_values": [item["three_pct"] for item in title_pairs],
                "title_three_pct_d_values": [item["three_pct_d"] for item in title_pairs],
                "title_seeds": [item["seed"] for item in title_pairs],
            }
        )
    champion_history = pd.DataFrame(champion_history_rows)
    if not champion_history.empty:
        merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged.get("championship_count", 0).fillna(0).astype(int)
    merged["title_years"] = merged.get("title_years", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged.get("title_seeds", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_three_prate_edge_values"] = merged.get("title_three_prate_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_three_pct_edge_values"] = merged.get("title_three_pct_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_three_pct_d_edge_values"] = merged.get("title_three_pct_d_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_three_prate_values"] = merged.get("title_three_prate_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_three_pct_values"] = merged.get("title_three_pct_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_three_pct_d_values"] = merged.get("title_three_pct_d_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged.get("title_history", "").fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(["projected_seed", "day5_score"], ascending=[True, False], kind="stable").reset_index(drop=True)
    upset_history = build_analysis_07_upset_history(tournament_df, full_ref, logo_cache_dir)
    overperformer_history = build_analysis_07_overperformer_history(tournament_df, logo_cache_dir)
    return merged, upset_history, overperformer_history


def validate_analysis_07_table(table: pd.DataFrame) -> None:
    champion_rows = table.loc[table["is_former_champion"]].copy()
    required_title_columns = [
        "title_three_prate_edge_values",
        "title_three_pct_edge_values",
        "title_three_pct_d_edge_values",
        "title_three_prate_values",
        "title_three_pct_values",
        "title_three_pct_d_values",
        "title_seeds",
        "title_years",
    ]
    for row in champion_rows.itertuples(index=False):
        title_years = list(getattr(row, "title_years", []))
        if not title_years:
            raise ValueError(f"Former champion {row.team_display} is missing title years for badge plotting")
        title_count = len(title_years)
        for column in required_title_columns:
            values = list(getattr(row, column, []))
            if len(values) != title_count:
                raise ValueError(
                    f"Former champion {row.team_display} has {title_count} title years but {len(values)} values in {column}"
                )
            if any(pd.isna(value) for value in values):
                raise ValueError(f"Former champion {row.team_display} has missing values in {column}")


def plot_analysis_07(table: pd.DataFrame, upset_history: pd.DataFrame, overperformer_history: pd.DataFrame, out_path: Path) -> None:
    plot_df = table.copy()
    x_arrays = [plot_df["3PRATE"].to_numpy(dtype=float)]
    y_arrays = [plot_df["3P%"].to_numpy(dtype=float)]
    if not upset_history.empty:
        x_arrays.append(upset_history["3PRATE"].to_numpy(dtype=float))
        y_arrays.append(upset_history["3P%"].to_numpy(dtype=float))
    if not overperformer_history.empty:
        x_arrays.append(overperformer_history["3PRATE"].to_numpy(dtype=float))
        y_arrays.append(overperformer_history["3P%"].to_numpy(dtype=float))
    x_values = np.concatenate(x_arrays)
    y_values = np.concatenate(y_arrays)
    x_pad = max((float(np.nanmax(x_values)) - float(np.nanmin(x_values))) * 0.12, 0.8)
    y_pad = max((float(np.nanmax(y_values)) - float(np.nanmin(y_values))) * 0.12, 0.8)
    x_min = float(np.nanmin(x_values)) - x_pad
    x_max = float(np.nanmax(x_values)) + x_pad
    y_min = float(np.nanmin(y_values)) - y_pad
    y_max = float(np.nanmax(y_values)) + y_pad
    x_split = float(plot_df["3PRATE"].mean())
    y_split = float(plot_df["3P%"].mean())

    support_color_map = {"positive": POSITIVE, "neutral": PROJECT_PALETTE["steel"], "negative": NEGATIVE}

    set_project_theme()
    fig, ax = plt.subplots(figsize=(16.0, 12.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.86, bottom=0.11, left=0.08, right=0.96)

    ax.axhline(y_split, color=SECONDARY, linestyle=":", linewidth=1.6, zorder=0)
    ax.axvline(x_split, color=SECONDARY, linestyle=":", linewidth=1.6, zorder=0)

    quadrant_positions = [
        (0.22, 0.90, "High rate\nHigh success"),
        (0.78, 0.90, "Low rate\nHigh success"),
        (0.22, 0.10, "High rate\nLow success"),
        (0.78, 0.10, "Low rate\nLow success"),
    ]
    for x_pos, y_pos, label in quadrant_positions:
        ax.text(
            x_pos,
            y_pos,
            label,
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=11.2,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(alpha=0.90),
            zorder=1,
        )

    for _, row in plot_df.iterrows():
        x_value = float(row["3PRATE"])
        y_value = float(row["3P%"])
        halo_color = support_color_map.get(str(row.get("defense_context", "neutral")), PROJECT_PALETTE["steel"])
        logo_path = row.get("logo_path")
        is_former_champion = bool(row.get("is_former_champion", False))
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if is_former_champion:
                title_x_values = list(row.get("title_three_prate_values", []))
                title_y_values = list(row.get("title_three_pct_values", []))
                title_years = list(row.get("title_years", []))
                point_counts: dict[tuple[float, float], int] = {}
                for title_year, title_x_value, title_y_value in zip(title_years, title_x_values, title_y_values):
                    point_key = (round(float(title_x_value), 3), round(float(title_y_value), 3))
                    point_counts[point_key] = point_counts.get(point_key, 0) + 1
                    offset_idx = point_counts[point_key] - 1
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.052),
                        (float(title_x_value), float(title_y_value)),
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
        else:
            ax.text(
                x_value,
                y_value,
                str(row["team_display"]),
                ha="center",
                va="center",
                fontsize=7.2,
                fontweight="bold",
                color=PROJECT_PALETTE["deep_ocean"],
                bbox=styled_box(facecolor="white", edgecolor=halo_color),
                zorder=4,
            )

    if not upset_history.empty:
        point_counts: dict[tuple[float, float], int] = {}
        for _, row in upset_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["3PRATE"])
            y_value = float(row["3P%"])
            point_key = (round(x_value, 3), round(y_value, 3))
            point_counts[point_key] = point_counts.get(point_key, 0) + 1
            offset_idx = point_counts[point_key] - 1
            upset_artist = AnnotationBbox(
                OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.050),
                (x_value, y_value),
                xybox=(14 + (offset_idx * 12), -14 - (offset_idx * 10)),
                boxcoords="offset points",
                frameon=True,
                bboxprops={
                    "boxstyle": "circle,pad=0.28",
                    "facecolor": UPSET_BADGE_FACE,
                    "edgecolor": UPSET_BADGE_EDGE,
                    "linewidth": 1.25,
                    "alpha": 0.96,
                },
                zorder=2,
            )
            ax.add_artist(upset_artist)

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(logo_path).exists():
                continue
            x_value = float(row["3PRATE"])
            y_value = float(row["3P%"])
            ax.scatter([x_value], [y_value], s=920, marker="^", facecolor=OVERPERFORMER_BADGE_FACE, edgecolor=OVERPERFORMER_BADGE_EDGE, linewidth=1.2, zorder=1.85)
            ax.add_artist(AnnotationBbox(OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.056), (x_value, y_value), frameon=False, zorder=2.15))

    legend_handles = [
        Line2D([], [], color=SECONDARY, linestyle=":", linewidth=1.6, label="Projected field average"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=POSITIVE, markeredgewidth=2.0, markersize=10, label="Positive 3P defense support"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=PROJECT_PALETTE["steel"], markeredgewidth=2.0, markersize=10, label="Neutral 3P defense support"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=NEGATIVE, markeredgewidth=2.0, markersize=10, label="Negative 3P defense support"),
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

    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_min, y_max)
    ax.set_xlabel(metric_label("3PRATE"), fontsize=12, fontweight="semibold")
    ax.set_ylabel(metric_label("3P%"), fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", labelsize=11, length=0, pad=7)

    fig.suptitle("Projected 2026 three-point profiles", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        "Right means a higher three-point attempt rate. Up means a higher three-point percentage. Dashed lines mark projected field averages and halo color shows three-point defense support.",
        ha="center",
        va="center",
        fontsize=10.2,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=190, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_07_defense(table: pd.DataFrame, upset_history: pd.DataFrame, overperformer_history: pd.DataFrame, out_path: Path) -> None:
    plot_df = table.copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    seed_reference = (
        plot_df.groupby("projected_seed", as_index=False)[["full_seed_avg_three_pct_d", "recent_seed_avg_three_pct_d"]]
        .first()
        .sort_values("projected_seed", kind="stable")
    )
    support_color_map = {"positive": POSITIVE, "neutral": PROJECT_PALETTE["steel"], "negative": NEGATIVE}

    set_project_theme()
    fig, ax = plt.subplots(figsize=(22.8, 12.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=SECONDARY, linewidth=0.7, alpha=0.16, zorder=0)

    for row in seed_reference.itertuples(index=False):
        seed = float(row.projected_seed)
        ax.plot(
            [float(row.full_seed_avg_three_pct_d), float(row.full_seed_avg_three_pct_d)],
            [seed - 0.26, seed + 0.26],
            color=ACCENT,
            linewidth=3.0,
            solid_capstyle="round",
            zorder=1,
        )
        ax.plot(
            [float(row.recent_seed_avg_three_pct_d), float(row.recent_seed_avg_three_pct_d)],
            [seed - 0.18, seed + 0.18],
            color=SECONDARY,
            linewidth=2.1,
            alpha=0.78,
            solid_capstyle="round",
            zorder=1,
        )

    for _, row in plot_df.iterrows():
        x_value = float(row["3P%D"])
        y_value = float(row["plot_y"])
        halo_color = support_color_map.get(str(row.get("offense_context", "neutral")), PROJECT_PALETTE["steel"])
        logo_path = row.get("logo_path")
        is_former_champion = bool(row.get("is_former_champion", False))
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if is_former_champion:
                seed_counts: dict[int, int] = {}
                for title_seed, title_value in zip(row["title_seeds"], row["title_three_pct_d_values"]):
                    seed_value = int(title_seed)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    x_offset = -18 - ((seed_counts[seed_value] - 1) * 14)
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.062),
                        (float(title_value), float(seed_value)),
                        xybox=(x_offset, 0),
                        boxcoords="offset points",
                        frameon=True,
                        bboxprops={
                            "boxstyle": "round,pad=0.22",
                            "facecolor": "#F5E7A1",
                            "edgecolor": "#D4A72C",
                            "linewidth": 1.35,
                            "alpha": 0.96,
                        },
                        zorder=2,
                    )
                    ax.add_artist(champ_artist)
            artist = AnnotationBbox(
                OffsetImage(logo_image, zoom=0.095),
                (x_value, y_value),
                frameon=False,
                zorder=3,
            )
            ax.add_artist(artist)
        else:
            ax.text(
                x_value,
                y_value,
                str(row["team_display"]),
                ha="center",
                va="center",
                fontsize=7.2,
                fontweight="bold",
                color=PROJECT_PALETTE["deep_ocean"],
                bbox=styled_box(facecolor="white", edgecolor=halo_color),
                zorder=4,
            )

    if not upset_history.empty:
        seed_counts: dict[int, int] = {}
        for _, row in upset_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(str(logo_path)).exists():
                continue
            seed_value = int(row["seed_int"])
            seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
            upset_artist = AnnotationBbox(
                OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.060),
                (float(row["3P%D"]), float(seed_value)),
                xybox=(18 + ((seed_counts[seed_value] - 1) * 14), 0),
                boxcoords="offset points",
                frameon=True,
                bboxprops={
                    "boxstyle": "circle,pad=0.30",
                    "facecolor": UPSET_BADGE_FACE,
                    "edgecolor": UPSET_BADGE_EDGE,
                    "linewidth": 1.25,
                    "alpha": 0.96,
                },
                zorder=2,
            )
            ax.add_artist(upset_artist)

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(str(logo_path)).exists():
                continue
            x_value = float(row["3P%D"])
            y_value = float(row["seed_int"])
            ax.scatter([x_value], [y_value], s=920, marker="^", facecolor=OVERPERFORMER_BADGE_FACE, edgecolor=OVERPERFORMER_BADGE_EDGE, linewidth=1.2, zorder=1.85)
            ax.add_artist(AnnotationBbox(OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.058), (x_value, y_value), frameon=False, zorder=2.15))

    x_values = np.concatenate(
        [
            plot_df["3P%D"].to_numpy(dtype=float),
            upset_history["3P%D"].to_numpy(dtype=float) if not upset_history.empty else np.array([], dtype=float),
            seed_reference["full_seed_avg_three_pct_d"].to_numpy(dtype=float),
            seed_reference["recent_seed_avg_three_pct_d"].to_numpy(dtype=float),
        ]
    )
    x_min = float(np.nanmin(x_values))
    x_max = float(np.nanmax(x_values))
    x_padding_left = max((x_max - x_min) * 0.22, 0.55)
    x_padding_right = max((x_max - x_min) * 0.06, 0.20)
    ax.set_xlim(x_min - x_padding_left, x_max + x_padding_right)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(seed) for seed in range(1, 17)], fontsize=11, fontweight="semibold")
    ax.set_xlabel(metric_label("3P%D"), fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    if "3P%D" in LOWER_BETTER:
        ax.invert_xaxis()
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        Line2D([], [], color=ACCENT, marker="|", linestyle="None", markersize=16, markeredgewidth=3, label="Full-sample seed avg"),
        Line2D([], [], color=SECONDARY, marker="|", linestyle="None", markersize=14, markeredgewidth=2.2, label="Recent seed avg"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=POSITIVE, markeredgewidth=2.0, markersize=10, label="Positive offense support"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=PROJECT_PALETTE["steel"], markeredgewidth=2.0, markersize=10, label="Neutral offense support"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=NEGATIVE, markeredgewidth=2.0, markersize=10, label="Negative offense support"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge (title seed)"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor=UPSET_BADGE_FACE, markeredgecolor=UPSET_BADGE_EDGE, markeredgewidth=1.3, markersize=10, label="Historical 1-3 seed R64 upset"),
        Line2D([], [], marker="^", linestyle="None", markerfacecolor=OVERPERFORMER_BADGE_FACE, markeredgecolor=OVERPERFORMER_BADGE_EDGE, markeredgewidth=1.3, markersize=15, label="Historical 3+ round overperformer"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper right",
        bbox_to_anchor=(0.985, 0.985),
        frameon=True,
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=ACCENT,
        fontsize=9.5,
        title="Seed reference",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.1)
    legend_text_color = PROJECT_PALETTE["paper"]
    legend.get_title().set_color(legend_text_color)
    for text in legend.get_texts():
        text.set_color(legend_text_color)

    fig.suptitle("Projected 2026 opponent three-point defense", fontsize=22, fontweight="bold", y=0.965)
    fig.text(0.5, 0.92, "Lower values are better here. Halo color shows whether the team also brings a positive or negative offensive three-point profile.", ha="center", va="center", fontsize=10.2, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def write_summary_05(table: pd.DataFrame, out_path: Path) -> None:
    best = table.sort_values("mean_overperformance", ascending=False, kind="stable").iloc[0]
    lines = [
        "# Three-Point Analysis 5: Rate and Success Interaction",
        "",
        f"- Best profile: `{best['profile']}` at `{best['mean_overperformance']:+.3f}` overperformance points.",
        "- Interpretation: Day 5 should focus on how volume and success combine, not on either metric in isolation.",
        "- Figure structure: ranked profile view on the left, 3PRATE x 3P% overperformance heatmap on the right.",
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
        "# Three-Point Analysis 6: Era Comparison",
        "",
        f"- Full-sample `3PRATE` rho: `{full['three_prate_seed_adjusted_rho']:+.3f}`. Modern-era `3PRATE` rho: `{modern['three_prate_seed_adjusted_rho']:+.3f}`.",
        f"- Full-sample `3P%` rho: `{full['three_pct_seed_adjusted_rho']:+.3f}`. Modern-era `3P%` rho: `{modern['three_pct_seed_adjusted_rho']:+.3f}`.",
        f"- Full-sample `3P%D` rho: `{full['three_pct_d_seed_adjusted_rho']:+.3f}`. Modern-era `3P%D` rho: `{modern['three_pct_d_seed_adjusted_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_07(table: pd.DataFrame, upset_history: pd.DataFrame, out_path: Path) -> None:
    best = table.nlargest(8, "day5_score")[["team_display", "projected_seed", "full_three_prate_edge", "full_three_pct_edge", "full_three_pct_d_edge"]]
    worst = table.nsmallest(8, "day5_score")[["team_display", "projected_seed", "full_three_prate_edge", "full_three_pct_edge", "full_three_pct_d_edge"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Three-Point Analysis 7: 2026 Profile Map",
        "",
        "- This chart puts rate and success on the axes so the 2026 field can be read in one glance.",
        f"- Gold-backed logos in the chart mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        f"- Rose-backed logos mark the `{len(upset_history)}` historical 1-3 seeds that lost in the first round.",
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


def write_summary_07_defense(table: pd.DataFrame, upset_history: pd.DataFrame, out_path: Path) -> None:
    best = table.nlargest(6, "full_three_pct_d_edge")[["team_display", "projected_seed", "full_three_pct_d_edge", "offense_profile_score"]]
    worst = table.nsmallest(6, "full_three_pct_d_edge")[["team_display", "projected_seed", "full_three_pct_d_edge", "offense_profile_score"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Three-Point Analysis 7B: 2026 Defensive Field View",
        "",
        "- This companion chart leads with opponent three-point percentage allowed, then uses the offensive profile as support.",
        f"- Gold-backed logos in the chart mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        f"- Rose-backed logos mark the `{len(upset_history)}` historical 1-3 seeds that lost in the first round.",
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
    shutil.copyfile(paths["fig_05"], thread_dir / "1_three_point_hook.png")


def main() -> int:
    paths = get_paths()
    full_df = load_three_point_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    table05 = build_analysis_05_table(over_df)
    table06 = build_analysis_06_table(over_df)
    table07, upset_history, overperformer_history = build_analysis_07_table(full_df, tournament_df, paths["seeds_2026"], paths["logo_cache"])
    validate_analysis_07_table(table07)

    table05.round(6).to_csv(paths["csv_05"], index=False)
    table06.round(6).to_csv(paths["csv_06"], index=False)
    table07.round(6).to_csv(paths["csv_07"], index=False)

    plot_analysis_05(table05, over_df, paths["fig_05"])
    plot_analysis_07(table07, upset_history, overperformer_history, paths["fig_07"])
    plot_analysis_07_defense(table07, upset_history, overperformer_history, paths["fig_07_defense"])
    write_summary_05(table05, paths["md_05"])
    write_summary_06(table06, paths["md_06"])
    write_summary_07(table07, upset_history, paths["md_07"])
    write_summary_07_defense(table07, upset_history, paths["md_07_defense"])
    create_thread_assets(paths)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
