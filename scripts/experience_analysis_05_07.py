#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
import numpy as np
import pandas as pd

from experience_analysis_common import (
    ACCENT,
    COACH_ANALYSIS_START_YEAR,
    COACH_SUPPORT_METRIC,
    CURRENT_FIELD_YEAR,
    TEAM_HISTORY_START_YEAR,
    LABEL_TEXT,
    PLAYER_PRIMARY_METRIC,
    PLAYER_SUPPORT_METRIC,
    SECONDARY,
    TEXT_DARK,
    TEXT_LIGHT,
    TRANSFER_PORTAL_YEAR,
    apply_report_background,
    build_overperformance_data,
    build_seed_reference_tables,
    cache_logo_images,
    format_year_tick,
    filter_coach_analysis,
    get_experience_base_paths,
    load_experience_data,
    load_logo_image,
    load_or_compute_coach_metric_selection,
    load_projected_field,
    load_tournament_data,
    metric_label,
    metric_slug,
    normalize_team_key,
    set_project_theme,
    styled_box,
    with_repo_relative_logo_paths,
)

PLAYER_VISUAL_METRIC = PLAYER_SUPPORT_METRIC


def player_visual_label() -> str:
    return metric_label(PLAYER_VISUAL_METRIC)


def player_visual_edge(values: pd.Series | np.ndarray) -> np.ndarray:
    return -np.asarray(values, dtype=float)


def get_paths() -> dict[str, Path]:
    base = get_experience_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": base["logo_cache"],
        "fig_05": root / "reports" / "figures" / "combined_experience_archetypes.png",
        "fig_06": root / "reports" / "figures" / "experience_by_year.png",
        "fig_06_player": root / "reports" / "figures" / "player_exp_over_time.png",
        "fig_06_coach": root / "reports" / "figures" / "coach_exp_over_time.png",
        "fig_07": root / "reports" / "figures" / "experience_2026_field.png",
        "fig_07_coach": root / "reports" / "figures" / "experience_2026_coach_field.png",
        "csv_05": root / "reports" / "combined_experience_archetypes_table.csv",
        "csv_06": root / "reports" / "experience_by_year_table.csv",
        "csv_07": root / "reports" / "experience_2026_field_table.csv",
        "md_05": root / "reports" / "combined_experience_archetypes_summary.md",
        "md_06": root / "reports" / "experience_by_year_summary.md",
        "md_07": root / "reports" / "experience_2026_field_summary.md",
    }


def build_analysis_05_table(over_df: pd.DataFrame, coach_primary_metric: str) -> pd.DataFrame:
    exp_vs_seed = player_visual_edge(over_df[f"{metric_slug(PLAYER_VISUAL_METRIC)}_vs_seed"])
    coach_vs_seed = over_df[f"{metric_slug(coach_primary_metric)}_vs_seed"]
    plot_df = over_df.copy()
    plot_df["archetype"] = np.select(
        [
            (exp_vs_seed >= 0) & (coach_vs_seed >= 0),
            (exp_vs_seed >= 0) & (coach_vs_seed < 0),
            (exp_vs_seed < 0) & (coach_vs_seed >= 0),
            (exp_vs_seed < 0) & (coach_vs_seed < 0),
        ],
        [
            "above both seed baselines",
            "above player experience only",
            "above coach experience only",
            "below both seed baselines",
        ],
        default="mixed",
    )
    rows = []
    order = [
        "above both seed baselines",
        "above player experience only",
        "above coach experience only",
        "below both seed baselines",
    ]
    for archetype in order:
        group = plot_df[plot_df["archetype"] == archetype]
        rows.append(
            {
                "archetype": archetype,
                "team_count": int(len(group)),
                "mean_overperformance": float(group["overperformance"].mean()),
                "median_overperformance": float(group["overperformance"].median()),
                "adv_rate": float((group["overperformance"] > 0).mean()),
                "avg_exp": float(group[PLAYER_PRIMARY_METRIC].mean()),
                "avg_exp_rank": float(group[PLAYER_SUPPORT_METRIC].mean()),
                "avg_coach_primary": float(group[coach_primary_metric].mean()),
                "avg_coach_win_pct_5y": float(group[COACH_SUPPORT_METRIC].mean()),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_05(table: pd.DataFrame, out_path: Path) -> None:
    plot_df = table.copy().sort_values("mean_overperformance", kind="stable")
    y = np.arange(len(plot_df))
    values = plot_df["mean_overperformance"].to_numpy(float)
    x_min = float(np.nanmin(values))
    x_max = float(np.nanmax(values))
    x_pad = max((x_max - x_min) * 0.20, 0.05)

    set_project_theme()
    fig, ax = plt.subplots(figsize=(13.6, 7.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.24, right=0.92)

    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.5)
    ax.hlines(y, 0, values, color=ACCENT, linewidth=2.6)
    ax.scatter(values, y, color=ACCENT, s=102, zorder=3)
    for y_pos, value in zip(y, values):
        ax.text(
            value + (0.012 if value >= 0 else -0.012),
            y_pos,
            f"{value:+.3f}",
            ha="left" if value >= 0 else "right",
            va="center",
            fontsize=10.0,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )
    ax.set_yticks(y)
    ax.set_yticklabels([str(value).title() for value in plot_df["archetype"].tolist()], fontsize=10.6, fontweight="semibold")
    ax.set_xlabel("Mean finish - exact-seed expectation", fontsize=11.5, fontweight="semibold")
    ax.set_xlim(x_min - x_pad, x_max + x_pad)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10.2, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=8)
    fig.suptitle("Combined player and coach experience archetypes", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        f"{COACH_ANALYSIS_START_YEAR}+ teams are split by whether raw Exp. and the lead coach metric sit above or below their exact-seed baselines",
        ha="center",
        va="center",
        fontsize=9.9,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_06_table(
    over_df: pd.DataFrame,
    coach_over_df: pd.DataFrame,
    coach_primary_metric: str,
) -> pd.DataFrame:
    exp_col = f"{metric_slug(PLAYER_SUPPORT_METRIC)}_vs_seed"
    coach_col = f"{metric_slug(coach_primary_metric)}_vs_seed"
    rows = []
    for year, group in over_df.groupby("year", sort=True):
        coach_group = coach_over_df[coach_over_df["year"] == year]
        rows.append(
            {
                "year": int(year),
                "team_count": int(len(group)),
                "player_seed_adjusted_rho": float(group["overperformance"].corr(-group[exp_col], method="spearman")),
                "coach_seed_adjusted_rho": float(
                    coach_group["overperformance"].corr(coach_group[coach_col], method="spearman")
                ) if len(coach_group) > 2 else np.nan,
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_06(table: pd.DataFrame, coach_primary_metric: str, out_path: Path) -> None:
    years = table["year"].to_numpy(dtype=float)
    player_values = table["player_seed_adjusted_rho"].to_numpy(dtype=float)
    coach_table = table[table["coach_seed_adjusted_rho"].notna()].copy()
    coach_years = coach_table["year"].to_numpy(dtype=float)
    coach_values = coach_table["coach_seed_adjusted_rho"].to_numpy(dtype=float)

    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(15.8, 9.8), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.09, right=0.95, hspace=0.30)

    for ax, panel_years, values, panel_title in [
        (axes[0], years, player_values, "Season-wide Exp rank by year"),
        (axes[1], coach_years, coach_values, f"{metric_label(coach_primary_metric)} by year"),
    ]:
        mean_value = float(np.nanmean(values))
        ax.plot(panel_years, values, color=ACCENT, linewidth=2.8, marker="o", markersize=5.2)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.axhline(mean_value, color=TEXT_LIGHT, linestyle="--", linewidth=1.2, alpha=0.95)
        ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=1.8)
        ax.text(
            panel_years.max() + 0.12,
            mean_value,
            "Avg rho",
            ha="left",
            va="center",
            fontsize=9.8,
            fontweight="semibold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )
        ax.set_title(panel_title, fontsize=12.6, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.set_xticks(panel_years)
        ax.set_xticklabels([str(int(year)) for year in panel_years], fontsize=10.0, fontweight="semibold")
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", labelsize=10.2, length=0, pad=7)

    axes[1].set_xlabel("Season", fontsize=11.5, fontweight="semibold")
    axes[0].set_ylabel("Spearman rho", fontsize=11.5, fontweight="semibold")
    axes[1].set_ylabel("Spearman rho", fontsize=11.5, fontweight="semibold")
    fig.suptitle("Experience by year", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        f"Seed-adjusted player Exp rank edge on top, seed-adjusted coach experience ({COACH_ANALYSIS_START_YEAR}+) on bottom",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_single_analysis_06(
    table: pd.DataFrame,
    value_column: str,
    panel_title: str,
    subtitle: str,
    out_path: Path,
) -> None:
    plot_table = table[table[value_column].notna()].copy()
    years = plot_table["year"].to_numpy(dtype=float)
    values = plot_table[value_column].to_numpy(dtype=float)
    mean_value = float(np.nanmean(values))

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.8, 6.6), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.09, right=0.95)

    ax.plot(years, values, color=ACCENT, linewidth=2.8, marker="o", markersize=5.2)
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
    ax.axhline(mean_value, color=TEXT_LIGHT, linestyle="--", linewidth=1.2, alpha=0.95)
    ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=1.8)
    ax.text(
        years.max() + 0.12,
        mean_value,
        "Avg rho",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="semibold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax.set_title(panel_title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax.set_xticks(years)
    ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.0, fontweight="semibold")
    ax.set_xlabel("Season", fontsize=11.5, fontweight="semibold")
    ax.set_ylabel("Spearman rho", fontsize=11.5, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=7)
    ax.tick_params(axis="y", labelsize=10.2, length=0, pad=7)

    fig.suptitle(panel_title, fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, subtitle, ha="center", va="center", fontsize=10.0, color=TEXT_LIGHT)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_table(
    full_df: pd.DataFrame,
    tournament_df: pd.DataFrame,
    coach_tournament_df: pd.DataFrame,
    coach_primary_metric: str,
    seeds_path: Path,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df)
    if len(field) != 68:
        raise ValueError(f"Expected 68 projected 2026 teams after join; found {len(field)}")

    exp_slug = metric_slug(PLAYER_VISUAL_METRIC)
    coach_slug = metric_slug(coach_primary_metric)
    exp_full_reference, exp_recent_reference = build_seed_reference_tables(
        tournament_df,
        [PLAYER_VISUAL_METRIC],
    )
    coach_full_reference, coach_recent_reference = build_seed_reference_tables(
        coach_tournament_df,
        [coach_primary_metric],
    )
    coach_full_reference = coach_full_reference.drop(
        columns=[column for column in coach_full_reference.columns if column.endswith("_seed_team_count")]
    )
    coach_recent_reference = coach_recent_reference.drop(
        columns=[column for column in coach_recent_reference.columns if column.endswith("_seed_team_count")]
    )
    merged = (
        field.merge(exp_full_reference, on="projected_seed", how="left")
        .merge(exp_recent_reference, on="projected_seed", how="left")
        .merge(coach_full_reference, on="projected_seed", how="left")
        .merge(coach_recent_reference, on="projected_seed", how="left")
    )
    merged["full_exp_delta"] = merged[f"full_seed_avg_{exp_slug}"] - merged[PLAYER_VISUAL_METRIC]
    merged["recent_exp_delta"] = merged[f"recent_seed_avg_{exp_slug}"] - merged[PLAYER_VISUAL_METRIC]
    merged["full_exp_z"] = merged["full_exp_delta"] / merged[f"full_seed_std_{exp_slug}"].replace(0, np.nan)
    merged["recent_exp_z"] = merged["recent_exp_delta"] / merged[f"recent_seed_std_{exp_slug}"].replace(0, np.nan)
    merged["full_coach_delta"] = merged[coach_primary_metric] - merged[f"full_seed_avg_{coach_slug}"]
    merged["recent_coach_delta"] = merged[coach_primary_metric] - merged[f"recent_seed_avg_{coach_slug}"]
    merged["full_coach_z"] = merged["full_coach_delta"] / merged[f"full_seed_std_{coach_slug}"].replace(0, np.nan)
    merged["recent_coach_z"] = merged["recent_coach_delta"] / merged[f"recent_seed_std_{coach_slug}"].replace(0, np.nan)

    def classify_exp_position(z_score: float, threshold: float = 0.35) -> str:
        if pd.isna(z_score):
            return "near baseline"
        if z_score >= threshold:
            return "above baseline"
        if z_score <= -threshold:
            return "below baseline"
        return "near baseline"

    merged["full_exp_position"] = merged["full_exp_z"].apply(classify_exp_position)
    merged["recent_exp_position"] = merged["recent_exp_z"].apply(classify_exp_position)
    merged["full_coach_position"] = merged["full_coach_z"].apply(classify_exp_position)
    champ_cols = ["Team", "year", "seed_int", PLAYER_VISUAL_METRIC]
    if coach_primary_metric in tournament_df.columns:
        champ_cols.append(coach_primary_metric)
    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", champ_cols]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )
    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_rows = [
            {
                "year": int(row.year),
                "seed": int(row.seed_int),
                "exp_visual": float(getattr(row, PLAYER_VISUAL_METRIC)),
                "coach_raw": float(getattr(row, coach_primary_metric, 0) or 0),
            }
            for row in group.itertuples(index=False)
        ]
        title_pairs = [(row["year"], row["seed"]) for row in title_rows]
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_pairs)),
                "last_title_year": int(title_pairs[-1][0]),
                "last_title_seed": int(title_pairs[-1][1]),
                "title_years": [year for year, _ in title_pairs],
                "title_seeds": [seed for _, seed in title_pairs],
                "title_exp_values": [row["exp_visual"] for row in title_rows],
                "title_coach_values": [row["coach_raw"] for row in title_rows],
                "title_history": "; ".join([f"{year} ({seed})" for year, seed in title_pairs]),
            }
        )
    champion_history = pd.DataFrame(champion_history_rows)
    merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged["championship_count"].fillna(0).astype(int)
    merged["last_title_seed"] = merged["last_title_seed"].astype("Int64")
    merged["title_years"] = merged["title_years"].apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged["title_seeds"].apply(lambda value: value if isinstance(value, list) else [])
    merged["title_exp_values"] = merged["title_exp_values"].apply(lambda value: value if isinstance(value, list) else [])
    merged["title_coach_values"] = merged["title_coach_values"].apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged["title_history"].fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(
        ["projected_seed", PLAYER_VISUAL_METRIC, "barthag"],
        ascending=[True, True, False],
        kind="stable",
    ).reset_index(drop=True)
    return merged


def plot_analysis_07(field_table: pd.DataFrame, coach_primary_metric: str, out_path: Path) -> None:
    exp_slug = metric_slug(PLAYER_VISUAL_METRIC)
    plot_df = field_table.copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    seed_reference = (
        plot_df.groupby("projected_seed", as_index=False)[[f"full_seed_avg_{exp_slug}", f"recent_seed_avg_{exp_slug}"]]
        .first()
        .sort_values("projected_seed", kind="stable")
    )

    set_project_theme()
    fig, ax = plt.subplots(figsize=(22.4, 12.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=SECONDARY, linewidth=0.7, alpha=0.16, zorder=0)

    for row in seed_reference.itertuples(index=False):
        seed = float(row.projected_seed)
        ax.plot(
            [getattr(row, f"full_seed_avg_{exp_slug}"), getattr(row, f"full_seed_avg_{exp_slug}")],
            [seed - 0.26, seed + 0.26],
            color=ACCENT,
            linewidth=3.0,
            solid_capstyle="round",
            zorder=1,
        )
        ax.plot(
            [getattr(row, f"recent_seed_avg_{exp_slug}"), getattr(row, f"recent_seed_avg_{exp_slug}")],
            [seed - 0.18, seed + 0.18],
            color=SECONDARY,
            linewidth=2.1,
            alpha=0.78,
            solid_capstyle="round",
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(getattr(row, PLAYER_VISUAL_METRIC))
        y_value = float(row.plot_y)
        is_former_champion = bool(getattr(row, "is_former_champion", False))
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if is_former_champion:
                title_seeds = list(getattr(row, "title_seeds", []))
                title_exp_values = list(getattr(row, "title_exp_values", []))
                seed_counts: dict[int, int] = {}
                for title_seed, title_exp in zip(title_seeds, title_exp_values):
                    seed_value = int(title_seed)
                    title_x = float(title_exp)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    x_offset = -18 - ((seed_counts[seed_value] - 1) * 14)
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.075),
                        (title_x, float(seed_value)),
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
                OffsetImage(logo_image, zoom=0.11),
                (x_value, y_value),
                frameon=False,
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
                fontsize=7.0,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(facecolor="white", edgecolor=ACCENT),
                zorder=4,
            )
            if is_former_champion:
                title_seeds = list(getattr(row, "title_seeds", []))
                title_exp_values = list(getattr(row, "title_exp_values", []))
                seed_counts: dict[int, int] = {}
                for title_seed, title_exp in zip(title_seeds, title_exp_values):
                    seed_value = int(title_seed)
                    title_x = float(title_exp)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    ax.text(
                        title_x - 0.20 - ((seed_counts[seed_value] - 1) * 0.12),
                        float(seed_value),
                        "Champ",
                        ha="center",
                        va="center",
                        fontsize=6.2,
                        fontweight="bold",
                        color=LABEL_TEXT,
                        bbox=styled_box(facecolor="#F5E7A1", edgecolor="#D4A72C"),
                        zorder=3,
                    )

    x_values = np.concatenate(
        [
            plot_df[PLAYER_VISUAL_METRIC].to_numpy(dtype=float),
            seed_reference[f"full_seed_avg_{exp_slug}"].to_numpy(dtype=float),
            seed_reference[f"recent_seed_avg_{exp_slug}"].to_numpy(dtype=float),
        ]
    )
    x_min = float(np.nanmin(x_values))
    x_max = float(np.nanmax(x_values))
    x_padding_left = max((x_max - x_min) * 0.06, 0.18)
    x_padding_right = max((x_max - x_min) * 0.22, 0.55)
    x_lower_bound = min(x_min - x_padding_left, -max((x_max - x_min) * 0.08, 8.0))
    ax.set_xlim(x_lower_bound, x_max + x_padding_right)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(seed) for seed in range(1, 17)], fontsize=11, fontweight="semibold")
    ax.set_xlabel(f"{player_visual_label()} (1 = most experienced)", fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.invert_xaxis()
    ax.set_xticks([tick for tick in ax.get_xticks() if tick > 0])
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        plt.Line2D([0], [0], color=ACCENT, linewidth=3.0, label="Full-sample seed avg"),
        plt.Line2D([0], [0], color=SECONDARY, linewidth=2.1, label="Recent seed avg"),
        plt.Line2D([0], [0], marker="o", linestyle="", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge\n(title seed)"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper right",
        bbox_to_anchor=(0.985, 0.985),
        frameon=True,
        facecolor=TEXT_DARK,
        edgecolor=ACCENT,
        fontsize=9.4,
        title="Series label",
        title_fontsize=10.0,
        labelspacing=0.55,
        handletextpad=0.6,
        borderpad=0.7,
    )
    legend.get_frame().set_linewidth(1.15)
    legend.get_title().set_color(TEXT_LIGHT)
    for text in legend.get_texts():
        text.set_color(TEXT_LIGHT)

    fig.suptitle("Projected 2026 seeds and player Exp rank", fontsize=23, fontweight="bold", y=0.975)
    fig.text(
        0.5,
        0.915,
        "Lower season-wide Exp ranks mean more experience; logos to the right of their seed markers are more experienced than past tournament teams on that seed line, and gold-backed badges mark former champs at title seed lines (March 2026 projection)",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_07_coach(field_table: pd.DataFrame, coach_primary_metric: str, out_path: Path) -> None:
    coach_slug = metric_slug(coach_primary_metric)
    plot_df = field_table.copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    seed_reference = (
        plot_df.groupby("projected_seed", as_index=False)[[f"full_seed_avg_{coach_slug}", f"recent_seed_avg_{coach_slug}"]]
        .first()
        .sort_values("projected_seed", kind="stable")
    )

    set_project_theme()
    fig, ax = plt.subplots(figsize=(22.4, 12.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=SECONDARY, linewidth=0.7, alpha=0.16, zorder=0)

    for row in seed_reference.itertuples(index=False):
        seed = float(row.projected_seed)
        ax.plot(
            [getattr(row, f"full_seed_avg_{coach_slug}"), getattr(row, f"full_seed_avg_{coach_slug}")],
            [seed - 0.26, seed + 0.26],
            color=ACCENT,
            linewidth=3.0,
            solid_capstyle="round",
            zorder=1,
        )
        ax.plot(
            [getattr(row, f"recent_seed_avg_{coach_slug}"), getattr(row, f"recent_seed_avg_{coach_slug}")],
            [seed - 0.18, seed + 0.18],
            color=SECONDARY,
            linewidth=2.1,
            alpha=0.78,
            solid_capstyle="round",
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(getattr(row, coach_primary_metric))
        y_value = float(row.plot_y)
        is_former_champion = bool(getattr(row, "is_former_champion", False))
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if is_former_champion:
                title_seeds = list(getattr(row, "title_seeds", []))
                title_coach_values = list(getattr(row, "title_coach_values", []))
                seed_counts: dict[int, int] = {}
                for title_seed, title_coach in zip(title_seeds, title_coach_values):
                    seed_value = int(title_seed)
                    title_x = float(title_coach)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    x_offset = -18 - ((seed_counts[seed_value] - 1) * 14)
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.075),
                        (title_x, float(seed_value)),
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
                OffsetImage(logo_image, zoom=0.11),
                (x_value, y_value),
                frameon=False,
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
                fontsize=7.0,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(facecolor="white", edgecolor=ACCENT),
                zorder=4,
            )
            if is_former_champion:
                title_seeds = list(getattr(row, "title_seeds", []))
                title_coach_values = list(getattr(row, "title_coach_values", []))
                seed_counts: dict[int, int] = {}
                for title_seed, title_coach in zip(title_seeds, title_coach_values):
                    seed_value = int(title_seed)
                    title_x = float(title_coach)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    ax.text(
                        title_x - 0.20 - ((seed_counts[seed_value] - 1) * 0.12),
                        float(seed_value),
                        "Champ",
                        ha="center",
                        va="center",
                        fontsize=6.2,
                        fontweight="bold",
                        color=LABEL_TEXT,
                        bbox=styled_box(facecolor="#F5E7A1", edgecolor="#D4A72C"),
                        zorder=3,
                    )

    x_values = np.concatenate(
        [
            plot_df[coach_primary_metric].to_numpy(dtype=float),
            seed_reference[f"full_seed_avg_{coach_slug}"].to_numpy(dtype=float),
            seed_reference[f"recent_seed_avg_{coach_slug}"].to_numpy(dtype=float),
        ]
    )
    x_min = float(np.nanmin(x_values))
    x_max = float(np.nanmax(x_values))
    x_padding = max((x_max - x_min) * 0.06, 0.6)
    ax.set_xlim(x_min - x_padding, x_max + x_padding)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(seed) for seed in range(1, 17)], fontsize=11, fontweight="semibold")
    ax.set_xlabel(metric_label(coach_primary_metric), fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        plt.Line2D([0], [0], color=ACCENT, linewidth=3.0, label="Full-sample seed avg"),
        plt.Line2D([0], [0], color=SECONDARY, linewidth=2.1, label="Recent seed avg"),
        plt.Line2D([0], [0], marker="o", linestyle="", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge (title seed)"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="lower right",
        bbox_to_anchor=(0.985, 0.18),
        frameon=True,
        facecolor=TEXT_DARK,
        edgecolor=ACCENT,
        fontsize=10.0,
        title="Series label",
        title_fontsize=10.4,
    )
    legend.get_frame().set_linewidth(1.15)
    legend.get_title().set_color(TEXT_LIGHT)
    for text in legend.get_texts():
        text.set_color(TEXT_LIGHT)

    fig.suptitle("Projected 2026 seeds and coach experience", fontsize=23, fontweight="bold", y=0.975)
    fig.text(
        0.5,
        0.915,
        f"Logos to the right of their seed markers are above the {COACH_ANALYSIS_START_YEAR}-2025 seed-line average on {metric_label(coach_primary_metric).lower()}; "
        "gold-backed badges mark former champs at title seed lines (March 2026 projection)",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_analysis_05_summary(path: Path, table: pd.DataFrame) -> None:
    best = table.loc[table["mean_overperformance"].idxmax()]
    worst = table.loc[table["mean_overperformance"].idxmin()]
    lines = [
        "# Experience Analysis 5: Combined Experience Archetypes",
        "",
        f"- Best archetype: `{best['archetype']}` at `{best['mean_overperformance']:+.3f}` mean finish vs exact-seed expectation.",
        f"- Worst archetype: `{worst['archetype']}` at `{worst['mean_overperformance']:+.3f}` mean finish vs exact-seed expectation.",
        f"- The best archetype also carried an average coach tournament win rate of `{best['avg_coach_win_pct_5y']:.3f}`.",
        f"- Seasons: `{COACH_ANALYSIS_START_YEAR}+` only, as in every coach analysis. The team-history cache starts in `{TEAM_HISTORY_START_YEAR}`, so 5-year coach counts before `{COACH_ANALYSIS_START_YEAR}` are left-censored; both seed baselines are averages over the same seasons.",
        "- Interpretation: each team is split at its exact seed line's average on both metrics; these are descriptive group means with no significance test.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_06_summary(path: Path, table: pd.DataFrame, coach_primary_metric: str) -> None:
    player_best = table.loc[table["player_seed_adjusted_rho"].idxmax()]
    coach_best = table.loc[table["coach_seed_adjusted_rho"].idxmax()]
    player_positive = int((table["player_seed_adjusted_rho"] > 0).sum())
    coach_positive = int((table["coach_seed_adjusted_rho"] > 0).sum())
    lines = [
        "# Experience Analysis 6: Experience by Year",
        "",
        f"- Player season-wide `Exp` rank edge was positive in `{player_positive}` of `{len(table)}` seasons, with the strongest year at `{int(player_best['year'])}` (`{player_best['player_seed_adjusted_rho']:+.3f}`).",
        f"- Coach experience (`{metric_label(coach_primary_metric)}`) was positive in `{coach_positive}` of `{int(table['coach_seed_adjusted_rho'].notna().sum())}` plotted seasons, with the strongest year at `{int(coach_best['year'])}` (`{coach_best['coach_seed_adjusted_rho']:+.3f}`).",
        "- Interpretation: the per-season rhos show how much each relationship moves from year to year; each rests on that season's `team_count` teams.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_07_summary(path: Path, field_table: pd.DataFrame, coach_primary_metric: str) -> None:
    most = field_table.sort_values(["full_exp_z", "barthag"], ascending=[False, False], kind="stable").head(5)
    least = field_table.sort_values(["full_exp_z", "barthag"], ascending=[True, False], kind="stable").head(5)
    position_counts = field_table["full_exp_position"].value_counts()
    tenure_cap = CURRENT_FIELD_YEAR - TEAM_HISTORY_START_YEAR + 1
    capped = field_table.loc[field_table["coach_years_with_program"] >= tenure_cap, "coach"].tolist()
    capped_text = f" ({', '.join(f'`{coach}`' for coach in capped)})" if capped else ""

    def format_team_list(table: pd.DataFrame) -> str:
        parts = []
        for row in table.itertuples(index=False):
            parts.append(
                f"`{row.team}` ({int(row.projected_seed)}) season-wide rank `{row.exp_rank_season:.0f}`, Exp. `{row.exp_raw:.2f}`, "
                f"`{row.full_exp_delta:+.1f}` ranks vs the seed-line average"
            )
        return "; ".join(parts)

    lines = [
        f"# Experience Analysis 7: Projected {CURRENT_FIELD_YEAR} Field vs Seed-Line Experience",
        "",
        f"The March {CURRENT_FIELD_YEAR} bracketology projection is joined to the `{CURRENT_FIELD_YEAR}` experience sample. Each team is "
        "compared with past tournament teams on the same seed line. This is a pre-tournament snapshot; it uses no "
        f"{CURRENT_FIELD_YEAR} results.",
        "",
        f"- Player metric: `{PLAYER_VISUAL_METRIC}` (`{player_visual_label()}`, 1 = most experienced), against `2008-2025` seed-line averages. "
        "`full_exp_delta` is the seed-line average rank minus the team's rank, so positive means more experienced than the seed line.",
        f"- Coach metric: `{coach_primary_metric}` (`{metric_label(coach_primary_metric)}`), against `{COACH_ANALYSIS_START_YEAR}-2025` seed-line averages.",
        "- Positions are `above`, `near` or `below baseline` at 0.35 standard deviations from the seed line's average.",
        f"- `coach_years_with_program` is shown for reference only. It is counted from `{TEAM_HISTORY_START_YEAR}`, the first season "
        f"in the team-history cache, so it tops out at `{tenure_cap}` in `{CURRENT_FIELD_YEAR}`, and a `{tenure_cap}`{capped_text} means at least `{tenure_cap}` seasons.",
        f"- Most experienced relative to their seed line (by z-score): {format_team_list(most)}.",
        f"- Least experienced relative to their seed line (by z-score): {format_team_list(least)}.",
        f"- Player position counts: `{int(position_counts.get('above baseline', 0))}` above, "
        f"`{int(position_counts.get('near baseline', 0))}` near, `{int(position_counts.get('below baseline', 0))}` below.",
        "",
        "## Table",
        "",
        field_table[
            [
                "projected_seed",
                "team",
                "conf",
                "barthag",
                PLAYER_PRIMARY_METRIC,
                PLAYER_SUPPORT_METRIC,
                "coach",
                "coach_tourn_games_5y",
                "coach_years_with_program",
                COACH_SUPPORT_METRIC,
                "full_exp_delta",
                "recent_exp_delta",
                "full_coach_delta",
                "recent_coach_delta",
                "full_exp_position",
                "full_coach_position",
            ]
        ].round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_experience_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    coach_tournament_df = filter_coach_analysis(tournament_df)
    coach_primary_metric, _ = load_or_compute_coach_metric_selection(coach_tournament_df, paths["metric_selection"])
    over_df = build_overperformance_data(tournament_df)
    coach_over_df = build_overperformance_data(coach_tournament_df)

    # The coach baseline is censored before COACH_ANALYSIS_START_YEAR, so the archetypes use the same
    # seasons as every other coach analysis, with both seed baselines rebuilt on them.
    table_05 = build_analysis_05_table(coach_over_df, coach_primary_metric)
    table_06 = build_analysis_06_table(over_df, coach_over_df, coach_primary_metric)
    table_07 = build_analysis_07_table(full_df, tournament_df, coach_tournament_df, coach_primary_metric, paths["seeds_2026"], paths["logo_cache"])

    table_05.round(6).to_csv(paths["csv_05"], index=False)
    table_06.round(6).to_csv(paths["csv_06"], index=False)
    with_repo_relative_logo_paths(table_07).round(6).to_csv(paths["csv_07"], index=False)

    plot_analysis_05(table_05, paths["fig_05"])
    plot_analysis_06(table_06, coach_primary_metric, paths["fig_06"])
    plot_single_analysis_06(
        table_06,
        "player_seed_adjusted_rho",
        "Player Exp rank vs overperformance by year",
        "Seed-adjusted player Exp rank edge relationship with finish versus exact-seed expectation",
        paths["fig_06_player"],
    )
    plot_single_analysis_06(
        table_06,
        "coach_seed_adjusted_rho",
        f"{metric_label(coach_primary_metric)} vs overperformance by year",
        f"Seed-adjusted coach-experience relationship with finish versus exact-seed expectation using {COACH_ANALYSIS_START_YEAR}+ seasons",
        paths["fig_06_coach"],
    )
    plot_analysis_07(table_07, coach_primary_metric, paths["fig_07"])
    plot_analysis_07_coach(table_07, coach_primary_metric, paths["fig_07_coach"])

    write_analysis_05_summary(paths["md_05"], table_05)
    write_analysis_06_summary(paths["md_06"], table_06, coach_primary_metric)
    write_analysis_07_summary(paths["md_07"], table_07, coach_primary_metric)

    print(f"Coach primary metric: {coach_primary_metric}")
    for key in ["fig_05", "fig_06", "fig_07", "md_05", "md_06", "md_07", "csv_05", "csv_06", "csv_07"]:
        print(f"Wrote {key}: {paths[key]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
