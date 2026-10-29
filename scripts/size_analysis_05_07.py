#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.lines import Line2D
from matplotlib.offsetbox import AnnotationBbox, OffsetImage

from size_analysis_common import (
    ACCENT,
    BIG_MAN_FIGURE_NOTE,
    BIG_MAN_OUTCOME_NOTE,
    CURRENT_FIELD_YEAR,
    HIGHLIGHT,
    LABEL_TEXT,
    NEUTRAL,
    NEGATIVE,
    POSITIVE,
    SECONDARY,
    TEXT_DARK,
    TEXT_LIGHT,
    apply_report_background,
    assign_era,
    build_overperformance_data,
    build_seed_group_carryover_table,
    build_seed_reference_tables,
    cache_logo_images,
    get_size_base_paths,
    load_logo_image,
    load_or_compute_metric_selection,
    load_projected_field,
    load_size_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    normalize_team_key,
    set_project_theme,
    styled_box,
)
from size_big_man_over_time import build_table as build_big_man_over_time_table, plot_table as plot_big_man_over_time, write_summary as write_big_man_over_time_summary

ARCHETYPE_ORDER = ["above seed baseline", "below seed baseline"]
BIG_MAN_METRIC = "max_c_porpag"
DOMINANT_BIG_MAN_THRESHOLD = 4.0


def get_paths() -> dict[str, Path]:
    base = get_size_base_paths()
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": base["logo_cache"],
        "fig_05a": project_root / "reports" / "figures" / "size_big_man_signal.png",
        "fig_05": project_root / "reports" / "figures" / "size_archetypes_vs_overperformance.png",
        "fig_06": project_root / "reports" / "figures" / "size_era_comparison.png",
        "fig_07": project_root / "reports" / "figures" / "size_2026_field.png",
        "fig_07_big_man": project_root / "reports" / "figures" / "size_2026_big_man.png",
        "csv_05a": project_root / "reports" / "size_big_man_signal_table.csv",
        "csv_05": project_root / "reports" / "size_archetypes_vs_overperformance_table.csv",
        "csv_06": project_root / "reports" / "size_era_comparison_table.csv",
        "csv_07": project_root / "reports" / "size_2026_field_table.csv",
        "csv_07_big_man": project_root / "reports" / "size_2026_big_man_table.csv",
        "md_05a": project_root / "reports" / "size_big_man_signal_summary.md",
        "md_05": project_root / "reports" / "size_archetypes_vs_overperformance_summary.md",
        "md_06": project_root / "reports" / "size_era_comparison_summary.md",
        "md_07": project_root / "reports" / "size_2026_field_summary.md",
        "md_07_big_man": project_root / "reports" / "size_2026_big_man_summary.md",
        "fig_07_guard_height": project_root / "reports" / "figures" / "size_2026_guard_height.png",
        "csv_07_guard_height": project_root / "reports" / "size_2026_guard_height_table.csv",
        "guard_height_input": project_root / "data" / "processed" / "team_avg_top3_guard_height_2026.csv",
    }


def build_big_man_signal_table(over_df: pd.DataFrame) -> tuple[pd.DataFrame, float]:
    if BIG_MAN_METRIC not in over_df.columns:
        raise ValueError(f"Missing `{BIG_MAN_METRIC}` in overperformance dataset")

    work = over_df.copy()
    work["dominant_big_man"] = work[BIG_MAN_METRIC] >= DOMINANT_BIG_MAN_THRESHOLD
    raw_rho = float(work["finish_depth"].corr(work[BIG_MAN_METRIC], method="spearman"))

    rows: list[dict[str, object]] = []

    def summarize(segment: str, group: pd.DataFrame) -> None:
        dominant = group[group["dominant_big_man"]].copy()
        other = group[~group["dominant_big_man"]].copy()
        rows.append(
            {
                "segment": segment,
                "threshold": DOMINANT_BIG_MAN_THRESHOLD,
                "dominant_team_count": int(len(dominant)),
                "other_team_count": int(len(other)),
                "dominant_share": float(len(dominant) / len(group)) if len(group) else np.nan,
                "dominant_mean_overperformance": float(dominant["overperformance"].mean()) if len(dominant) else np.nan,
                "other_mean_overperformance": float(other["overperformance"].mean()) if len(other) else np.nan,
                "overperformance_gap": float(dominant["overperformance"].mean() - other["overperformance"].mean())
                if len(dominant) and len(other)
                else np.nan,
                "dominant_adv_rate": float((dominant["overperformance"] > 0).mean()) if len(dominant) else np.nan,
                "other_adv_rate": float((other["overperformance"] > 0).mean()) if len(other) else np.nan,
                "adv_rate_gap": float((dominant["overperformance"] > 0).mean() - (other["overperformance"] > 0).mean())
                if len(dominant) and len(other)
                else np.nan,
            }
        )

    summarize("Overall", work)
    for seed_group, group in work.groupby("seed_group", sort=False):
        summarize(str(seed_group), group)

    order = ["Overall", "1-2", "3-4", "5-6", "7-8", "9-10", "11-13", "14-16"]
    table = pd.DataFrame(rows)
    table["segment"] = pd.Categorical(table["segment"], categories=order, ordered=True)
    table = table.sort_values("segment", kind="stable").reset_index(drop=True)
    return table, raw_rho


def plot_big_man_signal(big_man_table: pd.DataFrame, out_path: Path) -> None:
    plot_df = big_man_table.copy()
    plot_df["y"] = np.arange(len(plot_df))[::-1]

    def choose_color(row: pd.Series) -> str:
        segment = str(row["segment"])
        if segment == "Overall":
            return ACCENT
        if float(row["overperformance_gap"]) < 0:
            return NEGATIVE
        return NEUTRAL

    plot_df["color"] = plot_df.apply(choose_color, axis=1)
    band_only = plot_df[plot_df["segment"] != "Overall"].copy()
    strongest_positive = band_only.loc[band_only["overperformance_gap"].idxmax()]
    strongest_negative = band_only.loc[band_only["overperformance_gap"].idxmin()]
    overall = plot_df.loc[plot_df["segment"] == "Overall"].iloc[0]

    set_project_theme()
    fig, ax = plt.subplots(figsize=(14.4, 8.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.20, right=0.95)

    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.8, zorder=0)
    x_min = float(plot_df["overperformance_gap"].min())
    x_max = float(plot_df["overperformance_gap"].max())
    x_pad = max((x_max - x_min) * 0.16, 0.05)

    for row in plot_df.itertuples(index=False):
        value = float(row.overperformance_gap)
        y_value = float(row.y)
        color = str(row.color)
        n_total = int(row.dominant_team_count) + int(row.other_team_count)
        ax.hlines(
            y=y_value,
            xmin=min(0.0, value),
            xmax=max(0.0, value),
            color=color,
            linewidth=3.2,
            alpha=0.96,
            zorder=1,
        )
        ax.scatter(
            value,
            y_value,
            s=210,
            color=color,
            alpha=0.98,
            zorder=3,
        )
        ax.text(
            value + (0.01 if value >= 0 else -0.01),
            y_value,
            f"{value:+.2f}  n={n_total}",
            ha="left" if value >= 0 else "right",
            va="center",
            fontsize=10.4,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    ax.set_yticks(plot_df["y"])
    ax.set_yticklabels(plot_df["segment"].astype(str).tolist(), fontsize=10.8, fontweight="semibold")
    ax.set_xlabel(
        "Overperformance gap (dominant big team vs rest)",
        fontsize=11.4,
        fontweight="semibold",
    )
    ax.set_xlim(x_min - x_pad, x_max + x_pad)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10.5, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=8)

    n_overall = int(overall["dominant_team_count"]) + int(overall["other_team_count"])
    ax.text(
        0.03,
        0.96,
        f"Overall: {overall['dominant_mean_overperformance']:+.2f} vs {overall['other_mean_overperformance']:+.2f}  (n={n_overall})\nAdvancer-rate gap: {overall['adv_rate_gap']:+.1%}",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.2,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(facecolor="white", edgecolor=ACCENT, alpha=1.0),
    )

    ax.text(
        strongest_positive["overperformance_gap"] + 0.01,
        float(strongest_positive["y"]) + 0.38,
        f"Largest gap: {strongest_positive['segment']}",
        ha="left",
        va="bottom",
        fontsize=9.9,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax.text(
        strongest_negative["overperformance_gap"] - 0.01,
        float(strongest_negative["y"]) - 0.38,
        f"Smallest gap: {strongest_negative['segment']}",
        ha="right",
        va="top",
        fontsize=9.9,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    fig.suptitle("Dominant big-team split by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        f"A dominant big team is one whose top C has PORPAG >= {DOMINANT_BIG_MAN_THRESHOLD:.1f}; values show average finish versus seed expectation.\n"
        f"{BIG_MAN_FIGURE_NOTE}; descriptive only, no significance test.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_05_table(over_df: pd.DataFrame, primary_metric: str) -> pd.DataFrame:
    primary_col = f"{metric_slug(primary_metric)}_vs_seed"
    table = over_df.copy()
    table["archetype"] = np.where(table[primary_col] > 0, "above seed baseline", "below seed baseline")
    archetype_table = (
        table.groupby("archetype", as_index=False)
        .agg(
            team_count=("Team", "size"),
            mean_overperformance=("overperformance", "mean"),
            median_overperformance=("overperformance", "median"),
            primary_vs_seed_mean=(primary_col, "mean"),
        )
    )
    archetype_table["archetype"] = pd.Categorical(archetype_table["archetype"], categories=ARCHETYPE_ORDER, ordered=True)
    return archetype_table.sort_values("archetype", kind="stable").reset_index(drop=True)


def plot_analysis_05(archetype_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    best_row = archetype_table.loc[archetype_table["mean_overperformance"].idxmax()]
    worst_row = archetype_table.loc[archetype_table["mean_overperformance"].idxmin()]
    title = f"Overperformance by {metric_label(primary_metric).lower()} seed baseline"

    plot_df = archetype_table.copy()
    plot_df["y"] = np.arange(len(plot_df))[::-1]
    colors = {
        "above seed baseline": POSITIVE,
        "below seed baseline": ACCENT,
    }

    set_project_theme()
    fig, ax = plt.subplots(figsize=(14.2, 8.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.14, left=0.18, right=0.95)

    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    x_min = float(plot_df["mean_overperformance"].min())
    x_max = float(plot_df["mean_overperformance"].max())
    x_pad = max((x_max - x_min) * 0.16, 0.01)
    for row in plot_df.itertuples(index=False):
        archetype = str(row.archetype)
        value = float(row.mean_overperformance)
        y_value = float(row.y)
        color = colors[archetype]
        ax.hlines(
            y=y_value,
            xmin=min(0.0, value),
            xmax=max(0.0, value),
            color=color,
            linewidth=3.0,
            alpha=0.95,
            zorder=1,
        )
        ax.scatter(
            value,
            y_value,
            s=220,
            color=color,
            alpha=0.95,
            zorder=3,
        )
        ax.text(
            value + (0.002 if value >= 0 else -0.002),
            y_value,
            f"{value:+.2f} | n={int(row.team_count)}",
            ha="left" if value >= 0 else "right",
            va="center",
            fontsize=10.2,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    ax.set_yticks(plot_df["y"])
    ax.set_yticklabels(
        [str(value).title() for value in plot_df["archetype"].tolist()],
        fontsize=10.7,
        fontweight="semibold",
    )
    ax.set_xlabel("Mean finish - exact-seed expectation", fontsize=11.5, fontweight="semibold")
    ax.set_xlim(x_min - x_pad, x_max + x_pad)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10.5, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=8)
    ax.text(
        0.03,
        0.95,
        f"Best: {str(best_row['archetype']).title()} ({best_row['mean_overperformance']:+.2f})\nWorst: {str(worst_row['archetype']).title()} ({worst_row['mean_overperformance']:+.2f})",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.3,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    fig.suptitle(title, fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        f"Teams are split only by whether `{primary_metric}` sits above or below the exact-seed baseline",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_06_table(over_df: pd.DataFrame, primary_metric: str) -> pd.DataFrame:
    metric_vs_seed_col = f"{metric_slug(primary_metric)}_vs_seed"
    rows: list[dict[str, object]] = []
    for year, group in over_df.groupby("year", sort=True):
        rows.append(
            {
                "year": int(year),
                "team_count": int(len(group)),
                "raw_rho": float(group["finish_depth"].corr(group[primary_metric], method="spearman")),
                "seed_adjusted_rho": float(group["overperformance"].corr(group[metric_vs_seed_col], method="spearman")),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_06(year_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    x = year_table["year"].to_numpy(dtype=int)
    raw_values = year_table["raw_rho"].to_numpy(dtype=float)
    seed_values = year_table["seed_adjusted_rho"].to_numpy(dtype=float)
    title = f"{metric_label(primary_metric)} and tournament success by year"

    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(14.6, 9.6), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.09, left=0.12, right=0.95, hspace=0.36)

    for ax, values, panel_title in [
        (axes[0], raw_values, "Pooled raw size vs finish"),
        (axes[1], seed_values, "Exact-seed-adjusted size vs overperformance"),
    ]:
        ax.plot(x, values, color=ACCENT, linewidth=2.6, marker="o", markersize=7)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
        mean_value = float(np.mean(values))
        ax.axhline(mean_value, color=TEXT_LIGHT, linestyle="--", linewidth=1.2, alpha=0.95)
        ax.axvline(2022, color=SECONDARY, linestyle=":", linewidth=1.8)
        y_min = float(np.min(values))
        y_max = float(np.max(values))
        y_pad = max((y_max - y_min) * 0.28, 0.025)
        ax.set_ylim(y_min - y_pad, y_max + y_pad)
        ax.set_title(panel_title, fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=12)
        ax.set_xticks(x)
        ax.set_xticklabels([str(value) for value in x], fontsize=10.2, fontweight="semibold")
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", labelsize=10.2, length=0, pad=7)
        x_label_pos = float(x.max()) + 0.1
        ax.text(
            x_label_pos,
            mean_value,
            "Avg rho",
            ha="left",
            va="center",
            fontsize=10.0,
            fontweight="semibold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    axes[1].set_xlabel("Season", fontsize=11.5, fontweight="semibold")
    axes[0].set_ylabel("Spearman rho", fontsize=11.5, fontweight="semibold")
    axes[1].set_ylabel("Spearman rho", fontsize=11.5, fontweight="semibold")

    fig.suptitle(title, fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        "Top: raw relationship by season. Bottom: seed-adjusted relationship by season.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_07_table(
    full_df: pd.DataFrame,
    tournament_df: pd.DataFrame,
    primary_metric: str,
    seeds_path: Path,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df)
    if len(field) != 68:
        raise ValueError(f"Expected 68 projected 2026 teams after join; found {len(field)}")

    full_reference, recent_reference = build_seed_reference_tables(tournament_df, [primary_metric])
    group_table = build_seed_group_carryover_table(tournament_df, primary_metric)
    merged = field.merge(full_reference, on="projected_seed", how="left").merge(
        recent_reference,
        on="projected_seed",
        how="left",
    )
    merged["seed_group"] = merged["projected_seed"].map(
        {
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
    )
    merged = merged.merge(group_table, on="seed_group", how="left")

    primary_slug = metric_slug(primary_metric)
    merged["full_primary_delta"] = merged[primary_metric] - merged[f"full_seed_avg_{primary_slug}"]
    merged["recent_primary_delta"] = merged[primary_metric] - merged[f"recent_seed_avg_{primary_slug}"]
    merged["full_primary_z"] = merged["full_primary_delta"] / merged[f"full_seed_std_{primary_slug}"].replace(0, np.nan)
    merged["recent_primary_z"] = merged["recent_primary_delta"] / merged[f"recent_seed_std_{primary_slug}"].replace(0, np.nan)

    def classify_position(z_score: float, threshold: float = 0.35) -> str:
        if pd.isna(z_score):
            return "near baseline"
        if z_score >= threshold:
            return "above baseline"
        if z_score <= -threshold:
            return "below baseline"
        return "near baseline"

    merged["full_primary_position"] = merged["full_primary_z"].apply(classify_position)
    merged["recent_primary_position"] = merged["recent_primary_z"].apply(classify_position)
    def classify_team(row: pd.Series) -> tuple[str, str]:
        high_use = row.get("seed_action_tier") == "High-use pocket"
        secondary_use = row.get("seed_action_tier") == "Secondary-use pocket"
        full_positive = row["full_primary_position"] == "above baseline"
        full_negative = row["full_primary_position"] == "below baseline"
        recent_positive = row["recent_primary_position"] == "above baseline"
        recent_negative = row["recent_primary_position"] == "below baseline"

        if full_positive and high_use:
            return "positive carryover", "stronger carryover case"
        if full_positive and secondary_use:
            return "positive carryover", "some support from recent trends" if recent_positive else "mixed evidence"
        if full_negative and high_use:
            return "negative carryover", "weak carryover case"
        if full_negative and secondary_use:
            return "negative carryover", "weak carryover case" if recent_negative else "mixed evidence"
        if full_positive and recent_positive:
            return "positive carryover", "some support from recent trends"
        if full_positive:
            return "cautious positive", "mixed evidence"
        if full_negative:
            return "cautious negative", "mixed evidence"
        if recent_positive:
            return "cautious positive", "some support from recent trends"
        if recent_negative:
            return "cautious negative", "some support from recent trends"
        return "mixed", "mixed evidence"

    merged[["carryover_direction", "carryover_confidence_note"]] = merged.apply(
        lambda row: pd.Series(classify_team(row)),
        axis=1,
    )
    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", ["Team", "year", "seed_int"]]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )
    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_pairs = [(int(row.year), int(row.seed_int)) for row in group.itertuples(index=False)]
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_pairs)),
                "last_title_year": int(title_pairs[-1][0]),
                "last_title_seed": int(title_pairs[-1][1]),
                "title_years": [year for year, _ in title_pairs],
                "title_seeds": [seed for _, seed in title_pairs],
                "title_history": "; ".join([f"{year} ({seed})" for year, seed in title_pairs]),
            }
        )
    champion_history = pd.DataFrame(champion_history_rows)
    merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged["championship_count"].fillna(0).astype(int)
    merged["last_title_seed"] = merged["last_title_seed"].astype("Int64")
    merged["title_years"] = merged["title_years"].apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged["title_seeds"].apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged["title_history"].fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(
        ["projected_seed", primary_metric, "barthag"],
        ascending=[True, False, False],
        kind="stable",
    ).reset_index(drop=True)
    return merged


def build_analysis_07_big_man_table(
    full_df: pd.DataFrame,
    tournament_df: pd.DataFrame,
    seeds_path: Path,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df)
    if BIG_MAN_METRIC not in field.columns:
        raise ValueError(f"`{BIG_MAN_METRIC}` is missing from the projected field join")
    if len(field) != 68:
        raise ValueError(f"Expected 68 projected 2026 teams after join; found {len(field)}")

    # The 2026 values come from a pre-tournament player export, while every 2008-2025 value includes that
    # team's NCAA tournament games, so the field is not compared with historical seed-line averages.
    merged = field.copy()
    merged["seed_group"] = merged["projected_seed"].map(
        {
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
    )
    merged["dominant_big_man"] = merged[BIG_MAN_METRIC] >= DOMINANT_BIG_MAN_THRESHOLD
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(
        ["projected_seed", BIG_MAN_METRIC, "barthag"],
        ascending=[True, False, False],
        kind="stable",
    ).reset_index(drop=True)
    return merged


def plot_analysis_07(field_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    primary_slug = metric_slug(primary_metric)
    plot_df = field_table.copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    seed_reference = (
        plot_df.groupby("projected_seed", as_index=False)[[f"full_seed_avg_{primary_slug}", f"recent_seed_avg_{primary_slug}"]]
        .first()
        .sort_values("projected_seed", kind="stable")
    )

    set_project_theme()
    fig, ax = plt.subplots(figsize=(22.8, 12.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=SECONDARY, linewidth=0.7, alpha=0.16, zorder=0)

    for row in seed_reference.itertuples(index=False):
        seed = float(row.projected_seed)
        ax.plot(
            [getattr(row, f"full_seed_avg_{primary_slug}"), getattr(row, f"full_seed_avg_{primary_slug}")],
            [seed - 0.26, seed + 0.26],
            color=ACCENT,
            linewidth=3.0,
            solid_capstyle="round",
            zorder=1,
        )
        ax.plot(
            [getattr(row, f"recent_seed_avg_{primary_slug}"), getattr(row, f"recent_seed_avg_{primary_slug}")],
            [seed - 0.18, seed + 0.18],
            color=SECONDARY,
            linewidth=2.1,
            alpha=0.78,
            solid_capstyle="round",
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(getattr(row, primary_metric))
        y_value = float(row.plot_y)
        logo_path = getattr(row, "logo_path", None)
        is_former_champion = bool(getattr(row, "is_former_champion", False))
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if is_former_champion:
                title_seeds = list(getattr(row, "title_seeds", []))
                seed_counts: dict[int, int] = {}
                for title_seed in title_seeds:
                    seed_value = int(title_seed)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    x_offset = -18 - ((seed_counts[seed_value] - 1) * 14)
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.075),
                        (x_value, float(seed_value)),
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
                fontsize=7.2,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(facecolor="white", edgecolor=ACCENT),
                zorder=4,
            )
            if is_former_champion:
                title_seeds = list(getattr(row, "title_seeds", []))
                seed_counts: dict[int, int] = {}
                for title_seed in title_seeds:
                    seed_value = int(title_seed)
                    seed_counts[seed_value] = seed_counts.get(seed_value, 0) + 1
                    ax.text(
                        x_value - 0.26 - ((seed_counts[seed_value] - 1) * 0.18),
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
            plot_df[primary_metric].to_numpy(dtype=float),
            seed_reference[f"full_seed_avg_{primary_slug}"].to_numpy(dtype=float),
            seed_reference[f"recent_seed_avg_{primary_slug}"].to_numpy(dtype=float),
        ]
    )
    x_min = float(np.nanmin(x_values))
    x_max = float(np.nanmax(x_values))
    x_padding = max((x_max - x_min) * 0.06, 0.25)
    ax.set_xlim(x_min - x_padding, x_max + x_padding)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(seed) for seed in range(1, 17)], fontsize=11, fontweight="semibold")
    ax.set_xlabel(metric_label(primary_metric), fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        Line2D([], [], color=ACCENT, marker="|", linestyle="None", markersize=16, markeredgewidth=3, label="Full-sample seed avg"),
        Line2D([], [], color=SECONDARY, marker="|", linestyle="None", markersize=14, markeredgewidth=2.2, label="Recent seed avg"),
        Line2D(
            [],
            [],
            color="none",
            marker="o",
            linestyle="None",
            markersize=11,
            markerfacecolor="#F5E7A1",
            markeredgecolor="#D4A72C",
            markeredgewidth=1.3,
            label="Former champ badge (title seed)",
        ),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=TEXT_DARK,
        edgecolor=ACCENT,
        fontsize=9.5,
        title="Seed reference",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.1)
    legend_text_color = TEXT_LIGHT
    legend.get_title().set_color(legend_text_color)
    for text in legend.get_texts():
        text.set_color(legend_text_color)

    fig.suptitle(
        f"Projected {CURRENT_FIELD_YEAR} seed lines vs. {metric_label(primary_metric).lower()}",
        fontsize=22,
        fontweight="bold",
        y=0.965,
    )
    fig.text(
        0.5,
        0.92,
        "Logos to the right of their seed markers are above that seed line's typical EFFHGT; smaller gold-backed badges sit on each former champ's most recent title seed line",
        ha="center",
        va="center",
        fontsize=10.2,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_07_big_man(field_table: pd.DataFrame, out_path: Path) -> None:
    plot_df = field_table.copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    set_project_theme()
    fig, ax = plt.subplots(figsize=(22.8, 12.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=SECONDARY, linewidth=0.7, alpha=0.16, zorder=0)

    ax.axvline(DOMINANT_BIG_MAN_THRESHOLD, color=HIGHLIGHT, linewidth=2.2, linestyle="--", zorder=1)
    ax.text(
        DOMINANT_BIG_MAN_THRESHOLD,
        1.01,
        "Dominant big-team threshold",
        transform=ax.get_xaxis_transform(),
        ha="center",
        va="bottom",
        fontsize=10.0,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(facecolor="white", edgecolor=HIGHLIGHT),
    )

    for row in plot_df.itertuples(index=False):
        x_value = float(getattr(row, BIG_MAN_METRIC))
        y_value = float(row.plot_y)
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            artist = AnnotationBbox(
                OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.11),
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
                fontsize=7.2,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(facecolor="white", edgecolor=ACCENT),
                zorder=4,
            )

    x_values = np.concatenate(
        [
            plot_df[BIG_MAN_METRIC].to_numpy(dtype=float),
            np.array([DOMINANT_BIG_MAN_THRESHOLD], dtype=float),
        ]
    )
    x_min = float(np.nanmin(x_values))
    x_max = float(np.nanmax(x_values))
    x_padding = max((x_max - x_min) * 0.06, 0.25)
    ax.set_xlim(x_min - x_padding, x_max + x_padding)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(seed) for seed in range(1, 17)], fontsize=11, fontweight="semibold")
    ax.set_xlabel(metric_label(BIG_MAN_METRIC), fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        Line2D([], [], color=HIGHLIGHT, linestyle="--", linewidth=2.2, label="Dominant threshold"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=TEXT_DARK,
        edgecolor=ACCENT,
        fontsize=9.5,
    )
    legend.get_frame().set_linewidth(1.1)
    for text in legend.get_texts():
        text.set_color(TEXT_LIGHT)

    fig.suptitle(
        f"Projected {CURRENT_FIELD_YEAR} field: top-center PORPAG by seed line",
        fontsize=22,
        fontweight="bold",
        y=0.965,
    )
    fig.text(
        0.5,
        0.92,
        "Dashed line is the dominant-big-team threshold. 2026 values come from a pre-tournament player export; "
        "earlier seasons include NCAA tournament games, so no historical seed-line average is drawn.",
        ha="center",
        va="center",
        fontsize=10.2,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_guard_height_field_table(
    field_table: pd.DataFrame,
    guard_height_csv: Path,
) -> pd.DataFrame:
    """Merge avg-top-3-guard-height onto the existing 2026 field table."""
    gh = pd.read_csv(guard_height_csv)
    gh = gh.rename(columns={"avg_top3_guard_height_in": "guard_height_in"})
    merged = field_table.merge(gh[["team", "guard_height_in"]], on="team", how="left")
    return merged


def plot_guard_height_field(field_table: pd.DataFrame, out_path: Path) -> None:
    """Scatter: projected seed (y) vs avg top-3 non-big height (x), with team logos."""
    METRIC = "guard_height_in"
    plot_df = field_table.dropna(subset=[METRIC]).copy()
    plot_df["plot_y"] = plot_df["projected_seed"].astype(float)
    for seed, idx in plot_df.groupby("projected_seed", sort=True).groups.items():
        indices = list(idx)
        offsets = np.linspace(-0.24, 0.24, len(indices)) if len(indices) > 1 else np.array([0.0])
        for position, offset in zip(indices, offsets):
            plot_df.loc[position, "plot_y"] = float(seed) + float(offset)

    seed_avg = plot_df.groupby("projected_seed", as_index=False)[METRIC].mean()
    seed_avg.columns = ["projected_seed", "seed_avg_guard_height"]

    set_project_theme()
    fig, ax = plt.subplots(figsize=(22.8, 12.4), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.97)

    for seed in range(1, 17):
        ax.axhline(seed, color=SECONDARY, linewidth=0.7, alpha=0.16, zorder=0)

    for row in seed_avg.itertuples(index=False):
        seed = float(row.projected_seed)
        ax.plot(
            [row.seed_avg_guard_height, row.seed_avg_guard_height],
            [seed - 0.26, seed + 0.26],
            color=ACCENT,
            linewidth=3.0,
            solid_capstyle="round",
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(getattr(row, METRIC))
        y_value = float(row.plot_y)
        logo_path = getattr(row, "logo_path", None)
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            artist = AnnotationBbox(
                OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.11),
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
                fontsize=7.2,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(facecolor="white", edgecolor=ACCENT),
                zorder=4,
            )

    all_x = plot_df[METRIC].to_numpy(dtype=float)
    x_min, x_max = float(np.nanmin(all_x)), float(np.nanmax(all_x))
    x_pad = max((x_max - x_min) * 0.06, 0.5)
    ax.set_xlim(x_min - x_pad, x_max + x_pad)
    ax.set_ylim(16.55, 0.45)
    ax.set_yticks(np.arange(1, 17))
    ax.set_yticklabels([str(s) for s in range(1, 17)], fontsize=11, fontweight="semibold")

    def inches_to_ft_in(value: float) -> str:
        total = int(round(value))
        return f"{total // 12}-{total % 12}"

    ax.xaxis.set_major_formatter(plt.FuncFormatter(lambda v, _: inches_to_ft_in(v)))
    ax.set_xlabel("Avg height (top-3 non-big by min)", fontsize=12, fontweight="semibold")
    ax.set_ylabel("Projected seed", fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)

    legend_handles = [
        Line2D([], [], color=ACCENT, marker="|", linestyle="None", markersize=16, markeredgewidth=3, label="Seed-line avg"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=TEXT_DARK,
        edgecolor=ACCENT,
        fontsize=9.5,
        title="Reference",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.1)
    legend.get_title().set_color(TEXT_LIGHT)
    for text in legend.get_texts():
        text.set_color(TEXT_LIGHT)

    fig.suptitle(
        f"Projected {CURRENT_FIELD_YEAR} field: perimeter height (top-3 non-big by minutes)",
        fontsize=22,
        fontweight="bold",
        y=0.965,
    )
    fig.text(
        0.5,
        0.92,
        "Logos to the right of the seed marker have taller perimeter players than that seed line's average",
        ha="center",
        va="center",
        fontsize=10.2,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_big_man_signal_md(path: Path, big_man_table: pd.DataFrame, raw_rho: float) -> None:
    overall = big_man_table.loc[big_man_table["segment"] == "Overall"].iloc[0]
    band_only = big_man_table[big_man_table["segment"] != "Overall"].copy()
    largest_gap = band_only.loc[band_only["overperformance_gap"].idxmax()]
    smallest_gap = band_only.loc[band_only["overperformance_gap"].idxmin()]
    lines = [
        "# Size Analysis 5A: Dominant Big-Man Split",
        "",
        f"- Outcome contamination: {BIG_MAN_OUTCOME_NOTE}",
        f"- Dominant big-team threshold: `{BIG_MAN_METRIC} >= {DOMINANT_BIG_MAN_THRESHOLD:.1f}`.",
        f"- Raw pooled relationship with finish depth: `{raw_rho:+.3f}` Spearman rho. Deeper runs add tournament "
        "games to the player stats, so part of this is the outcome measuring itself.",
        f"- Overall exact-seed split: dominant big teams average `{overall['dominant_mean_overperformance']:+.3f}` vs `{overall['other_mean_overperformance']:+.3f}` for the rest of the field.",
        f"- Seed-band gaps run from `{smallest_gap['overperformance_gap']:+.3f}` (`{smallest_gap['segment']}`) to "
        f"`{largest_gap['overperformance_gap']:+.3f}` (`{largest_gap['segment']}`), on "
        f"`{int(band_only['dominant_team_count'].min())}`-`{int(band_only['dominant_team_count'].max())}` dominant teams per band. "
        "No significance test is applied.",
        "",
        "## Table",
        "",
        big_man_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_05_md(path: Path, archetype_table: pd.DataFrame, primary_metric: str) -> None:
    best = archetype_table.loc[archetype_table["mean_overperformance"].idxmax()]
    worst = archetype_table.loc[archetype_table["mean_overperformance"].idxmin()]
    lines = [
        "# Size Analysis 5: Size Archetypes vs Overperformance",
        "",
        f"- Metric used here: `{primary_metric}` (`{metric_label(primary_metric)}`).",
        f"- Best archetype: `{best['archetype']}` at `{best['mean_overperformance']:+.3f}` mean finish vs exact-seed expectation.",
        f"- Worst archetype: `{worst['archetype']}` at `{worst['mean_overperformance']:+.3f}` mean finish vs exact-seed expectation.",
        "- Interpretation: size is most actionable when it shows up as a seed-relative archetype rather than as a raw top-to-bottom ranking.",
        "",
        "## Table",
        "",
        archetype_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_06_md(path: Path, year_table: pd.DataFrame, primary_metric: str) -> None:
    strongest_raw = year_table.loc[year_table["raw_rho"].idxmax()]
    strongest_adjusted = year_table.loc[year_table["seed_adjusted_rho"].idxmax()]
    weakest_adjusted = year_table.loc[year_table["seed_adjusted_rho"].idxmin()]
    lines = [
        "# Size Analysis 6: Year-by-Year Comparison",
        "",
        f"- Primary metric: `{primary_metric}` (`{metric_label(primary_metric)}`).",
        f"- Strongest raw year: `{int(strongest_raw['year'])}` at `{strongest_raw['raw_rho']:+.3f}`.",
        f"- Strongest exact-seed-adjusted year: `{int(strongest_adjusted['year'])}` at `{strongest_adjusted['seed_adjusted_rho']:+.3f}`.",
        f"- Weakest exact-seed-adjusted year: `{int(weakest_adjusted['year'])}` at `{weakest_adjusted['seed_adjusted_rho']:+.3f}`.",
        "- Interpretation: the year view shows whether size behaves consistently over time; any modern bump should still be treated as a qualifier, not a replacement for the full sample.",
        "",
        "## Table",
        "",
        year_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_07_md(
    path: Path,
    field_table: pd.DataFrame,
    tournament_df: pd.DataFrame,
    over_df: pd.DataFrame,
    primary_metric: str,
) -> None:
    primary_vs_seed_col = f"{metric_slug(primary_metric)}_vs_seed"
    seed_adjusted_rho = float(over_df["overperformance"].corr(over_df[primary_vs_seed_col], method="spearman"))
    yearly = (
        tournament_df.groupby("year")
        .apply(lambda group: float(group["finish_depth"].corr(group[primary_metric], method="spearman")))
        .reset_index(name="raw_rho")
    )
    positive_years = int((yearly["raw_rho"] > 0).sum())
    seed_group_view = (
        field_table[["seed_group", "full_group_rho", "recent_group_rho", "seed_action_tier"]]
        .drop_duplicates()
        .sort_values("seed_group", kind="stable")
    )
    high_use = seed_group_view[seed_group_view["seed_action_tier"] == "High-use pocket"]
    secondary_use = seed_group_view[seed_group_view["seed_action_tier"] == "Secondary-use pocket"]
    former_champions = field_table[field_table["is_former_champion"]].copy().sort_values(
        ["projected_seed", "barthag"],
        ascending=[True, False],
        kind="stable",
    )
    secondary_use_text = (
        ", ".join(
            [
                f"`{row.seed_group}` (full {row.full_group_rho:+.3f}, recent {row.recent_group_rho:+.3f})"
                for row in secondary_use.itertuples(index=False)
            ]
        )
        if not secondary_use.empty
        else "none"
    )
    former_champion_text = (
        ", ".join(
            [
                f"`{row.team}` ({int(row.projected_seed)}) â€” {row.title_history}"
                for row in former_champions.itertuples(index=False)
            ]
        )
        if not former_champions.empty
        else "none"
    )

    positive_teams = field_table[field_table["carryover_direction"] == "positive carryover"].copy()
    negative_teams = field_table[field_table["carryover_direction"] == "negative carryover"].copy()
    cautious_teams = field_table[
        field_table["carryover_direction"].isin(["cautious positive", "cautious negative", "mixed"])
    ].copy()
    if positive_teams.empty:
        positive_teams = field_table[field_table["full_primary_position"] == "above baseline"].copy()
    if negative_teams.empty:
        negative_teams = field_table[field_table["full_primary_position"] == "below baseline"].copy()
    if cautious_teams.empty:
        cautious_teams = field_table.copy()

    positive_teams = positive_teams.sort_values(["full_primary_delta", "barthag"], ascending=[False, False], kind="stable").head(5)
    negative_teams = negative_teams.sort_values(["full_primary_delta", "barthag"], ascending=[True, False], kind="stable").head(5)
    cautious_teams = cautious_teams.assign(abs_primary_delta=lambda frame: frame["full_primary_delta"].abs()).sort_values(
        ["abs_primary_delta", "barthag"], ascending=[False, False], kind="stable"
    ).head(5)

    def format_team_list(table: pd.DataFrame) -> str:
        parts = []
        for row in table.itertuples(index=False):
            parts.append(
                f"`{row.team}` ({int(row.projected_seed)}) — {row.full_primary_delta:+.2f} vs full seed baseline; `{row.carryover_confidence_note}`"
            )
        return "; ".join(parts)

    display_table = field_table[
        [
            "projected_seed",
            "team",
            "conf",
            "is_former_champion",
            "championship_count",
            "last_title_year",
            "last_title_seed",
            "title_history",
            primary_metric,
            "full_primary_delta",
            "recent_primary_delta",
            "full_primary_position",
            "seed_action_tier",
            "carryover_confidence_note",
        ]
    ].copy()

    lines = [
        f"# Size Analysis 7: {CURRENT_FIELD_YEAR} Field Application",
        "",
        f"Projected seeds from `data/raw/2026 expected seeds.xlsx` are joined to the `{CURRENT_FIELD_YEAR}` size sample. The scatter is contextual: logos to the right of their seed markers are bigger on `{primary_metric}` than that seed line usually carries. Full-sample trends drive the read; recent trends only raise or lower confidence.",
        "",
        "## What tends to carry over most reliably",
        "",
        f"- Exact-seed-adjusted `{primary_metric}` is still context, not destiny. Across the full `2008-2025` tournament sample, the seed-adjusted relationship with overperformance is `{seed_adjusted_rho:+.3f}`.",
        f"- The direction still holds over time. `{primary_metric}` was positively correlated with finish depth in `{positive_years}/{len(yearly)}` completed tournament seasons.",
        f"- The most actionable seed pockets in the historical sample are: {', '.join([f'`{row.seed_group}` ({row.full_group_rho:+.3f})' for row in high_use.itertuples(index=False)]) or 'none'}",
        "- Use those pockets as the main place to weight size, then treat the rest of the field as context or tiebreaker territory.",
        "",
        "## What recent trends may also carry over",
        "",
        f"- Recent seasons (`2022-2025`) are a qualifier rather than a reset. The only full-sample pocket still worth monitoring is {secondary_use_text}.",
        "- Recent results do not create a new strong size pocket on their own, so they should raise or lower confidence only after the full-sample case is already clear.",
        "- If a 2026 team sits above both its full-sample and recent EFFHGT baselines, confidence can move up one level. If only the recent baseline likes it, keep the conclusion soft.",
        "",
        "## 2026 seed bands to weight size most",
        "",
        f"- High-use pockets: {', '.join([f'`{row.seed_group}`' for row in high_use.itertuples(index=False)]) or 'none'}",
        f"- Secondary-use pockets: {', '.join([f'`{row.seed_group}`' for row in secondary_use.itertuples(index=False)]) or 'none'}",
        "- In low-use pockets, big or long teams still matter as context, but they do not earn the same independent bracket weight.",
        f"- For dominant-big-team support, use the separate `reports/figures/size_2026_big_man.png` page rather than overloading this main size chart.",
        f"- Gold-backed logos in the chart mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        "",
        "## Specific 2026 teams to view positively, negatively, or cautiously",
        "",
        f"- Stronger carryover cases: {format_team_list(positive_teams)}",
        f"- Weak carryover cases: {format_team_list(negative_teams)}",
        f"- Caution flags: {format_team_list(cautious_teams)}",
        "",
        "## Table",
        "",
        display_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_07_big_man_md(path: Path, field_table: pd.DataFrame, big_man_table: pd.DataFrame) -> None:
    overall = big_man_table.loc[big_man_table["segment"] == "Overall"].iloc[0]
    dominant = field_table[field_table["dominant_big_man"]].sort_values(
        ["projected_seed", BIG_MAN_METRIC],
        ascending=[True, False],
        kind="stable",
    )
    dominant_text = (
        "; ".join(
            f"`{row.team}` ({int(row.projected_seed)}) `{getattr(row, BIG_MAN_METRIC):.2f}`"
            for row in dominant.itertuples(index=False)
        )
        if not dominant.empty
        else "none"
    )

    export_table = field_table[
        [
            "projected_seed",
            "team",
            "conf",
            BIG_MAN_METRIC,
            "dominant_big_man",
            "seed_group",
        ]
    ].copy()

    lines = [
        f"# Size Analysis 7B: {CURRENT_FIELD_YEAR} Dominant Big Men in the Projected Field",
        "",
        f"A dominant big team is one whose top center has `{BIG_MAN_METRIC} >= {DOMINANT_BIG_MAN_THRESHOLD:.1f}`. "
        f"This page lists the projected `{CURRENT_FIELD_YEAR}` field against that threshold only.",
        "",
        "## Outcome contamination",
        "",
        f"- {BIG_MAN_OUTCOME_NOTE}",
        f"- The `{CURRENT_FIELD_YEAR}` values come from a pre-tournament player export (player game counts match the "
        "team's pre-tournament games), so they are not comparable with the tournament-inclusive 2008-2025 values. The field is therefore not compared with "
        "historical seed-line averages, and no team is rated on the historical split.",
        f"- For reference only, the contaminated 2008-2025 split is `{overall['dominant_mean_overperformance']:+.3f}` "
        f"for dominant big teams vs `{overall['other_mean_overperformance']:+.3f}` for the rest of the field "
        "(finish minus exact-seed expectation).",
        "",
        f"## Projected {CURRENT_FIELD_YEAR} teams at or above the threshold",
        "",
        f"- `{len(dominant)}` of `{len(field_table)}` teams: {dominant_text}.",
        "",
        "## Table",
        "",
        export_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_size_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    primary_metric = "EFFHGT"

    over_df = build_overperformance_data(tournament_df)
    big_man_table, big_man_raw_rho = build_big_man_signal_table(over_df)
    plot_big_man_signal(big_man_table, paths["fig_05a"])
    big_man_table.round(6).to_csv(paths["csv_05a"], index=False)
    write_big_man_signal_md(paths["md_05a"], big_man_table, big_man_raw_rho)

    archetype_table = build_analysis_05_table(over_df, primary_metric)
    plot_analysis_05(archetype_table, primary_metric, paths["fig_05"])
    archetype_table.round(6).to_csv(paths["csv_05"], index=False)
    write_analysis_05_md(paths["md_05"], archetype_table, primary_metric)

    year_table = build_big_man_over_time_table(tournament_df)
    plot_big_man_over_time(year_table, paths["fig_06"])
    year_table.round(6).to_csv(paths["csv_06"], index=False)
    write_big_man_over_time_summary(year_table, paths["md_06"])

    field_table = build_analysis_07_table(
        full_df,
        tournament_df,
        primary_metric,
        paths["seeds_2026"],
        paths["logo_cache"],
    )
    plot_analysis_07(field_table, primary_metric, paths["fig_07"])
    export_columns = [
        "projected_seed",
        "team",
        "conf",
        "barthag",
        "is_former_champion",
        "championship_count",
        "last_title_year",
        "last_title_seed",
        "title_history",
        primary_metric,
        "seed_group",
        "seed_action_tier",
        "full_group_rho",
        "recent_group_rho",
        f"full_seed_avg_{metric_slug(primary_metric)}",
        f"recent_seed_avg_{metric_slug(primary_metric)}",
        "full_primary_delta",
        "recent_primary_delta",
        "full_primary_position",
        "recent_primary_position",
        "carryover_direction",
        "carryover_confidence_note",
        "logo_fallback",
    ]
    field_table[export_columns].round(6).to_csv(paths["csv_07"], index=False)
    write_analysis_07_md(paths["md_07"], field_table, tournament_df, over_df, primary_metric)

    field_big_man_table = build_analysis_07_big_man_table(
        full_df,
        tournament_df,
        paths["seeds_2026"],
        paths["logo_cache"],
    )
    plot_analysis_07_big_man(field_big_man_table, paths["fig_07_big_man"])
    big_man_export_columns = [
        "projected_seed",
        "team",
        "conf",
        "barthag",
        BIG_MAN_METRIC,
        "dominant_big_man",
        "seed_group",
        "logo_fallback",
    ]
    field_big_man_table[big_man_export_columns].round(6).to_csv(paths["csv_07_big_man"], index=False)
    write_analysis_07_big_man_md(paths["md_07_big_man"], field_big_man_table, big_man_table)

    # --- Guard-height chart ---
    if paths["guard_height_input"].exists():
        guard_field = build_guard_height_field_table(field_table, paths["guard_height_input"])
        plot_guard_height_field(guard_field, paths["fig_07_guard_height"])
        guard_export_cols = [
            "projected_seed", "team", "conf", "barthag",
            "guard_height_in", primary_metric,
            "seed_group", "carryover_direction",
        ]
        guard_field[[c for c in guard_export_cols if c in guard_field.columns]].round(4).to_csv(
            paths["csv_07_guard_height"], index=False
        )
        print(f"Wrote fig_07_guard_height: {paths['fig_07_guard_height']}")
        print(f"Wrote csv_07_guard_height: {paths['csv_07_guard_height']}")
    else:
        print(f"Skipped guard-height chart (missing {paths['guard_height_input']})")

    print(f"Primary metric: {primary_metric}")
    for key in [
        "fig_05a",
        "fig_05",
        "fig_06",
        "fig_07",
        "fig_07_big_man",
        "md_05a",
        "md_05",
        "md_06",
        "md_07",
        "md_07_big_man",
        "fig_07_guard_height",
        "csv_07_guard_height",
        "csv_05a",
        "csv_05",
        "csv_06",
        "csv_07",
        "csv_07_big_man",
    ]:
        print(f"Wrote {key}: {paths[key]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
