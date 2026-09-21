#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from three_point_analysis_common import (
    ACCENT,
    FINISH_DISPLAY,
    FINISH_ORDER,
    HIGHLIGHT,
    LABEL_TEXT,
    PROJECT_PALETTE,
    SECONDARY,
    SEED_GROUP_ORDER,
    TEXT_DARK,
    apply_report_background,
    build_overperformance_data,
    choose_primary_metric,
    get_three_point_base_paths,
    load_three_point_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    set_project_theme,
    styled_box,
    three_point_edge,
)

METRICS = ["3PRATE", "3P%", "3P%D"]
METRIC_COLORS = {
    "3PRATE": ACCENT,
    "3P%": PROJECT_PALETTE["lake_blue"],
    "3P%D": HIGHLIGHT,
}


def get_paths() -> dict[str, Path]:
    base = get_three_point_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "fig_02": root / "reports" / "figures" / "three_point_vs_finish_pooled.png",
        "fig_02_unadjusted": root / "reports" / "figures" / "three_point_vs_finish_pooled_unadjusted.png",
        "fig_03": root / "reports" / "figures" / "three_point_profile_stability_by_year.png",
        "fig_04": root / "reports" / "figures" / "three_point_vs_finish_by_seed_group.png",
        "csv_02": root / "reports" / "three_point_vs_finish_pooled_table.csv",
        "csv_03": root / "reports" / "three_point_profile_stability_by_year_table.csv",
        "csv_04": root / "reports" / "three_point_vs_finish_by_seed_group_table.csv",
        "md_02": root / "reports" / "three_point_vs_finish_pooled_summary.md",
        "md_03": root / "reports" / "three_point_profile_stability_by_year_summary.md",
        "md_04": root / "reports" / "three_point_vs_finish_by_seed_group_summary.md",
    }


def build_analysis_02_table(tournament_df: pd.DataFrame, over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for metric in METRICS:
        slug = metric_slug(metric)
        raw_metric = three_point_edge(tournament_df[metric], metric)
        rows.append(
            {
                "metric": metric,
                "label": metric_label(metric),
                "raw_spearman": float(tournament_df["finish_depth"].corr(raw_metric, method="spearman")),
                "seed_adjusted_spearman": float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman")),
                "champ_mean": float(tournament_df.loc[tournament_df["finish"] == "CHAMPS", metric].mean()),
                "r64_mean": float(tournament_df.loc[tournament_df["finish"] == "R64", metric].mean()),
                "champ_minus_r64": float(
                    tournament_df.loc[tournament_df["finish"] == "CHAMPS", metric].mean()
                    - tournament_df.loc[tournament_df["finish"] == "R64", metric].mean()
                ),
                "field_mean": float(tournament_df[metric].mean()),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_02(
    tournament_df: pd.DataFrame,
    over_df: pd.DataFrame,
    summary_table: pd.DataFrame,
    out_path: Path,
    out_unadjusted: Path,
) -> None:
    grouped = (
        tournament_df.groupby("finish", as_index=False)
        .agg(team_count=("Team", "size"), three_pct_mean=("3P%", "mean"))
        .copy()
    )
    grouped["finish_depth"] = grouped["finish"].map({k: i + 1 for i, k in enumerate(FINISH_ORDER)})
    grouped["finish_label"] = grouped["finish"].map(FINISH_DISPLAY)
    grouped = grouped.sort_values("finish_depth", kind="stable").reset_index(drop=True)

    x = np.arange(len(grouped), dtype=float)
    y = grouped["three_pct_mean"].to_numpy(dtype=float)
    field_average = float(tournament_df["3P%"].mean())
    raw_rho = float(summary_table.loc[summary_table["metric"] == "3P%", "raw_spearman"].iloc[0])

    set_project_theme()
    fig, ax = plt.subplots(figsize=(14.4, 8.0), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.08, right=0.94)
    ax.plot(x, y, color=PROJECT_PALETTE["lake_blue"], linewidth=2.8, zorder=3)
    ax.scatter(x, y, color=PROJECT_PALETTE["lake_blue"], s=92, zorder=4)
    ax.axhline(field_average, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    ax.set_xticks(x)
    ax.set_xticklabels(
        [f"{label}\n(n={count})" for label, count in zip(grouped["finish_label"], grouped["team_count"])],
        fontsize=10.5,
        fontweight="semibold",
    )
    ax.set_ylabel("Average three-point percentage", fontsize=11.0, fontweight="semibold")
    ax.set_xlabel("Tournament finish", fontsize=11.0, fontweight="semibold")
    ax.set_title("Unadjusted three-point success by finish", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=8)
    ax.tick_params(axis="y", length=0, pad=6)
    ax.text(
        0.03,
        0.95,
        f"Raw rho = {raw_rho:+.3f}",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.2,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    fig.suptitle("Three-point success and finish, unadjusted", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, "Round means only. This view is descriptive and still includes seed structure.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_unadjusted.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_unadjusted, dpi=220, bbox_inches="tight")
    plt.close(fig)

    season_focus_start = 2019
    season_tournament_df = tournament_df[tournament_df["year"] >= season_focus_start].copy()
    season_over_df = over_df[over_df["year"] >= season_focus_start].copy()
    season_summary = build_analysis_02_table(season_tournament_df, season_over_df)
    row_specs = [
        ("All tournament years", over_df, summary_table),
        (f"{season_focus_start}+ only", season_over_df, season_summary),
    ]

    set_project_theme()
    fig, axes = plt.subplots(2, 3, figsize=(21.2, 13.0), constrained_layout=False)
    apply_report_background(fig, axes.ravel())
    fig.subplots_adjust(top=0.87, bottom=0.10, left=0.05, right=0.98, wspace=0.16, hspace=0.28)

    for row_idx, (row_label, row_over_df, row_summary) in enumerate(row_specs):
        y_seed = row_over_df["overperformance"].to_numpy(dtype=float)
        for col_idx, metric in enumerate(METRICS):
            ax = axes[row_idx, col_idx]
            slug = metric_slug(metric)
            color = METRIC_COLORS[metric]
            x_metric = row_over_df[f"{slug}_edge"].to_numpy(dtype=float)
            slope, intercept = np.polyfit(x_metric, y_seed, 1)
            line_x = np.linspace(float(np.nanmin(x_metric)), float(np.nanmax(x_metric)), 200)
            line_y = intercept + slope * line_x
            rho_value = float(row_summary.loc[row_summary["metric"] == metric, "seed_adjusted_spearman"].iloc[0])
            ax.scatter(x_metric, y_seed, s=24, color=color, alpha=0.22, linewidths=0, zorder=1)
            ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
            ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
            ax.plot(line_x, line_y, color=color, linestyle="--", linewidth=2.6, zorder=3)
            ax.set_title(f"{metric_label(metric)} vs overperformance", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
            ax.set_xlabel("Metric edge vs exact-seed baseline", fontsize=11.0, fontweight="semibold")
            ax.set_ylabel("Finish vs exact-seed expectation", fontsize=11.0, fontweight="semibold")
            ax.grid(False)
            ax.spines["top"].set_visible(False)
            ax.spines["right"].set_visible(False)
            ax.tick_params(axis="x", length=0, pad=6)
            ax.tick_params(axis="y", length=0, pad=6)
            ax.text(
                0.03,
                0.95,
                f"{metric} rho = {rho_value:+.3f}",
                transform=ax.transAxes,
                ha="left",
                va="top",
                fontsize=10.0,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )
            if col_idx == 0:
                ax.text(
                    0.03,
                    0.80,
                    f"{row_label}\n(n={len(row_over_df)})",
                    transform=ax.transAxes,
                    ha="left",
                    va="top",
                    fontsize=10.0,
                    fontweight="bold",
                    color=LABEL_TEXT,
                    bbox=styled_box(),
                )

    axes[-1, -1].text(
        0.97,
        0.08,
        "Positive x = better than your seed baseline on that metric",
        transform=axes[-1, -1].transAxes,
        ha="right",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    fig.suptitle("Three-point metrics versus tournament overperformance", fontsize=22, fontweight="bold", y=0.975)
    fig.text(0.5, 0.92, f"Top row pools all tournament teams. Bottom row repeats the same three metrics using only {season_focus_start}+ teams.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_03_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for year, group in over_df.groupby("year", sort=True):
        rows.append(
            {
                "year": int(year),
                "team_count": int(len(group)),
                "three_prate_seed_adjusted_rho": float(group["overperformance"].corr(group["three_prate_edge"], method="spearman")),
                "three_pct_seed_adjusted_rho": float(group["overperformance"].corr(group["three_pct_edge"], method="spearman")),
                "three_pct_d_seed_adjusted_rho": float(group["overperformance"].corr(group["three_pct_d_edge"], method="spearman")),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_03(table: pd.DataFrame, out_path: Path) -> None:
    years = table["year"].to_numpy(dtype=float)
    specs = [
        ("three_prate_seed_adjusted_rho", "Three-point attempt rate by year", ACCENT),
        ("three_pct_seed_adjusted_rho", "Three-point percentage by year", PROJECT_PALETTE["lake_blue"]),
        ("three_pct_d_seed_adjusted_rho", "Opponent three-point percentage by year", HIGHLIGHT),
    ]

    set_project_theme()
    fig, axes = plt.subplots(3, 1, figsize=(15.4, 12.0), constrained_layout=False, sharex=True)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.87, bottom=0.10, left=0.08, right=0.95, hspace=0.22)

    for ax, (column, title, color) in zip(axes, specs):
        mean_value = float(table[column].mean())
        ax.plot(years, table[column], color=color, linewidth=2.7, marker="o", markersize=5.0)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
        ax.axhline(mean_value, color=color, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
        ax.axvline(2022, color=SECONDARY, linestyle=":", linewidth=1.7)
        ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=6)
        ax.text(
            0.98,
            0.12,
            f"Dashed mean = {mean_value:+.3f}",
            transform=ax.transAxes,
            ha="right",
            va="bottom",
            fontsize=9.6,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    axes[-1].set_xticks(years)
    axes[-1].set_xticklabels([str(int(year)) for year in years], fontsize=10.0)
    axes[-1].set_xlabel("Season", fontsize=11.0, fontweight="semibold")
    fig.suptitle("Do three-point edges travel year to year?", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.915, "Each series is measured relative to exact-seed baselines so the chart shows actual bracket usefulness, not just team strength.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_04_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for seed_group in SEED_GROUP_ORDER:
        group = over_df[over_df["seed_group"] == seed_group].copy()
        if group.empty:
            continue
        rows.append(
            {
                "seed_group": seed_group,
                "team_count": int(len(group)),
                "three_prate_seed_band_rho": float(group["overperformance"].corr(group["three_prate_edge"], method="spearman")),
                "three_pct_seed_band_rho": float(group["overperformance"].corr(group["three_pct_edge"], method="spearman")),
                "three_pct_d_seed_band_rho": float(group["overperformance"].corr(group["three_pct_d_edge"], method="spearman")),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_04(table: pd.DataFrame, over_df: pd.DataFrame, out_path: Path) -> None:
    panel_specs = [
        ("three_prate_seed_band_rho", "3PRATE relationship to overperformance by seed band", ACCENT),
        ("three_pct_seed_band_rho", "3P% relationship to overperformance by seed band", PROJECT_PALETTE["lake_blue"]),
        ("three_pct_d_seed_band_rho", "3P%D relationship to overperformance by seed band", HIGHLIGHT),
    ]
    recent_start = 2019
    recent_table = build_analysis_04_table(over_df[over_df["year"] >= recent_start].copy())
    groups = recent_table["seed_group"].tolist()
    y = np.arange(len(groups), dtype=float)

    set_project_theme()
    fig, axes = plt.subplots(1, 3, figsize=(22.0, 8.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.06, right=0.98, wspace=0.18)

    for col_idx, (column, title, color) in enumerate(panel_specs):
        ax = axes[col_idx]
        values = recent_table[column]
        ax.barh(y, values, height=0.58, color=color, alpha=0.88)
        ax.scatter(values, y, s=76, color=PROJECT_PALETTE["fog"], zorder=3)
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
        ax.set_yticks(y)
        ax.set_yticklabels([f"{group}\n(n={count})" for group, count in zip(groups, recent_table["team_count"])], fontsize=10.0, fontweight="semibold")
        ax.set_xlabel("Spearman rho with overperformance", fontsize=11.0, fontweight="semibold")
        ax.set_ylabel("Seed band", fontsize=11.0, fontweight="semibold")
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=6)
        ax.tick_params(axis="y", length=0, pad=7)
        ax.invert_yaxis()
        for yi, xi in zip(y, values):
            ax.text(
                xi + (0.006 if xi >= 0 else -0.006),
                yi,
                f"{xi:+.2f}",
                ha="left" if xi >= 0 else "right",
                va="center",
                fontsize=9.6,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )
        if col_idx == 0:
            ax.text(
                0.03,
                0.95,
                f"{recent_start}+ only\n(n={int(recent_table['team_count'].sum())})",
                transform=ax.transAxes,
                ha="left",
                va="top",
                fontsize=10.0,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )

    fig.suptitle("Where three-point edges matter by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, f"Modern-era view only: seed-band bars use tournament teams from {recent_start}+.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_summary_02(table: pd.DataFrame, out_path: Path) -> None:
    lines = [
        "# Three-Point Analysis 2: Pooled Relationship to Finish",
        "",
        "- Over the full sample, none of the three metrics is a huge standalone seed-adjusted edge.",
        "- Whereas, once looking in the modern 3-pt era, we see the insight strengthen for 3pt % on both offense and defense.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_03(table: pd.DataFrame, out_path: Path) -> None:
    lines = [
        "# Three-Point Analysis 3: Stability By Year",
        "",
        "- Looking at the trend directly shows variation and an inconsistent story over time.",
        "- While it seems 3pt % helps on O and D a little more in the modern era, the trend still varies year to year.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_04(table: pd.DataFrame, out_path: Path) -> None:
    strongest = {
        "3PRATE": table.sort_values("three_prate_seed_band_rho", ascending=False, kind="stable").iloc[0],
        "3P%": table.sort_values("three_pct_seed_band_rho", ascending=False, kind="stable").iloc[0],
        "3P%D": table.sort_values("three_pct_d_seed_band_rho", ascending=False, kind="stable").iloc[0],
    }
    lines = [
        "# Three-Point Analysis 4: Where It Matters By Seed Band",
        "",
        f"- Strongest `3PRATE` seed band: `{strongest['3PRATE']['seed_group']}` at `{strongest['3PRATE']['three_prate_seed_band_rho']:+.3f}`.",
        f"- Strongest `3P%` seed band: `{strongest['3P%']['seed_group']}` at `{strongest['3P%']['three_pct_seed_band_rho']:+.3f}`.",
        f"- Strongest `3P%D` seed band: `{strongest['3P%D']['seed_group']}` at `{strongest['3P%D']['three_pct_d_seed_band_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_three_point_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    metric_selection = choose_primary_metric(over_df)
    table02 = build_analysis_02_table(tournament_df, over_df)
    table03 = build_analysis_03_table(over_df)
    table04 = build_analysis_04_table(over_df)

    paths["metric_selection"].parent.mkdir(parents=True, exist_ok=True)
    metric_selection.round(6).to_csv(paths["metric_selection"], index=False)
    table02.round(6).to_csv(paths["csv_02"], index=False)
    table03.round(6).to_csv(paths["csv_03"], index=False)
    table04.round(6).to_csv(paths["csv_04"], index=False)

    plot_analysis_02(tournament_df, over_df, table02, paths["fig_02"], paths["fig_02_unadjusted"])
    plot_analysis_03(table03, paths["fig_03"])
    plot_analysis_04(table04, over_df, paths["fig_04"])
    write_summary_02(table02, paths["md_02"])
    write_summary_03(table03, paths["md_03"])
    write_summary_04(table04, paths["md_04"])
    print(f"Wrote figure: {paths['fig_02']}")
    print(f"Wrote figure: {paths['fig_03']}")
    print(f"Wrote figure: {paths['fig_04']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
