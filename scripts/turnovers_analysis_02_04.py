#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from turnovers_analysis_common import (
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
    get_turnovers_base_paths,
    load_tournament_data,
    load_turnovers_data,
    metric_label,
    set_project_theme,
    styled_box,
)


def get_paths() -> dict[str, Path]:
    base = get_turnovers_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "fig_02": root / "reports" / "figures" / "turnovers_vs_finish_pooled.png",
        "fig_02_unadjusted": root / "reports" / "figures" / "turnovers_vs_finish_pooled_unadjusted.png",
        "fig_03": root / "reports" / "figures" / "turnovers_defensive_disruption_by_year.png",
        "fig_03_offense": root / "reports" / "figures" / "turnovers_offensive_ball_security_by_year.png",
        "fig_04": root / "reports" / "figures" / "turnovers_vs_finish_by_seed_group.png",
        "fig_04_blocks": root / "reports" / "figures" / "turnovers_blocks_vs_finish_by_seed_group.png",
        "csv_02": root / "reports" / "turnovers_vs_finish_pooled_table.csv",
        "csv_03": root / "reports" / "turnovers_defensive_disruption_by_year_table.csv",
        "csv_04": root / "reports" / "turnovers_vs_finish_by_seed_group_table.csv",
        "csv_04_blocks": root / "reports" / "turnovers_blocks_vs_finish_by_seed_group_table.csv",
        "md_02": root / "reports" / "turnovers_vs_finish_pooled_summary.md",
        "md_03": root / "reports" / "turnovers_defensive_disruption_by_year_summary.md",
        "md_04": root / "reports" / "turnovers_vs_finish_by_seed_group_summary.md",
        "md_04_blocks": root / "reports" / "turnovers_blocks_vs_finish_by_seed_group_summary.md",
    }


def build_analysis_02_table(tournament_df: pd.DataFrame, over_df: pd.DataFrame) -> pd.DataFrame:
    raw_rho = float(tournament_df["finish_depth"].corr(-tournament_df["TOR"], method="spearman"))
    seed_rho = float(over_df["overperformance"].corr(over_df["tor_edge"], method="spearman"))
    tord_seed_rho = float(over_df["overperformance"].corr(over_df["tord_edge"], method="spearman"))
    blk_seed_rho = float(over_df["overperformance"].corr(over_df["blk_pct_edge"], method="spearman"))
    blked_seed_rho = float(over_df["overperformance"].corr(over_df["blked_pct_edge"], method="spearman"))
    return pd.DataFrame(
        [
            {
                "metric": "TOR",
                "raw_spearman": raw_rho,
                "seed_adjusted_spearman": seed_rho,
                "tord_seed_adjusted_spearman": tord_seed_rho,
                "blk_seed_adjusted_spearman": blk_seed_rho,
                "blked_seed_adjusted_spearman": blked_seed_rho,
                "champ_mean_tor": float(tournament_df.loc[tournament_df["finish"] == "CHAMPS", "TOR"].mean()),
                "r64_mean_tor": float(tournament_df.loc[tournament_df["finish"] == "R64", "TOR"].mean()),
                "champ_minus_r64": float(tournament_df.loc[tournament_df["finish"] == "CHAMPS", "TOR"].mean() - tournament_df.loc[tournament_df["finish"] == "R64", "TOR"].mean()),
                "field_mean_tor": float(tournament_df["TOR"].mean()),
            }
        ]
    )


def plot_analysis_02(tournament_df: pd.DataFrame, over_df: pd.DataFrame, summary_table: pd.DataFrame, out_path: Path, out_unadjusted: Path) -> None:
    grouped = (
        tournament_df.groupby("finish", as_index=False)
        .agg(team_count=("Team", "size"), tor_mean=("TOR", "mean"))
        .copy()
    )
    grouped["finish_depth"] = grouped["finish"].map({k: i + 1 for i, k in enumerate(FINISH_ORDER)})
    grouped["finish_label"] = grouped["finish"].map(FINISH_DISPLAY)
    grouped = grouped.sort_values("finish_depth", kind="stable").reset_index(drop=True)

    x = np.arange(len(grouped), dtype=float)
    y = grouped["tor_mean"].to_numpy(dtype=float)
    field_average = float(tournament_df["TOR"].mean())
    raw_rho = float(summary_table.loc[0, "raw_spearman"])
    seed_rho = float(summary_table.loc[0, "seed_adjusted_spearman"])

    set_project_theme()
    fig, ax = plt.subplots(figsize=(14.4, 8.0), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.08, right=0.94)
    ax.plot(x, y, color=ACCENT, linewidth=2.8, zorder=3)
    ax.scatter(x, y, color=ACCENT, s=92, zorder=4)
    ax.axhline(field_average, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    ax.set_xticks(x)
    ax.set_xticklabels([f"{label}\n(n={count})" for label, count in zip(grouped["finish_label"], grouped["team_count"])], fontsize=10.5, fontweight="semibold")
    ax.set_ylabel("Average offensive turnover rate", fontsize=11.0, fontweight="semibold")
    ax.set_xlabel("Tournament finish", fontsize=11.0, fontweight="semibold")
    ax.set_title("Unadjusted turnover profile by finish", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=8)
    ax.tick_params(axis="y", length=0, pad=6)
    ax.text(0.03, 0.95, f"Raw rho = {raw_rho:+.3f}", transform=ax.transAxes, ha="left", va="top", fontsize=10.2, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    ax.text(0.97, 0.08, "Lower is better here", transform=ax.transAxes, ha="right", va="bottom", fontsize=9.8, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    fig.suptitle("Turnovers and finish, unadjusted", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, "Round means only. This view is descriptive and still includes seed structure.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_unadjusted.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_unadjusted, dpi=220, bbox_inches="tight")
    plt.close(fig)

    set_project_theme()
    fig, axes = plt.subplots(2, 2, figsize=(18.0, 11.4), constrained_layout=False)
    apply_report_background(fig, axes.flatten())
    fig.subplots_adjust(top=0.84, bottom=0.11, left=0.06, right=0.97, wspace=0.12, hspace=0.20)

    y_seed = over_df["overperformance"].to_numpy(dtype=float)
    panel_specs = [
        (
            axes[0, 0],
            "tor_edge",
            ACCENT,
            "TOR vs overperformance",
            "TOR rho",
            float(summary_table.loc[0, "seed_adjusted_spearman"]),
        ),
        (
            axes[0, 1],
            "tord_edge",
            HIGHLIGHT,
            "TORD vs overperformance",
            "TORD rho",
            float(summary_table.loc[0, "tord_seed_adjusted_spearman"]),
        ),
        (
            axes[1, 0],
            "blk_pct_edge",
            PROJECT_PALETTE["lake_blue"],
            "BLK% vs overperformance",
            "BLK% rho",
            float(summary_table.loc[0, "blk_seed_adjusted_spearman"]),
        ),
        (
            axes[1, 1],
            "blked_pct_edge",
            PROJECT_PALETTE["steel"],
            "BLKED% vs overperformance",
            "BLKED% rho",
            float(summary_table.loc[0, "blked_seed_adjusted_spearman"]),
        ),
    ]

    for ax, edge_col, color, title, rho_label, rho_value in panel_specs:
        x_metric = over_df[edge_col].to_numpy(dtype=float)
        slope, intercept = np.polyfit(x_metric, y_seed, 1)
        line_x = np.linspace(float(np.nanmin(x_metric)), float(np.nanmax(x_metric)), 200)
        line_y = intercept + slope * line_x
        ax.scatter(x_metric, y_seed, s=24, color=color, alpha=0.20, linewidths=0, zorder=1)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
        ax.plot(line_x, line_y, color=color, linestyle="--", linewidth=2.6, zorder=3)
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
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
            f"{rho_label} = {rho_value:+.3f}",
            transform=ax.transAxes,
            ha="left",
            va="top",
            fontsize=10.0,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    axes[1, 1].text(
        0.97,
        0.08,
        "Positive x = better than your seed baseline on that metric",
        transform=axes[1, 1].transAxes,
        ha="right",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    fig.suptitle("Turnover and block metrics versus tournament overperformance", fontsize=22, fontweight="bold", y=0.975)
    fig.text(0.5, 0.91, "Each point is one tournament team. Offense and defense are shown in separate panels.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
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
                "tor_seed_adjusted_rho": float(group["overperformance"].corr(group["tor_edge"], method="spearman")),
                "tord_seed_adjusted_rho": float(group["overperformance"].corr(group["tord_edge"], method="spearman")),
                "blk_seed_adjusted_rho": float(group["overperformance"].corr(group["blk_pct_edge"], method="spearman")),
                "blked_seed_adjusted_rho": float(group["overperformance"].corr(group["blked_pct_edge"], method="spearman")),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_03(table: pd.DataFrame, out_path: Path) -> None:
    years = table["year"].to_numpy(dtype=float)
    tor_mean = float(table["tor_seed_adjusted_rho"].mean())
    tord_mean = float(table["tord_seed_adjusted_rho"].mean())
    blked_mean = float(table["blked_seed_adjusted_rho"].mean())
    blk_mean = float(table["blk_seed_adjusted_rho"].mean())

    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(15.4, 11.4), constrained_layout=False, sharex=True)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.86, bottom=0.10, left=0.08, right=0.95, hspace=0.22)

    top_ax, bottom_ax = axes

    top_ax.plot(years, table["tord_seed_adjusted_rho"], color=ACCENT, linewidth=2.7, marker="o", markersize=5.0, label="Turnovers forced")
    top_ax.plot(years, table["blk_seed_adjusted_rho"], color=HIGHLIGHT, linewidth=2.4, marker="o", markersize=4.6, label="Block pressure")
    top_ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
    top_ax.axhline(tord_mean, color=ACCENT, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
    top_ax.axhline(blk_mean, color=HIGHLIGHT, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
    top_ax.axvline(2022, color=SECONDARY, linestyle=":", linewidth=1.7)
    top_ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
    top_ax.set_title("Defense: turnovers forced and blocks by year", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
    top_ax.grid(False)
    top_ax.spines["top"].set_visible(False)
    top_ax.spines["right"].set_visible(False)
    top_ax.tick_params(axis="x", length=0, pad=7)
    top_ax.tick_params(axis="y", length=0, pad=6)
    top_ax.legend(loc="upper left")
    top_ax.text(
        0.98,
        0.12,
        f"Dashed means: turnovers forced {tord_mean:+.3f}, block pressure {blk_mean:+.3f}",
        transform=top_ax.transAxes,
        ha="right",
        va="bottom",
        fontsize=9.6,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    bottom_ax.plot(years, table["tor_seed_adjusted_rho"], color=ACCENT, linewidth=2.7, marker="o", markersize=5.0, label="Ball security")
    bottom_ax.plot(years, table["blked_seed_adjusted_rho"], color=HIGHLIGHT, linewidth=2.4, marker="o", markersize=4.6, label="Avoiding blocks")
    bottom_ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
    bottom_ax.axhline(tor_mean, color=ACCENT, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
    bottom_ax.axhline(blked_mean, color=HIGHLIGHT, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
    bottom_ax.axvline(2022, color=SECONDARY, linestyle=":", linewidth=1.7)
    bottom_ax.set_xticks(years)
    bottom_ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.0)
    bottom_ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
    bottom_ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
    bottom_ax.set_title("Offense: ball security and avoiding blocks by year", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
    bottom_ax.grid(False)
    bottom_ax.spines["top"].set_visible(False)
    bottom_ax.spines["right"].set_visible(False)
    bottom_ax.tick_params(axis="x", length=0, pad=7)
    bottom_ax.tick_params(axis="y", length=0, pad=6)
    bottom_ax.legend(loc="upper left")
    bottom_ax.text(
        0.98,
        0.12,
        f"Dashed means: ball security {tor_mean:+.3f}, avoiding blocks {blked_mean:+.3f}",
        transform=bottom_ax.transAxes,
        ha="right",
        va="bottom",
        fontsize=9.6,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    fig.suptitle("Do turnover control and block pressure travel year to year?", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.915, "Top panel shows defense. Bottom panel shows offense. All four series are measured relative to exact-seed baselines.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_03_offense(table: pd.DataFrame, out_path: Path) -> None:
    years = table["year"].to_numpy(dtype=float)
    tor_mean = float(table["tor_seed_adjusted_rho"].mean())
    blked_mean = float(table["blked_seed_adjusted_rho"].mean())

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.0, 7.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.08, right=0.95)
    ax.plot(years, table["tor_seed_adjusted_rho"], color=ACCENT, linewidth=2.7, marker="o", markersize=5.0, label="Ball security")
    ax.plot(years, table["blked_seed_adjusted_rho"], color=HIGHLIGHT, linewidth=2.4, marker="o", markersize=4.6, label="Avoiding blocks")
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
    ax.axhline(tor_mean, color=ACCENT, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
    ax.axhline(blked_mean, color=HIGHLIGHT, linestyle="--", linewidth=1.5, alpha=0.9, zorder=0)
    ax.axvline(2022, color=SECONDARY, linestyle=":", linewidth=1.7)
    ax.set_xticks(years)
    ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.0)
    ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
    ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
    ax.set_title("Offensive ball security by year", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=12)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=6)
    ax.legend(loc="upper left")
    ax.text(
        0.98,
        0.16,
        f"Dashed means: ball security {tor_mean:+.3f}, avoiding blocks {blked_mean:+.3f}",
        transform=ax.transAxes,
        ha="right",
        va="bottom",
        fontsize=9.6,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    fig.suptitle("Do offensive turnover control and shot protection avoidance travel year to year?", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, "Blue = ball security. Green = avoiding blocks. Both are measured relative to exact-seed baselines.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
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
                "tor_seed_band_rho": float(group["overperformance"].corr(group["tor_edge"], method="spearman")),
                "tord_seed_band_rho": float(group["overperformance"].corr(group["tord_edge"], method="spearman")),
                "tor_mean_edge": float(group["tor_edge"].mean()),
                "tord_mean_edge": float(group["tord_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


def build_analysis_04_blocks_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for seed_group in SEED_GROUP_ORDER:
        group = over_df[over_df["seed_group"] == seed_group].copy()
        if group.empty:
            continue
        rows.append(
            {
                "seed_group": seed_group,
                "team_count": int(len(group)),
                "blk_pct_seed_band_rho": float(group["overperformance"].corr(group["blk_pct_edge"], method="spearman")),
                "blked_pct_seed_band_rho": float(group["overperformance"].corr(group["blked_pct_edge"], method="spearman")),
                "blk_pct_mean_edge": float(group["blk_pct_edge"].mean()),
                "blked_pct_mean_edge": float(group["blked_pct_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_04(table: pd.DataFrame, out_path: Path) -> None:
    groups = table["seed_group"].tolist()
    y = np.arange(len(groups), dtype=float)

    set_project_theme()
    fig, axes = plt.subplots(1, 2, figsize=(17.8, 8.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.07, right=0.97, wspace=0.16)

    left_ax, right_ax = axes
    left_ax.barh(y, table["tor_seed_band_rho"], height=0.58, color=ACCENT, alpha=0.88)
    right_ax.barh(y, table["tord_seed_band_rho"], height=0.58, color=HIGHLIGHT, alpha=0.88)
    left_ax.scatter(table["tor_seed_band_rho"], y, s=76, color=PROJECT_PALETTE["fog"], zorder=3)
    right_ax.scatter(table["tord_seed_band_rho"], y, s=76, color=PROJECT_PALETTE["fog"], zorder=3)

    for ax, values, title in [
        (left_ax, table["tor_seed_band_rho"], "TOR relationship to overperformance by seed band"),
        (right_ax, table["tord_seed_band_rho"], "TORD relationship to overperformance by seed band"),
    ]:
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
        ax.set_yticks(y)
        ax.set_yticklabels([f"{group}\n(n={count})" for group, count in zip(groups, table["team_count"])], fontsize=10.0, fontweight="semibold")
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
                fontsize=9.5,
                fontweight="bold",
                color=LABEL_TEXT,
            )

    fig.suptitle("Turnover-overperformance relationship by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, "Each bar is the within-seed-band Spearman relationship with overperformance. Positive means teams better than their seed baseline on the metric tended to overperform inside that band.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_04_blocks(table: pd.DataFrame, out_path: Path) -> None:
    groups = table["seed_group"].tolist()
    y = np.arange(len(groups), dtype=float)

    set_project_theme()
    fig, axes = plt.subplots(1, 2, figsize=(17.8, 8.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.12, left=0.07, right=0.97, wspace=0.16)

    left_ax, right_ax = axes
    left_ax.barh(y, table["blk_pct_seed_band_rho"], height=0.58, color=PROJECT_PALETTE["lake_blue"], alpha=0.88)
    right_ax.barh(y, table["blked_pct_seed_band_rho"], height=0.58, color=PROJECT_PALETTE["steel"], alpha=0.88)
    left_ax.scatter(table["blk_pct_seed_band_rho"], y, s=76, color=PROJECT_PALETTE["fog"], zorder=3)
    right_ax.scatter(table["blked_pct_seed_band_rho"], y, s=76, color=PROJECT_PALETTE["fog"], zorder=3)

    for ax, values, title in [
        (left_ax, table["blk_pct_seed_band_rho"], "BLK% relationship to overperformance by seed band"),
        (right_ax, table["blked_pct_seed_band_rho"], "BLKED% relationship to overperformance by seed band"),
    ]:
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
        ax.set_yticks(y)
        ax.set_yticklabels([f"{group}\n(n={count})" for group, count in zip(groups, table["team_count"])], fontsize=10.0, fontweight="semibold")
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
                fontsize=9.5,
                fontweight="bold",
                color=LABEL_TEXT,
            )

    fig.suptitle("Block-overperformance relationship by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, "Each bar is the within-seed-band Spearman relationship with overperformance. Positive means teams better than their seed baseline on the block metric tended to overperform inside that band.", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_summary_02(table: pd.DataFrame, metric_selection: pd.DataFrame, out_path: Path) -> None:
    row = table.iloc[0]
    lines = [
        "# Turnovers Analysis 2: Offensive Ball Security vs Finish",
        "",
        f"- Lead offense metric: `TOR` (`{metric_label('TOR')}`).",
        f"- Raw pooled relationship: `{row['raw_spearman']:+.3f}`.",
        f"- Exact-seed-controlled TOR relationship: `{row['seed_adjusted_spearman']:+.3f}`.",
        f"- Exact-seed-controlled TORD relationship: `{row['tord_seed_adjusted_spearman']:+.3f}`.",
        f"- Exact-seed-controlled BLK% relationship: `{row['blk_seed_adjusted_spearman']:+.3f}`.",
        f"- Exact-seed-controlled BLKED% relationship: `{row['blked_seed_adjusted_spearman']:+.3f}`.",
        f"- Champions averaged `{row['champ_mean_tor']:.2f}` TOR vs `{row['r64_mean_tor']:.2f}` for R64 exits.",
        f"- Interpretation: the raw TOR relationship (`{row['raw_spearman']:+.3f}`) is `{row['seed_adjusted_spearman']:+.3f}` once exact seed is controlled, so the seed-controlled panel is the relevant comparison.",
        "",
        "## Metric selection",
        "",
        metric_selection.round(3).to_markdown(index=False),
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_03(table: pd.DataFrame, out_path: Path) -> None:
    lines = [
        "# Turnovers Analysis 3: Defensive Disruption by Year",
        "",
        f"- Average seed-adjusted turnover-force rho: `{table['tord_seed_adjusted_rho'].mean():+.3f}`.",
        f"- Average seed-adjusted block-pressure rho: `{table['blk_seed_adjusted_rho'].mean():+.3f}`.",
        f"- Seasons with a positive seed-adjusted rho: `{int((table['tord_seed_adjusted_rho'] > 0).sum())}` of `{len(table)}` for turnover forcing, `{int((table['blk_seed_adjusted_rho'] > 0).sum())}` of `{len(table)}` for block pressure.",
        "- Interpretation: single-season estimates rest on one tournament each and are not tested for significance here; read the averages, not any one year.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_04(table: pd.DataFrame, out_path: Path) -> None:
    strongest_tor = table.iloc[table["tor_seed_band_rho"].idxmax()]
    strongest_tord = table.iloc[table["tord_seed_band_rho"].idxmax()]
    lines = [
        "# Turnovers Analysis 4: Where Turnovers Matter by Seed Band",
        "",
        f"- Strongest TOR seed band: `{strongest_tor['seed_group']}` at `{strongest_tor['tor_seed_band_rho']:+.3f}`.",
        f"- Strongest TORD seed band: `{strongest_tord['seed_group']}` at `{strongest_tord['tord_seed_band_rho']:+.3f}`.",
        f"- TOR band rhos run from `{table['tor_seed_band_rho'].min():+.3f}` to `{table['tor_seed_band_rho'].max():+.3f}`; TORD from `{table['tord_seed_band_rho'].min():+.3f}` to `{table['tord_seed_band_rho'].max():+.3f}`.",
        "- Interpretation: these are descriptive band correlations; `reports/robustness_summary.md` tests each one against a within-seed permutation null.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_04_blocks(table: pd.DataFrame, out_path: Path) -> None:
    strongest_blk = table.iloc[table["blk_pct_seed_band_rho"].idxmax()]
    strongest_blked = table.iloc[table["blked_pct_seed_band_rho"].idxmax()]
    lines = [
        "# Turnovers Analysis 4b: Where Blocks Matter by Seed Band",
        "",
        f"- Strongest BLK% seed band: `{strongest_blk['seed_group']}` at `{strongest_blk['blk_pct_seed_band_rho']:+.3f}`.",
        f"- Strongest BLKED% seed band: `{strongest_blked['seed_group']}` at `{strongest_blked['blked_pct_seed_band_rho']:+.3f}`.",
        f"- BLK% band rhos run from `{table['blk_pct_seed_band_rho'].min():+.3f}` to `{table['blk_pct_seed_band_rho'].max():+.3f}`; BLKED% from `{table['blked_pct_seed_band_rho'].min():+.3f}` to `{table['blked_pct_seed_band_rho'].max():+.3f}`.",
        "- Interpretation: these are descriptive band correlations; `reports/robustness_summary.md` tests each one against a within-seed permutation null.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_turnovers_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)
    metric_selection = choose_primary_metric(over_df)
    table02 = build_analysis_02_table(tournament_df, over_df)
    table03 = build_analysis_03_table(over_df)
    table04 = build_analysis_04_table(over_df)
    table04_blocks = build_analysis_04_blocks_table(over_df)

    metric_selection.round(6).to_csv(paths["metric_selection"], index=False)
    table02.round(6).to_csv(paths["csv_02"], index=False)
    table03.round(6).to_csv(paths["csv_03"], index=False)
    table04.round(6).to_csv(paths["csv_04"], index=False)
    table04_blocks.round(6).to_csv(paths["csv_04_blocks"], index=False)

    plot_analysis_02(tournament_df, over_df, table02, paths["fig_02"], paths["fig_02_unadjusted"])
    plot_analysis_03(table03, paths["fig_03"])
    plot_analysis_03_offense(table03, paths["fig_03_offense"])
    plot_analysis_04(table04, paths["fig_04"])
    plot_analysis_04_blocks(table04_blocks, paths["fig_04_blocks"])
    write_summary_02(table02, metric_selection, paths["md_02"])
    write_summary_03(table03, paths["md_03"])
    write_summary_04(table04, paths["md_04"])
    write_summary_04_blocks(table04_blocks, paths["md_04_blocks"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
