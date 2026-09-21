#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from tempo_analysis_common import (
    ACCENT,
    HIGHLIGHT,
    LABEL_TEXT,
    NEGATIVE,
    PROJECT_PALETTE,
    SECONDARY,
    SEED_GROUP_ORDER,
    TEXT_DARK,
    apply_report_background,
    build_overperformance_data,
    get_tempo_base_paths,
    load_tempo_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    set_project_theme,
    styled_box,
)

PANEL_SPECS = [
    ("AdjT", ACCENT),
    ("AdjOE", HIGHLIGHT),
    ("AdjDE", PROJECT_PALETTE["lake_blue"]),
    ("Barthag", PROJECT_PALETTE["steel"]),
]


def get_paths() -> dict[str, Path]:
    base = get_tempo_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "fig_pooled": root / "reports" / "figures" / "tempo_vs_finish_pooled.png",
        "fig_year": root / "reports" / "figures" / "tempo_by_year.png",
        "fig_seed": root / "reports" / "figures" / "tempo_vs_finish_by_seed_group.png",
        "csv_pooled": root / "reports" / "tempo_vs_finish_pooled_table.csv",
        "csv_year": root / "reports" / "tempo_by_year_table.csv",
        "csv_seed": root / "reports" / "tempo_vs_finish_by_seed_group_table.csv",
        "md_pooled": root / "reports" / "tempo_vs_finish_pooled_summary.md",
        "md_year": root / "reports" / "tempo_by_year_summary.md",
        "md_seed": root / "reports" / "tempo_vs_finish_by_seed_group_summary.md",
    }


def build_pooled_table(over_df: pd.DataFrame) -> pd.DataFrame:
    return pd.DataFrame(
        [
            {
                "metric": "AdjT",
                "seed_adjusted_rho": float(over_df["overperformance"].corr(over_df["adjt_edge"], method="spearman")),
                "mean_x": float(over_df["adjt_edge"].mean()),
            }
        ]
    )


def build_year_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for year, group in over_df.groupby("year", as_index=False):
        row = {"year": int(year), "team_count": int(len(group))}
        for metric, _ in PANEL_SPECS:
            slug = metric_slug(metric)
            row[f"{slug}_rho"] = float(group["overperformance"].corr(group[f"{slug}_edge"], method="spearman"))
        rows.append(row)
    return pd.DataFrame(rows).sort_values("year", kind="stable").reset_index(drop=True)


def build_seed_group_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    modern_df = over_df.loc[over_df["year"] >= 2014].copy()
    for seed_group, group in over_df.groupby("seed_group", as_index=False):
        modern_group = modern_df.loc[modern_df["seed_group"] == seed_group]
        rows.append(
            {
                "seed_group": seed_group,
                "team_count_all": int(len(group)),
                "team_count_since_2014": int(len(modern_group)),
                "adjt_rho_all": float(group["overperformance"].corr(group["adjt_edge"], method="spearman")),
                "adjt_rho_since_2014": float(
                    modern_group["overperformance"].corr(modern_group["adjt_edge"], method="spearman")
                ),
            }
        )
    table = pd.DataFrame(rows)
    table["seed_group"] = pd.Categorical(table["seed_group"], categories=SEED_GROUP_ORDER, ordered=True)
    table = table.sort_values("seed_group", kind="stable").reset_index(drop=True)
    table["seed_group"] = table["seed_group"].astype(str)
    return table


def plot_pooled(table: pd.DataFrame, over_df: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, ax = plt.subplots(1, 1, figsize=(13.4, 8.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.10, right=0.97)

    x = over_df["adjt_edge"].to_numpy(dtype=float)
    y = over_df["overperformance"].to_numpy(dtype=float)
    rho = float(table.loc[table["metric"] == "AdjT", "seed_adjusted_rho"].iloc[0])
    slope, intercept = np.polyfit(x, y, 1)
    x_line = np.linspace(float(np.nanmin(x)), float(np.nanmax(x)), 160)
    y_line = slope * x_line + intercept

    ax.scatter(x, y, s=11, color=ACCENT, alpha=0.24, edgecolors="none")
    ax.plot(x_line, y_line, color=ACCENT, linewidth=2.0, linestyle="--", alpha=0.95)
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
    ax.set_xlabel("AdjT edge vs exact-seed baseline", fontsize=10.8, fontweight="semibold")
    ax.set_ylabel("Finish vs exact-seed expectation", fontsize=10.8, fontweight="semibold")
    ax.set_title("Adjusted tempo", fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)
    ax.text(
        0.03,
        0.94,
        f"rho = {rho:+.3f}\n(n={len(over_df)})",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=8.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    fig.suptitle("Adjusted tempo versus March overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.90,
        "Seed-adjusted pooled view for tempo only. This page now answers the cleanest version of the Day 8 question.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def plot_year(table: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, ax = plt.subplots(1, 1, figsize=(13.4, 8.0), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.10, right=0.97)

    years = table["year"].to_numpy(dtype=int)
    values = table["adjt_rho"].to_numpy(dtype=float)
    smooth = table["adjt_rho"].rolling(window=3, center=True, min_periods=1).mean().to_numpy(dtype=float)
    band = table["adjt_rho"].rolling(window=3, center=True, min_periods=1).std().fillna(0.0).to_numpy(dtype=float)

    ax.fill_between(years, smooth - band, smooth + band, color=ACCENT, alpha=0.16, linewidth=0, zorder=1)
    smooth_line, = ax.plot(
        years,
        smooth,
        color=PROJECT_PALETTE["fog"],
        linewidth=3.0,
        alpha=0.95,
        zorder=2,
        label="3-year rolling trend",
    )
    yearly_line, = ax.plot(
        years,
        values,
        color=ACCENT,
        linewidth=2.6,
        marker="o",
        markersize=4.5,
        zorder=3,
        label="Yearly AdjT signal",
    )
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
    ax.set_title("Adjusted tempo", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
    ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
    ax.set_xticks(years)
    ax.set_xticklabels([str(year) for year in years], fontsize=9.2)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=7)
    legend = ax.legend(
        handles=[yearly_line, smooth_line],
        loc="upper right",
        frameon=True,
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=SECONDARY,
        framealpha=0.98,
        fontsize=9.4,
    )
    legend.get_frame().set_linewidth(1.1)
    for text in legend.get_texts():
        text.set_color(PROJECT_PALETTE["paper"])

    fig.suptitle("Adjusted tempo signal by tournament year", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.90,
        "The blue line shows each tournament year. The lighter band and center line add a 3-year rolling trend to make the direction easier to read.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def plot_seed_group(table: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, axes = plt.subplots(1, 2, figsize=(15.6, 8.4), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.07, right=0.97, wspace=0.18)

    x = np.arange(len(table))
    panel_specs = [
        ("adjt_rho_all", "All years", ACCENT),
        ("adjt_rho_since_2014", "Since 2014", PROJECT_PALETTE["fog"]),
    ]
    for ax, (column, title, color) in zip(axes, panel_specs):
        values = table[column].to_numpy(dtype=float)
        bars = ax.bar(x, values, width=0.60, color=[color if value >= 0 else NEGATIVE for value in values])
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xticks(x)
        ax.set_xticklabels(table["seed_group"].tolist(), fontsize=10.0, fontweight="semibold")
        ax.set_ylabel("Seed-band Spearman rho", fontsize=10.8, fontweight="semibold")
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)
        for rect in bars:
            value = float(rect.get_height())
            ax.text(
                rect.get_x() + rect.get_width() / 2,
                value + (0.012 if value >= 0 else -0.012),
                f"{value:+.3f}",
                ha="center",
                va="bottom" if value >= 0 else "top",
                fontsize=8.8,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )

    fig.suptitle("Where tempo matters by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        "Each seed band compares the full sample to the modern era since 2014, using tempo only.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def write_summary_pooled(table: pd.DataFrame, out_path: Path) -> None:
    lines = [
        "# Tempo Analysis 2: Pooled Relationship",
        "",
        f"- AdjT seed-adjusted rho: `{table.loc[table['metric'] == 'AdjT', 'seed_adjusted_rho'].iloc[0]:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_year(table: pd.DataFrame, out_path: Path) -> None:
    lines = [
        "# Tempo Analysis 3: By Year",
        "",
        f"- AdjT was positive in `{int((table['adjt_rho'] > 0).sum())}` of `{len(table)}` tournament years.",
        "",
        "## Table",
        "",
        table[["year", "team_count", "adjt_rho"]].round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_seed(table: pd.DataFrame, out_path: Path) -> None:
    best_all = table.loc[table["adjt_rho_all"].idxmax()]
    best_modern = table.loc[table["adjt_rho_since_2014"].idxmax()]
    lines = [
        "# Tempo Analysis 4: Seed-Band Split",
        "",
        f"- Best AdjT band across all years: `{best_all['seed_group']}` at `{best_all['adjt_rho_all']:+.3f}`.",
        f"- Best AdjT band since 2014: `{best_modern['seed_group']}` at `{best_modern['adjt_rho_since_2014']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_tempo_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    pooled = build_pooled_table(over_df)
    yearly = build_year_table(over_df)
    seed = build_seed_group_table(over_df)

    pooled.round(6).to_csv(paths["csv_pooled"], index=False)
    yearly.round(6).to_csv(paths["csv_year"], index=False)
    seed.round(6).to_csv(paths["csv_seed"], index=False)

    plot_pooled(pooled, over_df, paths["fig_pooled"])
    plot_year(yearly, paths["fig_year"])
    plot_seed_group(seed, paths["fig_seed"])

    write_summary_pooled(pooled, paths["md_pooled"])
    write_summary_year(yearly, paths["md_year"])
    write_summary_seed(seed, paths["md_seed"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
