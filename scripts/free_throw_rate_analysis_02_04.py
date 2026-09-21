#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from free_throw_rate_analysis_common import (
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
    get_free_throw_rate_base_paths,
    load_free_throw_rate_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    set_project_theme,
    styled_box,
)

PANEL_SPECS = [
    ("FTR", ACCENT),
    ("FT%", HIGHLIGHT),
    ("FTRD", PROJECT_PALETTE["lake_blue"]),
]


def get_paths() -> dict[str, Path]:
    base = get_free_throw_rate_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "fig_pooled": root / "reports" / "figures" / "free_throw_rate_vs_finish_pooled.png",
        "fig_year": root / "reports" / "figures" / "free_throw_rate_by_year.png",
        "fig_seed": root / "reports" / "figures" / "free_throw_rate_vs_finish_by_seed_group.png",
        "csv_pooled": root / "reports" / "free_throw_rate_vs_finish_pooled_table.csv",
        "csv_year": root / "reports" / "free_throw_rate_by_year_table.csv",
        "csv_seed": root / "reports" / "free_throw_rate_vs_finish_by_seed_group_table.csv",
        "md_pooled": root / "reports" / "free_throw_rate_vs_finish_pooled_summary.md",
        "md_year": root / "reports" / "free_throw_rate_by_year_summary.md",
        "md_seed": root / "reports" / "free_throw_rate_vs_finish_by_seed_group_summary.md",
    }


def build_pooled_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for metric, _ in PANEL_SPECS:
        slug = metric_slug(metric)
        rows.append(
            {
                "metric": metric,
                "seed_adjusted_rho": float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman")),
                "mean_edge": float(over_df[f"{slug}_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


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
    for seed_group, group in over_df.groupby("seed_group", as_index=False):
        row = {"seed_group": seed_group, "team_count": int(len(group))}
        for metric, _ in PANEL_SPECS:
            slug = metric_slug(metric)
            row[f"{slug}_rho"] = float(group["overperformance"].corr(group[f"{slug}_edge"], method="spearman"))
        rows.append(row)
    table = pd.DataFrame(rows)
    table["seed_group"] = pd.Categorical(table["seed_group"], categories=SEED_GROUP_ORDER, ordered=True)
    table = table.sort_values("seed_group", kind="stable").reset_index(drop=True)
    table["seed_group"] = table["seed_group"].astype(str)
    return table


def plot_pooled(table: pd.DataFrame, over_df: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, axes = plt.subplots(1, 3, figsize=(20.0, 7.0), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.05, right=0.99, wspace=0.14)

    for ax, (metric, color) in zip(axes, PANEL_SPECS):
        slug = metric_slug(metric)
        x = over_df[f"{slug}_edge"].to_numpy(dtype=float)
        y = over_df["overperformance"].to_numpy(dtype=float)
        rho = float(table.loc[table["metric"] == metric, "seed_adjusted_rho"].iloc[0])
        slope, intercept = np.polyfit(x, y, 1)
        x_line = np.linspace(float(np.nanmin(x)), float(np.nanmax(x)), 160)
        y_line = slope * x_line + intercept

        ax.scatter(x, y, s=11, color=color, alpha=0.24, edgecolors="none")
        ax.plot(x_line, y_line, color=color, linewidth=2.0, linestyle="--", alpha=0.95)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xlabel("Metric edge vs exact-seed baseline", fontsize=10.8, fontweight="semibold")
        ax.set_ylabel("Finish vs exact-seed expectation", fontsize=10.8, fontweight="semibold")
        ax.set_title(metric_label(metric), fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
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

    fig.suptitle("Free throw metrics versus March overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Seed-adjusted pooled view for line reliance, line conversion, and foul-prone defense.",
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
    fig, axes = plt.subplots(3, 1, figsize=(12.8, 13.4), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.88, bottom=0.08, left=0.10, right=0.96, hspace=0.24)

    years = table["year"].to_numpy(dtype=int)
    for ax, (metric, color) in zip(axes, PANEL_SPECS):
        slug = metric_slug(metric)
        ax.plot(years, table[f"{slug}_rho"], color=color, linewidth=2.6, marker="o", markersize=4.5)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_title(metric_label(metric), fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
        ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_xticks(years)
        ax.set_xticklabels([str(year) for year in years], fontsize=9.3)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)

    fig.suptitle("Free throw signal by tournament year", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.925,
        "This checks whether line reliance, make rate, and foul pressure held up year to year or only surfaced in certain tournaments.",
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
    fig, axes = plt.subplots(1, 3, figsize=(20.0, 7.0), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.05, right=0.99, wspace=0.14)

    x = np.arange(len(table))
    for ax, (metric, color) in zip(axes, PANEL_SPECS):
        slug = metric_slug(metric)
        values = table[f"{slug}_rho"].to_numpy(dtype=float)
        bar_colors = [color if value >= 0 else NEGATIVE for value in values]
        ax.bar(x, values, color=bar_colors, width=0.60)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xticks(x)
        ax.set_xticklabels(table["seed_group"].tolist(), fontsize=10.0, fontweight="semibold")
        ax.set_ylabel("Seed-band Spearman rho", fontsize=10.8, fontweight="semibold")
        ax.set_title(metric_label(metric), fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)
        for idx, value in enumerate(values):
            ax.text(
                idx,
                value + (0.012 if value >= 0 else -0.012),
                f"{value:+.3f}",
                ha="center",
                va="bottom" if value >= 0 else "top",
                fontsize=8.8,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )

    fig.suptitle("Where free throw edges matter by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "This localizes line reliance, free throw shooting, and foul-prone defense inside each seed band.",
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
        "# Free Throw Analysis 2: Pooled Relationship",
        "",
        f"- FTR seed-adjusted rho: `{table.loc[table['metric'] == 'FTR', 'seed_adjusted_rho'].iloc[0]:+.3f}`.",
        f"- FT% seed-adjusted rho: `{table.loc[table['metric'] == 'FT%', 'seed_adjusted_rho'].iloc[0]:+.3f}`.",
        f"- FTRD seed-adjusted rho: `{table.loc[table['metric'] == 'FTRD', 'seed_adjusted_rho'].iloc[0]:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_year(table: pd.DataFrame, out_path: Path) -> None:
    ftr_positive = int((table["ftr_rho"] > 0).sum())
    ft_positive = int((table["ft_pct_rho"] > 0).sum())
    ftrd_positive = int((table["ftrd_rho"] > 0).sum())
    lines = [
        "# Free Throw Analysis 3: By Year",
        "",
        f"- FTR was positive in only `{ftr_positive}` of `{len(table)}` tournament years.",
        f"- FT% was positive in `{ft_positive}` of `{len(table)}` tournament years.",
        f"- FTRD was positive in `{ftrd_positive}` of `{len(table)}` tournament years.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_seed(table: pd.DataFrame, out_path: Path) -> None:
    best_ftr = table.loc[table["ftr_rho"].idxmax()]
    worst_ftr = table.loc[table["ftr_rho"].idxmin()]
    best_ft = table.loc[table["ft_pct_rho"].idxmax()]
    best_ftrd = table.loc[table["ftrd_rho"].idxmax()]
    lines = [
        "# Free Throw Analysis 4: Seed-Band Split",
        "",
        f"- Best FTR band: `{best_ftr['seed_group']}` at `{best_ftr['ftr_rho']:+.3f}`.",
        f"- Worst FTR band: `{worst_ftr['seed_group']}` at `{worst_ftr['ftr_rho']:+.3f}`.",
        f"- Best FT% band: `{best_ft['seed_group']}` at `{best_ft['ft_pct_rho']:+.3f}`.",
        f"- Best FTRD band: `{best_ftrd['seed_group']}` at `{best_ftrd['ftrd_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_free_throw_rate_data(paths["input"])
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
