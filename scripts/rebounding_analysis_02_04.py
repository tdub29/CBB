#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from rebounding_analysis_common import (
    ACCENT,
    ERAS,
    HIGHLIGHT,
    LABEL_TEXT,
    NEGATIVE,
    POSITIVE,
    PROJECT_PALETTE,
    SECONDARY,
    SEED_GROUP_ORDER,
    TEXT_DARK,
    apply_report_background,
    build_overperformance_data,
    get_rebounding_base_paths,
    load_rebounding_data,
    load_tournament_data,
    metric_label,
    metric_slug,
    set_project_theme,
    styled_box,
)


def get_paths() -> dict[str, Path]:
    base = get_rebounding_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "fig_pooled": root / "reports" / "figures" / "rebounding_vs_finish_pooled.png",
        "fig_year": root / "reports" / "figures" / "rebounding_by_year.png",
        "fig_seed": root / "reports" / "figures" / "rebounding_vs_finish_by_seed_group.png",
        "csv_pooled": root / "reports" / "rebounding_vs_finish_pooled_table.csv",
        "csv_year": root / "reports" / "rebounding_by_year_table.csv",
        "csv_seed": root / "reports" / "rebounding_vs_finish_by_seed_group_table.csv",
        "md_pooled": root / "reports" / "rebounding_vs_finish_pooled_summary.md",
        "md_year": root / "reports" / "rebounding_by_year_summary.md",
        "md_seed": root / "reports" / "rebounding_vs_finish_by_seed_group_summary.md",
    }


def build_pooled_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for metric in ["ORB", "DRB"]:
        slug = metric_slug(metric)
        rho = float(over_df["overperformance"].corr(over_df[f"{slug}_edge"], method="spearman"))
        for bucket_label, mask in [
            ("Better than seed baseline", over_df[f"{slug}_edge"] >= 0),
            ("Worse than seed baseline", over_df[f"{slug}_edge"] < 0),
        ]:
            group = over_df.loc[mask].copy()
            rows.append(
                {
                    "metric": metric,
                    "bucket": bucket_label,
                    "team_count": int(len(group)),
                    "mean_overperformance": float(group["overperformance"].mean()),
                    "median_overperformance": float(group["overperformance"].median()),
                    "seed_adjusted_rho": rho,
                }
            )
    return pd.DataFrame(rows)


def build_year_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for year, group in over_df.groupby("year", as_index=False):
        rows.append(
            {
                "year": int(year),
                "team_count": int(len(group)),
                "orb_seed_adjusted_rho": float(group["overperformance"].corr(group["orb_edge"], method="spearman")),
                "drb_seed_adjusted_rho": float(group["overperformance"].corr(group["drb_edge"], method="spearman")),
            }
        )
    return pd.DataFrame(rows).sort_values("year", kind="stable").reset_index(drop=True)


def build_seed_group_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for seed_group, group in over_df.groupby("seed_group", as_index=False):
        rows.append(
            {
                "seed_group": seed_group,
                "team_count": int(len(group)),
                "orb_seed_adjusted_rho": float(group["overperformance"].corr(group["orb_edge"], method="spearman")),
                "drb_seed_adjusted_rho": float(group["overperformance"].corr(group["drb_edge"], method="spearman")),
            }
        )
    table = pd.DataFrame(rows)
    table["seed_group"] = pd.Categorical(table["seed_group"], categories=SEED_GROUP_ORDER, ordered=True)
    table = table.sort_values("seed_group", kind="stable").reset_index(drop=True)
    table["seed_group"] = table["seed_group"].astype(str)
    return table


def plot_pooled(table: pd.DataFrame, over_df: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, axes = plt.subplots(1, 2, figsize=(15.8, 7.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.07, right=0.98, wspace=0.14)

    for ax, metric, color in zip(axes, ["ORB", "DRB"], [ACCENT, HIGHLIGHT]):
        slug = metric_slug(metric)
        x = over_df[f"{slug}_edge"].to_numpy(dtype=float)
        y = over_df["overperformance"].to_numpy(dtype=float)
        rho = float(table.loc[table["metric"] == metric, "seed_adjusted_rho"].iloc[0])
        slope, intercept = np.polyfit(x, y, 1)
        x_line = np.linspace(float(np.nanmin(x)), float(np.nanmax(x)), 120)
        y_line = slope * x_line + intercept

        ax.scatter(x, y, s=11, color=color, alpha=0.24, edgecolors="none")
        ax.plot(x_line, y_line, color=color, linewidth=2.0, linestyle="--", alpha=0.95)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xlabel("Metric edge vs exact-seed baseline", fontsize=11.0, fontweight="semibold")
        ax.set_ylabel("Finish vs exact-seed expectation", fontsize=11.0, fontweight="semibold")
        ax.set_title(metric_label(metric), fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)
        ax.text(
            0.03,
            0.94,
            f"{metric} rho = {rho:+.3f}\nAll tournament years\n(n={len(over_df)})",
            transform=ax.transAxes,
            ha="left",
            va="top",
            fontsize=8.8,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    fig.suptitle("Rebounding versus March overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Each panel shows the pooled seed-adjusted relationship between rebounding edge and tournament overperformance.",
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
    fig, axes = plt.subplots(2, 1, figsize=(12.6, 10.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.86, bottom=0.10, left=0.10, right=0.96, hspace=0.22)

    specs = [
        ("orb_seed_adjusted_rho", "Offensive rebounding rate", ACCENT),
        ("drb_seed_adjusted_rho", "Defensive rebounding rate", HIGHLIGHT),
    ]
    years = table["year"].to_numpy(dtype=int)
    for ax, (column, title, color) in zip(axes, specs):
        ax.plot(years, table[column], color=color, linewidth=2.6, marker="o", markersize=4.5)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
        ax.set_ylabel("Seed-adjusted Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_xticks(years)
        ax.set_xticklabels([str(year) for year in years])
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=9.5, length=0, pad=7)
        ax.tick_params(axis="y", labelsize=10.0, length=0, pad=7)

    fig.suptitle("Rebounding signal by tournament year", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        "Year-by-year seed-adjusted relationships show whether offensive and defensive rebounding stayed stable or came in waves.",
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
    fig, axes = plt.subplots(1, 2, figsize=(15.8, 7.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.14, left=0.07, right=0.98, wspace=0.14)

    specs = [
        ("orb_seed_adjusted_rho", "Offensive rebounding rate", ACCENT),
        ("drb_seed_adjusted_rho", "Defensive rebounding rate", HIGHLIGHT),
    ]
    x = np.arange(len(table))
    for ax, (column, title, color) in zip(axes, specs):
        values = table[column].to_numpy(dtype=float)
        bar_colors = [color if value >= 0 else NEGATIVE for value in values]
        ax.bar(x, values, color=bar_colors, width=0.60)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2)
        ax.set_xticks(x)
        ax.set_xticklabels(table["seed_group"].tolist(), fontsize=10.0, fontweight="semibold")
        ax.set_ylabel("Seed-band Spearman rho", fontsize=11.0, fontweight="semibold")
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=7)
        low = min(float(np.nanmin(values)), 0.0)
        high = max(float(np.nanmax(values)), 0.0)
        ax.set_ylim(low - 0.15 * (high - low), high + 0.15 * (high - low))
        for idx, value in enumerate(values):
            ax.annotate(
                f"{value:+.3f}",
                (idx, value),
                xytext=(0, 6 if value >= 0 else -6),
                textcoords="offset points",
                ha="center",
                va="bottom" if value >= 0 else "top",
                fontsize=9.0,
                fontweight="bold",
                color=LABEL_TEXT,
                bbox=styled_box(),
            )

    fig.suptitle("Rebounding vs seed-adjusted finish by seed band", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.905,
        "Each panel correlates rebounding with seed-adjusted finish inside one seed band instead of pooling the whole field.",
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
        "# Rebounding Analysis 2: Pooled Relationship",
        "",
        f"- ORB seed-adjusted rho: `{table.loc[table['metric'] == 'ORB', 'seed_adjusted_rho'].iloc[0]:+.3f}`.",
        f"- DRB seed-adjusted rho: `{table.loc[table['metric'] == 'DRB', 'seed_adjusted_rho'].iloc[0]:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_year(table: pd.DataFrame, out_path: Path) -> None:
    orb_best = table.loc[table["orb_seed_adjusted_rho"].idxmax()]
    drb_best = table.loc[table["drb_seed_adjusted_rho"].idxmax()]
    lines = [
        "# Rebounding Analysis 3: Year-by-Year Stability",
        "",
        f"- Best ORB year: `{int(orb_best['year'])}` at `{orb_best['orb_seed_adjusted_rho']:+.3f}`.",
        f"- Best DRB year: `{int(drb_best['year'])}` at `{drb_best['drb_seed_adjusted_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_seed(table: pd.DataFrame, out_path: Path) -> None:
    orb_best = table.loc[table["orb_seed_adjusted_rho"].idxmax()]
    drb_best = table.loc[table["drb_seed_adjusted_rho"].idxmax()]
    lines = [
        "# Rebounding Analysis 4: Seed-Band Split",
        "",
        f"- Strongest ORB seed band: `{orb_best['seed_group']}` at `{orb_best['orb_seed_adjusted_rho']:+.3f}`.",
        f"- Strongest DRB seed band: `{drb_best['seed_group']}` at `{drb_best['drb_seed_adjusted_rho']:+.3f}`.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_rebounding_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    pooled = build_pooled_table(over_df)
    by_year = build_year_table(over_df)
    by_seed = build_seed_group_table(over_df)

    pooled.round(6).to_csv(paths["csv_pooled"], index=False)
    by_year.round(6).to_csv(paths["csv_year"], index=False)
    by_seed.round(6).to_csv(paths["csv_seed"], index=False)

    plot_pooled(pooled, over_df, paths["fig_pooled"])
    plot_year(by_year, paths["fig_year"])
    plot_seed_group(by_seed, paths["fig_seed"])

    write_summary_pooled(pooled, paths["md_pooled"])
    write_summary_year(by_year, paths["md_year"])
    write_summary_seed(by_seed, paths["md_seed"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
