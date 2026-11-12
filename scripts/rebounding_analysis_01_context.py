#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import pandas as pd

from rebounding_analysis_common import (
    ACCENT,
    HIGHLIGHT,
    PROJECT_PALETTE,
    TEXT_DARK,
    apply_report_background,
    choose_primary_metric,
    format_year_tick,
    get_rebounding_base_paths,
    load_rebounding_data,
    load_tournament_data,
    build_overperformance_data,
    set_project_theme,
)


def get_paths() -> dict[str, Path]:
    base = get_rebounding_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "fig": root / "reports" / "figures" / "rebounding_context_over_time.png",
        "csv": root / "reports" / "rebounding_context_over_time_table.csv",
        "md": root / "reports" / "rebounding_context_over_time_summary.md",
    }


def build_context_table(tournament_df: pd.DataFrame) -> pd.DataFrame:
    table = (
        tournament_df.groupby("year", as_index=False)[["ORB", "DRB"]]
        .mean()
        .sort_values("year", kind="stable")
        .reset_index(drop=True)
    )
    return table


def plot_context(table: pd.DataFrame, out_path: Path) -> None:
    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(12.6, 10.2), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.86, bottom=0.10, left=0.10, right=0.96, hspace=0.22)

    specs = [
        ("ORB", "Offensive rebounding rate", ACCENT),
        ("DRB", "Defensive rebounding rate", HIGHLIGHT),
    ]
    years = table["year"].to_numpy()

    for ax, (column, title, color) in zip(axes, specs):
        ax.plot(years, table[column], color=color, linewidth=2.8, marker="o", markersize=4.8)
        ax.axvline(2022, color=PROJECT_PALETTE["steel"], linestyle=":", linewidth=1.2)
        ax.set_title(title, fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
        ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
        ax.set_ylabel("Tournament field average", fontsize=11.0, fontweight="semibold")
        ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=9.5, length=0, pad=7)
        ax.tick_params(axis="y", labelsize=10.0, length=0, pad=7)

    fig.suptitle("Rebounding context over time", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.92,
        "Field-level tournament averages from 2008-2026 with 2020 excluded, offensive and defensive rebounding shown separately.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def write_summary(table: pd.DataFrame, out_path: Path) -> None:
    first = table.iloc[0]
    last = table.iloc[-1]
    lines = [
        "# Rebounding Analysis 1: Context Over Time",
        "",
        f"- Offensive rebounding rate moved from `{first['ORB']:.2f}` in `{int(first['year'])}` to `{last['ORB']:.2f}` in `{int(last['year'])}`.",
        f"- Defensive rebounding rate moved from `{first['DRB']:.2f}` in `{int(first['year'])}` to `{last['DRB']:.2f}` in `{int(last['year'])}`.",
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

    metric_selection = choose_primary_metric(over_df)
    context_table = build_context_table(tournament_df)

    paths["metric_selection"].parent.mkdir(parents=True, exist_ok=True)
    metric_selection.round(6).to_csv(paths["metric_selection"], index=False)
    context_table.round(6).to_csv(paths["csv"], index=False)
    plot_context(context_table, paths["fig"])
    write_summary(context_table, paths["md"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
