#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

from three_point_analysis_common import (
    ACCENT,
    CURRENT_FIELD_YEAR,
    HIGHLIGHT,
    LABEL_TEXT,
    PROJECT_PALETTE,
    SECONDARY,
    TEXT_DARK,
    TRANSFER_PORTAL_YEAR,
    apply_report_background,
    format_year_tick,
    get_three_point_base_paths,
    load_three_point_data,
    metric_label,
    set_project_theme,
    styled_box,
)


def get_paths() -> dict[str, Path]:
    base = get_three_point_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "fig": root / "reports" / "figures" / "three_point_context_over_time.png",
        "csv": root / "reports" / "three_point_context_over_time_table.csv",
        "md": root / "reports" / "three_point_context_over_time_summary.md",
    }


def build_context_table(df: pd.DataFrame) -> pd.DataFrame:
    historical = df[df["year"].between(2008, CURRENT_FIELD_YEAR) & (df["year"] != 2020)].copy()
    context = (
        historical.groupby("year", as_index=False)
        .agg(
            field_team_count=("Team", "size"),
            avg_three_prate=("3PRATE", "mean"),
            avg_three_pct=("3P%", "mean"),
        )
        .sort_values("year", kind="stable")
        .reset_index(drop=True)
    )
    return context


def _label_last_point(ax: plt.Axes, years: np.ndarray, values: np.ndarray, offset: float = 0.0) -> None:
    ax.text(
        years[-1] + 0.35,
        values[-1] + offset,
        f"{values[-1]:.1f}",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )


def plot_context(context: pd.DataFrame, out_path: Path) -> None:
    years = context["year"].to_numpy(dtype=float)
    prate = context["avg_three_prate"].to_numpy(dtype=float)
    three_pct = context["avg_three_pct"].to_numpy(dtype=float)

    set_project_theme()
    fig, ax = plt.subplots(1, 1, figsize=(15.6, 6.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.85, bottom=0.14, left=0.08, right=0.96)

    ax.plot(years, prate, color=ACCENT, linewidth=2.9, marker="o", markersize=4.8, label=metric_label("3PRATE"))
    ax.plot(
        years,
        three_pct,
        color=HIGHLIGHT,
        linewidth=2.8,
        marker="o",
        markersize=4.5,
        alpha=0.96,
        label=metric_label("3P%"),
    )
    ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=1.9, alpha=0.95)
    ax.set_title("Three-point environment over time", fontsize=13.0, fontweight="bold", color=TEXT_DARK, pad=10)
    ax.set_ylabel("Field average", fontsize=11.0, fontweight="semibold")
    ax.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
    ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
    ax.set_xticks(years)
    ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.0)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=7)
    ax.tick_params(axis="y", length=0, pad=6, labelsize=10.0)
    ax.legend(loc="upper left", frameon=False, fontsize=10.6, labelcolor=TEXT_DARK)
    ax.text(
        TRANSFER_PORTAL_YEAR + 0.2,
        float(np.nanmax(np.r_[prate, three_pct])),
        "2022 era split",
        ha="left",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    _label_last_point(ax, years, prate)
    _label_last_point(ax, years, three_pct)

    fig.suptitle("Three-point context over time", fontsize=23, fontweight="bold", y=0.975)
    fig.text(
        0.5,
        0.915,
        f"Tournament-field averages for 3PRATE and 3P% from 2008-{CURRENT_FIELD_YEAR} (2020 excluded)",
        ha="center",
        va="center",
        fontsize=10.2,
        color=PROJECT_PALETTE["steel"],
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_summary(context: pd.DataFrame, out_path: Path) -> None:
    first = context.iloc[0]
    last = context.iloc[-1]
    lines = [
        "# Three-Point Analysis 1: Context Over Time",
        "",
        f"- Field-average `3PRATE` moved from `{first['avg_three_prate']:.2f}` in 2008 to `{last['avg_three_prate']:.2f}` in {CURRENT_FIELD_YEAR}.",
        f"- Field-average `3P%` moved from `{first['avg_three_pct']:.2f}` in 2008 to `{last['avg_three_pct']:.2f}` in {CURRENT_FIELD_YEAR}.",
        "- Interpretation: three-point volume has drifted over time, so the later tournament tests need this era context.",
        "",
        "## Table",
        "",
        context.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_three_point_data(paths["input"])
    context = build_context_table(full_df)
    paths["csv"].parent.mkdir(parents=True, exist_ok=True)
    context.round(6).to_csv(paths["csv"], index=False)
    plot_context(context, paths["fig"])
    write_summary(context, paths["md"])
    print(f"Wrote figure: {paths['fig']}")
    print(f"Wrote summary: {paths['md']}")
    print(f"Wrote table: {paths['csv']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
