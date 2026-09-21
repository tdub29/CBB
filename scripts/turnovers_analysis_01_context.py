#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

from turnovers_analysis_common import (
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
        "fig": root / "reports" / "figures" / "turnovers_context_over_time.png",
        "csv": root / "reports" / "turnovers_context_over_time_table.csv",
        "md": root / "reports" / "turnovers_context_over_time_summary.md",
    }


def build_context_table(df: pd.DataFrame) -> pd.DataFrame:
    historical = df[df["year"].between(2008, CURRENT_FIELD_YEAR) & (df["year"] != 2020)].copy()
    tournament = load_tournament_data(df)
    tournament_counts = tournament.groupby("year", as_index=False).agg(tournament_team_count=("Team", "size"))
    context = (
        historical.groupby("year", as_index=False)
        .agg(
            field_team_count=("Team", "size"),
            avg_tor=("TOR", "mean"),
            avg_tord=("TORD", "mean"),
            avg_blk_pct=("BLK%", "mean"),
            avg_blked_pct=("BLKED%", "mean"),
        )
        .sort_values("year", kind="stable")
        .reset_index(drop=True)
    )
    context = context.merge(tournament_counts, on="year", how="left")
    return context


def plot_context(context: pd.DataFrame, out_path: Path) -> None:
    years = context["year"].to_numpy(dtype=float)
    turnover_gap = (context["avg_tor"] - context["avg_tord"]).to_numpy(dtype=float)
    tor_values = context["avg_tor"].to_numpy(dtype=float)
    tord_values = context["avg_tord"].to_numpy(dtype=float)
    blk_values = context["avg_blk_pct"].to_numpy(dtype=float)

    set_project_theme()
    fig = plt.figure(figsize=(15.4, 9.2), constrained_layout=False)
    gs = fig.add_gridspec(2, 1, height_ratios=[1.12, 1.0])
    ax_turnover = fig.add_subplot(gs[0, :])
    ax_blk = fig.add_subplot(gs[1, :])
    axes = [ax_turnover, ax_blk]
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.96, hspace=0.34)

    for ax in axes:
        ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=1.9, alpha=0.95)
        ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
        ax.set_xticks(years)
        ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.0)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", length=0, pad=7)
        ax.tick_params(axis="y", length=0, pad=6, labelsize=10.0)

    ax_turnover.plot(years, tor_values, color=ACCENT, linewidth=3.0, marker="o", markersize=4.8, label="Offensive turnover rate")
    ax_turnover.plot(years, tord_values, color=HIGHLIGHT, linewidth=2.8, marker="o", markersize=4.5, alpha=0.95, label="Defensive turnovers forced")
    ax_turnover.fill_between(years, tor_values, tord_values, color=PROJECT_PALETTE["steel"], alpha=0.10)
    ax_turnover.set_title(
        "Field turnover environment over time",
        fontsize=13.4,
        fontweight="bold",
        color=TEXT_DARK,
        pad=10,
    )
    ax_turnover.set_ylabel("Field average", fontsize=11.0, fontweight="semibold")
    ax_turnover.legend(loc="upper right", frameon=False, fontsize=11.2, labelcolor=TEXT_DARK)
    ax_turnover.text(
        TRANSFER_PORTAL_YEAR + 0.2,
        float(np.nanmax(np.r_[tor_values, tord_values])),
        "2022 era split",
        ha="left",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax_turnover.text(
        years[-1] + 0.35,
        tor_values[-1] + 0.06,
        f"{tor_values[-1]:.1f}",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax_turnover.text(
        years[-1] + 0.35,
        tord_values[-1] - 0.08,
        f"{tord_values[-1]:.1f}",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax_blk.plot(years, blk_values, color=PROJECT_PALETTE["lake_blue"], linewidth=2.9, marker="o", markersize=4.6)
    ax_blk.set_title("Block rate over time", fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
    ax_blk.text(
        TRANSFER_PORTAL_YEAR + 0.2,
        float(np.nanmax(blk_values)),
        "2022 era split",
        ha="left",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax_blk.text(
        years[-1] + 0.35,
        blk_values[-1],
        f"{blk_values[-1]:.1f}",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    ax_blk.set_xlabel("Season", fontsize=11.0, fontweight="semibold")
    ax_blk.set_ylabel("Field average", fontsize=11.0, fontweight="semibold")
    fig.suptitle("Turnover context over time", fontsize=23, fontweight="bold", y=0.975)
    fig.text(
        0.5,
        0.91,
        f"Tournament-field averages from 2008-{CURRENT_FIELD_YEAR} with 2020 excluded; turnovers on top, one block context panel below",
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
        "# Turnovers Analysis 1: Context Over Time",
        "",
        f"- Field-average offensive turnover rate moved from `{first['avg_tor']:.2f}` in 2008 to `{last['avg_tor']:.2f}` in {CURRENT_FIELD_YEAR}.",
        f"- Field-average defensive turnover pressure moved from `{first['avg_tord']:.2f}` to `{last['avg_tord']:.2f}` over the same span.",
        f"- The field-level gap between those two turnover series is tiny on average (`{(context['avg_tor'] - context['avg_tord']).abs().mean():.2f}`), so they should be shown as one shared environment story rather than two separate headline panels.",
        f"- Block pressure context: `BLK%` moved from `{first['avg_blk_pct']:.2f}` to `{last['avg_blk_pct']:.2f}`, while `BLKED%` moved from `{first['avg_blked_pct']:.2f}` to `{last['avg_blked_pct']:.2f}`.",
        "- Interpretation: the field environment changed enough that turnover edges should be interpreted against modern baselines, not early-sample assumptions.",
        "",
        "## Table",
        "",
        context.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_turnovers_data(paths["input"])
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
