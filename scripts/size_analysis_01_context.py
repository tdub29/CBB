#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

from size_analysis_common import (
    ACCENT,
    CURRENT_FIELD_YEAR,
    HIGHLIGHT,
    LABEL_TEXT,
    SECONDARY,
    TEXT_DARK,
    TEXT_LIGHT,
    TRANSFER_PORTAL_YEAR,
    apply_report_background,
    format_year_tick,
    get_size_base_paths,
    load_projected_field,
    load_size_data,
    load_tournament_data,
    metric_label,
    set_project_theme,
    styled_box,
)

DOMINANT_BIG_MAN_THRESHOLD = 4.0
POSITIONAL_DATA_START_YEAR = 2010


def get_paths() -> dict[str, Path]:
    base = get_size_base_paths()
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "seeds_2026": base["seeds_2026"],
        "fig": project_root / "reports" / "figures" / "size_context_over_time.png",
        "md": project_root / "reports" / "size_context_over_time_summary.md",
        "csv": project_root / "reports" / "size_context_over_time_table.csv",
    }


def build_context_table(df: pd.DataFrame, primary_metric: str, seeds_path: Path) -> pd.DataFrame:
    historical = df[df["year"].between(2008, CURRENT_FIELD_YEAR) & (df["year"] != 2020)].copy()
    context = (
        historical.groupby("year", as_index=False)
        .agg(
            field_team_count=("Team", "size"),
            field_avg_effhgt=("EFFHGT", "mean"),
        )
        .sort_values("year", kind="stable")
        .reset_index(drop=True)
    )
    tournament = load_tournament_data(df)
    dominant_big_counts = (
        tournament.assign(dominant_big_man=tournament["max_c_porpag"].fillna(0) >= DOMINANT_BIG_MAN_THRESHOLD)
        .groupby("year", as_index=False)
        .agg(
            tournament_team_count=("Team", "size"),
            dominant_big_man_count=("dominant_big_man", "sum"),
        )
    )
    context = context.merge(dominant_big_counts, on="year", how="left")
    projected_field = load_projected_field(seeds_path, df)
    projected_dominant_big_count = int((projected_field["max_c_porpag"].fillna(0) >= DOMINANT_BIG_MAN_THRESHOLD).sum())
    context.loc[context["year"] == CURRENT_FIELD_YEAR, "tournament_team_count"] = int(len(projected_field))
    context.loc[context["year"] == CURRENT_FIELD_YEAR, "dominant_big_man_count"] = float(projected_dominant_big_count)
    context["tournament_team_count"] = context["tournament_team_count"].fillna(0).astype(int)
    context["dominant_big_man_count"] = context["dominant_big_man_count"].astype(float)
    context.loc[context["year"] < POSITIONAL_DATA_START_YEAR, "dominant_big_man_count"] = np.nan
    return context


def plot_context(context_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    years = context_table["year"].to_numpy(dtype=float)
    eff = context_table["field_avg_effhgt"].to_numpy(dtype=float)
    big_counts = context_table["dominant_big_man_count"].to_numpy(dtype=float)
    page_title = "Size context over time: field height and dominant big men"

    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(16.5, 11.0), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.82, bottom=0.10, left=0.08, right=0.95, hspace=0.34)

    top_ax, bottom_ax = axes

    top_ax.plot(years, eff, color=ACCENT, linewidth=2.8, marker="o", markersize=4.5)
    top_ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=2.0, alpha=0.95, zorder=0)
    top_ax.spines["top"].set_visible(False)
    top_ax.spines["right"].set_visible(False)
    top_ax.grid(False)
    top_ax.tick_params(axis="x", labelsize=10.5, length=0, pad=7)
    top_ax.tick_params(axis="y", labelsize=10.5, length=0, pad=7)
    top_ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
    top_ax.set_xticks(years)
    top_ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.5)

    top_ax.text(
        TRANSFER_PORTAL_YEAR + 0.2,
        float(np.nanmax(eff)) + 0.08,
        "2022 transfer-rules era",
        ha="left",
        va="bottom",
        fontsize=10.5,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    top_ax.text(
        years[0] - 0.15,
        eff[0] + 0.02,
        f"{eff[0]:.2f}",
        ha="right",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    top_ax.text(
        years[-1] + 0.45,
        eff[-1],
        f"{eff[-1]:.2f}",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    top_ax.set_ylabel("Field avg EFFHGT", fontsize=11.5, fontweight="semibold")
    top_ax.set_xlabel("Season", fontsize=11.5, fontweight="semibold")
    top_ax.set_xlim(years.min() - 0.5, years.max() + 1.8)
    top_ax.set_ylim(float(np.nanmin(eff)) - 0.18, float(np.nanmax(eff)) + 0.03)
    top_ax.text(
        0.5,
        0.985,
        "Each point is the field-average EFFHGT for one completed season",
        transform=top_ax.transAxes,
        ha="center",
        va="top",
        fontsize=9.3,
        color=TEXT_LIGHT,
    )
    top_ax.set_title("Field-average effective height", fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
    for label, year in zip(top_ax.get_xticklabels(), years):
        if int(year) == CURRENT_FIELD_YEAR:
            label.set_fontweight("bold")
            label.set_color(LABEL_TEXT)

    bottom_ax.plot(years, big_counts, color=HIGHLIGHT, linewidth=2.8, marker="o", markersize=4.8)
    bottom_ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=2.0, alpha=0.95, zorder=0)
    bottom_ax.spines["top"].set_visible(False)
    bottom_ax.spines["right"].set_visible(False)
    bottom_ax.grid(False)
    bottom_ax.tick_params(axis="x", labelsize=10.5, length=0, pad=7)
    bottom_ax.tick_params(axis="y", labelsize=10.5, length=0, pad=7)
    bottom_ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
    bottom_ax.set_xticks(years)
    bottom_ax.set_xticklabels([str(int(year)) for year in years], fontsize=10.5)
    bottom_ax.set_xlim(years.min() - 0.5, years.max() + 1.8)
    bottom_ax.set_ylim(float(np.nanmin(big_counts)) - 1.0, float(np.nanmax(big_counts)) + 1.8)
    bottom_ax.set_ylabel("Dominant big men", fontsize=11.5, fontweight="semibold")
    bottom_ax.set_xlabel("Season", fontsize=11.5, fontweight="semibold")
    bottom_ax.set_title(
        "Tournament count of dominant big men",
        fontsize=12.8,
        fontweight="bold",
        color=TEXT_DARK,
        pad=10,
    )
    bottom_ax.text(
        0.5,
        0.985,
        f"Count of tournament teams whose top C posted max_c_porpag >= {DOMINANT_BIG_MAN_THRESHOLD:.1f}; 2026 uses the projected field and no positional data exists in 2008-2009",
        transform=bottom_ax.transAxes,
        ha="center",
        va="top",
        fontsize=9.3,
        color=TEXT_LIGHT,
    )
    first_valid_idx = int(np.flatnonzero(~np.isnan(big_counts))[0])
    bottom_ax.text(
        years[first_valid_idx] - 0.15,
        big_counts[first_valid_idx] + 0.20,
        f"{int(big_counts[first_valid_idx])}",
        ha="right",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    bottom_ax.text(
        years[-1] + 0.45,
        big_counts[-1],
        f"{int(big_counts[-1])}",
        ha="left",
        va="center",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    bottom_ax.text(
        TRANSFER_PORTAL_YEAR + 0.2,
        float(np.nanmax(big_counts)) + 0.7,
        "2022 transfer-rules era",
        ha="left",
        va="bottom",
        fontsize=10.5,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    bottom_ax.text(
        2008.15,
        float(np.nanmin(big_counts)) + 1.2,
        "No center-position data\nfor 2008-2009",
        ha="left",
        va="center",
        fontsize=10.0,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    for label, year in zip(bottom_ax.get_xticklabels(), years):
        if int(year) == CURRENT_FIELD_YEAR:
            label.set_fontweight("bold")
            label.set_color(LABEL_TEXT)

    fig.suptitle(page_title, fontsize=23, fontweight="bold", y=0.988)
    fig.text(
        0.5,
        0.948,
        f"Field-level EFFHGT and dominant-big-man counts from 2008-{CURRENT_FIELD_YEAR}, with 2020 excluded and positional data starting in {POSITIONAL_DATA_START_YEAR}",
        ha="center",
        va="center",
        fontsize=10.2,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_summary(
    context_table: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    latest_row = context_table.iloc[-1]
    earliest_row = context_table.iloc[0]
    field_eff_change = float(latest_row["field_avg_effhgt"] - earliest_row["field_avg_effhgt"])
    dominant_big_rows = context_table[context_table["dominant_big_man_count"].notna()].copy()
    dominant_big_earliest = dominant_big_rows.iloc[0]
    dominant_big_latest = dominant_big_rows.iloc[-1]
    dominant_big_change = int(dominant_big_latest["dominant_big_man_count"] - dominant_big_earliest["dominant_big_man_count"])
    lines = [
        "# Size Analysis 1: Size Context Over Time",
        "",
        f"- Locked metric for the size thread: `{primary_metric}` (`{metric_label(primary_metric)}`).",
        f"- Full-field `EFFHGT` moved from `{earliest_row['field_avg_effhgt']:.2f}` in `{int(earliest_row['year'])}` to `{latest_row['field_avg_effhgt']:.2f}` in `{int(latest_row['year'])}` (`{field_eff_change:+.2f}`), and the chart now includes `{CURRENT_FIELD_YEAR}`.",
        f"- Dominant-big-man count starts in `{int(dominant_big_earliest['year'])}` because there is no positional data for `2008-2009`; it moved from `{int(dominant_big_earliest['dominant_big_man_count'])}` in `{int(dominant_big_earliest['year'])}` to `{int(dominant_big_latest['dominant_big_man_count'])}` in `{int(dominant_big_latest['year'])}` (`{dominant_big_change:+d}`), using `max_c_porpag >= {DOMINANT_BIG_MAN_THRESHOLD:.1f}`. The `{CURRENT_FIELD_YEAR}` point uses the projected field.",
        "- Interpretation: this chart is field-level context only. Size Day does not use conference or power-conference framing.",
        "",
        "## Table",
        "",
        context_table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_size_data(paths["input"])
    primary_metric = "EFFHGT"
    context_table = build_context_table(full_df, primary_metric, paths["seeds_2026"])
    context_table.round(6).to_csv(paths["csv"], index=False)
    plot_context(context_table, primary_metric, paths["fig"])
    write_summary(context_table, primary_metric, paths["md"])

    print(f"Primary metric: {primary_metric}")
    print(f"Wrote figure: {paths['fig']}")
    print(f"Wrote summary: {paths['md']}")
    print(f"Wrote table: {paths['csv']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
