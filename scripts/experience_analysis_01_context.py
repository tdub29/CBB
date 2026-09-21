#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter

from experience_analysis_common import (
    ACCENT,
    COACH_ANALYSIS_START_YEAR,
    LABEL_TEXT,
    PLAYER_PRIMARY_METRIC,
    SECONDARY,
    TEXT_DARK,
    TEXT_LIGHT,
    TRANSFER_PORTAL_YEAR,
    apply_report_background,
    format_year_tick,
    filter_coach_analysis,
    get_experience_base_paths,
    load_experience_data,
    load_or_compute_coach_metric_selection,
    load_tournament_data,
    metric_label,
    set_project_theme,
    styled_box,
)


def get_paths() -> dict[str, Path]:
    base = get_experience_base_paths()
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "fig": project_root / "reports" / "figures" / "experience_context_over_time.png",
        "md": project_root / "reports" / "experience_context_over_time_summary.md",
        "csv": project_root / "reports" / "experience_context_over_time_table.csv",
    }


def build_context_table(df: pd.DataFrame, coach_primary_metric: str) -> pd.DataFrame:
    historical = df[df["year"] != 2020].copy()
    player_context = (
        historical.groupby("year", as_index=False)
        .agg(
            field_team_count=("Team", "size"),
            field_avg_exp=(PLAYER_PRIMARY_METRIC, "mean"),
        )
    )
    coach_context = (
        filter_coach_analysis(historical)
        .groupby("year", as_index=False)
        .agg(field_avg_coach_primary=(coach_primary_metric, "mean"))
    )
    context = (
        player_context.merge(coach_context, on="year", how="left")
        .sort_values("year", kind="stable")
        .reset_index(drop=True)
    )
    return context


def plot_context(context_table: pd.DataFrame, coach_primary_metric: str, out_path: Path) -> None:
    exp_years = context_table["year"].to_numpy(dtype=float)
    exp_values = context_table["field_avg_exp"].to_numpy(dtype=float)
    coach_context = context_table[context_table["field_avg_coach_primary"].notna()].copy()
    coach_years = coach_context["year"].to_numpy(dtype=float)
    coach_values = coach_context["field_avg_coach_primary"].to_numpy(dtype=float)

    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(16.4, 10.8), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.08, right=0.95, hspace=0.34)

    panels = [
        (axes[0], exp_values, "Exp. over time", "Field avg Exp."),
        (axes[1], coach_values, f"{metric_label(coach_primary_metric)} over time", metric_label(coach_primary_metric)),
    ]
    for ax, values, panel_title, y_label in panels:
        panel_years = exp_years if y_label == "Field avg Exp." else coach_years
        ax.plot(panel_years, values, color=ACCENT, linewidth=2.8, marker="o", markersize=5.0)
        ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=2.0, alpha=0.95, zorder=0)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=10.5, length=0, pad=7)
        ax.tick_params(axis="y", labelsize=10.5, length=0, pad=7)
        ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
        ax.set_xticks(panel_years)
        ax.set_xticklabels([str(int(year)) for year in panel_years], fontsize=10.2)
        ax.set_xlim(panel_years.min() - 0.5, panel_years.max() + 1.5)
        ax.set_title(panel_title, fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=12)
        ax.set_ylabel(y_label, fontsize=11.5, fontweight="semibold")
        ax.text(
            TRANSFER_PORTAL_YEAR + 0.2,
            float(np.nanmax(values)) + max((float(np.nanmax(values)) - float(np.nanmin(values))) * 0.06, 0.05),
            "2022 transfer-rules era",
            ha="left",
            va="bottom",
            fontsize=10.2,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )
        ax.text(
            panel_years[-1] + 0.35,
            values[-1],
            f"{values[-1]:.2f}",
            ha="left",
            va="center",
            fontsize=9.8,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    axes[1].set_xlabel("Season", fontsize=11.5, fontweight="semibold")
    fig.suptitle("Player and coach experience over time", fontsize=23, fontweight="bold", y=0.975)
    fig.text(
        0.5,
        0.915,
        "Field-average player experience on top, lead coach-experience metric on bottom",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_summary(path: Path, context_table: pd.DataFrame, coach_primary_metric: str, metric_table: pd.DataFrame) -> None:
    first_row = context_table.iloc[0]
    last_row = context_table.iloc[-1]
    selection_note = str(metric_table["selection_note"].iloc[0])
    lines = [
        "# Experience Analysis 1: Experience Context Over Time",
        "",
        f"- Player metric: `{PLAYER_PRIMARY_METRIC}` (`{metric_label(PLAYER_PRIMARY_METRIC)}`).",
        f"- Lead coach metric: `{coach_primary_metric}` (`{metric_label(coach_primary_metric)}`), using `{COACH_ANALYSIS_START_YEAR}+` seasons only.",
        f"- Coach metric rule: {selection_note}",
        f"- Field-average `Exp.` moved from `{first_row['field_avg_exp']:.2f}` in `{int(first_row['year'])}` to `{last_row['field_avg_exp']:.2f}` in `{int(last_row['year'])}`.",
        f"- Field-average `{metric_label(coach_primary_metric)}` moved from `{context_table.loc[context_table['field_avg_coach_primary'].notna(), 'field_avg_coach_primary'].iloc[0]:.2f}` in `{int(context_table.loc[context_table['field_avg_coach_primary'].notna(), 'year'].iloc[0])}` to `{context_table.loc[context_table['field_avg_coach_primary'].notna(), 'field_avg_coach_primary'].iloc[-1]:.2f}` in `{int(context_table.loc[context_table['field_avg_coach_primary'].notna(), 'year'].iloc[-1])}`.",
        "- Interpretation: this page is setup only. It shows how experienced the field and the average head coach looked by season before testing whether either signal actually matters in March.",
        "",
        "## Table",
        "",
        context_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_experience_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    coach_primary_metric, metric_table = load_or_compute_coach_metric_selection(
        filter_coach_analysis(tournament_df),
        paths["metric_selection"],
    )
    context_table = build_context_table(full_df, coach_primary_metric)
    context_table.round(6).to_csv(paths["csv"], index=False)
    plot_context(context_table, coach_primary_metric, paths["fig"])
    write_summary(paths["md"], context_table, coach_primary_metric, metric_table)
    print(f"Coach primary metric: {coach_primary_metric}")
    print(f"Wrote figure: {paths['fig']}")
    print(f"Wrote summary: {paths['md']}")
    print(f"Wrote table: {paths['csv']}")
    print(f"Wrote metric rule: {paths['metric_selection']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
