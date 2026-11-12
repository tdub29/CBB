#!/usr/bin/env python3
from __future__ import annotations

import argparse
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import matplotlib.patheffects as pe
from matplotlib.ticker import FuncFormatter

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.visualization.palette import PROJECT_PALETTE, apply_report_background, set_project_theme

CONFERENCE_RENAMES = {
    "P10": "P12",
}
POWER_CONFERENCES = {"ACC", "B10", "B12", "BE", "SEC", "P12"}
TRANSFER_PORTAL_YEAR = 2022
CONFERENCE_COLORS = [
    PROJECT_PALETTE["signal_blue"],
    PROJECT_PALETTE["lake_blue"],
    PROJECT_PALETTE["sky_blue"],
    PROJECT_PALETTE["fog"],
    PROJECT_PALETTE["sage"],
    PROJECT_PALETTE["steel"],
    PROJECT_PALETTE["navy"],
    PROJECT_PALETTE["storm"],
]
TEXT_DARK = PROJECT_PALETTE["fog"]
TEXT_LIGHT = PROJECT_PALETTE["steel"]
CONFERENCE_STYLE_OVERRIDES = {
    "ACC": {
        "color": "#F08A63",
    },
    "A10": {
        "color": PROJECT_PALETTE["lake_blue"],
        "linewidth": 3.1,
        "alpha": 1.0,
        "zorder": 4,
        "endpoint_size": 34,
    },
    "B12": {
        "color": PROJECT_PALETTE["fog"],
    },
    "SEC": {
        "color": "#E7B14C",
    },
    "P12": {
        "color": PROJECT_PALETTE["sage"],
        "linewidth": 3.1,
        "alpha": 1.0,
        "zorder": 4,
        "endpoint_size": 34,
    },
}


def format_year_tick(value: float, _: object) -> str:
    return str(int(value)) if float(value).is_integer() else ""


def format_barthag_tick(value: float, _: object) -> str:
    return f"{value:.2f}"


def parse_args() -> argparse.Namespace:
    return argparse.ArgumentParser(
        description="SOS Analysis 1: conference strength by year."
    ).parse_args()


def styled_label_box() -> dict[str, object]:
    return {
        "facecolor": PROJECT_PALETTE["midnight"],
        "edgecolor": PROJECT_PALETTE["storm"],
        "linewidth": 1.0,
        "alpha": 0.94,
        "boxstyle": "round,pad=0.25",
    }


def get_conference_style(conference: str, idx: int) -> dict[str, object]:
    style = {
        "color": CONFERENCE_COLORS[idx % len(CONFERENCE_COLORS)],
        "linewidth": 2.15,
        "alpha": 0.95,
        "zorder": 2,
        "endpoint_size": 0,
    }
    style.update(CONFERENCE_STYLE_OVERRIDES.get(conference, {}))
    return style


def get_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input": project_root / "data" / "processed" / "sos_sample.csv",
        "figure": project_root / "reports" / "figures" / "sos_conf_strength_over_time.png",
        "report": project_root / "reports" / "sos_conf_strength_analysis.md",
        "rank_table": project_root / "reports" / "sos_conf_strength_rankings.csv",
        "gap_table": project_root / "reports" / "sos_conf_strength_gap.csv",
        "breakpoint_table": project_root / "reports" / "sos_conf_strength_breakpoints.csv",
    }


def display_path(path: Path) -> str:
    """Path relative to the repo root, so the report does not depend on where the repo was cloned."""
    try:
        return path.resolve().relative_to(REPO_ROOT).as_posix()
    except ValueError:
        return path.as_posix()


def load_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    if "year" not in df.columns and "Year" in df.columns:
        df = df.rename(columns={"Year": "year"})
    if "Conf" not in df.columns and "conf" in df.columns:
        df = df.rename(columns={"conf": "Conf"})

    required = {"Team", "year", "Conf", "Barthag", "conf_avg_barthag"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    out = df.copy()
    out["year"] = pd.to_numeric(out["year"], errors="coerce")
    out["Barthag"] = pd.to_numeric(out["Barthag"], errors="coerce")
    out["conf_avg_barthag"] = pd.to_numeric(out["conf_avg_barthag"], errors="coerce")
    out["Conf"] = out["Conf"].astype(str).str.strip().replace(CONFERENCE_RENAMES)
    out = out.dropna(subset=["year", "Barthag", "Conf"])
    out = out[out["year"] != 2020].copy()
    return out


def build_conference_year_table(df: pd.DataFrame) -> pd.DataFrame:
    conf_year = (
        df.groupby(["year", "Conf"], as_index=False)
        .agg(
            avg_barthag=("Barthag", "mean"),
            stored_conf_avg_barthag=("conf_avg_barthag", "mean"),
            team_count=("Team", "nunique"),
        )
        .sort_values(["year", "avg_barthag"], ascending=[True, False], kind="stable")
    )
    conf_year["rank"] = conf_year.groupby("year")["avg_barthag"].rank(
        ascending=False, method="dense"
    )
    conf_year["group"] = np.where(
        conf_year["Conf"].isin(POWER_CONFERENCES), "Power", "Mid-major"
    )
    conf_year["avg_diff_vs_stored"] = (
        conf_year["avg_barthag"] - conf_year["stored_conf_avg_barthag"]
    ).abs()
    return conf_year


def build_gap_table(conf_year: pd.DataFrame) -> pd.DataFrame:
    gap = (
        conf_year.groupby(["year", "group"])["avg_barthag"]
        .mean()
        .unstack("group")
        .reset_index()
        .sort_values("year")
    )
    for column in ("Power", "Mid-major"):
        if column not in gap.columns:
            gap[column] = np.nan
    gap["gap"] = gap["Power"] - gap["Mid-major"]
    gap["gap_change"] = gap["gap"].diff().abs()
    return gap


def build_breakpoints(conf_year: pd.DataFrame, gap: pd.DataFrame) -> pd.DataFrame:
    top_sets = (
        conf_year[conf_year["rank"] <= 6]
        .groupby("year")["Conf"]
        .apply(lambda values: set(values.tolist()))
        .sort_index()
    )

    transitions: list[dict[str, object]] = []
    years = sorted(top_sets.index.tolist())
    gap_lookup = gap.set_index("year")["gap"]

    for previous_year, current_year in zip(years[:-1], years[1:]):
        previous = top_sets.loc[previous_year]
        current = top_sets.loc[current_year]
        union = previous | current
        churn = len(previous.symmetric_difference(current)) / len(union) if union else 0.0
        gap_delta = abs(float(gap_lookup.loc[current_year] - gap_lookup.loc[previous_year]))
        transitions.append(
            {
                "break_year": int(current_year),
                "previous_year": int(previous_year),
                "top6_churn": churn,
                "gap_delta": gap_delta,
                "entered_top6": ", ".join(sorted(current - previous)) or "-",
                "left_top6": ", ".join(sorted(previous - current)) or "-",
            }
        )

    breakpoint_table = pd.DataFrame(transitions)
    if breakpoint_table.empty:
        breakpoint_table["score"] = pd.Series(dtype=float)
        return breakpoint_table

    gap_std = breakpoint_table["gap_delta"].std(ddof=0)
    if pd.isna(gap_std) or gap_std == 0:
        breakpoint_table["gap_delta_z"] = 0.0
    else:
        breakpoint_table["gap_delta_z"] = (
            breakpoint_table["gap_delta"] - breakpoint_table["gap_delta"].mean()
        ) / gap_std

    breakpoint_table["score"] = breakpoint_table["top6_churn"] + breakpoint_table["gap_delta_z"].clip(lower=0)
    breakpoint_table = breakpoint_table.sort_values(
        ["score", "top6_churn", "gap_delta"], ascending=False, kind="stable"
    ).reset_index(drop=True)
    return breakpoint_table


def choose_plot_conferences(conf_year: pd.DataFrame) -> list[str]:
    top12 = conf_year[conf_year["rank"] <= 12].copy()
    summary = (
        top12.groupby("Conf", as_index=False)
        .agg(
            top12_appearances=("year", "nunique"),
            avg_rank=("rank", "mean"),
            peak_barthag=("avg_barthag", "max"),
        )
        .sort_values(
            ["top12_appearances", "avg_rank", "peak_barthag"],
            ascending=[False, True, False],
            kind="stable",
        )
    )
    return summary.head(10)["Conf"].tolist()


def build_plot_ticks(years: list[int], breakpoint_years: list[int]) -> list[int]:
    tick_set = {year for year in years if year % 2 == 0}
    tick_set.update(breakpoint_years)
    tick_set.update({years[0], years[-1]})
    return sorted(tick_set)


def adjust_label_positions(
    labels: list[tuple[str, float]], lower: float, upper: float, min_gap: float
) -> dict[str, float]:
    if not labels:
        return {}

    ordered = sorted(labels, key=lambda item: item[1])
    adjusted: list[list[float | str]] = [[name, value] for name, value in ordered]

    adjusted[0][1] = max(float(adjusted[0][1]), lower)
    for idx in range(1, len(adjusted)):
        adjusted[idx][1] = max(float(adjusted[idx][1]), float(adjusted[idx - 1][1]) + min_gap)

    overflow = float(adjusted[-1][1]) - upper
    if overflow > 0:
        for item in adjusted:
            item[1] = float(item[1]) - overflow

    adjusted[-1][1] = min(float(adjusted[-1][1]), upper)
    for idx in range(len(adjusted) - 2, -1, -1):
        adjusted[idx][1] = min(float(adjusted[idx][1]), float(adjusted[idx + 1][1]) - min_gap)

    underflow = lower - float(adjusted[0][1])
    if underflow > 0:
        for item in adjusted:
            item[1] = float(item[1]) + underflow

    return {str(name): float(value) for name, value in adjusted}


def build_era_ranges(years: list[int], breakpoint_years: list[int]) -> list[tuple[int, int]]:
    if not years:
        return []
    sorted_breaks = sorted(set(year for year in breakpoint_years if year in years))
    eras: list[tuple[int, int]] = []
    start = years[0]
    for break_year in sorted_breaks:
        eras.append((start, break_year - 1))
        start = break_year
    eras.append((start, years[-1]))
    return [era for era in eras if era[0] <= era[1]]


def select_breakpoint_years(
    breakpoint_table: pd.DataFrame, max_breaks: int = 3, min_spacing: int = 2
) -> list[int]:
    if breakpoint_table.empty:
        return []

    selected: list[int] = []
    for break_year in breakpoint_table["break_year"].astype(int).tolist():
        if all(abs(break_year - existing) >= min_spacing for existing in selected):
            selected.append(break_year)
        if len(selected) == max_breaks:
            break
    return sorted(selected)


def plot_results(
    conf_year: pd.DataFrame,
    gap: pd.DataFrame,
    breakpoint_table: pd.DataFrame,
    out_path: Path,
) -> list[int]:
    out_path.parent.mkdir(parents=True, exist_ok=True)

    selected = choose_plot_conferences(conf_year)
    plot_df = conf_year[conf_year["Conf"].isin(selected)].copy()
    years = sorted(conf_year["year"].astype(int).unique().tolist())

    breakpoint_years = select_breakpoint_years(breakpoint_table)
    tick_years = build_plot_ticks(years, [])
    sixth_place_by_year = (
        conf_year.sort_values(["year", "avg_barthag"], ascending=[True, False], kind="stable")
        .groupby("year")
        .nth(5)["avg_barthag"]
    )
    power_threshold = float(sixth_place_by_year.median())

    set_project_theme()
    fig, axes = plt.subplots(
        2,
        1,
        figsize=(17.5, 12),
        gridspec_kw={"height_ratios": [2.1, 1]},
        constrained_layout=False,
        sharex=True,
    )
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.88, bottom=0.08, left=0.07, right=0.96, hspace=0.34)

    ax = axes[0]
    y_values: list[float] = []
    last_points: list[tuple[str, float]] = []
    for idx, conference in enumerate(selected):
        conference_df = plot_df[plot_df["Conf"] == conference].sort_values("year")
        style = get_conference_style(conference, idx)
        y_values.extend(conference_df["avg_barthag"].tolist())
        ax.plot(
            conference_df["year"],
            conference_df["avg_barthag"],
            linewidth=float(style["linewidth"]),
            color=str(style["color"]),
            alpha=float(style["alpha"]),
            zorder=int(style["zorder"]),
        )
        if not conference_df.empty:
            last = conference_df.iloc[-1]
            last_points.append((conference, float(last["avg_barthag"])))
            if int(style["endpoint_size"]) > 0:
                ax.scatter(
                    [float(last["year"])],
                    [float(last["avg_barthag"])],
                    s=float(style["endpoint_size"]),
                    color=str(style["color"]),
                    edgecolor=PROJECT_PALETTE["paper"],
                    linewidth=0.9,
                    zorder=int(style["zorder"]) + 1,
                )

    ax.axhline(power_threshold, color=PROJECT_PALETTE["steel"], linestyle="--", linewidth=1.4, alpha=0.9)
    ax.axvline(TRANSFER_PORTAL_YEAR, color=PROJECT_PALETTE["slate"], linestyle=":", linewidth=2.2, alpha=0.95)
    ax.text(
        TRANSFER_PORTAL_YEAR + 0.12,
        max(y_values) + 0.015,
        "Current transfer-rules era starts",
        fontsize=12,
        fontweight="bold",
        color=PROJECT_PALETTE["fog"],
        ha="left",
        va="bottom",
        bbox=styled_label_box(),
    )
    ax.text(
        max(years) + 0.14,
        power_threshold + 0.006,
        "Power conf. threshold",
        fontsize=11,
        fontweight="bold",
        color=PROJECT_PALETTE["fog"],
        va="bottom",
        ha="left",
        bbox=styled_label_box(),
    )

    ax.set_xlabel("")
    ax.set_ylabel("Avg conference Barthag", fontsize=11, fontweight="semibold")
    ax.set_xticks(tick_years)
    ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
    ax.yaxis.set_major_formatter(FuncFormatter(format_barthag_tick))
    ax.set_xlim(min(years) - 0.5, max(years) + 1.2)
    lower = min(y_values + [power_threshold]) - 0.03
    upper = max(y_values + [power_threshold]) + 0.03
    ax.set_ylim(lower, upper)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10, length=0, pad=6, labelbottom=True)
    ax.tick_params(axis="y", labelsize=10, length=0, pad=6)
    label_positions = adjust_label_positions(last_points, lower + 0.015, upper - 0.015, min_gap=0.015)
    for idx, conference in enumerate(selected):
        conference_df = plot_df[plot_df["Conf"] == conference].sort_values("year")
        if conference_df.empty:
            continue
        style = get_conference_style(conference, idx)
        last = conference_df.iloc[-1]
        label_y = label_positions[conference]
        ax.plot(
            [float(last["year"]), max(years) + 0.1],
            [float(last["avg_barthag"]), label_y],
            color=str(style["color"]),
            linewidth=1.5 if conference in CONFERENCE_STYLE_OVERRIDES else 1.1,
            alpha=float(style["alpha"]),
            zorder=int(style["zorder"]),
        )
        text = ax.text(
            max(years) + 0.14,
            label_y,
            conference,
            fontsize=11,
            fontweight="bold",
            color=str(style["color"]),
            va="center",
            bbox=styled_label_box(),
        )
        text.set_path_effects([pe.withStroke(linewidth=2.4, foreground=PROJECT_PALETTE["midnight"])])

    gap_ax = axes[1]
    gap_ax.plot(
        gap["year"],
        gap["gap"],
        color=PROJECT_PALETTE["signal_blue"],
        linewidth=2.8,
        marker="o",
        markersize=4,
    )
    gap_ax.fill_between(gap["year"], gap["gap"], color=PROJECT_PALETTE["lake_blue"], alpha=0.28)
    gap_ax.axhline(gap["gap"].mean(), color=PROJECT_PALETTE["steel"], linestyle=":", linewidth=1.2)
    gap_ax.axvline(TRANSFER_PORTAL_YEAR, color=PROJECT_PALETTE["slate"], linestyle=":", linewidth=2.2, alpha=0.95)
    gap_ax.text(
        max(years) + 0.12,
        float(gap["gap"].mean()) + 0.004,
        "Avg gap",
        fontsize=10,
        fontweight="bold",
        color=PROJECT_PALETTE["fog"],
        va="bottom",
        ha="left",
        bbox=styled_label_box(),
    )

    gap_ax.set_title(
        "Power-minus-mid-major conference Barthag gap",
        fontsize=13,
        fontweight="bold",
        color=TEXT_DARK,
        pad=16,
    )
    gap_ax.text(
        0.5,
        1.005,
        "How much stronger the average power conference was than the average mid-major each season",
        transform=gap_ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    gap_ax.set_xlabel("Season", fontsize=11, fontweight="semibold")
    gap_ax.set_ylabel("Power - mid-major gap", fontsize=11, fontweight="semibold")
    gap_ax.set_xticks(tick_years)
    gap_ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
    gap_ax.yaxis.set_major_formatter(FuncFormatter(format_barthag_tick))
    gap_ax.set_ylim(0.00, 0.45)
    gap_ax.grid(False)
    gap_ax.spines["top"].set_visible(False)
    gap_ax.spines["right"].set_visible(False)
    gap_ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
    gap_ax.tick_params(axis="y", labelsize=10, length=0, pad=6)

    max_gap_row = gap.loc[gap["gap"].idxmax()]
    min_gap_row = gap.loc[gap["gap"].idxmin()]
    transfer_gap = float(gap.loc[gap["year"] == TRANSFER_PORTAL_YEAR, "gap"].iloc[0])
    pct_increase_since_transfer = ((float(max_gap_row["gap"]) - transfer_gap) / transfer_gap) * 100
    gap_ax.annotate(
        f"Widest gap: {int(max_gap_row['year'])}\n{max_gap_row['gap']:.3f} ({pct_increase_since_transfer:+.1f}% vs {TRANSFER_PORTAL_YEAR})",
        xy=(max_gap_row["year"], max_gap_row["gap"]),
        xytext=(max_gap_row["year"] - 1.1, max_gap_row["gap"] + 0.015),
        arrowprops={"arrowstyle": "->", "color": "#444444"},
        fontsize=11,
        fontweight="bold",
        bbox=styled_label_box(),
    )
    gap_ax.annotate(
        f"Narrowest gap: {int(min_gap_row['year'])}\n{min_gap_row['gap']:.3f}",
        xy=(min_gap_row["year"], min_gap_row["gap"]),
        xytext=(min_gap_row["year"] + 0.3, min_gap_row["gap"] - 0.03),
        arrowprops={"arrowstyle": "->", "color": "#444444"},
        fontsize=11,
        fontweight="bold",
        bbox=styled_label_box(),
    )

    top_pos = ax.get_position()
    top_center = (top_pos.x0 + top_pos.x1) / 2
    fig.text(
        top_center,
        top_pos.y1 + 0.018,
        "Conference strength over time",
        ha="center",
        va="bottom",
        fontsize=13,
        fontweight="bold",
        color=TEXT_DARK,
    )
    fig.text(
        top_center,
        top_pos.y1 + 0.003,
        f"Top panel shows the top {len(selected)} conferences by top-12 appearances, not all leagues; average conference Barthag by season with the power-conference threshold marked",
        ha="center",
        va="bottom",
        fontsize=9,
        color=TEXT_LIGHT,
    )

    fig.suptitle(
        "Conference Strength and Power gap over time",
        fontsize=22,
        fontweight="bold",
        y=0.975,
    )
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)
    return breakpoint_years


def write_outputs(
    conf_year: pd.DataFrame,
    gap: pd.DataFrame,
    breakpoint_table: pd.DataFrame,
    breakpoint_years: list[int],
    paths: dict[str, Path],
) -> None:
    paths["rank_table"].parent.mkdir(parents=True, exist_ok=True)
    conf_year.sort_values(["year", "rank", "Conf"]).to_csv(paths["rank_table"], index=False)
    gap.to_csv(paths["gap_table"], index=False)
    breakpoint_table.to_csv(paths["breakpoint_table"], index=False)

    years = sorted(conf_year["year"].astype(int).unique().tolist())
    eras = build_era_ranges(years, breakpoint_years)
    top_ranked = (
        conf_year.sort_values(["year", "rank", "avg_barthag"], ascending=[False, True, False])
        .groupby("year")
        .head(6)
        .copy()
    )
    latest_year = max(years)
    latest_top = top_ranked[top_ranked["year"] == latest_year][["Conf", "avg_barthag", "rank"]]
    strongest_breaks = breakpoint_table[
        breakpoint_table["break_year"].isin(breakpoint_years)
    ][["break_year", "top6_churn", "gap_delta", "entered_top6", "left_top6"]].sort_values(
        "break_year"
    )

    max_diff = conf_year["avg_diff_vs_stored"].max()
    mean_gap = gap["gap"].mean()
    max_gap_row = gap.loc[gap["gap"].idxmax()]
    min_gap_row = gap.loc[gap["gap"].idxmin()]

    lines = [
        "# SOS Analysis 1: Conference Strength by Year",
        "",
        f"- Input: `{display_path(paths['input'])}`",
        f"- Figure: `{display_path(paths['figure'])}`",
        f"- Rows analyzed: `{len(conf_year):,}` conference-year rows from `{years[0]}-{years[-1]}` (2020 excluded).",
        f"- Conference avg validation: max absolute diff between recomputed and stored `conf_avg_barthag` = `{max_diff:.10f}`.",
        "",
        "## Key Findings",
        "",
        f"- Mean power-vs-mid-major gap across the sample: `{mean_gap:.3f}` Barthag.",
        f"- Widest gap: `{int(max_gap_row['year'])}` at `{max_gap_row['gap']:.3f}`.",
        f"- Narrowest gap: `{int(min_gap_row['year'])}` at `{min_gap_row['gap']:.3f}`.",
        f"- Suggested breakpoint years: `{', '.join(str(year) for year in breakpoint_years) if breakpoint_years else 'none'}`.",
        "",
        "## Suggested Era Ranges",
        "",
    ]

    for start, end in eras:
        lines.append(f"- `{start}-{end}`")

    lines.extend(
        [
            "",
            f"## Latest Season Top Conferences ({latest_year})",
            "",
            latest_top.to_markdown(index=False),
            "",
            "## Breakpoint Candidates",
            "",
            strongest_breaks.to_markdown(index=False),
            "",
            "## Review Notes",
            "",
            "- Breakpoints are based on a combined score of top-6 membership churn and year-over-year power-gap change.",
            "- Suggested breakpoints are filtered to avoid adjacent-year cuts that create artificial one-year eras.",
            "- The chart favors readability over completeness by plotting the conferences with the strongest and most persistent top-12 presence.",
            "- Supporting CSV tables contain the full conference-year ranking history and gap series for a second-pass audit.",
        ]
    )

    paths["report"].write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    parse_args()
    paths = get_paths()
    df = load_data(paths["input"])
    conf_year = build_conference_year_table(df)
    gap = build_gap_table(conf_year)
    breakpoint_table = build_breakpoints(conf_year, gap)
    breakpoint_years = plot_results(conf_year, gap, breakpoint_table, paths["figure"])
    write_outputs(conf_year, gap, breakpoint_table, breakpoint_years, paths)

    print(f"Wrote figure: {paths['figure']}")
    print(f"Wrote report: {paths['report']}")
    print(f"Wrote rankings: {paths['rank_table']}")
    print(f"Wrote gap table: {paths['gap_table']}")
    print(f"Wrote breakpoints: {paths['breakpoint_table']}")
    if breakpoint_years:
        print("Suggested breakpoint years:", ", ".join(str(year) for year in breakpoint_years))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
