#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from experience_analysis_common import (
    ACCENT,
    COACH_ANALYSIS_START_YEAR,
    COACH_SUPPORT_METRIC,
    FINISH_DISPLAY,
    FINISH_ORDER,
    HIGHLIGHT,
    LABEL_TEXT,
    PLAYER_PRIMARY_METRIC,
    PLAYER_SUPPORT_METRIC,
    SECONDARY,
    SEED_GROUP_MAP,
    SEED_GROUP_ORDER,
    TEXT_DARK,
    TEXT_LIGHT,
    apply_report_background,
    build_overperformance_data,
    get_experience_base_paths,
    filter_coach_analysis,
    load_experience_data,
    load_or_compute_coach_metric_selection,
    load_tournament_data,
    metric_label,
    metric_slug,
    set_project_theme,
    styled_box,
)

LATE_ROUND_GATES = [
    ("Elite Eight+", 4),
    ("Final Four+", 5),
    ("Finals+", 6),
    ("Champions", 7),
]

PLAYER_VISUAL_METRIC = PLAYER_SUPPORT_METRIC


def player_visual_label() -> str:
    return metric_label(PLAYER_VISUAL_METRIC)


def player_visual_edge(values: pd.Series | np.ndarray) -> np.ndarray:
    return -np.asarray(values, dtype=float)


def get_paths() -> dict[str, Path]:
    base = get_experience_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "metric_selection": base["metric_selection"],
        "fig_02": root / "reports" / "figures" / "player_exp_vs_finish_pooled.png",
        "fig_03": root / "reports" / "figures" / "player_exp_late_rounds.png",
        "fig_04": root / "reports" / "figures" / "coach_experience_vs_march.png",
        "csv_02": root / "reports" / "player_exp_vs_finish_pooled_table.csv",
        "csv_03": root / "reports" / "player_exp_late_rounds_table.csv",
        "csv_04": root / "reports" / "coach_experience_vs_march_table.csv",
        "md_02": root / "reports" / "player_exp_vs_finish_pooled_summary.md",
        "md_03": root / "reports" / "player_exp_late_rounds_summary.md",
        "md_04": root / "reports" / "coach_experience_vs_march_summary.md",
        "fig_seed_group_player": root / "reports" / "figures" / "experience_player_vs_finish_by_seed_group.png",
        "fig_seed_group_coach": root / "reports" / "figures" / "experience_coach_vs_finish_by_seed_group.png",
        "csv_seed_group": root / "reports" / "experience_vs_finish_by_seed_group_table.csv",
        "md_seed_group": root / "reports" / "experience_vs_finish_by_seed_group_summary.md",
    }


def build_analysis_02_table(df: pd.DataFrame, over_df: pd.DataFrame) -> pd.DataFrame:
    champs = df.loc[df["finish"] == "CHAMPS"]
    r64 = df.loc[df["finish"] == "R64"]
    player_rank_vs_seed = player_visual_edge(over_df[f"{metric_slug(PLAYER_VISUAL_METRIC)}_vs_seed"])
    return pd.DataFrame(
        [
            {
                "metric": PLAYER_VISUAL_METRIC,
                # The edge Series must carry the frame's index: .corr() aligns on labels, and a fresh
                # 0..n-1 index would pair each finish with another team's experience.
                "raw_spearman": float(
                    df["finish_depth"].corr(
                        pd.Series(player_visual_edge(df[PLAYER_VISUAL_METRIC]), index=df.index), method="spearman"
                    )
                ),
                "seed_adjusted_spearman": float(
                    over_df["overperformance"].corr(pd.Series(player_rank_vs_seed, index=over_df.index), method="spearman")
                ),
                "field_avg_exp": float(df[PLAYER_PRIMARY_METRIC].mean()),
                "champ_avg_exp": float(champs[PLAYER_PRIMARY_METRIC].mean()),
                "r64_avg_exp": float(r64[PLAYER_PRIMARY_METRIC].mean()),
                "champ_minus_r64_exp": float(champs[PLAYER_PRIMARY_METRIC].mean() - r64[PLAYER_PRIMARY_METRIC].mean()),
                "field_avg_exp_rank": float(df[PLAYER_SUPPORT_METRIC].mean()),
                "champ_avg_exp_rank": float(champs[PLAYER_SUPPORT_METRIC].mean()),
                "r64_avg_exp_rank": float(r64[PLAYER_SUPPORT_METRIC].mean()),
                "champ_minus_r64_exp_rank": float(
                    champs[PLAYER_SUPPORT_METRIC].mean() - r64[PLAYER_SUPPORT_METRIC].mean()
                ),
            }
        ]
    )


def plot_analysis_02(df: pd.DataFrame, over_df: pd.DataFrame, summary_table: pd.DataFrame, out_path: Path) -> None:
    raw_rho = float(summary_table.loc[0, "raw_spearman"])
    seed_rho = float(summary_table.loc[0, "seed_adjusted_spearman"])

    scatter_df = df[["finish", PLAYER_VISUAL_METRIC]].copy()
    scatter_df["x_base"] = scatter_df["finish"].map({f: i for i, f in enumerate(FINISH_ORDER)})
    scatter_df["x"] = scatter_df["x_base"] + np.random.default_rng(42).uniform(-0.22, 0.22, len(scatter_df))
    raw_slope, raw_intercept = np.polyfit(
        scatter_df["x_base"].to_numpy(float), scatter_df[PLAYER_VISUAL_METRIC].to_numpy(float), 1
    )
    raw_line_x = np.linspace(float(scatter_df["x_base"].min()), float(scatter_df["x_base"].max()), 200)
    raw_line_y = raw_intercept + raw_slope * raw_line_x
    round_means = df.groupby("finish", observed=False)[PLAYER_VISUAL_METRIC].mean().reindex(FINISH_ORDER)
    mean_x = np.arange(len(FINISH_ORDER), dtype=float)
    mean_y = round_means.to_numpy(float)
    field_average = float(df[PLAYER_VISUAL_METRIC].mean())

    x_seed = player_visual_edge(over_df[f"{metric_slug(PLAYER_VISUAL_METRIC)}_vs_seed"])
    y_seed = over_df["overperformance"].to_numpy(float)
    seed_slope, seed_intercept = np.polyfit(x_seed, y_seed, 1)
    seed_line_x = np.linspace(float(np.nanmin(x_seed)), float(np.nanmax(x_seed)), 200)
    seed_line_y = seed_intercept + seed_slope * seed_line_x

    set_project_theme()
    fig, axes = plt.subplots(1, 2, figsize=(17.6, 8.8), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.83, bottom=0.11, left=0.07, right=0.97, wspace=0.16)

    left_ax, right_ax = axes
    left_ax.scatter(
        scatter_df["x"].to_numpy(float),
        scatter_df[PLAYER_VISUAL_METRIC].to_numpy(float),
        s=24,
        color=TEXT_DARK,
        alpha=0.38,
        linewidths=0,
        zorder=1,
    )
    left_ax.plot(raw_line_x, raw_line_y, color=SECONDARY, linestyle="--", linewidth=2.2, zorder=2)
    left_ax.plot(mean_x, mean_y, color=ACCENT, linewidth=2.8, zorder=3)
    left_ax.scatter(mean_x, mean_y, color=ACCENT, s=86, zorder=4)
    left_ax.axhline(field_average, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    left_ax.set_xticks(mean_x)
    left_ax.set_xticklabels([FINISH_DISPLAY[f] for f in FINISH_ORDER], fontsize=10.5, fontweight="semibold")
    left_ax.set_xlabel("Tournament finish", fontsize=11, fontweight="semibold")
    left_ax.set_ylabel(f"{player_visual_label()} (1 = most experienced)", fontsize=11, fontweight="semibold")
    left_ax.set_title("Raw pooled view", fontsize=13, fontweight="bold", color=TEXT_DARK, pad=16)
    left_ax.text(
        0.03,
        0.95,
        f"Raw rho = {raw_rho:+.3f}",
        transform=left_ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.5,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    left_ax.text(
        len(mean_x) - 0.08,
        field_average,
        "Field avg",
        ha="right",
        va="bottom",
        fontsize=9.6,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    left_ax.invert_yaxis()

    right_ax.scatter(x_seed, y_seed, s=24, color=TEXT_DARK, alpha=0.38, linewidths=0, zorder=1)
    right_ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    right_ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    right_ax.plot(seed_line_x, seed_line_y, color=ACCENT, linestyle="--", linewidth=2.6, zorder=2)
    right_ax.set_xlabel(
        "Season-wide Exp rank edge vs exact-seed baseline, negated (+ = more experienced)",
        fontsize=11,
        fontweight="semibold",
    )
    right_ax.set_ylabel("Finish vs exact-seed expectation", fontsize=11, fontweight="semibold")
    right_ax.set_title("Exact-seed controlled view", fontsize=13, fontweight="bold", color=TEXT_DARK, pad=16)
    right_ax.text(
        0.03,
        0.95,
        f"Within-seed rho = {seed_rho:+.3f}",
        transform=right_ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.5,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )
    right_ax.text(
        0.03,
        0.885,
        "0 = seed-line baseline\npositive = more experienced",
        transform=right_ax.transAxes,
        ha="left",
        va="top",
        fontsize=9.6,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    for ax in axes:
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
        ax.tick_params(axis="y", labelsize=10, length=0, pad=6)

    fig.suptitle("Player experience and tournament finish", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        "Left: raw tournament results. Right: each team relative to its exact seed line. "
        "Both rhos use the negated Exp rank, so + means more-experienced teams finished deeper.",
        ha="center",
        va="center",
        fontsize=10.1,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_03_table(df: pd.DataFrame) -> pd.DataFrame:
    baseline_exp = float(df[PLAYER_PRIMARY_METRIC].mean())
    baseline_rank = float(df[PLAYER_SUPPORT_METRIC].mean())
    rows = [
        {
            "gate": "Seeded field",
            "team_count": int(len(df)),
            "avg_exp": baseline_exp,
            "avg_exp_rank": baseline_rank,
            "exp_vs_seeded_field": 0.0,
            "exp_rank_vs_seeded_field": 0.0,
            "rank_advantage_vs_seeded_field": 0.0,
        }
    ]
    for label, threshold in LATE_ROUND_GATES:
        gate_df = df[df["finish_depth"] >= threshold].copy()
        avg_exp = float(gate_df[PLAYER_PRIMARY_METRIC].mean())
        avg_rank = float(gate_df[PLAYER_SUPPORT_METRIC].mean())
        rows.append(
            {
                "gate": label,
                "team_count": int(len(gate_df)),
                "avg_exp": avg_exp,
                "avg_exp_rank": avg_rank,
                "exp_vs_seeded_field": avg_exp - baseline_exp,
                "exp_rank_vs_seeded_field": avg_rank - baseline_rank,
                "rank_advantage_vs_seeded_field": baseline_rank - avg_rank,
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_03(table: pd.DataFrame, out_path: Path) -> None:
    plot_df = table[table["gate"] != "Seeded field"].copy()
    y = np.arange(len(plot_df))
    baseline_row = table.loc[table["gate"] == "Seeded field"].iloc[0]

    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(14.8, 9.8), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.16, right=0.93, hspace=0.30)

    top_ax, bottom_ax = axes
    top_ax.axvline(float(baseline_row["avg_exp_rank"]), color=SECONDARY, linestyle=":", linewidth=1.6)
    top_ax.hlines(y, baseline_row["avg_exp_rank"], plot_df["avg_exp_rank"], color=ACCENT, linewidth=2.4)
    top_ax.scatter(plot_df["avg_exp_rank"], y, color=ACCENT, s=96, zorder=3)
    top_ax.set_yticks(y)
    top_ax.set_yticklabels(plot_df["gate"].tolist(), fontsize=10.8, fontweight="semibold")
    top_ax.set_title("Season-wide Exp rank by late-round gate", fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
    top_ax.set_xlabel("Season-wide Exp rank (1 = most experienced)", fontsize=11.2, fontweight="semibold")
    top_ax.text(
        float(baseline_row["avg_exp_rank"]),
        len(plot_df) - 0.35,
        "Seeded field avg",
        ha="left",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    bottom_ax.axvline(0.0, color=SECONDARY, linestyle=":", linewidth=1.6)
    bottom_ax.hlines(y, 0.0, plot_df["rank_advantage_vs_seeded_field"], color=ACCENT, linewidth=2.4)
    bottom_ax.scatter(plot_df["rank_advantage_vs_seeded_field"], y, color=ACCENT, s=96, zorder=3)
    bottom_ax.set_yticks(y)
    bottom_ax.set_yticklabels(plot_df["gate"].tolist(), fontsize=10.8, fontweight="semibold")
    bottom_ax.set_title("Season-wide Exp rank edge by late-round gate", fontsize=12.8, fontweight="bold", color=TEXT_DARK, pad=10)
    bottom_ax.set_xlabel("Rank edge vs seeded-field baseline (positive = more experienced)", fontsize=11.2, fontweight="semibold")
    bottom_ax.text(
        0.0,
        len(plot_df) - 0.35,
        "Seeded field baseline",
        ha="left",
        va="bottom",
        fontsize=9.8,
        fontweight="bold",
        color=LABEL_TEXT,
        bbox=styled_box(),
    )

    for ax in axes:
        ax.invert_yaxis()
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=10.0, length=0, pad=6)
        ax.tick_params(axis="y", length=0, pad=7)

    fig.suptitle("Player experience in the later rounds", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        "Each gate is compared directly to the seeded tournament baseline",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_04_table(df: pd.DataFrame, over_df: pd.DataFrame, coach_primary_metric: str) -> pd.DataFrame:
    comparison_metric = "coach_years_with_program"
    if coach_primary_metric == comparison_metric:
        comparison_metric = "coach_tourn_games_5y"

    metrics = [coach_primary_metric, comparison_metric, COACH_SUPPORT_METRIC]
    rows = []
    for metric in metrics:
        slug = metric_slug(metric)
        rows.append(
            {
                "metric": metric,
                "raw_spearman": float(df["finish_depth"].corr(df[metric], method="spearman")),
                "seed_adjusted_spearman": float(
                    over_df["overperformance"].corr(over_df[f"{slug}_vs_seed"], method="spearman")
                ),
                "field_mean": float(df[metric].mean()),
                "champ_mean": float(df.loc[df["finish"] == "CHAMPS", metric].mean()),
                "r64_mean": float(df.loc[df["finish"] == "R64", metric].mean()),
                "champ_minus_r64": float(
                    df.loc[df["finish"] == "CHAMPS", metric].mean() - df.loc[df["finish"] == "R64", metric].mean()
                ),
            }
        )
    return pd.DataFrame(rows)


def _plot_raw_panel(ax: plt.Axes, df: pd.DataFrame, metric: str, panel_title: str) -> None:
    scatter_df = df[["finish", metric]].copy()
    scatter_df["x_base"] = scatter_df["finish"].map({f: i for i, f in enumerate(FINISH_ORDER)})
    scatter_df["x"] = scatter_df["x_base"] + np.random.default_rng(42).uniform(-0.22, 0.22, len(scatter_df))
    round_means = df.groupby("finish", observed=False)[metric].mean().reindex(FINISH_ORDER)
    mean_x = np.arange(len(FINISH_ORDER), dtype=float)
    mean_y = round_means.to_numpy(float)
    ax.scatter(scatter_df["x"], scatter_df[metric], s=18, color=TEXT_DARK, alpha=0.34, linewidths=0, zorder=1)
    ax.plot(mean_x, mean_y, color=ACCENT, linewidth=2.5, zorder=2)
    ax.scatter(mean_x, mean_y, color=ACCENT, s=72, zorder=3)
    ax.axhline(float(df[metric].mean()), color=SECONDARY, linestyle=":", linewidth=1.3, zorder=0)
    ax.set_xticks(mean_x)
    ax.set_xticklabels([FINISH_DISPLAY[f] for f in FINISH_ORDER], fontsize=9.4, fontweight="semibold")
    ax.set_title(panel_title, fontsize=11.5, fontweight="bold", color=TEXT_DARK, pad=10)


def _plot_adjusted_panel(ax: plt.Axes, over_df: pd.DataFrame, metric: str, panel_title: str) -> None:
    slug = metric_slug(metric)
    x_values = over_df[f"{slug}_vs_seed"].to_numpy(float)
    y_values = over_df["overperformance"].to_numpy(float)
    slope, intercept = np.polyfit(x_values, y_values, 1)
    line_x = np.linspace(float(np.nanmin(x_values)), float(np.nanmax(x_values)), 200)
    line_y = intercept + slope * line_x
    ax.scatter(x_values, y_values, s=18, color=TEXT_DARK, alpha=0.34, linewidths=0, zorder=1)
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2, zorder=0)
    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.2, zorder=0)
    ax.plot(line_x, line_y, color=ACCENT, linestyle="--", linewidth=2.2, zorder=2)
    ax.set_title(panel_title, fontsize=11.5, fontweight="bold", color=TEXT_DARK, pad=10)


def plot_analysis_04(df: pd.DataFrame, over_df: pd.DataFrame, coach_primary_metric: str, out_path: Path) -> None:
    set_project_theme()
    fig, axes = plt.subplots(2, 1, figsize=(12.2, 11.4), constrained_layout=False)
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.10, right=0.95, hspace=0.30)

    _plot_raw_panel(axes[0], df, coach_primary_metric, metric_label(coach_primary_metric))
    _plot_adjusted_panel(axes[1], over_df, coach_primary_metric, metric_label(coach_primary_metric))

    axes[0].set_ylabel(metric_label(coach_primary_metric), fontsize=10.8, fontweight="semibold")
    axes[1].set_ylabel("Finish vs exact-seed expectation", fontsize=10.8, fontweight="semibold")
    axes[1].set_xlabel(f"{metric_label(coach_primary_metric)} vs exact-seed baseline", fontsize=10.8, fontweight="semibold")

    for ax in axes:
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=9.5, length=0, pad=6)
        ax.tick_params(axis="y", labelsize=9.5, length=0, pad=6)

    fig.suptitle("Coach experience and March success", fontsize=22, fontweight="bold", y=0.97)
    fig.text(
        0.5,
        0.91,
        "Top panel shows the raw pooled view. Bottom panel shows the exact-seed-adjusted view for the lead coach metric.",
        ha="center",
        va="center",
        fontsize=10.0,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_analysis_02_summary(path: Path, summary_table: pd.DataFrame) -> None:
    row = summary_table.iloc[0]
    raw_direction = "shallower" if row["raw_spearman"] < 0 else "deeper"
    seed_direction = "below" if row["seed_adjusted_spearman"] < 0 else "above"
    lines = [
        "# Experience Analysis 2: Player Experience vs Finish (Pooled)",
        "",
        "- Sign convention: both rhos (`raw_spearman` and `seed_adjusted_spearman` in the table) use the negated "
        "season-wide `Exp` rank, so a positive rho means more-experienced teams finished deeper.",
        f"- Raw relationship between player experience and finish depth: `{row['raw_spearman']:+.3f}` "
        f"(more-experienced teams tended to finish {raw_direction}).",
        f"- Within exact seed line (experience edge vs the seed line's average, against finish vs the seed line's "
        f"average): `{row['seed_adjusted_spearman']:+.3f}` (teams more experienced than their seed peers tended to "
        f"finish {seed_direction} seed expectation).",
        f"- Champion average `Exp.` vs R64 average: `{row['champ_avg_exp']:.2f}` vs `{row['r64_avg_exp']:.2f}` (`{row['champ_minus_r64_exp']:+.2f}`).",
        f"- Champion average season-wide `Exp` rank vs R64 average: `{row['champ_avg_exp_rank']:.1f}` vs `{row['r64_avg_exp_rank']:.1f}` (`{row['champ_minus_r64_exp_rank']:+.1f}`, where lower rank means more experience).",
        "- The controlled panel expresses each team's season-wide Exp rank as an edge versus its exact-seed baseline.",
        "",
        "## Table",
        "",
        summary_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_03_summary(path: Path, table: pd.DataFrame) -> None:
    final_four = table.loc[table["gate"] == "Final Four+"].iloc[0]
    champs = table.loc[table["gate"] == "Champions"].iloc[0]
    elite_eight = table.loc[table["gate"] == "Elite Eight+"].iloc[0]
    lines = [
        "# Experience Analysis 3: Late-Round Player Experience Test",
        "",
        f"- `Elite Eight+` teams averaged a season-wide `Exp` rank of `{elite_eight['avg_exp_rank']:.1f}` versus the seeded-field baseline of `{table.iloc[0]['avg_exp_rank']:.1f}`.",
        f"- `Final Four+` teams averaged a season-wide `Exp` rank of `{final_four['avg_exp_rank']:.1f}` and a rank edge of `{final_four['rank_advantage_vs_seeded_field']:+.1f}`.",
        f"- Champions averaged a season-wide `Exp` rank of `{champs['avg_exp_rank']:.1f}` and a rank edge of `{champs['rank_advantage_vs_seeded_field']:+.1f}`.",
        "- Interpretation: this is the direct late-round test. It shows whether roster maturity gets more concentrated the deeper teams get into the bracket.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_04_summary(path: Path, table: pd.DataFrame, coach_primary_metric: str) -> None:
    lead_row = table.loc[table["metric"] == coach_primary_metric].iloc[0]
    tenure_row = table.loc[table["metric"] == "coach_years_with_program"].iloc[0]
    support_row = table.loc[table["metric"] == COACH_SUPPORT_METRIC].iloc[0]
    lines = [
        "# Experience Analysis 4: Coach Experience vs March Success",
        "",
        f"- Lead coach metric: `{coach_primary_metric}` (`{metric_label(coach_primary_metric)}`) with seed-adjusted rho `{lead_row['seed_adjusted_spearman']:+.3f}` on `{COACH_ANALYSIS_START_YEAR}+` seasons.",
        f"- `coach_years_with_program` seed-adjusted rho: `{tenure_row['seed_adjusted_spearman']:+.3f}`.",
        f"- Support metric `coach_tourn_win_pct_5y` seed-adjusted rho: `{support_row['seed_adjusted_spearman']:+.3f}`.",
        "- Interpretation: coach experience belongs in the Day 3 argument as a co-headline track, but recent March reps and tenure do not carry the exact same signal strength.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def build_seed_group_table(
    tournament_df: pd.DataFrame,
    over_df: pd.DataFrame,
    coach_over_df: pd.DataFrame,
    coach_primary_metric: str,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    seed_df = tournament_df[tournament_df["seed_int"].between(1, 16)].copy()
    seed_df["seed_group"] = seed_df["seed_int"].map(SEED_GROUP_MAP)
    expected_depths = seed_df.groupby("seed_group")["finish_depth"].mean().to_dict()
    seed_df["expected_finish_depth"] = seed_df["seed_group"].map(expected_depths)
    seed_df["finish_vs_expected"] = seed_df["finish_depth"] - seed_df["expected_finish_depth"]

    exp_slug = metric_slug(PLAYER_SUPPORT_METRIC)
    coach_slug = metric_slug(coach_primary_metric)

    rows = []
    for seed_group in SEED_GROUP_ORDER:
        subset = seed_df[seed_df["seed_group"] == seed_group]
        over_subset = over_df[over_df["seed_group"] == seed_group]
        player_rho = float(
            over_subset["overperformance"].corr(
                pd.Series(player_visual_edge(over_subset[f"{exp_slug}_vs_seed"]), index=over_subset.index),
                method="spearman",
            )
        ) if len(over_subset) > 2 else 0.0
        coach_subset = coach_over_df[coach_over_df["seed_group"] == seed_group]
        coach_rho = float(
            coach_subset["overperformance"].corr(
                coach_subset[f"{coach_slug}_vs_seed"], method="spearman"
            )
        ) if len(coach_subset) > 2 else 0.0
        rows.append({
            "seed_group": seed_group,
            "expected_finish_depth": float(subset["expected_finish_depth"].mean()),
            "team_count": int(len(subset)),
            "coach_team_count": int(len(coach_subset)),
            "player_exp_rho": player_rho,
            "coach_exp_rho": coach_rho,
        })
    return pd.DataFrame(rows), seed_df


def _over_with_seed_group(over_df: pd.DataFrame) -> pd.DataFrame:
    out = over_df.copy()
    if "seed_group" not in out.columns:
        out["seed_group"] = out["seed_int"].map(SEED_GROUP_MAP)
    return out


def plot_seed_group_player(
    seed_table: pd.DataFrame,
    over_df: pd.DataFrame,
    out_path: Path,
) -> None:
    exp_slug = metric_slug(PLAYER_SUPPORT_METRIC)
    over_with_group = _over_with_seed_group(over_df)

    x_all = player_visual_edge(over_with_group[f"{exp_slug}_vs_seed"])
    y_all = over_with_group["overperformance"].to_numpy(float)
    x_min = float(np.nanmin(x_all))
    x_max = float(np.nanmax(x_all))
    y_min = float(np.nanmin(y_all))
    y_max = float(np.nanmax(y_all))
    x_pad = max((x_max - x_min) * 0.06, 0.05)
    y_pad = max((y_max - y_min) * 0.06, 0.12)

    set_project_theme()
    fig, axes = plt.subplots(2, 4, figsize=(18.4, 10.6), constrained_layout=False)
    axes_flat = axes.flatten()
    apply_report_background(fig, axes_flat)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.07, right=0.98, hspace=0.34, wspace=0.18)

    for ax, seed_group in zip(axes_flat[: len(SEED_GROUP_ORDER)], SEED_GROUP_ORDER):
        row = seed_table.loc[seed_table["seed_group"] == seed_group].iloc[0]
        subset = over_with_group[over_with_group["seed_group"] == seed_group]
        x = player_visual_edge(subset[f"{exp_slug}_vs_seed"])
        y = subset["overperformance"].to_numpy(float)

        ax.scatter(x, y, s=28, color=TEXT_DARK, alpha=0.38, linewidths=0, zorder=1)
        if len(x) > 2 and np.std(x) > 0:
            slope, intercept = np.polyfit(x, y, 1)
            line_x = np.linspace(x_min - x_pad, x_max + x_pad, 200)
            line_y = intercept + slope * line_x
            ax.plot(line_x, line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.0, zorder=2)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2, zorder=0)
        ax.set_xlim(x_min - x_pad, x_max + x_pad)
        ax.set_ylim(y_min - y_pad, y_max + y_pad)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=9.5, length=0, pad=5)
        ax.tick_params(axis="y", labelsize=9.5, length=0, pad=5)
        ax.set_title(
            f"{seed_group} | exp. {row['expected_finish_depth']:.2f}",
            fontsize=11,
            fontweight="bold",
            color=TEXT_DARK,
            pad=8,
        )
        ax.text(
            0.04,
            0.93,
            f"rho = {row['player_exp_rho']:+.3f}\nn = {int(row['team_count'])}",
            transform=ax.transAxes,
            ha="left",
            va="top",
            fontsize=9.4,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    axes_flat[-1].axis("off")
    for ax in axes[:, 0]:
        ax.set_ylabel("Finish - seed expectation", fontsize=10.5, fontweight="semibold")
    for ax in axes[1, :-1]:
        ax.set_xlabel(
            "Exp rank edge vs seed baseline, negated (+ = more experienced)",
            fontsize=10.5,
            fontweight="semibold",
        )
    fig.suptitle(
        "Player experience vs overperformance by seed band",
        fontsize=22,
        fontweight="bold",
        y=0.97,
    )
    fig.text(
        0.5,
        0.91,
        "Each panel shows team-level overperformance vs season-wide Exp rank edge within one seed band; "
        "rho uses the negated rank, so + means more-experienced teams beat their seed line",
        ha="center",
        va="center",
        fontsize=9.9,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_seed_group_coach(
    seed_table: pd.DataFrame,
    over_df: pd.DataFrame,
    coach_primary_metric: str,
    out_path: Path,
) -> None:
    coach_slug = metric_slug(coach_primary_metric)
    over_with_group = _over_with_seed_group(over_df)

    x_all = over_with_group[f"{coach_slug}_vs_seed"].to_numpy(float)
    y_all = over_with_group["overperformance"].to_numpy(float)
    x_min = float(np.nanmin(x_all))
    x_max = float(np.nanmax(x_all))
    y_min = float(np.nanmin(y_all))
    y_max = float(np.nanmax(y_all))
    x_pad = max((x_max - x_min) * 0.06, 0.2)
    y_pad = max((y_max - y_min) * 0.06, 0.12)

    set_project_theme()
    fig, axes = plt.subplots(2, 4, figsize=(18.4, 10.6), constrained_layout=False)
    axes_flat = axes.flatten()
    apply_report_background(fig, axes_flat)
    fig.subplots_adjust(top=0.84, bottom=0.10, left=0.07, right=0.98, hspace=0.34, wspace=0.18)

    for ax, seed_group in zip(axes_flat[: len(SEED_GROUP_ORDER)], SEED_GROUP_ORDER):
        row = seed_table.loc[seed_table["seed_group"] == seed_group].iloc[0]
        subset = over_with_group[over_with_group["seed_group"] == seed_group]
        x = subset[f"{coach_slug}_vs_seed"].to_numpy(float)
        y = subset["overperformance"].to_numpy(float)

        ax.scatter(x, y, s=28, color=TEXT_DARK, alpha=0.38, linewidths=0, zorder=1)
        if len(x) > 2 and np.std(x) > 0:
            slope, intercept = np.polyfit(x, y, 1)
            line_x = np.linspace(x_min - x_pad, x_max + x_pad, 200)
            line_y = intercept + slope * line_x
            ax.plot(line_x, line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.0, zorder=2)
        ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2, zorder=0)
        ax.set_xlim(x_min - x_pad, x_max + x_pad)
        ax.set_ylim(y_min - y_pad, y_max + y_pad)
        ax.grid(False)
        ax.spines["top"].set_visible(False)
        ax.spines["right"].set_visible(False)
        ax.tick_params(axis="x", labelsize=9.5, length=0, pad=5)
        ax.tick_params(axis="y", labelsize=9.5, length=0, pad=5)
        ax.set_title(
            f"{seed_group} | exp. {row['expected_finish_depth']:.2f}",
            fontsize=11,
            fontweight="bold",
            color=TEXT_DARK,
            pad=8,
        )
        ax.text(
            0.04,
            0.93,
            f"rho = {row['coach_exp_rho']:+.3f}\nn = {int(row['coach_team_count'])}",
            transform=ax.transAxes,
            ha="left",
            va="top",
            fontsize=9.4,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(),
        )

    axes_flat[-1].axis("off")
    for ax in axes[:, 0]:
        ax.set_ylabel("Finish - seed expectation", fontsize=10.5, fontweight="semibold")
    for ax in axes[1, :-1]:
        ax.set_xlabel(
            f"{metric_label(coach_primary_metric)} vs exact-seed baseline",
            fontsize=10.5,
            fontweight="semibold",
        )
    fig.suptitle(
        "Coach experience vs overperformance by seed band",
        fontsize=22,
        fontweight="bold",
        y=0.97,
    )
    fig.text(
        0.5,
        0.91,
        f"Each panel shows team-level overperformance vs {metric_label(coach_primary_metric)} (vs seed baseline) "
        f"within one seed band, {COACH_ANALYSIS_START_YEAR}+ seasons",
        ha="center",
        va="center",
        fontsize=9.9,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_seed_group_summary(
    path: Path,
    seed_table: pd.DataFrame,
    coach_primary_metric: str,
) -> None:
    strongest_player = seed_table.loc[seed_table["player_exp_rho"].idxmax()]
    strongest_coach = seed_table.loc[seed_table["coach_exp_rho"].idxmax()]
    weakest_player = seed_table.loc[seed_table["player_exp_rho"].idxmin()]
    player_positive = seed_table.loc[seed_table["player_exp_rho"] > 0, "seed_group"].tolist()
    coach_positive = seed_table.loc[seed_table["coach_exp_rho"] > 0, "seed_group"].tolist()
    lines = [
        "# Experience Analysis: Overperformance by Seed Band",
        "",
        "- Sign convention: `player_exp_rho` correlates finish vs exact-seed expectation with the negated season-wide "
        "`Exp` rank edge, and `coach_exp_rho` with the coach metric's edge, so a positive rho means teams more "
        "experienced than their seed peers tended to finish further above seed expectation.",
        f"- Strongest player Exp. seed band: `{strongest_player['seed_group']}` at `{strongest_player['player_exp_rho']:+.3f}`.",
        f"- Strongest coach Exp. seed band: `{strongest_coach['seed_group']}` at `{strongest_coach['coach_exp_rho']:+.3f}` (`{metric_label(coach_primary_metric)}`).",
        f"- Weakest player Exp. seed band: `{weakest_player['seed_group']}` at `{weakest_player['player_exp_rho']:+.3f}`.",
        f"- Player Exp. positive seed bands: {', '.join(f'`{g}`' for g in player_positive) or '`none`'}.",
        f"- Coach Exp. positive seed bands: {', '.join(f'`{g}`' for g in coach_positive) or '`none`'}.",
        f"- Player Exp. band rhos run from `{seed_table['player_exp_rho'].min():+.3f}` to "
        f"`{seed_table['player_exp_rho'].max():+.3f}`; coach Exp. band rhos from "
        f"`{seed_table['coach_exp_rho'].min():+.3f}` to `{seed_table['coach_exp_rho'].max():+.3f}`.",
        f"- Coach Exp. uses `{COACH_ANALYSIS_START_YEAR}+` seasons only, so its band n is `coach_team_count`, not `team_count`.",
        "- These are descriptive band correlations with no significance test or multiple-comparison correction, "
        "so a band's sign alone is not evidence of an effect.",
        "",
        "## Table",
        "",
        seed_table.round(3).to_markdown(index=False),
    ]
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_experience_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    coach_tournament_df = filter_coach_analysis(tournament_df)
    coach_primary_metric, _ = load_or_compute_coach_metric_selection(coach_tournament_df, paths["metric_selection"])
    over_df = build_overperformance_data(tournament_df)
    coach_over_df = build_overperformance_data(coach_tournament_df)

    table_02 = build_analysis_02_table(tournament_df, over_df)
    table_03 = build_analysis_03_table(tournament_df)
    table_04 = build_analysis_04_table(coach_tournament_df, coach_over_df, coach_primary_metric)
    table_sg, seed_points = build_seed_group_table(tournament_df, over_df, coach_over_df, coach_primary_metric)

    table_02.round(6).to_csv(paths["csv_02"], index=False)
    table_03.round(6).to_csv(paths["csv_03"], index=False)
    table_04.round(6).to_csv(paths["csv_04"], index=False)
    table_sg.round(6).to_csv(paths["csv_seed_group"], index=False)

    plot_analysis_02(tournament_df, over_df, table_02, paths["fig_02"])
    plot_analysis_03(table_03, paths["fig_03"])
    plot_analysis_04(coach_tournament_df, coach_over_df, coach_primary_metric, paths["fig_04"])
    plot_seed_group_player(table_sg, over_df, paths["fig_seed_group_player"])
    plot_seed_group_coach(
        table_sg,
        coach_over_df,
        coach_primary_metric,
        paths["fig_seed_group_coach"],
    )

    write_analysis_02_summary(paths["md_02"], table_02)
    write_analysis_03_summary(paths["md_03"], table_03)
    write_analysis_04_summary(paths["md_04"], table_04, coach_primary_metric)
    write_seed_group_summary(paths["md_seed_group"], table_sg, coach_primary_metric)

    print(f"Coach primary metric: {coach_primary_metric}")
    for key in ["fig_02", "fig_03", "fig_04", "fig_seed_group_player", "fig_seed_group_coach", "md_02", "md_03", "md_04", "md_seed_group", "csv_02", "csv_03", "csv_04", "csv_seed_group"]:
        print(f"Wrote {key}: {paths[key]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
