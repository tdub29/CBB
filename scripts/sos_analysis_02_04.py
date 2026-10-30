#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import sys

import matplotlib.pyplot as plt
import matplotlib.patheffects as pe
import numpy as np
import pandas as pd
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.ticker import FuncFormatter

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.visualization.palette import PROJECT_PALETTE, apply_report_background, set_project_theme

FINISH_ORDER = [
    "R64",
    "R32",
    "Sweet Sixteen",
    "Elite Eight",
    "Final Four",
    "Finals",
    "CHAMPS",
]
FINISH_DEPTH = {
    "R64": 1,
    "R32": 2,
    "Sweet Sixteen": 3,
    "Elite Eight": 4,
    "Final Four": 5,
    "Finals": 6,
    "CHAMPS": 7,
}
FINISH_DISPLAY = {
    "R64": "R64",
    "R32": "R32",
    "Sweet Sixteen": "Sweet 16",
    "Elite Eight": "Elite 8",
    "Final Four": "Final 4",
    "Finals": "Finals",
    "CHAMPS": "Champ",
}
SEED_GROUP_MAP = {
    1: "1-2",
    2: "1-2",
    3: "3-4",
    4: "3-4",
    5: "5-6",
    6: "5-6",
    7: "7-8",
    8: "7-8",
    9: "9-10",
    10: "9-10",
    11: "11-13",
    12: "11-13",
    13: "11-13",
}
SEED_GROUP_ORDER = ["1-2", "3-4", "5-6", "7-8", "9-10", "11-13"]
CONFERENCE_RENAMES = {
    "P10": "P12",
}
ACCENT = PROJECT_PALETTE["signal_blue"]
SECONDARY = PROJECT_PALETTE["steel"]
HIGHLIGHT = PROJECT_PALETTE["sage"]
TEXT_DARK = PROJECT_PALETTE["fog"]
TEXT_LIGHT = PROJECT_PALETTE["steel"]
TRANSFER_PORTAL_YEAR = 2022
CURRENT_FIELD_YEAR = 2026


def styled_box() -> dict[str, object]:
    return {
        "facecolor": PROJECT_PALETTE["midnight"],
        "edgecolor": PROJECT_PALETTE["storm"],
        "linewidth": 1.0,
        "alpha": 0.94,
        "boxstyle": "round,pad=0.25",
    }


def mean_label_box() -> dict[str, object]:
    return {
        "facecolor": PROJECT_PALETTE["midnight"],
        "edgecolor": PROJECT_PALETTE["signal_blue"],
        "linewidth": 1.15,
        "alpha": 0.98,
        "boxstyle": "round,pad=0.22",
    }


def format_year_tick(value: float, _: object) -> str:
    return str(int(value)) if float(value).is_integer() else ""


def pretty_metric(metric: str) -> str:
    return "Elite SOS" if metric == "Elite SOS" else "Conference avg Barthag"


def seed_group_slug(seed_group: str) -> str:
    return seed_group.replace("-", "_")


def get_paths() -> dict[str, Path]:
    project_root = Path(__file__).resolve().parent.parent
    return {
        "input": project_root / "data" / "processed" / "sos_sample.csv",
        "fig_02": project_root / "reports" / "figures" / "sos_vs_finish_pooled.png",
        "fig_02_unadjusted": project_root / "reports" / "figures" / "sos_vs_finish_pooled_unadjusted.png",
        "fig_02_seed_only": project_root / "reports" / "figures" / "sos_vs_finish_pooled_seed_controlled_only.png",
        "fig_metric_selection": project_root / "reports" / "figures" / "sos_metric_selection.png",
        "fig_03": project_root / "reports" / "figures" / "sos_vs_finish_by_year.png",
        "fig_04": project_root / "reports" / "figures" / "sos_vs_finish_by_seed_group.png",
        "fig_04_panel_dir": project_root / "reports" / "figures" / "sos_vs_finish_by_seed_group_panels",
        "md_02": project_root / "reports" / "sos_vs_finish_pooled_summary.md",
        "md_02_unadjusted": project_root / "reports" / "sos_vs_finish_pooled_unadjusted_summary.md",
        "md_03": project_root / "reports" / "sos_vs_finish_by_year_summary.md",
        "md_04": project_root / "reports" / "sos_vs_finish_by_seed_group_summary.md",
        "csv_02": project_root / "reports" / "sos_vs_finish_pooled_table.csv",
        "csv_02_seed_baselines": project_root / "reports" / "sos_vs_finish_seed_baselines.csv",
        "csv_03_yearly": project_root / "reports" / "sos_vs_finish_by_year_correlations.csv",
        "csv_03_heatmap": project_root / "reports" / "sos_vs_finish_by_year_table.csv",
        "csv_04": project_root / "reports" / "sos_vs_finish_by_seed_group_table.csv",
        "csv_metric_selection": project_root / "reports" / "sos_metric_selection.csv",
    }


def load_tournament_data(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    rename_map = {}
    if "Year" in df.columns and "year" not in df.columns:
        rename_map["Year"] = "year"
    if "conf" in df.columns and "Conf" not in df.columns:
        rename_map["conf"] = "Conf"
    if rename_map:
        df = df.rename(columns=rename_map)

    required = {"Team", "year", "seed", "finish", "Elite SOS", "conf_avg_barthag", "Conf"}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    out = df.copy()
    out["year"] = pd.to_numeric(out["year"], errors="coerce")
    out["seed"] = pd.to_numeric(out["seed"], errors="coerce")
    out["Elite SOS"] = pd.to_numeric(out["Elite SOS"], errors="coerce")
    out["conf_avg_barthag"] = pd.to_numeric(out["conf_avg_barthag"], errors="coerce")
    out["Conf"] = out["Conf"].astype(str).str.strip().replace(CONFERENCE_RENAMES)
    out["finish"] = out["finish"].astype(str).str.strip()
    out = out[out["year"] != 2020].copy()
    # Completed tournaments before the projected field's season only: the team-history cache now also carries
    # 2026 seeds and finishes, which a rebuilt sample would otherwise pass into the 2008-2025 statistics.
    out = out[out["year"] < CURRENT_FIELD_YEAR].copy()
    out = out[out["seed"].notna() & out["finish"].notna()].copy()
    out = out[out["finish"] != "R68"].copy()
    out = out[out["finish"].isin(FINISH_ORDER)].copy()
    out["finish_depth"] = out["finish"].map(FINISH_DEPTH)
    out["finish_label"] = out["finish"].map(FINISH_DISPLAY)
    out["seed_int"] = out["seed"].astype(int)
    return out


def choose_primary_metric(df: pd.DataFrame) -> tuple[str, pd.DataFrame]:
    correlations = []
    for metric in ["Elite SOS", "conf_avg_barthag"]:
        rho = df["finish_depth"].corr(df[metric], method="spearman")
        correlations.append({"metric": metric, "spearman_rho": float(rho)})

    correlation_df = pd.DataFrame(correlations).sort_values("spearman_rho", ascending=False, kind="stable")
    elite_rho = float(correlation_df.loc[correlation_df["metric"] == "Elite SOS", "spearman_rho"].iloc[0])
    conf_rho = float(correlation_df.loc[correlation_df["metric"] == "conf_avg_barthag", "spearman_rho"].iloc[0])
    lead = abs(elite_rho - conf_rho)
    if lead >= 0.05:
        primary_metric = str(correlation_df.iloc[0]["metric"])
        rule_note = f"{pretty_metric(primary_metric)} led by {lead:.3f}, so it becomes the primary metric."
    else:
        primary_metric = "Elite SOS"
        rule_note = f"Elite SOS led by only {lead:.3f} rho, below the 0.05 switch threshold."

    correlation_df["is_primary_metric"] = correlation_df["metric"].eq(primary_metric)
    correlation_df["selection_note"] = rule_note
    return primary_metric, correlation_df


def build_analysis_02_table(df: pd.DataFrame) -> pd.DataFrame:
    grouped = (
        df.groupby("finish", as_index=False)
        .agg(
            team_count=("Team", "size"),
            elite_sos_mean=("Elite SOS", "mean"),
            elite_sos_median=("Elite SOS", "median"),
            conf_avg_barthag_mean=("conf_avg_barthag", "mean"),
            conf_avg_barthag_median=("conf_avg_barthag", "median"),
        )
        .copy()
    )
    grouped["finish_depth"] = grouped["finish"].map(FINISH_DEPTH)
    grouped["finish_label"] = grouped["finish"].map(FINISH_DISPLAY)
    grouped = grouped.sort_values("finish_depth", kind="stable").reset_index(drop=True)
    return grouped


def plot_analysis_02(
    df: pd.DataFrame,
    pooled_table: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    metric_column = "elite_sos_mean" if primary_metric == "Elite SOS" else "conf_avg_barthag_mean"
    field_average = float(pooled_table[metric_column].mul(pooled_table["team_count"]).sum() / pooled_table["team_count"].sum())
    x = np.arange(len(pooled_table), dtype=float)
    y = pooled_table[metric_column].to_numpy(dtype=float)
    labels = [f"{label}\n(n={count})" for label, count in zip(pooled_table["finish_label"], pooled_table["team_count"])]
    champ_value = float(pooled_table.loc[pooled_table["finish"] == "CHAMPS", metric_column].iloc[0])
    r64_value = float(pooled_table.loc[pooled_table["finish"] == "R64", metric_column].iloc[0])
    delta = champ_value - r64_value

    set_project_theme()
    fig, ax = plt.subplots(figsize=(16, 8.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.08, right=0.95)

    plot_metric = "Elite SOS" if primary_metric == "Elite SOS" else "conf_avg_barthag"
    scatter_df = df[["finish", plot_metric]].copy()
    scatter_df["x_base"] = scatter_df["finish"].map({finish: idx for idx, finish in enumerate(FINISH_ORDER)})
    jitter = np.random.default_rng(42).uniform(-0.22, 0.22, len(scatter_df))
    scatter_df["x"] = scatter_df["x_base"] + jitter
    ax.scatter(
        scatter_df["x"],
        scatter_df[plot_metric],
        s=22,
        color=PROJECT_PALETTE["steel"],
        alpha=0.42,
        linewidths=0,
        zorder=1,
    )
    trend_x = scatter_df["x_base"].to_numpy(dtype=float)
    trend_y = scatter_df[plot_metric].to_numpy(dtype=float)
    slope, intercept = np.polyfit(trend_x, trend_y, 1)
    line_x = np.linspace(trend_x.min(), trend_x.max(), 200)
    line_y = intercept + slope * line_x
    ax.plot(
        line_x,
        line_y,
        color=HIGHLIGHT,
        linewidth=2.2,
        linestyle="--",
        alpha=0.95,
        zorder=2,
    )
    ax.plot(x, y, color=ACCENT, linewidth=2.8, zorder=3)
    ax.scatter(x, y, color=ACCENT, s=120, zorder=4)
    ax.axhline(field_average, color=SECONDARY, linestyle=":", linewidth=1.6)
    ax.text(
        len(x) - 0.1,
        field_average + (0.02 if primary_metric == "conf_avg_barthag" else 0.65),
        "Field average",
        ha="right",
        va="bottom",
        fontsize=10,
        fontweight="bold",
        color=SECONDARY,
        bbox=styled_box(),
    )
    ax.text(
        0.02,
        0.94,
        "Dashed line = pooled trend",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10,
        fontweight="bold",
        color=HIGHLIGHT,
        bbox=styled_box(),
    )

    for idx, value in enumerate(y):
        vertical_offset = 12 if primary_metric == "Elite SOS" else 10
        label = ax.annotate(
            f"{value:.2f}",
            xy=(idx, value),
            xytext=(0, vertical_offset),
            textcoords="offset points",
            ha="center",
            va="bottom",
            fontsize=10,
            fontweight="bold",
            color=PROJECT_PALETTE["fog"],
            bbox=mean_label_box(),
            zorder=5,
        )
        label.set_path_effects([pe.withStroke(linewidth=1.25, foreground=PROJECT_PALETTE["midnight"])])

    margin = 5.5 if primary_metric == "Elite SOS" else 0.09
    ax.set_ylim(y.min() - margin, y.max() + margin)
    ax.set_xticks(x)
    ax.set_xticklabels(labels, fontsize=11, fontweight="semibold")
    ax.set_ylabel(pretty_metric(primary_metric), fontsize=11, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=8)
    ax.tick_params(axis="y", length=0, pad=6, labelsize=10)

    ax.text(
        0.5,
        1.04,
        "All tournament teams, with round means overlaid",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=13,
        fontweight="bold",
        color=TEXT_DARK,
    )
    ax.text(
        0.5,
        1.005,
        f"Scatter shows every team; blue markers show the mean {pretty_metric(primary_metric)} for each finish round",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    callout_offset = 3.1 if primary_metric == "Elite SOS" else 0.04
    ax.text(
        len(x) - 1.2,
        max(champ_value, r64_value) + callout_offset,
        f"Champ vs R64 gap\n{delta:+.2f} {pretty_metric(primary_metric)}",
        ha="left",
        va="bottom",
        fontsize=10.5,
        fontweight="bold",
        color=TEXT_DARK,
        bbox=styled_box(),
    )

    fig.suptitle(
        "Deeper tournament runs came from teams with tougher schedules overall",
        fontsize=22,
        fontweight="bold",
        y=0.975,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_metric_selection(correlation_df: pd.DataFrame, out_path: Path) -> None:
    correlation_plot = correlation_df.copy().sort_values("metric", ascending=False, kind="stable")
    bar_colors = [ACCENT if is_primary else "#c7ced6" for is_primary in correlation_plot["is_primary_metric"]]

    set_project_theme()
    fig, ax = plt.subplots(figsize=(10.5, 6.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.82, bottom=0.13, left=0.10, right=0.95)
    bars = ax.bar(
        [pretty_metric(metric) for metric in correlation_plot["metric"]],
        correlation_plot["spearman_rho"],
        color=bar_colors,
        width=0.58,
    )
    for bar, rho in zip(bars, correlation_plot["spearman_rho"]):
        ax.text(
            bar.get_x() + bar.get_width() / 2,
            rho + 0.015,
            f"{rho:.3f}",
            ha="center",
            va="bottom",
            fontsize=11,
            fontweight="bold",
            color=TEXT_DARK,
            bbox=styled_box(),
        )
    ax.set_ylim(0, max(0.55, float(correlation_plot["spearman_rho"].max()) + 0.1))
    ax.set_ylabel("Pooled Spearman rho", fontsize=11, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=8, labelsize=10.5)
    ax.tick_params(axis="y", length=0, pad=6, labelsize=10)
    fig.suptitle("SOS metric selection check", fontsize=18, fontweight="bold", y=0.96)
    ax.text(
        0.5,
        1.04,
        "Selection check only: compares the candidate SOS metrics to choose the primary one",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    ax.text(
        0.98,
        0.92,
        str(correlation_df["selection_note"].iloc[0]),
        transform=ax.transAxes,
        ha="right",
        va="top",
        fontsize=10,
        color=TEXT_DARK,
        bbox=styled_box(),
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def combine_analysis_02_views(
    unadjusted_path: Path,
    seed_controlled_path: Path,
    out_path: Path,
) -> None:
    unadjusted_image = plt.imread(unadjusted_path)
    seed_controlled_image = plt.imread(seed_controlled_path)

    fig, axes = plt.subplots(
        2,
        1,
        figsize=(16, 17.5),
        constrained_layout=False,
        gridspec_kw={"height_ratios": [1, 1]},
    )
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.98, bottom=0.03, left=0.02, right=0.98, hspace=0.16)

    axes[0].imshow(unadjusted_image)
    axes[0].axis("off")

    axes[1].imshow(seed_controlled_image)
    axes[1].axis("off")

    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_02_seed_controlled(
    df: pd.DataFrame,
    primary_metric: str,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    metric_column = "Elite SOS" if primary_metric == "Elite SOS" else "conf_avg_barthag"
    adjusted = df.copy()
    adjusted["seed_expected_finish"] = adjusted.groupby("seed_int")["finish_depth"].transform("mean")
    adjusted["seed_avg_metric"] = adjusted.groupby("seed_int")[metric_column].transform("mean")
    adjusted["finish_vs_seed"] = adjusted["finish_depth"] - adjusted["seed_expected_finish"]
    adjusted["metric_vs_seed"] = adjusted[metric_column] - adjusted["seed_avg_metric"]

    baselines = (
        adjusted.groupby("seed_int", as_index=False)
        .agg(
            seed_expected_finish=("seed_expected_finish", "mean"),
            seed_avg_metric=("seed_avg_metric", "mean"),
            team_count=("Team", "size"),
        )
        .sort_values("seed_int", kind="stable")
        .reset_index(drop=True)
    )
    return adjusted, baselines


def plot_analysis_02_seed_controlled(
    adjusted_df: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    x = adjusted_df["metric_vs_seed"].to_numpy(dtype=float)
    y = adjusted_df["finish_vs_seed"].to_numpy(dtype=float)
    within_seed_rho = float(adjusted_df["metric_vs_seed"].corr(adjusted_df["finish_vs_seed"], method="spearman"))
    slope, intercept = np.polyfit(x, y, 1)
    line_x = np.linspace(x.min(), x.max(), 200)
    line_y = intercept + slope * line_x

    set_project_theme()
    fig, ax = plt.subplots(figsize=(14.5, 8.5), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.84, bottom=0.12, left=0.10, right=0.95)

    ax.scatter(
        x,
        y,
        s=24,
        color=PROJECT_PALETTE["steel"],
        alpha=0.45,
        linewidths=0,
        zorder=1,
    )
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.3, zorder=0)
    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.3, zorder=0)
    ax.plot(line_x, line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.4, zorder=2)
    ax.text(
        0.98,
        0.95,
        f"Within-seed Spearman rho = {within_seed_rho:.3f}",
        transform=ax.transAxes,
        ha="right",
        va="top",
        fontsize=11,
        fontweight="bold",
        color=TEXT_DARK,
        bbox=styled_box(),
    )
    ax.text(
        0.02,
        0.95,
        "0 = seed-line baseline",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10,
        fontweight="bold",
        color=SECONDARY,
        bbox=styled_box(),
    )
    ax.text(
        0.02,
        0.89,
        "Dashed line = within-seed trend",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10,
        fontweight="bold",
        color=HIGHLIGHT,
        bbox=styled_box(),
    )
    ax.set_xlabel(f"{pretty_metric(primary_metric)} vs seed-line average", fontsize=11, fontweight="semibold")
    ax.set_ylabel("Finish vs seed-line expectation", fontsize=11, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
    ax.tick_params(axis="y", labelsize=10, length=0, pad=6)
    fig.suptitle(
        "After controlling for seed, the SOS relationship is much weaker",
        fontsize=20,
        fontweight="bold",
        y=0.97,
    )
    ax.text(
        0.5,
        1.03,
        "Each point is a tournament team, centered against its own exact seed-line baseline",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_03_tables(df: pd.DataFrame, primary_metric: str) -> tuple[pd.DataFrame, pd.DataFrame]:
    yearly = []
    for year, group in df.groupby("year", sort=True):
        yearly.append(
            {
                "year": int(year),
                "elite_sos_rho": float(group["finish_depth"].corr(group["Elite SOS"], method="spearman")),
                "conf_avg_barthag_rho": float(
                    group["finish_depth"].corr(group["conf_avg_barthag"], method="spearman")
                ),
            }
        )
    yearly_df = pd.DataFrame(yearly).sort_values("year", kind="stable").reset_index(drop=True)
    yearly_df["primary_metric"] = primary_metric
    yearly_df["primary_metric_rho"] = (
        yearly_df["elite_sos_rho"] if primary_metric == "Elite SOS" else yearly_df["conf_avg_barthag_rho"]
    )
    yearly_df["signal_bucket"] = np.select(
        [
            yearly_df["primary_metric_rho"] >= 0.40,
            yearly_df["primary_metric_rho"] > 0,
        ],
        ["Clear positive", "Positive but weaker"],
        default="Inverse / noisy",
    )

    heatmap = (
        df.groupby(["year", "finish"], as_index=False)[primary_metric]
        .mean()
        .pivot(index="year", columns="finish", values=primary_metric)
        .reindex(columns=FINISH_ORDER)
        .sort_index()
    )
    heatmap.columns = [FINISH_DISPLAY[column] for column in heatmap.columns]
    return yearly_df, heatmap


def plot_analysis_03(
    yearly_df: pd.DataFrame,
    heatmap: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    years = yearly_df["year"].to_numpy(dtype=int)
    rho = yearly_df["primary_metric_rho"].to_numpy(dtype=float)
    strongest = yearly_df.loc[yearly_df["primary_metric_rho"].idxmax()]
    weakest = yearly_df.loc[yearly_df["primary_metric_rho"].idxmin()]
    positive_count = int((yearly_df["primary_metric_rho"] > 0).sum())
    clear_count = int((yearly_df["signal_bucket"] == "Clear positive").sum())

    cmap = LinearSegmentedColormap.from_list(
        "sos_heatmap",
        [
            PROJECT_PALETTE["deep_ocean"],
            PROJECT_PALETTE["lake_blue"],
            PROJECT_PALETTE["sky_blue"],
            PROJECT_PALETTE["fog"],
        ],
    )

    set_project_theme()
    fig, axes = plt.subplots(
        2,
        1,
        figsize=(16, 11),
        gridspec_kw={"height_ratios": [2.2, 1.9]},
        constrained_layout=False,
    )
    apply_report_background(fig, axes)
    fig.subplots_adjust(top=0.88, bottom=0.08, left=0.08, right=0.93, hspace=0.42)

    ax = axes[0]
    ax.plot(years, rho, color=ACCENT, linewidth=2.8, marker="o", markersize=5)
    ax.axvline(TRANSFER_PORTAL_YEAR, color=PROJECT_PALETTE["steel"], linestyle=":", linewidth=2.0, alpha=0.95)
    ax.axhline(rho.mean(), color=SECONDARY, linestyle=":", linewidth=1.6)
    ax.text(
        years.max() + 0.35,
        rho.mean() + 0.005,
        "Mean rho",
        ha="left",
        va="bottom",
        fontsize=10,
        fontweight="bold",
            color=SECONDARY,
            bbox=styled_box(),
    )
    ax.annotate(
        f"Strongest: {int(strongest['year'])}\n{strongest['primary_metric_rho']:.3f}",
        xy=(strongest["year"], strongest["primary_metric_rho"]),
        xytext=(strongest["year"] - 2.5, strongest["primary_metric_rho"] + 0.03),
        arrowprops={"arrowstyle": "->", "color": PROJECT_PALETTE["steel"]},
        fontsize=10.5,
        fontweight="bold",
        bbox=styled_box(),
    )
    ax.annotate(
        f"Weakest: {int(weakest['year'])}\n{weakest['primary_metric_rho']:.3f}",
        xy=(weakest["year"], weakest["primary_metric_rho"]),
        xytext=(weakest["year"] + 0.3, weakest["primary_metric_rho"] + 0.03),
        arrowprops={"arrowstyle": "->", "color": PROJECT_PALETTE["steel"]},
        fontsize=10.5,
        fontweight="bold",
        bbox=styled_box(),
    )
    ax.set_xlim(years.min() - 0.4, years.max() + 1.1)
    ax.set_ylim(0.28, max(0.66, float(rho.max()) + 0.05))
    ax.text(
        TRANSFER_PORTAL_YEAR - 0.18,
        ax.get_ylim()[1] - 0.015,
        "Current transfer-rules era starts",
        ha="right",
        va="top",
        fontsize=10.5,
        fontweight="bold",
        color=TEXT_DARK,
        bbox=styled_box(),
    )
    ax.set_xticks(years)
    ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick))
    ax.set_ylabel(f"{pretty_metric(primary_metric)} rho", fontsize=11, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=10, length=0, pad=6)
    ax.tick_params(axis="y", labelsize=10, length=0, pad=6)
    ax.text(
        0.5,
        1.04,
        "Year-by-year stability",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=13,
        fontweight="bold",
        color=TEXT_DARK,
    )
    ax.text(
        0.5,
        1.005,
        f"Each point is that tournament's Spearman correlation between finish depth and {pretty_metric(primary_metric)}",
        transform=ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )

    heat_ax = axes[1]
    image = heat_ax.imshow(heatmap.to_numpy(dtype=float), aspect="auto", cmap=cmap, interpolation="nearest")
    heatmap_years = heatmap.index.to_numpy(dtype=int)
    if TRANSFER_PORTAL_YEAR in heatmap_years:
        portal_row = int(np.where(heatmap_years == TRANSFER_PORTAL_YEAR)[0][0])
        heat_ax.axhline(portal_row - 0.5, color=PROJECT_PALETTE["steel"], linestyle=":", linewidth=2.0, alpha=0.95)
    heat_ax.set_yticks(np.arange(len(heatmap.index)))
    heat_ax.set_yticklabels(heatmap.index.astype(int), fontsize=10)
    heat_ax.set_xticks(np.arange(len(heatmap.columns)))
    heat_ax.set_xticklabels(heatmap.columns.tolist(), fontsize=10.5, fontweight="semibold")
    heat_ax.tick_params(axis="x", length=0, pad=8)
    heat_ax.tick_params(axis="y", length=0, pad=6)
    heat_ax.set_ylabel("Season", fontsize=11, fontweight="semibold")
    heat_ax.set_xlabel("Finish round", fontsize=11, fontweight="semibold")
    heat_ax.grid(False)
    for spine in heat_ax.spines.values():
        spine.set_visible(False)
    heat_ax.set_title(
        "Round-by-round mean SOS by season",
        fontsize=13,
        fontweight="bold",
        color=TEXT_DARK,
        pad=18,
    )
    heat_ax.text(
        0.5,
        1.02,
        "Darker cells mean tougher schedules for teams that reached that round in that year",
        transform=heat_ax.transAxes,
        ha="center",
        va="bottom",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )
    color_bar = fig.colorbar(image, ax=heat_ax, fraction=0.046, pad=0.025)
    color_bar.ax.set_ylabel(pretty_metric(primary_metric), fontsize=10.5, fontweight="semibold")
    color_bar.ax.tick_params(labelsize=9.5, length=0)

    fig.suptitle(
        "The SOS signal has peaked in past two years.",
        fontsize=22,
        fontweight="bold",
        y=0.975,
    )
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def expected_depth_labels(seed_df: pd.DataFrame) -> dict[str, str]:
    grouped = seed_df.groupby("seed_group")["finish_depth"].mean().sort_index()
    labels = {}
    for seed_group, depth in grouped.items():
        labels[str(seed_group)] = f"{float(depth):.2f}"
    return labels


def build_analysis_04_table(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    seed_df = df[df["seed_int"].between(1, 13)].copy()
    seed_df["seed_group"] = seed_df["seed_int"].map(SEED_GROUP_MAP)
    expected_depths = seed_df.groupby("seed_group")["finish_depth"].mean().to_dict()
    seed_df["expected_depth_group"] = seed_df["seed_group"].map(expected_depths)
    seed_df["finish_vs_expected"] = seed_df["finish_depth"] - seed_df["expected_depth_group"]
    seed_df["advanced_flag"] = np.where(
        seed_df["finish_depth"] > seed_df["expected_depth_group"],
        "Advancer",
        "Did not exceed expectation",
    )

    summary_rows: list[dict[str, object]] = []
    expected_labels = expected_depth_labels(seed_df)
    for seed_group in SEED_GROUP_ORDER:
        subset = seed_df[seed_df["seed_group"] == seed_group].copy()
        advancers = subset[subset["advanced_flag"] == "Advancer"]
        non_advancers = subset[subset["advanced_flag"] == "Did not exceed expectation"]
        summary_rows.append(
            {
                "seed_group": seed_group,
                "expected_finish_depth": float(expected_labels[seed_group]),
                "team_count": int(len(subset)),
                "advancer_count": int(len(advancers)),
                "non_advancer_count": int(len(non_advancers)),
                "elite_sos_spearman": float(subset["finish_vs_expected"].corr(subset["Elite SOS"], method="spearman")),
                "conf_avg_spearman": float(
                    subset["finish_vs_expected"].corr(subset["conf_avg_barthag"], method="spearman")
                ),
                "elite_sos_advancer_mean": float(advancers["Elite SOS"].mean()),
                "elite_sos_non_advancer_mean": float(non_advancers["Elite SOS"].mean()),
                "elite_sos_gap": float(advancers["Elite SOS"].mean() - non_advancers["Elite SOS"].mean()),
                "elite_sos_advancer_median": float(advancers["Elite SOS"].median()),
                "elite_sos_non_advancer_median": float(non_advancers["Elite SOS"].median()),
                "conf_avg_advancer_mean": float(advancers["conf_avg_barthag"].mean()),
                "conf_avg_non_advancer_mean": float(non_advancers["conf_avg_barthag"].mean()),
                "conf_avg_gap": float(
                    advancers["conf_avg_barthag"].mean() - non_advancers["conf_avg_barthag"].mean()
                ),
                "conf_avg_advancer_median": float(advancers["conf_avg_barthag"].median()),
                "conf_avg_non_advancer_median": float(non_advancers["conf_avg_barthag"].median()),
            }
        )

    return pd.DataFrame(summary_rows), seed_df


def plot_analysis_04(seed_table: pd.DataFrame, seed_points: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    if primary_metric == "Elite SOS":
        metric_column = "Elite SOS"
        rho_column = "elite_sos_spearman"
    else:
        metric_column = "conf_avg_barthag"
        rho_column = "conf_avg_spearman"

    set_project_theme()
    fig, axes = plt.subplots(2, 3, figsize=(16.5, 10.2), constrained_layout=False)
    axes_flat = axes.flatten()
    apply_report_background(fig, axes_flat)
    fig.subplots_adjust(top=0.83, bottom=0.10, left=0.07, right=0.98, hspace=0.30, wspace=0.16)

    x_min = float(seed_points[metric_column].min())
    x_max = float(seed_points[metric_column].max())
    y_min = float(seed_points["finish_vs_expected"].min())
    y_max = float(seed_points["finish_vs_expected"].max())
    x_pad = 1.2 if primary_metric == "Elite SOS" else 0.03
    y_pad = 0.3
    strongest_group = str(seed_table.loc[seed_table[rho_column].abs().idxmax(), "seed_group"])

    for ax, seed_group in zip(axes_flat, SEED_GROUP_ORDER):
        subset = seed_points[seed_points["seed_group"] == seed_group].copy()
        row = seed_table.loc[seed_table["seed_group"] == seed_group].iloc[0]
        plot_analysis_04_panel_axis(
            ax=ax,
            subset=subset,
            row=row,
            metric_column=metric_column,
            rho_column=rho_column,
            x_limits=(x_min - x_pad, x_max + x_pad),
            y_limits=(y_min - y_pad, y_max + y_pad),
            highlight_title=seed_group == strongest_group,
        )

    for ax in axes[:, 0]:
        ax.set_ylabel("Finish - expected finish", fontsize=10.5, fontweight="semibold")
    for ax in axes[1, :]:
        ax.set_xlabel(pretty_metric(primary_metric), fontsize=10.5, fontweight="semibold")

    fig.suptitle(
        "Within-group SOS signal is strongest in selective seed bands",
        fontsize=21,
        fontweight="bold",
        y=0.97,
    )
    fig.text(
        0.5,
        0.92,
        f"Each panel shows team-level finish minus expected finish depth versus {pretty_metric(primary_metric)} for one seed group",
        ha="center",
        va="center",
        fontsize=10,
        color=TEXT_LIGHT,
    )
    fig.text(
        0.5,
        0.895,
        "Dashed line = within-group linear trend; panel title highlight marks the strongest absolute Spearman correlation",
        ha="center",
        va="center",
        fontsize=9.5,
        color=TEXT_LIGHT,
    )

    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def plot_analysis_04_panel_axis(
    ax: plt.Axes,
    subset: pd.DataFrame,
    row: pd.Series,
    metric_column: str,
    rho_column: str,
    x_limits: tuple[float, float],
    y_limits: tuple[float, float],
    highlight_title: bool,
) -> None:
    ax.scatter(
        subset[metric_column],
        subset["finish_vs_expected"],
        s=28,
        color=PROJECT_PALETTE["fog"],
        alpha=0.42,
        linewidths=0,
        zorder=1,
    )
    slope, intercept = np.polyfit(subset[metric_column], subset["finish_vs_expected"], 1)
    line_x = np.linspace(x_limits[0], x_limits[1], 200)
    line_y = intercept + slope * line_x
    ax.plot(line_x, line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.0, zorder=2)
    ax.axhline(0, color=PROJECT_PALETTE["steel"], linestyle=":", linewidth=1.2, zorder=0)
    ax.set_xlim(*x_limits)
    ax.set_ylim(*y_limits)
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=9.5, length=0, pad=4)
    ax.tick_params(axis="y", labelsize=9.5, length=0, pad=4)
    title_color = HIGHLIGHT if highlight_title else TEXT_DARK
    ax.set_title(
        f"{row['seed_group']} | exp. {row['expected_finish_depth']:.2f}",
        fontsize=11,
        fontweight="bold",
        color=title_color,
        pad=8,
    )
    ax.text(
        0.04,
        0.93,
        f"rho = {row[rho_column]:+.3f}\nn = {int(row['team_count'])}",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=9.5,
        fontweight="bold",
        color=TEXT_DARK,
        bbox=styled_box(),
    )


def plot_analysis_04_individual(
    seed_table: pd.DataFrame,
    seed_points: pd.DataFrame,
    primary_metric: str,
    out_dir: Path,
) -> list[Path]:
    if primary_metric == "Elite SOS":
        metric_column = "Elite SOS"
        rho_column = "elite_sos_spearman"
    else:
        metric_column = "conf_avg_barthag"
        rho_column = "conf_avg_spearman"

    x_min = float(seed_points[metric_column].min())
    x_max = float(seed_points[metric_column].max())
    y_min = float(seed_points["finish_vs_expected"].min())
    y_max = float(seed_points["finish_vs_expected"].max())
    x_pad = 1.2 if primary_metric == "Elite SOS" else 0.03
    y_pad = 0.3
    x_limits = (x_min - x_pad, x_max + x_pad)
    y_limits = (y_min - y_pad, y_max + y_pad)
    strongest_group = str(seed_table.loc[seed_table[rho_column].abs().idxmax(), "seed_group"])

    out_dir.mkdir(parents=True, exist_ok=True)
    written_paths: list[Path] = []
    for seed_group in SEED_GROUP_ORDER:
        subset = seed_points[seed_points["seed_group"] == seed_group].copy()
        row = seed_table.loc[seed_table["seed_group"] == seed_group].iloc[0]

        set_project_theme()
        fig, ax = plt.subplots(figsize=(6.4, 5.6), constrained_layout=False)
        apply_report_background(fig, [ax])
        fig.subplots_adjust(top=0.84, bottom=0.16, left=0.14, right=0.97)

        plot_analysis_04_panel_axis(
            ax=ax,
            subset=subset,
            row=row,
            metric_column=metric_column,
            rho_column=rho_column,
            x_limits=x_limits,
            y_limits=y_limits,
            highlight_title=seed_group == strongest_group,
        )
        ax.set_xlabel(pretty_metric(primary_metric), fontsize=10.5, fontweight="semibold")
        ax.set_ylabel("Finish - expected finish", fontsize=10.5, fontweight="semibold")
        fig.suptitle(
            f"Within-group SOS signal: seed group {seed_group}",
            fontsize=17,
            fontweight="bold",
            y=0.96,
        )
        fig.text(
            0.5,
            0.905,
            "Scatter shows finish versus seed-group expectation with a within-group linear trend",
            ha="center",
            va="center",
            fontsize=9.5,
            color=TEXT_LIGHT,
        )

        out_path = out_dir / f"sos_vs_finish_by_seed_group_{seed_group_slug(seed_group)}.png"
        fig.savefig(out_path, dpi=220, bbox_inches="tight")
        plt.close(fig)
        written_paths.append(out_path)

    return written_paths


def write_analysis_02_summary(
    adjusted_df: pd.DataFrame,
    baselines: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    within_seed_rho = float(adjusted_df["metric_vs_seed"].corr(adjusted_df["finish_vs_seed"], method="spearman"))
    lines = [
        "# SOS Analysis 2: SOS vs Finish (Seed-Controlled Pooled View)",
        "",
        f"- Primary metric: `{pretty_metric(primary_metric)}`",
        "- Method: each team is centered against its exact seed-line average finish depth and average SOS.",
        f"- Headline finding: after controlling for seed, the within-seed Spearman relationship is `{within_seed_rho:.3f}`.",
        "- Interpretation: the broad pooled gradient is mostly seed structure; the residual SOS relationship inside seed lines is weak.",
        "",
        "## Seed Baselines",
        "",
        baselines.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_02_unadjusted_summary(
    pooled_table: pd.DataFrame,
    correlation_df: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    metric_column = "elite_sos_mean" if primary_metric == "Elite SOS" else "conf_avg_barthag_mean"
    champ_value = float(pooled_table.loc[pooled_table["finish"] == "CHAMPS", metric_column].iloc[0])
    r64_value = float(pooled_table.loc[pooled_table["finish"] == "R64", metric_column].iloc[0])
    field_average = float(pooled_table[metric_column].mul(pooled_table["team_count"]).sum() / pooled_table["team_count"].sum())
    lines = [
        "# SOS Analysis 2 Support: Unadjusted Pooled View",
        "",
        f"- Primary metric: `{pretty_metric(primary_metric)}`",
        f"- Metric rule: {correlation_df['selection_note'].iloc[0]}",
        f"- Headline finding: champions averaged `{champ_value:.2f}` {pretty_metric(primary_metric)} versus `{r64_value:.2f}` for Round-of-64 exits.",
        f"- Baseline: the field-wide average was `{field_average:.2f}` {pretty_metric(primary_metric)}.",
        f"- Interpretation: stronger schedules mapped to deeper runs overall, but much of this pooled signal is seed structure.",
        "",
        "## Pooled Finish Table",
        "",
        pooled_table[
            [
                "finish_label",
                "team_count",
                "elite_sos_mean",
                "elite_sos_median",
                "conf_avg_barthag_mean",
                "conf_avg_barthag_median",
            ]
        ]
        .rename(columns={"finish_label": "finish", "team_count": "n"})
        .round(3)
        .to_markdown(index=False),
        "",
        "## Metric Correlations",
        "",
        correlation_df[["metric", "spearman_rho", "is_primary_metric"]].round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_03_summary(
    yearly_df: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    strongest = yearly_df.loc[yearly_df["primary_metric_rho"].idxmax()]
    weakest = yearly_df.loc[yearly_df["primary_metric_rho"].idxmin()]
    positive_count = int((yearly_df["primary_metric_rho"] > 0).sum())
    lines = [
        "# SOS Analysis 3: SOS vs Finish by Year",
        "",
        f"- Primary metric: `{pretty_metric(primary_metric)}`",
        f"- Headline finding: `{positive_count}` of `{len(yearly_df)}` tournament years showed a positive SOS-to-finish relationship.",
        f"- Strongest year: `{int(strongest['year'])}` at rho `{strongest['primary_metric_rho']:.3f}`.",
        f"- Weakest year: `{int(weakest['year'])}` at rho `{weakest['primary_metric_rho']:.3f}`.",
        "- Interpretation: the pooled signal is not a one-off artifact; it shows up every year in this sample.",
        "",
        "## Yearly Correlations",
        "",
        yearly_df[
            ["year", "elite_sos_rho", "conf_avg_barthag_rho", "primary_metric_rho", "signal_bucket"]
        ]
        .round(3)
        .to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_analysis_04_summary(
    seed_table: pd.DataFrame,
    primary_metric: str,
    out_path: Path,
) -> None:
    rho_column = "elite_sos_spearman" if primary_metric == "Elite SOS" else "conf_avg_spearman"
    best_group = seed_table.loc[seed_table[rho_column].abs().idxmax()]
    weakest_group = seed_table.loc[seed_table[rho_column].abs().idxmin()]
    lines = [
        "# SOS Analysis 4: Seed Groups and Finish-vs-Expectation Scatter",
        "",
        f"- Primary metric: `{pretty_metric(primary_metric)}`",
        f"- Headline finding: seed group `{best_group['seed_group']}` had the strongest within-group Spearman relationship at `{best_group[rho_column]:+.3f}`.",
        f"- Weakest group: `{weakest_group['seed_group']}` at `{weakest_group[rho_column]:+.3f}`.",
        "- Interpretation: the SOS signal is not uniform across seed bands; the faceted scatter shows where schedule strength actually tracks overperformance.",
        "",
        "## Seed Group Summary",
        "",
        seed_table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    df = load_tournament_data(paths["input"])
    primary_metric, correlation_df = choose_primary_metric(df)

    pooled_table = build_analysis_02_table(df)
    plot_analysis_02(df, pooled_table, primary_metric, paths["fig_02_unadjusted"])
    pooled_table.round(6).to_csv(paths["csv_02"], index=False)
    plot_metric_selection(correlation_df, paths["fig_metric_selection"])
    adjusted_df, seed_baselines = build_analysis_02_seed_controlled(df, primary_metric)
    plot_analysis_02_seed_controlled(adjusted_df, primary_metric, paths["fig_02_seed_only"])
    combine_analysis_02_views(paths["fig_02_unadjusted"], paths["fig_02_seed_only"], paths["fig_02"])
    write_analysis_02_summary(
        adjusted_df,
        seed_baselines,
        primary_metric,
        paths["md_02"],
    )
    write_analysis_02_unadjusted_summary(pooled_table, correlation_df, primary_metric, paths["md_02_unadjusted"])
    seed_baselines.round(6).to_csv(paths["csv_02_seed_baselines"], index=False)

    yearly_df, heatmap = build_analysis_03_tables(df, primary_metric)
    plot_analysis_03(yearly_df, heatmap, primary_metric, paths["fig_03"])
    write_analysis_03_summary(yearly_df, primary_metric, paths["md_03"])
    yearly_df.round(6).to_csv(paths["csv_03_yearly"], index=False)
    heatmap.round(6).reset_index().rename(columns={"index": "year"}).to_csv(paths["csv_03_heatmap"], index=False)

    seed_table, seed_points = build_analysis_04_table(df)
    plot_analysis_04(seed_table, seed_points, primary_metric, paths["fig_04"])
    panel_paths = plot_analysis_04_individual(seed_table, seed_points, primary_metric, paths["fig_04_panel_dir"])
    write_analysis_04_summary(seed_table, primary_metric, paths["md_04"])
    seed_table.round(6).to_csv(paths["csv_04"], index=False)

    correlation_df.round(6).to_csv(paths["csv_metric_selection"], index=False)

    print(f"Primary metric: {primary_metric}")
    print(f"Wrote figure: {paths['fig_02']}")
    print(f"Wrote figure: {paths['fig_02_unadjusted']}")
    print(f"Wrote figure: {paths['fig_02_seed_only']}")
    print(f"Wrote figure: {paths['fig_metric_selection']}")
    print(f"Wrote figure: {paths['fig_03']}")
    print(f"Wrote figure: {paths['fig_04']}")
    for panel_path in panel_paths:
        print(f"Wrote figure: {panel_path}")
    print(f"Wrote summary: {paths['md_02']}")
    print(f"Wrote summary: {paths['md_02_unadjusted']}")
    print(f"Wrote summary: {paths['md_03']}")
    print(f"Wrote summary: {paths['md_04']}")
    print(f"Wrote table: {paths['csv_02']}")
    print(f"Wrote table: {paths['csv_02_seed_baselines']}")
    print(f"Wrote table: {paths['csv_03_yearly']}")
    print(f"Wrote table: {paths['csv_03_heatmap']}")
    print(f"Wrote table: {paths['csv_04']}")
    print(f"Wrote metric selection: {paths['csv_metric_selection']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
