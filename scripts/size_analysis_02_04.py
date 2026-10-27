#!/usr/bin/env python3
from __future__ import annotations
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.ticker import FuncFormatter
from size_analysis_common import ACCENT, FINISH_DISPLAY, FINISH_ORDER, HIGHLIGHT, LABEL_TEXT, SECONDARY, SEED_GROUP_ORDER, TEXT_DARK, TEXT_LIGHT, apply_report_background, build_overperformance_data, get_size_base_paths, load_size_data, load_tournament_data, metric_label, metric_slug, set_project_theme, styled_box
from size_analysis_05_07 import build_analysis_06_table as build_year_comparison_table, plot_analysis_06 as plot_year_comparison, write_analysis_06_md as write_year_comparison_summary

def get_paths() -> dict[str, Path]:
    base = get_size_base_paths(); root = Path(__file__).resolve().parent.parent
    return {"input": base["input_sample"], "fig_02": root / "reports" / "figures" / "size_vs_finish_pooled.png", "fig_03": root / "reports" / "figures" / "size_vs_finish_by_year.png", "fig_04": root / "reports" / "figures" / "size_vs_finish_by_seed_group.png", "csv_02": root / "reports" / "size_vs_finish_pooled_table.csv", "csv_03": root / "reports" / "size_vs_finish_by_year_table.csv", "csv_04": root / "reports" / "size_vs_finish_by_seed_group_table.csv", "md_02": root / "reports" / "size_vs_finish_pooled_summary.md", "md_03": root / "reports" / "size_vs_finish_by_year_summary.md", "md_04": root / "reports" / "size_vs_finish_by_seed_group_summary.md"}

def build_analysis_02_table(df: pd.DataFrame) -> tuple[pd.DataFrame, dict[str, pd.DataFrame]]:
    metric = "EFFHGT"; finish_vs_seed = df["finish_depth"] - df.groupby("seed_int")["finish_depth"].transform("mean"); metric_vs_seed = df[metric] - df.groupby("seed_int")[metric].transform("mean")
    adjusted = df[["Team", "year", "seed_int", "finish_depth", metric]].copy(); adjusted["finish_vs_seed"] = finish_vs_seed; adjusted["metric_vs_seed"] = metric_vs_seed
    champs = df.loc[df["finish"] == "CHAMPS", metric]; r64 = df.loc[df["finish"] == "R64", metric]
    table = pd.DataFrame([{"metric": metric, "raw_spearman": float(df["finish_depth"].corr(df[metric], method="spearman")), "seed_adjusted_spearman": float(finish_vs_seed.corr(metric_vs_seed, method="spearman")), "field_mean": float(df[metric].mean()), "champ_mean": float(champs.mean()), "r64_mean": float(r64.mean()), "champ_minus_r64": float(champs.mean() - r64.mean())}])
    return table, {metric: adjusted}

def plot_analysis_02(df: pd.DataFrame, adjusted_df: pd.DataFrame, summary_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    raw_rho = float(summary_table.loc[0, "raw_spearman"]); seed_rho = float(summary_table.loc[0, "seed_adjusted_spearman"])
    scatter_df = df[["finish", primary_metric]].copy(); scatter_df["x_base"] = scatter_df["finish"].map({f: i for i, f in enumerate(FINISH_ORDER)}); scatter_df["x"] = scatter_df["x_base"] + np.random.default_rng(42).uniform(-0.22, 0.22, len(scatter_df))
    raw_slope, raw_intercept = np.polyfit(scatter_df["x_base"].to_numpy(float), scatter_df[primary_metric].to_numpy(float), 1); raw_line_x = np.linspace(float(scatter_df["x_base"].min()), float(scatter_df["x_base"].max()), 200); raw_line_y = raw_intercept + raw_slope * raw_line_x
    round_means = df.groupby("finish", observed=False)[primary_metric].mean().reindex(FINISH_ORDER); mean_x = np.arange(len(FINISH_ORDER), dtype=float); mean_y = round_means.to_numpy(float); field_average = float(df[primary_metric].mean())
    x_seed = adjusted_df["metric_vs_seed"].to_numpy(float); y_seed = adjusted_df["finish_vs_seed"].to_numpy(float); seed_slope, seed_intercept = np.polyfit(x_seed, y_seed, 1); seed_line_x = np.linspace(float(np.nanmin(x_seed)), float(np.nanmax(x_seed)), 200); seed_line_y = seed_intercept + seed_slope * seed_line_x
    set_project_theme(); fig, axes = plt.subplots(1, 2, figsize=(17.6, 8.8), constrained_layout=False); apply_report_background(fig, axes); fig.subplots_adjust(top=0.83, bottom=0.11, left=0.07, right=0.97, wspace=0.16)
    left_ax, right_ax = axes
    left_ax.scatter(scatter_df["x"].to_numpy(float), scatter_df[primary_metric].to_numpy(float), s=24, color=TEXT_DARK, alpha=0.38, linewidths=0, zorder=1); left_ax.plot(raw_line_x, raw_line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.3, zorder=2); left_ax.plot(mean_x, mean_y, color=ACCENT, linewidth=2.8, zorder=3); left_ax.scatter(mean_x, mean_y, color=ACCENT, s=90, zorder=4); left_ax.axhline(field_average, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0)
    left_ax.set_xticks(mean_x); left_ax.set_xticklabels([FINISH_DISPLAY[f] for f in FINISH_ORDER], fontsize=10.5, fontweight="semibold"); left_ax.set_xlabel("Tournament finish", fontsize=11, fontweight="semibold"); left_ax.set_ylabel(metric_label(primary_metric), fontsize=11, fontweight="semibold"); left_ax.set_title("Unadjusted pooled view", fontsize=13, fontweight="bold", color=TEXT_DARK, pad=16)
    left_ax.text(0.80, 0.965, f"Raw rho = {raw_rho:+.3f}", transform=left_ax.transAxes, ha="left", va="top", fontsize=10.5, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    left_ax.text(0.80, 0.90, "Dashed line = pooled linear trend", transform=left_ax.transAxes, ha="left", va="top", fontsize=9.6, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    left_ax.text(len(mean_x) - 0.08, field_average, "Field avg", ha="right", va="bottom", fontsize=9.6, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    right_ax.scatter(x_seed, y_seed, s=24, color=TEXT_DARK, alpha=0.38, linewidths=0, zorder=1); right_ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0); right_ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.4, zorder=0); right_ax.plot(seed_line_x, seed_line_y, color=ACCENT, linestyle="--", linewidth=2.6, zorder=2)
    right_ax.set_xlabel(f"{metric_label(primary_metric)} vs exact-seed baseline", fontsize=11, fontweight="semibold"); right_ax.set_ylabel("Finish vs exact-seed expectation", fontsize=11, fontweight="semibold"); right_ax.set_title("Exact-seed controlled view", fontsize=13, fontweight="bold", color=TEXT_DARK, pad=16)
    right_ax.text(0.03, 0.95, f"Within-seed rho = {seed_rho:+.3f}", transform=right_ax.transAxes, ha="left", va="top", fontsize=10.5, fontweight="bold", color=LABEL_TEXT, bbox=styled_box()); right_ax.text(0.03, 0.885, "0 = seed-line baseline", transform=right_ax.transAxes, ha="left", va="top", fontsize=9.6, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    for ax in axes: ax.grid(False); ax.spines["top"].set_visible(False); ax.spines["right"].set_visible(False); ax.tick_params(axis="x", labelsize=10, length=0, pad=6); ax.tick_params(axis="y", labelsize=10, length=0, pad=6)
    fig.suptitle("Tournament finish and effective height", fontsize=22, fontweight="bold", y=0.97); fig.text(0.5, 0.91, "Left: raw tournament results. Right: each team relative to its exact seed line.", ha="center", va="center", fontsize=10.1, color=TEXT_LIGHT)
    out_path.parent.mkdir(parents=True, exist_ok=True); fig.savefig(out_path, dpi=220, bbox_inches="tight"); plt.close(fig)

def build_analysis_04_table(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    seed_df = df[df["seed_int"].between(1, 16)].copy(); expected_depths = seed_df.groupby("seed_group")["finish_depth"].mean().to_dict(); seed_df["expected_finish_depth"] = seed_df["seed_group"].map(expected_depths); seed_df["finish_vs_expected"] = seed_df["finish_depth"] - seed_df["expected_finish_depth"]
    rows = []
    for seed_group in SEED_GROUP_ORDER:
        subset = seed_df[seed_df["seed_group"] == seed_group].copy(); rows.append({"seed_group": seed_group, "expected_finish_depth": float(subset["expected_finish_depth"].mean()), "team_count": int(len(subset)), "effhgt_rho": float(subset["finish_vs_expected"].corr(subset["EFFHGT"], method="spearman"))})
    return pd.DataFrame(rows), seed_df

def plot_analysis_04(seed_table: pd.DataFrame, seed_points: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    rho_column = f"{metric_slug(primary_metric)}_rho"; x_min = float(seed_points[primary_metric].min()); x_max = float(seed_points[primary_metric].max()); y_min = float(seed_points["finish_vs_expected"].min()); y_max = float(seed_points["finish_vs_expected"].max()); x_pad = max((x_max - x_min) * 0.06, 0.3); y_pad = 0.24
    set_project_theme(); fig, axes = plt.subplots(2, 4, figsize=(18.4, 10.6), constrained_layout=False); axes_flat = axes.flatten(); apply_report_background(fig, axes_flat); fig.subplots_adjust(top=0.84, bottom=0.10, left=0.07, right=0.98, hspace=0.34, wspace=0.18)
    for ax, seed_group in zip(axes_flat, SEED_GROUP_ORDER):
        subset = seed_points[seed_points["seed_group"] == seed_group].copy(); row = seed_table.loc[seed_table["seed_group"] == seed_group].iloc[0]; slope, intercept = np.polyfit(subset[primary_metric], subset["finish_vs_expected"], 1); line_x = np.linspace(x_min - x_pad, x_max + x_pad, 200); line_y = intercept + slope * line_x
        ax.scatter(subset[primary_metric], subset["finish_vs_expected"], s=28, color=TEXT_DARK, alpha=0.38, linewidths=0); ax.plot(line_x, line_y, color=HIGHLIGHT, linestyle="--", linewidth=2.0); ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.2); ax.set_xlim(x_min - x_pad, x_max + x_pad); ax.set_ylim(y_min - y_pad, y_max + y_pad); ax.grid(False); ax.spines["top"].set_visible(False); ax.spines["right"].set_visible(False); ax.tick_params(axis="x", labelsize=9.5, length=0, pad=5); ax.tick_params(axis="y", labelsize=9.5, length=0, pad=5)
        ax.set_title(f"{seed_group} | exp. {row['expected_finish_depth']:.2f}", fontsize=11, fontweight="bold", color=TEXT_DARK, pad=8); ax.text(0.04, 0.93, f"rho = {row[rho_column]:+.3f}\nn = {int(row['team_count'])}", transform=ax.transAxes, ha="left", va="top", fontsize=9.4, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    axes_flat[-1].axis("off")
    for ax in axes[:, 0]: ax.set_ylabel("Finish - expected finish", fontsize=10.5, fontweight="semibold")
    for ax in axes[1, :-1]: ax.set_xlabel(metric_label(primary_metric), fontsize=10.5, fontweight="semibold")
    fig.suptitle("Finish versus effective height within each seed band", fontsize=22, fontweight="bold", y=0.97); fig.text(0.5, 0.91, "Each panel shows team-level finish versus seed expectation inside one seed band", ha="center", va="center", fontsize=9.9, color=TEXT_LIGHT)
    out_path.parent.mkdir(parents=True, exist_ok=True); fig.savefig(out_path, dpi=220, bbox_inches="tight"); plt.close(fig)

def write_analysis_02_summary(summary_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    row = summary_table.iloc[0]; lines = ["# Size Analysis 2: Size vs Finish (Pooled)", "", f"- Metric used here: `{primary_metric}` (`{metric_label(primary_metric)}`).", f"- Raw pooled relationship for `{primary_metric}`: `{row['raw_spearman']:+.3f}`.", f"- Exact-seed-controlled relationship for `{primary_metric}`: `{row['seed_adjusted_spearman']:+.3f}`.", f"- Interpretation: the raw rho is `{row['raw_spearman']:+.3f}` but `{row['seed_adjusted_spearman']:+.3f}` within exact seed line, so {'most of the raw gradient runs through seed' if abs(row['seed_adjusted_spearman']) < abs(row['raw_spearman']) / 2 else 'much of the raw gradient survives seed control'}.", "", "## Table", "", summary_table.round(3).to_markdown(index=False)]
    out_path.write_text("\n".join(lines), encoding="utf-8")

def write_analysis_04_summary(seed_table: pd.DataFrame, primary_metric: str, out_path: Path) -> None:
    rho_column = f"{metric_slug(primary_metric)}_rho"; strongest = seed_table.loc[seed_table[rho_column].idxmax()]; weakest = seed_table.loc[seed_table[rho_column].idxmin()]; positive_groups = seed_table.loc[seed_table[rho_column] > 0, "seed_group"].astype(str).tolist(); positives = ', '.join([f'`{v}`' for v in positive_groups]) if positive_groups else '`none`'
    lines = ["# Size Analysis 4: Size vs Finish by Seed Band", "", f"- Metric used here: `{primary_metric}` (`{metric_label(primary_metric)}`).", f"- Strongest seed band: `{strongest['seed_group']}` at `{strongest[rho_column]:+.3f}`.", f"- Weakest seed band: `{weakest['seed_group']}` at `{weakest[rho_column]:+.3f}`.", f"- Positive seed bands: {positives}.", "- Interpretation: band rhos are descriptive and each rests on one seed band's teams; `reports/robustness_summary.md` tests every band against a within-seed permutation null with a multiple-testing correction.", "", "## Table", "", seed_table.round(3).to_markdown(index=False)]
    out_path.write_text("\n".join(lines), encoding="utf-8")

def main() -> int:
    paths = get_paths(); full_df = load_size_data(paths["input"]); tournament_df = load_tournament_data(full_df); primary_metric = "EFFHGT"
    table02, adjusted = build_analysis_02_table(tournament_df); table03 = build_year_comparison_table(build_overperformance_data(tournament_df), primary_metric); table04, seed_points = build_analysis_04_table(tournament_df)
    table02.round(6).to_csv(paths["csv_02"], index=False); table03.round(6).to_csv(paths["csv_03"], index=False); table04.round(6).to_csv(paths["csv_04"], index=False)
    plot_analysis_02(tournament_df, adjusted[primary_metric], table02, primary_metric, paths["fig_02"]); plot_year_comparison(table03, primary_metric, paths["fig_03"]); plot_analysis_04(table04, seed_points, primary_metric, paths["fig_04"])
    write_analysis_02_summary(table02, primary_metric, paths["md_02"]); write_year_comparison_summary(paths["md_03"], table03, primary_metric); write_analysis_04_summary(table04, primary_metric, paths["md_04"])
    print(f"Primary metric: {primary_metric}"); print(f"Wrote figure: {paths['fig_02']}"); print(f"Wrote figure: {paths['fig_03']}"); print(f"Wrote figure: {paths['fig_04']}"); print(f"Wrote summary: {paths['md_02']}"); print(f"Wrote summary: {paths['md_03']}"); print(f"Wrote summary: {paths['md_04']}"); print(f"Wrote table: {paths['csv_02']}"); print(f"Wrote table: {paths['csv_03']}"); print(f"Wrote table: {paths['csv_04']}"); return 0

if __name__ == "__main__": raise SystemExit(main())
