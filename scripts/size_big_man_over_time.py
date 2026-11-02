#!/usr/bin/env python3
from __future__ import annotations
from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import FuncFormatter
from size_analysis_common import ACCENT, BIG_MAN_FIGURE_NOTE, BIG_MAN_OUTCOME_NOTE, HIGHLIGHT, LABEL_TEXT, SECONDARY, TEXT_DARK, TEXT_LIGHT, TRANSFER_PORTAL_YEAR, apply_report_background, build_overperformance_data, format_year_tick, get_size_base_paths, load_size_data, load_tournament_data, set_project_theme, styled_box
THRESHOLD = 4.0

def get_paths() -> dict[str, Path]:
    base = get_size_base_paths(); root = Path(__file__).resolve().parent.parent
    return {"input": base["input_sample"], "fig": root / "reports" / "figures" / "size_big_man_over_time.png", "csv": root / "reports" / "size_big_man_over_time_table.csv", "md": root / "reports" / "size_big_man_over_time_summary.md"}

def build_table(df: pd.DataFrame) -> pd.DataFrame:
    over = build_overperformance_data(df)
    over["dominant_big_man"] = over["max_c_porpag"].fillna(0) >= THRESHOLD
    grouped = over.groupby(["year", "dominant_big_man"], as_index=False).agg(team_count=("Team", "size"), mean_overperformance=("overperformance", "mean"), median_overperformance=("overperformance", "median"), adv_rate=("overperformance", lambda s: float((s > 0).mean())), avg_max_c_porpag=("max_c_porpag", "mean"))
    grouped["group_label"] = grouped["dominant_big_man"].map({True: "Dominant big man", False: "Rest of field"})
    return grouped.sort_values(["year", "dominant_big_man"], ascending=[True, False], kind="stable").reset_index(drop=True)

def plot_table(table: pd.DataFrame, out_path: Path) -> None:
    wide = table.pivot(index="year", columns="dominant_big_man", values="mean_overperformance").sort_index()
    dom = wide.get(True, pd.Series(index=wide.index, dtype=float))
    years = wide.index.to_numpy(float)
    set_project_theme(); fig, ax = plt.subplots(figsize=(16.2, 8.8), constrained_layout=False); apply_report_background(fig, [ax]); fig.subplots_adjust(top=0.84, bottom=0.12, left=0.08, right=0.95)
    ax.plot(years, dom.to_numpy(float), color=ACCENT, linewidth=2.8, marker="o", markersize=5.5, label="Dominant big man")
    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.5); ax.axvline(TRANSFER_PORTAL_YEAR, color=SECONDARY, linestyle=":", linewidth=2.0, alpha=0.95)
    ax.text(TRANSFER_PORTAL_YEAR + 0.2, float(dom.max(skipna=True)) + 0.05, "2022 transfer-rules era", ha="left", va="bottom", fontsize=10.4, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    dom_rows = table[table["dominant_big_man"]].copy(); best = dom_rows.loc[dom_rows["mean_overperformance"].idxmax()]; worst = dom_rows.loc[dom_rows["mean_overperformance"].idxmin()]
    ax.annotate(f"Best year: {int(best['year'])}\n{best['mean_overperformance']:+.2f}", xy=(float(best['year']), float(best['mean_overperformance'])), xytext=(float(best['year']) - 2.1, float(best['mean_overperformance']) + 0.06), arrowprops={"arrowstyle": "->", "color": SECONDARY, "linewidth": 1.2}, fontsize=10.1, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    ax.annotate(f"Worst year: {int(worst['year'])}\n{worst['mean_overperformance']:+.2f}", xy=(float(worst['year']), float(worst['mean_overperformance'])), xytext=(float(worst['year']) + 0.7, float(worst['mean_overperformance']) - 0.08), arrowprops={"arrowstyle": "->", "color": SECONDARY, "linewidth": 1.2}, fontsize=10.1, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    overall_dom = float(dom.mean(skipna=True))
    ax.text(0.03, 0.95, f"Overall avg: {overall_dom:+.2f}\nThreshold: max_c_porpag >= {THRESHOLD:.1f}", transform=ax.transAxes, ha="left", va="top", fontsize=10.2, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    ax.set_xlim(years.min() - 0.5, years.max() + 1.5); ax.set_xlabel("Season", fontsize=11.5, fontweight="semibold"); ax.set_ylabel("Mean finish - exact-seed expectation", fontsize=11.5, fontweight="semibold"); ax.xaxis.set_major_formatter(FuncFormatter(format_year_tick)); ax.grid(False); ax.spines["top"].set_visible(False); ax.spines["right"].set_visible(False); ax.tick_params(axis="x", labelsize=10.5, length=0, pad=7); ax.tick_params(axis="y", labelsize=10.5, length=0, pad=7)
    fig.suptitle("Overperformance over time for teams with dominant big men", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.91, f"Each line is the season-average finish versus exact-seed expectation. {BIG_MAN_FIGURE_NOTE}.", ha="center", va="center", fontsize=10.1, color=TEXT_LIGHT)
    out_path.parent.mkdir(parents=True, exist_ok=True); fig.savefig(out_path, dpi=220, bbox_inches="tight"); plt.close(fig)

def write_summary(table: pd.DataFrame, out_path: Path) -> None:
    dom = table[table["dominant_big_man"]].copy(); other = table[~table["dominant_big_man"]].copy(); best = dom.loc[dom["mean_overperformance"].idxmax()]; worst = dom.loc[dom["mean_overperformance"].idxmin()]
    lines = ["# Size Support: Dominant Big Men Over Time", "", f"- Outcome contamination: {BIG_MAN_OUTCOME_NOTE}", f"- Threshold: `max_c_porpag >= {THRESHOLD:.1f}`.", f"- Dominant-big-man teams averaged `{dom['mean_overperformance'].mean():+.3f}` overperformance across seasons, versus `{other['mean_overperformance'].mean():+.3f}` for the rest of the field.", f"- Best dominant-big-man season: `{int(best['year'])}` at `{best['mean_overperformance']:+.3f}`.", f"- Worst dominant-big-man season: `{int(worst['year'])}` at `{worst['mean_overperformance']:+.3f}`.", "", "## Table", "", table.round(3).to_markdown(index=False)]
    out_path.write_text("\n".join(lines), encoding="utf-8")

def main() -> int:
    paths = get_paths(); df = load_tournament_data(load_size_data(paths["input"])); table = build_table(df)
    table.round(6).to_csv(paths["csv"], index=False); plot_table(table, paths["fig"]); write_summary(table, paths["md"])
    print(f"Wrote figure: {paths['fig']}"); print(f"Wrote summary: {paths['md']}"); print(f"Wrote table: {paths['csv']}"); return 0

if __name__ == "__main__": raise SystemExit(main())
