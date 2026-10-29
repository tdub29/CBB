#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.offsetbox import AnnotationBbox, OffsetImage
import numpy as np
import pandas as pd

from size_analysis_common import cache_logo_images, load_logo_image, with_repo_relative_logo_paths
from turnovers_analysis_common import (
    ACCENT,
    CURRENT_FIELD_YEAR,
    ERAS,
    LABEL_TEXT,
    POSITIVE,
    PROJECT_PALETTE,
    SECONDARY,
    apply_report_background,
    build_overperformance_data,
    build_seed_reference_tables,
    get_turnovers_base_paths,
    load_projected_field,
    load_tournament_data,
    load_turnovers_data,
    metric_slug,
    normalize_team_key,
    set_project_theme,
    styled_box,
)

OVERPERFORMER_BADGE_FACE = "#B6DCCB"
OVERPERFORMER_BADGE_EDGE = "#2F6B57"


def get_paths() -> dict[str, Path]:
    base = get_turnovers_base_paths()
    root = Path(__file__).resolve().parent.parent
    return {
        "input": base["input_sample"],
        "seeds_2026": base["seeds_2026"],
        "logo_cache": Path(__file__).resolve().parent.parent / "data" / "external" / "team_logos" / "espn_ncaam",
        "fig_05": root / "reports" / "figures" / "turnovers_archetypes_vs_overperformance.png",
        "fig_07": root / "reports" / "figures" / "turnovers_2026_field.png",
        "fig_07_blocks": root / "reports" / "figures" / "turnovers_2026_field_blocks.png",
        "csv_05": root / "reports" / "turnovers_archetypes_vs_overperformance_table.csv",
        "csv_06": root / "reports" / "turnovers_era_comparison_table.csv",
        "csv_07": root / "reports" / "turnovers_2026_field_table.csv",
        "md_05": root / "reports" / "turnovers_archetypes_vs_overperformance_summary.md",
        "md_06": root / "reports" / "turnovers_era_comparison_summary.md",
        "md_07": root / "reports" / "turnovers_2026_field_summary.md",
        "md_07_blocks": root / "reports" / "turnovers_2026_field_blocks_summary.md",
    }


def build_analysis_05_table(over_df: pd.DataFrame) -> pd.DataFrame:
    plot_df = over_df.copy()
    plot_df["archetype"] = np.select(
        [
            (plot_df["tor_edge"] >= 0) & (plot_df["defensive_disruption_edge"] >= 0),
            (plot_df["tor_edge"] >= 0) & (plot_df["defensive_disruption_edge"] < 0),
            (plot_df["tor_edge"] < 0) & (plot_df["defensive_disruption_edge"] >= 0),
            (plot_df["tor_edge"] < 0) & (plot_df["defensive_disruption_edge"] < 0),
        ],
        [
            "clean offense + live defense",
            "clean offense only",
            "live defense only",
            "below both baselines",
        ],
        default="mixed",
    )
    rows = []
    order = ["clean offense + live defense", "clean offense only", "live defense only", "below both baselines"]
    for archetype in order:
        group = plot_df[plot_df["archetype"] == archetype]
        rows.append(
            {
                "archetype": archetype,
                "team_count": int(len(group)),
                "mean_overperformance": float(group["overperformance"].mean()),
                "median_overperformance": float(group["overperformance"].median()),
                "avg_tor_edge": float(group["tor_edge"].mean()),
                "avg_tord_edge": float(group["tord_edge"].mean()),
                "avg_blk_pct_edge": float(group["blk_pct_edge"].mean()),
                "avg_adjt_edge": float(group["adjt_edge"].mean()),
            }
        )
    return pd.DataFrame(rows)


def plot_analysis_05(table: pd.DataFrame, out_path: Path) -> None:
    plot_df = table.copy().sort_values("mean_overperformance", kind="stable")
    y = np.arange(len(plot_df))
    values = plot_df["mean_overperformance"].to_numpy(float)

    set_project_theme()
    fig, ax = plt.subplots(figsize=(13.8, 7.8), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.82, bottom=0.10, left=0.28, right=0.92)
    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.3)
    ax.hlines(y, 0, values, color=ACCENT, linewidth=2.7)
    ax.scatter(values, y, color=ACCENT, s=96, zorder=3)
    for y_pos, value in zip(y, values):
        ax.text(value + (0.012 if value >= 0 else -0.012), y_pos, f"{value:+.3f}", ha="left" if value >= 0 else "right", va="center", fontsize=9.8, fontweight="bold", color=LABEL_TEXT, bbox=styled_box())
    ax.set_yticks(y)
    ax.set_yticklabels(plot_df["archetype"].tolist(), fontsize=10.4, fontweight="semibold")
    ax.set_xlabel("Mean overperformance vs exact seed", fontsize=11.0, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", length=0, pad=6)
    ax.tick_params(axis="y", length=0, pad=8)
    fig.suptitle("Turnover archetypes versus overperformance", fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.90, "Teams split at their exact seed line's average on TOR edge and defensive disruption edge (TORD edge + 0.35 x BLK% edge).", ha="center", va="center", fontsize=10.0, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def build_analysis_06_table(over_df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    full_rho = float(over_df["overperformance"].corr(over_df["tor_edge"], method="spearman"))
    rows.append({"era": "2008-2025", "team_count": int(len(over_df)), "tor_seed_adjusted_rho": full_rho})
    for label, start, end in ERAS:
        group = over_df[over_df["year"].between(start, end)].copy()
        rows.append({"era": label, "team_count": int(len(group)), "tor_seed_adjusted_rho": float(group["overperformance"].corr(group["tor_edge"], method="spearman"))})
    return pd.DataFrame(rows)


def build_analysis_07_table(full_df: pd.DataFrame, tournament_df: pd.DataFrame, seeds_path: Path, logo_cache_dir: Path) -> pd.DataFrame:
    field = load_projected_field(seeds_path, full_df)
    full_ref, recent_ref = build_seed_reference_tables(tournament_df, ["TOR", "TORD", "BLK%", "BLKED%", "AdjT"])
    merged = field.merge(full_ref, on="projected_seed", how="left").merge(recent_ref, on="projected_seed", how="left")
    for metric in ["TOR", "TORD", "BLK%", "BLKED%", "AdjT"]:
        slug = metric_slug(metric)
        merged[f"full_{slug}_delta_raw"] = merged[metric] - merged[f"full_seed_avg_{slug}"]
        merged[f"full_{slug}_edge"] = -merged[f"full_{slug}_delta_raw"] if metric in {"TOR", "BLKED%"} else merged[f"full_{slug}_delta_raw"]
    merged["defensive_disruption_edge"] = merged["full_tord_edge"] + 0.35 * merged["full_blk_pct_edge"]
    merged["tempo_bucket"] = np.where(merged["full_adjt_edge"] >= 0, "faster than seed baseline", "slower than seed baseline")
    # Ad hoc weights, not fitted: used only to order teams within a seed line in the exported table.
    merged["turnover_profile_score"] = merged["full_tor_edge"] + 0.55 * merged["defensive_disruption_edge"]
    merged["disruption_context"] = np.where(
        merged["defensive_disruption_edge"] >= 0.75,
        "positive",
        np.where(merged["defensive_disruption_edge"] <= -0.75, "negative", "neutral"),
    )
    merged["logo_lookup_key"] = merged["team_display"].map(normalize_team_key)

    champion_rows = (
        tournament_df.loc[tournament_df["finish"] == "CHAMPS", ["Team", "year", "seed_int", "TOR", "TORD", "BLK%", "BLKED%"]]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["team_key", "year"], kind="stable")
    )
    champion_rows = champion_rows.merge(
        full_ref.rename(columns={"projected_seed": "seed_int"}),
        on="seed_int",
        how="left",
    )
    for metric in ["TOR", "TORD", "BLK%", "BLKED%"]:
        slug = metric_slug(metric)
        champion_rows[f"title_{slug}_delta_raw"] = champion_rows[metric] - champion_rows[f"full_seed_avg_{slug}"]
        champion_rows[f"title_{slug}_edge"] = -champion_rows[f"title_{slug}_delta_raw"] if metric in {"TOR", "BLKED%"} else champion_rows[f"title_{slug}_delta_raw"]
    champion_history_rows: list[dict[str, object]] = []
    for team_key, group in champion_rows.groupby("team_key", sort=False):
        title_rows = list(group.itertuples(index=False))
        champion_history_rows.append(
            {
                "team_key": team_key,
                "championship_count": int(len(title_rows)),
                "last_title_year": int(title_rows[-1].year),
                "last_title_seed": int(title_rows[-1].seed_int),
                "title_years": [int(row.year) for row in title_rows],
                "title_seeds": [int(row.seed_int) for row in title_rows],
                "title_tor_edge_values": [float(row.title_tor_edge) for row in title_rows],
                "title_tord_edge_values": [float(row.title_tord_edge) for row in title_rows],
                "title_blk_pct_edge_values": [float(row.title_blk_pct_edge) for row in title_rows],
                "title_blked_pct_edge_values": [float(row.title_blked_pct_edge) for row in title_rows],
                "title_history": "; ".join([f"{int(row.year)} ({int(row.seed_int)})" for row in title_rows]),
            }
        )
    champion_history = pd.DataFrame(champion_history_rows)
    merged["team_key"] = merged["team_display"].map(normalize_team_key)
    if not champion_history.empty:
        merged = merged.merge(champion_history, on="team_key", how="left")
    merged["championship_count"] = merged.get("championship_count", 0).fillna(0).astype(int)
    merged["title_years"] = merged.get("title_years", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_seeds"] = merged.get("title_seeds", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_tor_edge_values"] = merged.get("title_tor_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_tord_edge_values"] = merged.get("title_tord_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_blk_pct_edge_values"] = merged.get("title_blk_pct_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_blked_pct_edge_values"] = merged.get("title_blked_pct_edge_values", pd.Series([[]] * len(merged))).apply(lambda value: value if isinstance(value, list) else [])
    merged["title_history"] = merged.get("title_history", "").fillna("")
    merged["is_former_champion"] = merged["championship_count"] > 0
    merged = cache_logo_images(merged, logo_cache_dir)
    merged["logo_fallback"] = merged["logo_path"].isna()
    merged = merged.sort_values(["projected_seed", "turnover_profile_score"], ascending=[True, False], kind="stable").reset_index(drop=True)
    return merged


def build_analysis_07_overperformer_history(
    tournament_df: pd.DataFrame,
    full_ref: pd.DataFrame,
    logo_cache_dir: Path,
) -> pd.DataFrame:
    over_df = build_overperformance_data(tournament_df)
    history = (
        over_df.loc[
            over_df["overperformance"] >= 3,
            ["Team", "year", "seed_int", "TOR", "TORD", "BLK%", "BLKED%", "overperformance"],
        ]
        .assign(team_key=lambda frame: frame["Team"].map(normalize_team_key))
        .sort_values(["year", "seed_int", "Team"], kind="stable")
        .reset_index(drop=True)
    )
    if history.empty:
        return history
    history = history.merge(full_ref.rename(columns={"projected_seed": "seed_int"}), on="seed_int", how="left")
    for metric in ["TOR", "TORD", "BLK%", "BLKED%"]:
        slug = metric_slug(metric)
        history[f"full_{slug}_delta_raw"] = history[metric] - history[f"full_seed_avg_{slug}"]
        history[f"full_{slug}_edge"] = -history[f"full_{slug}_delta_raw"] if metric in {"TOR", "BLKED%"} else history[f"full_{slug}_delta_raw"]
    history["team_display"] = history["Team"].astype(str).str.strip()
    history["logo_lookup_key"] = history["team_key"]
    history = cache_logo_images(history, logo_cache_dir)
    history["logo_fallback"] = history["logo_path"].isna()
    return history


def validate_analysis_07_table(table: pd.DataFrame) -> None:
    champion_rows = table.loc[table["is_former_champion"]].copy()
    required_title_columns = [
        "title_years",
        "title_tor_edge_values",
        "title_tord_edge_values",
        "title_blk_pct_edge_values",
        "title_blked_pct_edge_values",
    ]
    for row in champion_rows.itertuples(index=False):
        title_years = list(getattr(row, "title_years", []))
        if not title_years:
            raise ValueError(f"Former champion {row.team_display} is missing title years for badge plotting")
        title_count = len(title_years)
        for column in required_title_columns[1:]:
            values = list(getattr(row, column, []))
            if len(values) != title_count:
                raise ValueError(
                    f"Former champion {row.team_display} has {title_count} title years but {len(values)} values in {column}"
                )
            if any(pd.isna(value) for value in values):
                raise ValueError(f"Former champion {row.team_display} has missing values in {column}")


def plot_analysis_07_pair(
    table: pd.DataFrame,
    overperformer_history: pd.DataFrame,
    out_path: Path,
    *,
    x_col: str,
    y_col: str,
    title: str,
    subtitle: str,
    x_label: str,
    y_label: str,
    quadrant_labels: list[tuple[float, float, str]],
) -> None:
    def title_edge_list_column(metric_edge_col: str) -> str:
        if not metric_edge_col.startswith("full_") or not metric_edge_col.endswith("_edge"):
            raise ValueError(f"Unexpected metric edge column: {metric_edge_col}")
        return f"title_{metric_edge_col[len('full_'):-len('_edge')]}_edge_values"

    plot_df = table.copy()

    set_project_theme()
    fig, ax = plt.subplots(figsize=(15.8, 12.2), constrained_layout=False)
    apply_report_background(fig, [ax])
    fig.subplots_adjust(top=0.86, bottom=0.11, left=0.08, right=0.96)

    ax.axhline(0, color=SECONDARY, linestyle=":", linewidth=1.6, zorder=0)
    ax.axvline(0, color=SECONDARY, linestyle=":", linewidth=1.6, zorder=0)

    x_values = plot_df[x_col].to_numpy(dtype=float)
    y_values = plot_df[y_col].to_numpy(dtype=float)
    if not overperformer_history.empty:
        x_values = np.concatenate([x_values, overperformer_history[x_col].to_numpy(dtype=float)])
        y_values = np.concatenate([y_values, overperformer_history[y_col].to_numpy(dtype=float)])
    x_pad = max((float(np.nanmax(x_values)) - float(np.nanmin(x_values))) * 0.12, 0.8)
    y_pad = max((float(np.nanmax(y_values)) - float(np.nanmin(y_values))) * 0.12, 0.8)
    x_min = float(np.nanmin(x_values)) - x_pad
    x_max = float(np.nanmax(x_values)) + x_pad
    y_min = float(np.nanmin(y_values)) - y_pad
    y_max = float(np.nanmax(y_values)) + y_pad

    for x_pos, y_pos, label in quadrant_labels:
        ax.text(
            x_pos,
            y_pos,
            label,
            transform=ax.transAxes,
            ha="center",
            va="center",
            fontsize=11.2,
            fontweight="bold",
            color=LABEL_TEXT,
            bbox=styled_box(alpha=0.90),
            zorder=1,
        )

    for row in plot_df.itertuples(index=False):
        x_value = float(getattr(row, x_col))
        y_value = float(getattr(row, y_col))
        frame_color = POSITIVE if getattr(row, "is_former_champion", False) else PROJECT_PALETTE["steel"]
        logo_path = getattr(row, "logo_path", None)
        is_former_champion = bool(getattr(row, "is_former_champion", False))
        if isinstance(logo_path, (str, Path)) and Path(logo_path).exists():
            logo_image = np.asarray(load_logo_image(logo_path))
            if is_former_champion:
                title_x_values = list(getattr(row, title_edge_list_column(x_col), []))
                title_y_values = list(getattr(row, title_edge_list_column(y_col), []))
                title_years = list(getattr(row, "title_years", []))
                point_counts: dict[tuple[float, float], int] = {}
                for title_year, title_x_value, title_y_value in zip(title_years, title_x_values, title_y_values):
                    point_key = (round(float(title_x_value), 3), round(float(title_y_value), 3))
                    point_counts[point_key] = point_counts.get(point_key, 0) + 1
                    offset_idx = point_counts[point_key] - 1
                    champ_artist = AnnotationBbox(
                        OffsetImage(logo_image, zoom=0.056),
                        (float(title_x_value), float(title_y_value)),
                        xybox=(-14 - (offset_idx * 12), -14 - (offset_idx * 10)),
                        boxcoords="offset points",
                        frameon=True,
                        bboxprops={
                            "boxstyle": "round,pad=0.18",
                            "facecolor": "#F5E7A1",
                            "edgecolor": "#D4A72C",
                            "linewidth": 1.35,
                            "alpha": 0.96,
                        },
                        zorder=2,
                    )
                    ax.add_artist(champ_artist)
            artist = AnnotationBbox(
                OffsetImage(logo_image, zoom=0.078),
                (x_value, y_value),
                frameon=False,
                zorder=3,
            )
            ax.add_artist(artist)
        else:
            ax.text(
                x_value,
                y_value,
                str(row.team_display),
                ha="center",
                va="center",
                fontsize=7.2,
                fontweight="bold",
                color=PROJECT_PALETTE["deep_ocean"],
                bbox=styled_box(facecolor="white", edgecolor=frame_color),
                zorder=4,
            )

    if not overperformer_history.empty:
        for _, row in overperformer_history.iterrows():
            logo_path = row.get("logo_path")
            if not isinstance(logo_path, (str, Path)) or not Path(str(logo_path)).exists():
                continue
            x_value = float(row[x_col])
            y_value = float(row[y_col])
            ax.scatter([x_value], [y_value], s=920, marker="^", facecolor=OVERPERFORMER_BADGE_FACE, edgecolor=OVERPERFORMER_BADGE_EDGE, linewidth=1.2, zorder=1.85)
            ax.add_artist(AnnotationBbox(OffsetImage(np.asarray(load_logo_image(logo_path)), zoom=0.056), (x_value, y_value), frameon=False, zorder=2.15))

    legend_handles = [
        Line2D([], [], color=SECONDARY, linestyle=":", linewidth=1.6, label="Seed baseline split"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="white", markeredgecolor=PROJECT_PALETTE["steel"], markeredgewidth=1.9, markersize=10, label="Projected 2026 team logo"),
        Line2D([], [], marker="o", linestyle="None", markerfacecolor="#F5E7A1", markeredgecolor="#D4A72C", markeredgewidth=1.3, markersize=10, label="Former champ badge"),
        Line2D([], [], marker="^", linestyle="None", markerfacecolor=OVERPERFORMER_BADGE_FACE, markeredgecolor=OVERPERFORMER_BADGE_EDGE, markeredgewidth=1.3, markersize=15, label="Historical 3+ round overperformer"),
    ]
    legend = ax.legend(
        handles=legend_handles,
        loc="upper left",
        bbox_to_anchor=(0.015, 0.985),
        frameon=True,
        facecolor=PROJECT_PALETTE["midnight"],
        edgecolor=ACCENT,
        fontsize=9.6,
        title="Quadrant guide",
        title_fontsize=10,
    )
    legend.get_frame().set_linewidth(1.1)
    legend_text_color = PROJECT_PALETTE["paper"]
    legend.get_title().set_color(legend_text_color)
    for text in legend.get_texts():
        text.set_color(legend_text_color)

    ax.set_xlim(x_min, x_max)
    ax.set_ylim(y_min, y_max)
    ax.set_xlabel(x_label, fontsize=12, fontweight="semibold")
    ax.set_ylabel(y_label, fontsize=12, fontweight="semibold")
    ax.grid(False)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(axis="x", labelsize=11, length=0, pad=7)
    ax.tick_params(axis="y", labelsize=11, length=0, pad=7)

    fig.suptitle(title, fontsize=22, fontweight="bold", y=0.97)
    fig.text(0.5, 0.92, subtitle, ha="center", va="center", fontsize=10.2, color=PROJECT_PALETTE["steel"])
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def write_summary_05(archetypes: pd.DataFrame, out_path: Path) -> None:
    best = archetypes.loc[archetypes["mean_overperformance"].idxmax()]
    lines = [
        "# Turnovers Analysis 5: Archetypes",
        "",
        f"- Best archetype: `{best['archetype']}` at `{best['mean_overperformance']:+.3f}` overperformance points.",
        "- Archetypes split teams at their exact seed line's average on TOR edge and defensive disruption edge (TORD edge + 0.35 x BLK% edge; the 0.35 weight is ad hoc).",
        "- These are descriptive group means of finish minus exact-seed expectation; no significance test is applied.",
        "",
        "## Archetypes",
        "",
        archetypes.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_06(table: pd.DataFrame, out_path: Path) -> None:
    modern_row = table.loc[table["era"] == "2022-2025"].iloc[0]
    full_row = table.loc[table["era"] == "2008-2025"].iloc[0]
    lines = [
        "# Turnovers Analysis 6: Era Comparison",
        "",
        f"- Full-sample (`2008-2025`) seed-adjusted TOR rho: `{full_row['tor_seed_adjusted_rho']:+.3f}` on `{int(full_row['team_count'])}` teams.",
        f"- `2022-2025` seed-adjusted TOR rho: `{modern_row['tor_seed_adjusted_rho']:+.3f}` on `{int(modern_row['team_count'])}` teams.",
        "- Interpretation: the eras are subsets of the full sample with no significance test; report the era estimates alongside the full-sample one rather than in place of it.",
        "",
        "## Table",
        "",
        table.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_07(table: pd.DataFrame, out_path: Path) -> None:
    work = table.assign(turnover_pair_score=table["full_tor_edge"] + table["full_tord_edge"])
    best = work.nlargest(6, "turnover_pair_score")[["team_display", "projected_seed", "full_tor_edge", "full_tord_edge"]]
    worst = work.nsmallest(6, "turnover_pair_score")[["team_display", "projected_seed", "full_tor_edge", "full_tord_edge"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Turnovers Analysis 7: 2026 TOR x TORD Pair View",
        "",
        "- The March 2026 bracketology projection on one plane: right is a lower turnover rate (TOR) than the seed line's 2008-2025 average, up is a higher forced-turnover rate (TORD).",
        "- `turnover_profile_score` in the table CSV is TOR edge + 0.55 x defensive disruption edge (TORD edge + 0.35 x BLK% edge). The weights are ad hoc, not fitted; the score only orders teams within a seed line.",
        f"- Gold-backed logos in the chart mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        "",
        "## Highest TOR edge + TORD edge",
        "",
        best.round(3).to_markdown(index=False),
        "",
        "## Lowest TOR edge + TORD edge",
        "",
        worst.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def write_summary_07_blocks(table: pd.DataFrame, out_path: Path) -> None:
    work = table.assign(block_pair_score=table["full_blked_pct_edge"] + table["full_blk_pct_edge"])
    best = work.nlargest(6, "block_pair_score")[["team_display", "projected_seed", "full_blked_pct_edge", "full_blk_pct_edge"]]
    worst = work.nsmallest(6, "block_pair_score")[["team_display", "projected_seed", "full_blked_pct_edge", "full_blk_pct_edge"]]
    former_champions = table[table["is_former_champion"]].copy().sort_values(["projected_seed", "barthag"], ascending=[True, False], kind="stable")
    former_champion_text = (
        ", ".join([f"`{row.team_display}` ({int(row.projected_seed)}) - {row.title_history}" for row in former_champions.itertuples(index=False)])
        if not former_champions.empty
        else "none"
    )
    lines = [
        "# Turnovers Analysis 7B: 2026 BLKED% x BLK% Pair View",
        "",
        "- The March 2026 bracketology projection on one plane: right is a lower blocked-shot rate (BLKED%) than the seed line's 2008-2025 average, up is a higher block rate (BLK%).",
        f"- Gold-backed logos in the chart mark former NCAA champions in the projected `{CURRENT_FIELD_YEAR}` field: {former_champion_text}.",
        "",
        "## Highest BLKED% edge + BLK% edge",
        "",
        best.round(3).to_markdown(index=False),
        "",
        "## Lowest BLKED% edge + BLK% edge",
        "",
        worst.round(3).to_markdown(index=False),
    ]
    out_path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    paths = get_paths()
    full_df = load_turnovers_data(paths["input"])
    tournament_df = load_tournament_data(full_df)
    over_df = build_overperformance_data(tournament_df)

    table05 = build_analysis_05_table(over_df)
    table06 = build_analysis_06_table(over_df)
    table07 = build_analysis_07_table(full_df, tournament_df, paths["seeds_2026"], paths["logo_cache"])
    full_ref, _ = build_seed_reference_tables(tournament_df, ["TOR", "TORD", "BLK%", "BLKED%", "AdjT"])
    overperformer_history = build_analysis_07_overperformer_history(tournament_df, full_ref, paths["logo_cache"])
    validate_analysis_07_table(table07)

    table05.round(6).to_csv(paths["csv_05"], index=False)
    table06.round(6).to_csv(paths["csv_06"], index=False)
    with_repo_relative_logo_paths(table07).round(6).to_csv(paths["csv_07"], index=False)

    plot_analysis_05(table05, paths["fig_05"])
    plot_analysis_07_pair(
        table07,
        overperformer_history,
        paths["fig_07"],
        x_col="full_tor_edge",
        y_col="full_tord_edge",
        title="Projected 2026 TOR x TORD profile",
        subtitle="Right means cleaner ball security than the seed baseline. Up means stronger turnover pressure than the seed baseline. Gold-backed badges mark former champs.",
        x_label="TOR edge vs exact-seed baseline",
        y_label="TORD edge vs exact-seed baseline",
        quadrant_labels=[
            (0.24, 0.90, "Loose TOR\nStrong TORD"),
            (0.76, 0.90, "Clean TOR\nStrong TORD"),
            (0.24, 0.10, "Loose TOR\nWeak TORD"),
            (0.76, 0.10, "Clean TOR\nWeak TORD"),
        ],
    )
    plot_analysis_07_pair(
        table07,
        overperformer_history,
        paths["fig_07_blocks"],
        x_col="full_blked_pct_edge",
        y_col="full_blk_pct_edge",
        title="Projected 2026 BLKED% x BLK% profile",
        subtitle="Right means teams get blocked less than the seed baseline. Up means stronger block pressure than the seed baseline. Gold-backed badges mark former champs.",
        x_label="BLKED% edge vs exact-seed baseline",
        y_label="BLK% edge vs exact-seed baseline",
        quadrant_labels=[
            (0.24, 0.90, "Blocked More\nStrong BLK%"),
            (0.76, 0.90, "Blocked Less\nStrong BLK%"),
            (0.24, 0.10, "Blocked More\nWeak BLK%"),
            (0.76, 0.10, "Blocked Less\nWeak BLK%"),
        ],
    )
    write_summary_05(table05, paths["md_05"])
    write_summary_06(table06, paths["md_06"])
    write_summary_07(table07, paths["md_07"])
    write_summary_07_blocks(table07, paths["md_07_blocks"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
