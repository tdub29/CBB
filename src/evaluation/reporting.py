"""Run reports: the model optimization Markdown report and the bracket simulation summary."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pandas as pd


def _build_narrative_summary(
    round_predictions: pd.DataFrame,
    n_sims: int,
    k_bradley_terry: float,
    bracket_path: str,
) -> str:
    """Create a concise model summary for the simulation output."""
    ordered = round_predictions.sort_values("p_Champ", ascending=False).reset_index(drop=True)
    top4_share = ordered["p_Champ"].head(4).sum()
    top4_lines = [
        f"{row.Team} ({row.p_Champ:.1%})"
        for row in ordered.head(4).itertuples(index=False)
    ]
    upset_pool = round_predictions[pd.to_numeric(round_predictions["Seed"], errors="coerce") >= 10]
    upset_line = "N/A"
    if not upset_pool.empty:
        upset = upset_pool.sort_values("p_S16", ascending=False).iloc[0]
        upset_line = f"{upset['Team']} (seed {int(upset['Seed'])}) with {upset['p_S16']:.1%} to reach S16"

    lines = [
        f"Summary: Top 4 title probabilities account for {top4_share:.1%} of simulated championships.",
        f"Context: bracket={bracket_path}, n_sims={n_sims:,}, k={k_bradley_terry:.2f}.",
        f"Top title contenders: {', '.join(top4_lines)}.",
        f"Highest S16 odds among seeds 10+: {upset_line}.",
        "Scenario testing: rerun with --bracket PATH and adjusted --n-sims/--k.",
    ]
    return "\n".join(lines)


def _md_table(headers: list[str], rows: list[list[str]]) -> str:
    """Render a simple GitHub-flavored Markdown table."""
    header_line = "| " + " | ".join(headers) + " |"
    sep_line = "| " + " | ".join(["---"] * len(headers)) + " |"
    body = ["| " + " | ".join(r) + " |" for r in rows]
    return "\n".join([header_line, sep_line] + body)


def _write_optimization_report(
    report_path: Path,
    *,
    run_tag: str,
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
    runtime_cfg: dict[str, Any],
    gen_feats: list[str],
    gen_meta: dict[str, Any],
    seed_meta: dict[int, dict[str, Any]],
    round_predictions_path: str | None,
    bracket_path: str,
) -> None:
    """Write a Markdown report showing per-model optimization evidence."""
    lines: list[str] = []
    lines.append(f"# Model Optimization Report ({run_tag})")
    lines.append("")
    lines.append("## How To Tell Models Are Optimized")
    lines.append("- Each model runs a Ridge `alpha` grid search and picks the best `alpha` by season-grouped CV MSE.")
    lines.append("- Folds keep each season whole (leave-one-season-out): one season is one bracket, so")
    lines.append("  teams from the same tournament never sit on both sides of a split.")
    lines.append("- The report shows baseline vs best CV MSE (lower is better) per model.")
    lines.append("- With tuning on, the reported MSE is leave-one-season-out MSE at the selected `alpha` on the")
    lines.append("  same splits that picked it, so it is the grid minimum and optimistic by that selection.")
    lines.append("- Seed models train on out-of-fold `predround` (general model fit without that season).")
    lines.append("- These numbers are for model selection inside the training years. Out-of-sample accuracy")
    lines.append("  against seed-only baselines (walk-forward + a held-out season) is in")
    lines.append("  `reports/predictive_analysis_*.md`, written by `scripts/run_analysis.py`.")
    lines.append("")
    lines.append("## Run Context")
    lines.append(f"- Training years: {year_start}-{year_end} (excluded: {', '.join(map(str, exclude_years)) or 'none'})")
    lines.append(f"- Ridge alpha tuning: {runtime_cfg['optimize_ridge_alpha']}")
    lines.append(f"- Seed-stratified models: {runtime_cfg['use_seed_models']}")
    lines.append(f"- Alpha grid: {runtime_cfg['ridge_alpha_grid']}")
    lines.append(f"- Bradley-Terry k: {runtime_cfg['k_bradley_terry']}")
    lines.append(f"- Monte Carlo sims: {runtime_cfg['n_sims']:,}")
    lines.append(f"- Bracket CSV: {bracket_path or '(none)'}")
    if round_predictions_path:
        lines.append(f"- Round predictions: {round_predictions_path}")
    lines.append("")

    lines.append("## General Model")
    lines.append(f"- Selected `alpha`: {gen_meta.get('ridge_alpha')}")
    tuning = gen_meta.get("tuning", {}) or {}
    mse_note = " at the selected `alpha` (grid minimum)" if tuning.get("tuned") else ""
    lines.append(f"- Leave-one-season-out MSE{mse_note}: {gen_meta.get('mse'):.6f}")
    lines.append(f"- Features ({len(gen_feats)}): {', '.join(gen_feats)}")
    if tuning.get("tuned") and tuning.get("results"):
        cv_label = tuning.get("cv") or "CV"
        rows = []
        for r in tuning["results"]:
            a = r["alpha"]
            mean_mse = r["mean_mse"]
            std_mse = r["std_mse"]
            marker = " (selected)" if float(a) == float(gen_meta.get("ridge_alpha")) else ""
            rows.append([f"{a:g}{marker}", f"{mean_mse:.6f}", f"{std_mse:.6f}", cv_label])
        lines.append("")
        lines.append("Tuning results (mean CV MSE):")
        lines.append(_md_table(["alpha", "mean_mse", "std_mse", "cv"], rows))
    else:
        reason = tuning.get("reason")
        if reason:
            lines.append(f"- Tuning skipped: {reason}")
    lines.append("")

    lines.append("## Seed Models (Summary)")
    if not runtime_cfg["use_seed_models"]:
        lines.append("- Disabled (`model.use_seed_models: false`); `predround` comes from the general model alone.")
    elif not seed_meta:
        lines.append("- No seed models trained (insufficient rows).")
    else:
        rows = []
        for model_id in sorted(seed_meta):
            m = seed_meta[model_id]
            tune = m.get("tuning", {}) or {}
            baseline = ""
            best = ""
            cv_label = tune.get("cv") or ""
            results = tune.get("results") or []
            if results:
                baseline_row = next((r for r in results if float(r["alpha"]) == 0.0), None)
                best_row = min(results, key=lambda r: r["mean_mse"])
                baseline = f"{baseline_row['mean_mse']:.6f}" if baseline_row else ""
                best = f"{best_row['mean_mse']:.6f}" if best_row else ""
            rows.append(
                [
                    str(model_id),
                    ",".join(map(str, m["hist_seeds"])),
                    ",".join(map(str, m["holdout_seeds"])),
                    str(m["n_train"]),
                    f"{m['ridge_alpha']:g}",
                    f"{m['season_cv_mse']:.6f}",
                    baseline,
                    best,
                    cv_label,
                ]
            )
        lines.append(
            _md_table(
                [
                    "model_id",
                    "train_seeds",
                    "apply_seeds",
                    "n_train",
                    "alpha",
                    "season_cv_mse",
                    "cv_mse_alpha0",
                    "cv_mse_best",
                    "cv",
                ],
                rows,
            )
        )
    lines.append("")
    lines.append("## Notes")
    lines.append("- `cv_mse_alpha0` is the baseline (OLS) mean CV MSE when `alpha=0` is in the grid.")
    lines.append("- `season_cv_mse` is leave-one-season-out MSE at the selected `alpha`. It is for picking")
    lines.append("  `alpha`, not an accuracy claim: the out-of-fold `predround` column is built once over all")
    lines.append("  seasons, so when season s is held out here, the training rows' `predround` came from general")
    lines.append("  models that saw s. Walk-forward scores in `reports/predictive_analysis_*.md` do not have this.")
    lines.append("- If you want to tune `k`, you need historical bracket + actual outcomes for calibration.")

    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
