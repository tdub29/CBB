#!/usr/bin/env python3
"""Walk-forward bracket simulation, scored per game against two seed baselines.

For each tournament Y (2021 onward, 2020 has none):
  1. Rebuild the feature-set selection of scripts/run_analysis.py (ridge, season-grouped CV)
     on seasons before Y only.
  2. Fit the Bradley-Terry scale k by maximum likelihood on those seasons' games, using the
     selection's out-of-fold strengths so k is not fit to in-sample predictions.
  3. Score Y's field with score_field(), which deletes every outcome column first, so PASE,
     PAKE and the result cannot reach the simulation directly or through the model.
  4. Simulate Y's real bracket (rebuilt from the games played) with a fixed seed, and pick a
     committed bracket (likelier winner of every game).
  5. Score every game played against deterministic seed chalk and historical seed-pair rates.

Writes reports/bracket_simulation_<report-year>_<tag>.md, data/processed/round_predictions_<Y>_<tag>.csv
and reports/figures/bracket_calibration_<first>_<last>_<tag>.png, <tag> = --out-tag (default: today).
The untagged bracket_simulation_2025.md and round_predictions_<Y>.csv are the 2026-09-29 run, made
before the charter on an earlier training CSV (sha256 8565be043d11155d); a rerun must not overwrite them.

Superseded for evaluation by scripts/run_powered_comparison.py, which walks this model forward
2010-2026 under the pre-registered charter; it imports fit_year_model and the calibration helpers.
"""

from __future__ import annotations

import argparse
import json
import sys
from datetime import date
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.run_analysis import (  # noqa: E402
    OUTCOME_COLUMNS,
    add_engineered_features,
    add_rank_features,
    build_estimator,
    build_feature_specs,
    choose_rank_base_columns,
    evaluate_feature_sets,
    markdown_table,
    run_provenance,
    score_field,
    seed_lookup_predictions,
)
from src.models.simulate import simulate_bracket  # noqa: E402
from src.models.tournament import (  # noqa: E402
    CHALK_CONFIDENCE,
    REACH_COLUMNS,
    ROUND_NAMES,
    bracket_from_games,
    brier_logloss,
    chalk_probability,
    committed_bracket,
    oriented_games,
    score_committed_bracket,
    seed_pair_rates,
)
from src.models.win_probability import fit_bradley_terry_k, p_win  # noqa: E402
from src.visualization.palette import PROJECT_PALETTE, apply_report_background, set_project_theme  # noqa: E402

METHODS = ("model", "seed_pair", "chalk")
LABELS = {"model": "Model (Bradley-Terry)", "seed_pair": "Seed-pair rates", "chalk": "Seed chalk"}
EPS = {"model": 1e-15, "seed_pair": 1e-15, "chalk": 1 - CHALK_CONFIDENCE}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--training-csv", default="data/processed/cbb_training_clean.csv")
    parser.add_argument("--games-csv", default="data/raw/tournament_games.csv")
    parser.add_argument("--first-year", type=int, default=2021)
    parser.add_argument("--report-year", type=int, default=2025)
    parser.add_argument("--n-sims", type=int, default=10000)
    parser.add_argument("--seed", type=int, default=123, help="Simulation RNG seed (configs project.seed).")
    parser.add_argument("--cv-folds", type=int, default=10)
    parser.add_argument("--random-state", type=int, default=42)
    parser.add_argument("--bootstrap", type=int, default=10000)
    parser.add_argument("--out-tag", default=f"{date.today():%Y%m%d}",
                        help="suffix on every output, so a rerun never replaces an earlier run's evidence")
    return parser.parse_args()


def prepare_model_frame(raw: pd.DataFrame) -> tuple[pd.DataFrame, list[str]]:
    """Engineered and within-season rank features, tournament rows only, 2020 dropped."""
    df = add_engineered_features(raw)
    df, rank_cols = add_rank_features(df, choose_rank_base_columns(df))
    tourn = pd.to_numeric(df["seed"], errors="coerce").notna() & pd.to_numeric(df["round"], errors="coerce").notna()
    return df[tourn & (df["year"] != 2020)].copy(), rank_cols


def fit_year_model(year: int, raw: pd.DataFrame, model_df: pd.DataFrame, rank_cols: list[str],
                   games: pd.DataFrame, cv_folds: int, random_state: int) -> dict:
    """Ridge strength model and Bradley-Terry k for tournament `year`, from earlier seasons only.

    Selection, fit and k see rows and games with year < `year`; the season itself is scored by
    score_field(), which deletes every outcome column first.
    """
    train = model_df[model_df["year"] < year]
    specs = build_feature_specs(train, rank_cols=rank_cols)
    results, cache = evaluate_feature_sets(train, specs, cv_folds, random_state, verbose=False)
    chosen = results.iloc[0]
    c = cache[str(chosen["feature_set"])]
    if set(c["features"]) & OUTCOME_COLUMNS:
        raise RuntimeError(f"{year}: outcome columns reached the model: {set(c['features']) & OUTCOME_COLUMNS}")
    est = build_estimator("ridge", {"alpha": c["best_alpha"]}, random_state=random_state)
    est.fit(c["X"], c["y"])
    artifact = {"model": est, "features": c["features"], "poly": c["spec"].poly,
                "feature_set": str(chosen["feature_set"])}

    oof = c["cv"]["oof"]
    keys = train.loc[oof.index, ["year", "Team"]]
    s_oof = dict(zip(zip(keys["year"].astype(int), keys["Team"]), oof))
    hist = games[(games["year"] < year) & ~games["no_contest"]]
    diffs = np.array([s_oof[(y, w)] - s_oof[(y, lo)] for y, w, lo in zip(hist["year"], hist["winner"], hist["loser"])])
    k = fit_bradley_terry_k(diffs)

    season = raw[raw["year"] == year]
    scored = score_field(artifact, season)  # outcome columns are deleted inside
    return {"strength_all": dict(zip(season["Team"], scored)), "k": k, "artifact": artifact,
            "n_features": len(c["features"]), "alpha": float(c["best_alpha"]), "cv_mse": float(chosen["cv_mse"]),
            "train": train, "strength_oof": s_oof}


def walk_forward_year(year: int, raw: pd.DataFrame, model_df: pd.DataFrame, rank_cols: list[str],
                      games: pd.DataFrame, args: argparse.Namespace) -> dict:
    fit = fit_year_model(year, raw, model_df, rank_cols, games, args.cv_folds, args.random_state)
    train, artifact, k, strength_all = fit["train"], fit["artifact"], fit["k"], fit["strength_all"]
    season_games = games[games["year"] == year]
    bracket = bracket_from_games(season_games)
    strength = {t: float(strength_all[t]) for t in bracket["Team"]}
    seed_of = dict(zip(bracket["Team"], bracket["Seed"]))
    pair = seed_pair_rates(games[(games["year"] < year) & ~games["no_contest"]])  # earlier tournaments only

    match = {
        "model": lambda a, b: p_win(strength[a], strength[b], k),
        "seed_pair": lambda a, b: pair(seed_of[a], seed_of[b]),
        "chalk": lambda a, b: chalk_probability(seed_of[a], seed_of[b]),
    }
    sims = {
        "model": simulate_bracket(bracket, strength, n_sims=args.n_sims, k=k, random_state=args.seed),
        "seed_pair": simulate_bracket(bracket, {t: float(s) for t, s in seed_of.items()},
                                      p_win_fn=lambda a, b, _k: pair(a, b), n_sims=args.n_sims,
                                      random_state=args.seed),
        "chalk": simulate_bracket(bracket, {t: float(s) for t, s in seed_of.items()},
                                  p_win_fn=lambda a, b, _k: chalk_probability(a, b), n_sims=args.n_sims,
                                  random_state=args.seed),
    }

    field = model_df[model_df["year"] == year].set_index("Team")
    wins = field["round"].astype(int).to_dict()

    og = oriented_games(season_games)
    for m in METHODS:
        og[f"p_{m}"] = [match[m](a, b) for a, b in zip(og["team_a"], og["team_b"])]

    adv_rows = []
    for m in METHODS:
        sim = sims[m].set_index("Team")
        for team in bracket["Team"]:
            for rnd, col in REACH_COLUMNS.items():
                adv_rows.append({"year": year, "method": m, "round": rnd, "Team": team,
                                 "p": float(sim.at[team, col]), "reached": int(wins[team] >= rnd)})

    picks = {m: committed_bracket(bracket, match[m]) for m in METHODS}
    pick_scores = {m: score_committed_bracket(picks[m], wins) for m in METHODS}

    round_true = field.loc[bracket["Team"], "round"].to_numpy(dtype=float)
    round_model = np.array([strength[t] for t in bracket["Team"]])
    round_seed = seed_lookup_predictions(train, field.loc[bracket["Team"]].reset_index())

    out_csv = bracket.merge(sims["model"], on="Team")
    out_csv.insert(0, "year", year)
    out_csv["strength_pred_wins"] = out_csv["Team"].map(strength)
    out_csv["actual_round"] = out_csv["Team"].map(wins)
    out_csv["committed_pick_wins"] = out_csv["Team"].map(
        lambda t: sum(1 for rnd in picks["model"] for slot in picks["model"][rnd] if slot.get(t, 0) == 1.0))
    out_csv["k_bradley_terry"] = k
    out_csv["feature_set"] = artifact["feature_set"]
    out_csv["sim_seed"] = args.seed
    out_csv["n_sims"] = args.n_sims

    return {
        "year": year, "feature_set": artifact["feature_set"], "n_features": fit["n_features"],
        "alpha": fit["alpha"], "cv_mse": fit["cv_mse"], "k": k,
        "games": og, "adv": pd.DataFrame(adv_rows), "picks": picks, "pick_scores": pick_scores,
        "bracket": bracket, "strength": strength, "seed_of": seed_of, "wins": wins, "sim_model": sims["model"],
        "round_mse_model": float(np.mean((round_model - round_true) ** 2)),
        "round_mse_seed": float(np.mean((round_seed - round_true) ** 2)),
        "round_mae_model": float(np.mean(np.abs(round_model - round_true))),
        "round_mae_seed": float(np.mean(np.abs(round_seed - round_true))),
        "csv": out_csv,
    }


def metric_table(frame: pd.DataFrame, by: str | None, p_prefix: str = "p_", y_col: str = "a_won") -> pd.DataFrame:
    groups = [("all", frame)] if by is None else list(frame.groupby(by))
    rows = []
    for key, g in groups:
        row = {by or "scope": key, "n": len(g)}
        for m in METHODS:
            b, ll = brier_logloss(g[f"{p_prefix}{m}"], g[y_col], eps=EPS[m])
            row[f"brier_{m}"], row[f"logloss_{m}"] = b, ll
        rows.append(row)
    return pd.DataFrame(rows)


def paired_bootstrap(games: pd.DataFrame, other: str, n: int, seed: int) -> tuple[float, float, float]:
    """Mean and 95% interval of Brier(model) - Brier(other), resampling games."""
    rng = np.random.default_rng(seed)
    y = games["a_won"].to_numpy(float)
    d = (games["p_model"].to_numpy() - y) ** 2 - (games[f"p_{other}"].to_numpy() - y) ** 2
    boots = d[rng.integers(0, len(d), size=(n, len(d)))].mean(axis=1)
    return float(d.mean()), float(np.quantile(boots, 0.025)), float(np.quantile(boots, 0.975))


def wilson(k: float, n: int, z: float = 1.96) -> tuple[float, float]:
    if n == 0:
        return (np.nan, np.nan)
    p = k / n
    centre = (p + z * z / (2 * n)) / (1 + z * z / n)
    half = z * np.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n)
    return (centre - half, centre + half)


def calibration_table(games: pd.DataFrame, method: str) -> pd.DataFrame:
    p = games[f"p_{method}"].to_numpy()
    y = games["a_won"].to_numpy()
    bins = np.minimum((p * 10).astype(int), 9)
    rows = []
    for b in range(10):
        mask = bins == b
        n = int(mask.sum())
        lo, hi = wilson(float(y[mask].sum()), n)
        rows.append({"bin": f"{b / 10:.1f}-{(b + 1) / 10:.1f}", "n": n,
                     "mean_pred": float(p[mask].mean()) if n else np.nan,
                     "observed": float(y[mask].mean()) if n else np.nan, "ci_low": lo, "ci_high": hi})
    return pd.DataFrame(rows)


def expected_calibration_error(table: pd.DataFrame) -> float:
    t = table[table["n"] > 0]
    return float((t["n"] * (t["observed"] - t["mean_pred"]).abs()).sum() / t["n"].sum())


def save_calibration_plot(tables: dict[str, pd.DataFrame], path: Path, title: str,
                          labels: dict[str, str] | None = None) -> None:
    fig, ax = plt.subplots(figsize=(8, 7))
    apply_report_background(fig)
    ax.plot([0, 1], [0, 1], linestyle="--", color=PROJECT_PALETTE["sage"], linewidth=1.5, label="Perfect calibration")
    colors = {"model": PROJECT_PALETTE["sky_blue"], "seed_pair": PROJECT_PALETTE["slate"]}  # readable on the dark theme
    offsets = {"model": -0.008, "seed_pair": 0.008}
    for m, t in tables.items():
        t = t[t["n"] > 0]
        x = t["mean_pred"] + offsets[m]
        ax.errorbar(x, t["observed"], yerr=[(t["observed"] - t["ci_low"]).clip(lower=0), (t["ci_high"] - t["observed"]).clip(lower=0)],
                    fmt="o", color=colors[m], capsize=3, label=f"{(labels or LABELS)[m]} (95% Wilson CI)")
        if m == "model":
            for xi, yi, n in zip(x, t["observed"], t["n"]):
                ax.annotate(f"n={n}", (xi, yi), textcoords="offset points", xytext=(6, -12), fontsize=8,
                            color=PROJECT_PALETTE["fog"])
    ax.set_xlim(0, 1)
    ax.set_ylim(-0.02, 1.02)
    ax.set_xlabel("Predicted P(better seed wins), decile bins")
    ax.set_ylabel("Observed win rate")
    ax.set_title(title)
    ax.legend(loc="upper left", fontsize=9)
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def main() -> int:
    args = parse_args()
    set_project_theme()
    training_csv = REPO_ROOT / args.training_csv
    games_csv = REPO_ROOT / args.games_csv
    raw = pd.read_csv(training_csv)
    games = pd.read_csv(games_csv)
    games["no_contest"] = games["no_contest"].astype(bool)
    manifest_path = games_csv.with_name(games_csv.stem + "_manifest.json")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8")) if manifest_path.exists() else {}

    model_df, rank_cols = prepare_model_frame(raw)

    processed_years = set(model_df["year"].astype(int))
    years = sorted(y for y in processed_years & set(games["year"]) if y >= args.first_year)
    missing_games = sorted(y for y in processed_years if y >= args.first_year and y not in set(games["year"]))
    if len(years) < 5:
        raise SystemExit(f"need at least 5 walk-forward tournaments, have {years}")

    results = []
    for year in years:
        print(f"[WalkForward] {year}", flush=True)
        results.append(walk_forward_year(year, raw, model_df, rank_cols, games, args))

    processed_dir = REPO_ROOT / "data/processed"
    for r in results:
        r["csv"].to_csv(processed_dir / f"round_predictions_{r['year']}_{args.out_tag}.csv", index=False)

    all_games = pd.concat([r["games"] for r in results], ignore_index=True)
    all_adv = pd.concat([r["adv"] for r in results], ignore_index=True)
    adv_wide = all_adv.pivot_table(index=["year", "round", "Team", "reached"], columns="method", values="p").reset_index()
    adv_wide.columns = [f"p_{c}" if c in METHODS else c for c in adv_wide.columns]

    by_round = metric_table(all_games, "round")
    by_round.insert(1, "round_name", by_round["round"].map(ROUND_NAMES))
    overall = metric_table(all_games, None)
    by_year = metric_table(all_games, "year")
    adv_by_round = metric_table(adv_wide, "round", y_col="reached")
    adv_by_round.insert(1, "round_reached", adv_by_round["round"].map(
        {1: "R32", 2: "S16", 3: "E8", 4: "F4", 5: "Final", 6: "Champion"}))
    boot = {m: paired_bootstrap(all_games, m, args.bootstrap, args.seed) for m in ("seed_pair", "chalk")}

    cal = {m: calibration_table(all_games, m) for m in ("model", "seed_pair")}
    ece = {m: expected_calibration_error(t) for m, t in cal.items()}
    first, last = years[0], years[-1]
    fig_path = REPO_ROOT / f"reports/figures/bracket_calibration_{first}_{last}_{args.out_tag}.png"
    save_calibration_plot(cal, fig_path, f"Game-level calibration, walk-forward {first}-{last} ({len(all_games)} games)")

    picks_rows = []
    for r in results:
        for m in METHODS:
            s = r["pick_scores"][m]
            picks_rows.append({"year": r["year"], "method": LABELS[m], "final_four_hits": s["final_four_hits"],
                               "champion_hit": s["champion_hit"], "correct_R64": s["correct_R64"],
                               "espn_points": s["espn_points"]})
    picks_df = pd.DataFrame(picks_rows)
    picks_total = picks_df.groupby("method", sort=False)[["final_four_hits", "champion_hit", "correct_R64",
                                                          "espn_points"]].sum().reset_index()

    stab = pd.DataFrame([{"year": r["year"], "train_seasons": f"2008-{r['year'] - 1}" + (" (no 2020)" if r["year"] > 2020 else ""), "feature_set": r["feature_set"],
                          "n_features": r["n_features"], "ridge_alpha": r["alpha"], "cv_mse": r["cv_mse"],
                          "k": r["k"]} for r in results])
    set_changes = int((stab["feature_set"] != stab["feature_set"].shift()).sum() - 1)
    k_ratio = float(stab["k"].max() / stab["k"].min())
    gate_pass = set_changes <= 1 and k_ratio <= 1.5

    round_rows = pd.DataFrame([{"year": r["year"], "model_mse": r["round_mse_model"], "seed_lookup_mse": r["round_mse_seed"],
                                "model_mae": r["round_mae_model"], "seed_lookup_mae": r["round_mae_seed"]}
                               for r in results])
    round_rows = pd.concat([round_rows.astype({"year": object}),
                            round_rows.drop(columns="year").mean().to_frame().T.assign(year="mean")],
                           ignore_index=True)

    rep = next(r for r in results if r["year"] == args.report_year)
    rep_picks = rep["picks"]["model"]
    pick_lines = []
    for rnd, slots in rep_picks.items():
        for slot in slots:
            team = max(slot, key=slot.get)
            pick_lines.append({"round": ROUND_NAMES[rnd], "pick": team, "seed": rep["seed_of"][team],
                               "region": rep["bracket"].set_index("Team").at[team, "Region"],
                               "won": "yes" if rep["wins"][team] >= rnd else "no"})
    pick_df = pd.DataFrame(pick_lines)
    one_seeds = rep["bracket"][rep["bracket"]["Seed"] == 1].set_index("Region")["Team"].to_dict()
    pick_df["region"] = pick_df["region"].map(lambda g: f"{g} ({one_seeds[g]} region)")
    top_champ = rep["sim_model"].sort_values("p_Champ", ascending=False).head(8)[
        ["Team", "p_F4", "p_Finals", "p_Champ", "expected_round"]]
    top_champ.insert(1, "Seed", top_champ["Team"].map(rep["seed_of"]))
    top_champ["actual_wins"] = top_champ["Team"].map(rep["wins"])
    champion = next(t for t, w in rep["wins"].items() if w == 6)
    final_four = [t for t, w in rep["wins"].items() if w >= 4]
    model_champ = max(rep_picks[6][0], key=rep_picks[6][0].get)
    model_f4 = [max(s, key=s.get) for s in rep_picks[4]]

    prov = run_provenance(training_csv)
    o = overall.iloc[0]
    tot = picks_total.set_index("method")
    lines: list[str] = []
    lines.append(f"# Bracket Simulation {args.report_year} (walk-forward {first}-{last})")
    lines.append("")
    lines.append("## Headline")
    lines.append(
        f"- Over {len(all_games)} tournament games in {len(years)} walked-forward tournaments ({first}-{last}), "
        f"the model's game-level Brier score is {o['brier_model']:.4f} (log loss {o['logloss_model']:.4f}) "
        f"against {o['brier_seed_pair']:.4f} ({o['logloss_seed_pair']:.4f}) for historical seed-pair win rates "
        f"and {o['brier_chalk']:.4f} ({o['logloss_chalk']:.4f}) for deterministic seed chalk."
    )
    for m in ("seed_pair", "chalk"):
        mean, lo, hi = boot[m]
        verdict = "better" if hi < 0 else ("worse" if lo > 0 else "not distinguishable")
        lines.append(f"- Brier(model) minus Brier({LABELS[m].lower()}) = {mean:+.4f}, 95% paired bootstrap "
                     f"interval [{lo:+.4f}, {hi:+.4f}]: the model is {verdict} at this sample size.")
    lines.append(
        f"- Committed brackets, {first}-{last}: Final Four hits model {tot.at[LABELS['model'], 'final_four_hits']:g}/"
        f"{4 * len(years)}, seed-pair {tot.at[LABELS['seed_pair'], 'final_four_hits']:g}, chalk "
        f"{tot.at[LABELS['chalk'], 'final_four_hits']:g}; champion hits model "
        f"{tot.at[LABELS['model'], 'champion_hit']:g}/{len(years)}, seed-pair {tot.at[LABELS['seed_pair'], 'champion_hit']:g}, "
        f"chalk {tot.at[LABELS['chalk'], 'champion_hit']:g} (chalk ties between 1 seeds count as a coin flip)."
    )
    lines.append(
        f"- {args.report_year} committed bracket: champion pick **{model_champ}** (actual champion {champion}); "
        f"Final Four picks {', '.join(model_f4)} (actual {', '.join(sorted(final_four))})."
    )
    lines.append(f"- Calibration error (ECE, decile bins): model {ece['model']:.4f}, seed-pair {ece['seed_pair']:.4f}.")
    lines.append(f"- Deterministic: simulation seed `{args.seed}`, `{args.n_sims:,}` simulations per bracket; "
                 f"the seed is also written into every `round_predictions_<year>_<tag>.csv`.")
    lines.append(f"- Stability gate (Build-Rebuild-and-Compare): **{'PASS' if gate_pass else 'FAIL'}** "
                 f"({set_changes} feature-set change(s) across builds, k max/min {k_ratio:.2f}).")
    lines.append("")
    lines.append("## Run Configuration")
    lines.append("- command: `python scripts/run_bracket_evaluation.py`")
    lines.append(f"- code_revision: `{prov['code_revision']}`; training data `{args.training_csv}` "
                 f"(sha256 prefix `{prov['input_sha256']}`)")
    lines.append(f"- games: `{args.games_csv}` from `{manifest.get('source', '?')}`, retrieved "
                 f"{manifest.get('retrieved_at', '?')}, sha256 prefix `{str(manifest.get('sha256', '?'))[:16]}`")
    lines.append(f"- walk-forward tournaments: {years} (2020 had no tournament). "
                 + (f"Seasons with rows but no game results yet: {missing_games}." if missing_games
                    else ("" if 2026 in processed_years
                          else "No 2026 rows exist in the processed training data at run time, so 2026 is not scored.")))
    lines.append(f"- simulation seed: `{args.seed}`; n_sims: `{args.n_sims}`; bootstrap resamples: `{args.bootstrap}`")
    lines.append("")
    lines.append("## Method")
    lines.append("- Prediction moment: the published bracket. The simulation starts at the Round of 64 with the "
                 "First Four already played; First Four games are not scored.")
    lines.append("- Team strength = predicted tournament wins from ridge on the feature set chosen by season-grouped "
                 "CV on seasons before the tournament (`scripts/run_analysis.py`). The field is scored by "
                 "`score_field()`, which deletes every outcome column (PASE, PAKE, round, result, finish and the "
                 "rest of `OUTCOME_COLUMNS`) before building features; each build also asserts that no outcome "
                 "column is in the selected feature list.")
    lines.append("- P(A beats B) = 1 / (1 + exp(-k (strength_A - strength_B))). k is fit by maximum likelihood on "
                 "earlier tournaments' games using out-of-fold strengths.")
    lines.append("- Seed-pair baseline: P(better seed wins) from every earlier tournament game between the same two "
                 "seeds, Laplace-smoothed ((wins + 1) / (games + 2)); equal seeds 0.5.")
    lines.append(f"- Seed chalk: the better seed wins with certainty (Brier uses 1/0). For log loss a chalk pick is "
                 f"treated as {CHALK_CONFIDENCE:.0%} confident, because a hard pick has infinite log loss on any "
                 f"upset; a more confident chalk only scores worse.")
    lines.append("- Game-level scores condition on the matchups that happened. The advancement table scores the "
                 "pre-tournament simulated probability of reaching each round for all 64 teams.")
    lines.append("- Committed bracket: the likelier winner of every game (by that method's head-to-head "
                 "probability), filled from the Round of 64. Exact 50/50 picks are split, so hits are expected values.")
    lines.append("- 2021 Oregon-VCU (no-contest, COVID) advances Oregon in the bracket and is not scored.")
    lines.append("")
    lines.append(f"## Game-level Brier and log loss by round (pooled {first}-{last})")
    overall_row = overall.rename(columns={"scope": "round"}).assign(round_name="all")
    lines.append(markdown_table(pd.concat([by_round, overall_row[by_round.columns]], ignore_index=True),
                                float_digits=4))
    lines.append("")
    lines.append("### By tournament")
    lines.append(markdown_table(by_year, float_digits=4))
    lines.append("")
    lines.append("## Pre-tournament advancement probabilities by round (64 teams per tournament)")
    lines.append(markdown_table(adv_by_round, float_digits=4))
    lines.append("")
    lines.append("## Calibration")
    lines.append(f"![Game-level calibration](figures/{fig_path.name})")
    lines.append("")
    lines.append("Each game is oriented to the better seed (equal seeds: alphabetical), so the x axis is "
                 "P(better seed wins). Intervals are 95% Wilson intervals; late-round bins are thin.")
    lines.append("")
    lines.append("### Model")
    lines.append(markdown_table(cal["model"], float_digits=3))
    lines.append("")
    lines.append("### Seed-pair rates")
    lines.append(markdown_table(cal["seed_pair"], float_digits=3))
    lines.append("")
    lines.append("## Committed-bracket hits")
    lines.append(markdown_table(picks_df, float_digits=2))
    lines.append("")
    lines.append("### Totals")
    lines.append(markdown_table(picks_total, float_digits=2))
    lines.append("")
    lines.append(f"## Committed {args.report_year} bracket (model)")
    lines.append(f"Regions are rebuilt from the games and named by their 1 seed; national semifinal pairings are "
                 f"A-B and C-D. Strengths from the model trained on 2008-{args.report_year - 1}, "
                 f"k = {rep['k']:.3f}.")
    lines.append("")
    lines.append(markdown_table(pick_df))
    lines.append("")
    lines.append("### Title probabilities, top 8 (simulated)")
    lines.append(markdown_table(top_champ.reset_index(drop=True), float_digits=3))
    lines.append("")
    lines.append("## Round prediction walked forward (MSE and MAE on 0-6 wins)")
    lines.append("Model strength as predicted wins against the historical mean wins for the same seed, both fit "
                 "on seasons before the tournament.")
    lines.append("")
    lines.append(markdown_table(round_rows, float_digits=4))
    lines.append("")
    lines.append("## Stability gate (Build-Rebuild-and-Compare)")
    lines.append("Each tournament's build repeats the same selection on earlier seasons only. Rule: pass if the "
                 "selected feature set changes at most once and k stays within a 1.5x band; nothing was re-tuned "
                 "after seeing a held-out tournament.")
    lines.append("")
    lines.append(markdown_table(stab, float_digits=4))
    lines.append("")
    lines.append("## Caveats")
    lines.append(f"- {len(years)} tournaments is a small sample: {int((all_games['round'] >= 5).sum())} Final Four "
                 "and title games in total, so late-round and champion comparisons are anecdotes, not evidence.")
    lines.append("- The bracket is scored from the Round of 64; First Four uncertainty is not modeled.")
    lines.append("- Strength comes from a regression on tournament wins, a noisy target; a matchup model fit "
                 "directly on game outcomes is the natural next candidate.")

    report_path = REPO_ROOT / f"reports/bracket_simulation_{args.report_year}_{args.out_tag}.md"
    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"Report written: {report_path}")
    print(f"Overall Brier model {o['brier_model']:.4f} | seed-pair {o['brier_seed_pair']:.4f} | chalk {o['brier_chalk']:.4f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
