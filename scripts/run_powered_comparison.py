#!/usr/bin/env python3
"""Powered walk-forward test: the model against historical seed-pair rates, every tournament 2010-2026.

Run the two stages in order:

  select  Walk forward over 2010-2019 ONLY. Score a fixed list of game-level candidates, pick the
          lowest pooled Brier, log every variant, and freeze the choice to
          reports/model_selection_frozen.json. Refuses to overwrite an existing frozen file.
  score   Needs the frozen file. (1) The existing ridge + Bradley-Terry model against seed-pair
          rates and chalk on every tournament 2010-2026 (2020 had none): paired per-game losses,
          year-block bootstrap, Diebold-Mariano test, minimum detectable effect. (2) The frozen
          model on 2021-2026, scored once. (3) Path-aware round probabilities: every method's
          head-to-head probabilities pushed through the real bracket exactly, so each team's
          chance of reaching a round sums over the opponents it could actually meet. (4) T-Rank's
          archived pregame line as an external reference. Writes reports/model_vs_seeds_powered.md.

Every prediction for tournament Y is fit on seasons before Y; seed-pair rates use every
tournament from 1985 to Y-1. Team inputs are pre-tournament columns only (prediction-time
contract of scripts/run_analysis.py), so PASE, PAKE, round, finish and results never reach them.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

import numpy as np
import pandas as pd
from scipy import stats
from scipy.optimize import minimize_scalar
from scipy.special import expit
from sklearn.linear_model import LinearRegression, LogisticRegression, Ridge
from sklearn.model_selection import KFold, LeaveOneOut, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.run_analysis import OUTCOME_COLUMNS, assert_prediction_time_contract, markdown_table, run_provenance  # noqa: E402
from scripts.run_bracket_evaluation import (  # noqa: E402
    calibration_table,
    expected_calibration_error,
    fit_year_model,
    prepare_model_frame,
    save_calibration_plot,
)
from src.models.tournament import BRACKET_ORDER, ROUND_NAMES, bracket_from_games, oriented_games, seed_pair_rates  # noqa: E402
from src.models.win_probability import fit_bradley_terry_k, p_win  # noqa: E402
from src.pipelines import madness as mad  # noqa: E402
from src.visualization.palette import set_project_theme  # noqa: E402

SELECT_YEARS = tuple(range(2010, 2020))
TEST_YEARS = (2021, 2022, 2023, 2024, 2025, 2026)
EVAL_YEARS = SELECT_YEARS + TEST_YEARS
FROZEN_PATH = REPO_ROOT / "reports/model_selection_frozen.json"
REPORT_PATH = REPO_ROOT / "reports/model_vs_seeds_powered.md"
FIG_PATH = REPO_ROOT / "reports/figures/frozen_model_calibration_2021_2026.png"
BOOT_SEED = 123  # configs project.seed; also the bootstrap seed
N_BOOT = 10000
# Practically meaningful edge, fixed before any score in this file was computed: 0.005 Brier is
# about 2.5% of the seed-pair Brier (0.19) and the size of the old 2021-2025 point estimate
# (-0.0049), so a test that cannot see it cannot confirm what was already observed. Log loss runs
# about 3x Brier on these games, hence 0.015.
MEANINGFUL = {"brier": 0.005, "logloss": 0.015}
CHALK_CAPS = {"chalk70": 0.70, "chalk99": 0.99}

TEAM_COLS = ["seed", "AdjOE", "AdjDE", "Barthag", "Adj T."]  # Torvik type=R table + bracket seed
# Headline metric of each of the nine seed-controlled topic analyses (reports/*_metric_selection.csv,
# scripts/topic_inference.py): SOS, coach experience, size, player experience, turnovers,
# three-point, rebounding, tempo, free-throw rate. Entered as team a minus team b.
TOPIC_COLS = ["ELITESOS", "coach_tourn_games_5y", "EFFHGT", "EXP.", "TOR", "3P%", "DRB", "Adj T.", "FTR"]
TOPIC_FEATURES = [f"topic_{c}" for c in TOPIC_COLS]
LOGIT_FEATURES = ["em_diff", "tempo_margin", "log5_logit", "seed_diff"]
SCALAR_BASES = {"em_logistic": "em_diff", "log5_scaled": "log5_logit", "tempo_margin": "tempo_margin"}
LOGIT_BASES = {"logit_diffs": LOGIT_FEATURES, "logit_topics": LOGIT_FEATURES + TOPIC_FEATURES}
STRENGTH_BASES = ("ridge_bt", "madness_bt")
C_GRID = (0.01, 0.1, 1.0, 10.0)
BASES = ([{"base": "ridge_bt"}, {"base": "madness_bt"}, {"base": "log5"}, {"base": "log5_scaled"},
          {"base": "em_logistic"}, {"base": "tempo_margin"}]
         + [{"base": b, "C": c} for b in LOGIT_BASES for c in C_GRID])
CANDIDATES = [{"base": "seed_pair"}] + BASES + [{**b, "blend": True} for b in BASES]
BASE_TEXT = {
    "seed_pair": "historical seed-pair win rates (the baseline itself)",
    "ridge_bt": "existing ridge on tournament wins, Bradley-Terry on predicted-wins difference",
    "madness_bt": "Trevor's prior family (src/pipelines/madness.py, port of madness.R): general ridge plus 14 "
                  "seed-stratified ridge models on tournament wins, intercept restored, Bradley-Terry on the difference",
    "logit_topics": "L2 logistic, no intercept, on the four core differences plus the nine topic-analysis "
                    "headline metrics (SOS, coach, size, experience, turnovers, 3P%, DRB, tempo, FTR) as differences",
    "log5": "Barthag log5, no fitted parameter",
    "log5_scaled": "Barthag log5 with a fitted temperature: logistic on logit(Barthag_a) - logit(Barthag_b)",
    "em_logistic": "logistic on efficiency-margin difference (AdjOE - AdjDE, team a minus team b)",
    "tempo_margin": "logistic on tempo-adjusted expected margin: EM difference x (tempo_a x tempo_b / D1 mean tempo) / 100",
    "logit_diffs": "L2 logistic, no intercept, on EM difference, tempo-adjusted margin, Barthag logit difference, seed difference",
}


CHARTER = {
    "question": "Do pre-tournament win probabilities beat historical seed-pair win rates on NCAA men's "
                "tournament games, and by how much, with enough power to answer either way?",
    "grain": "one row per tournament game from the Round of 64 on, oriented to the better seed "
             "(equal seeds: alphabetical); First Four excluded; 2021 Oregon-VCU no-contest excluded",
    "decision_time": "the published bracket on Selection Sunday, before any tournament game",
    "target": "a_won = 1 if the better-seeded team won",
    "inputs": "Torvik regular-season team table (type R) and bracket seed only; outcome columns (PASE, PAKE, "
              "round, finish, postseason record) are excluded by the prediction-time contract",
    "baselines": "null coin flip 0.5; constant = training-fold better-seed win rate; chalk at 0.70 and 0.99; "
                 "historical seed-pair rates 1985 to Y-1 (the baseline that matters); external: T-Rank archived pregame line",
    "primary_metric": "Brier score (0-1 scale), lower is better; secondary: log loss",
    "selection": "walk-forward 2010-2019 only; lowest pooled Brier among model candidates (seed-pair logged as reference, "
                 "never frozen), log loss breaks ties; every variant logged",
    "test": "walk-forward 2021-2026, scored once for the single frozen candidate",
    "uncertainty": "paired per-game loss difference; year-block bootstrap 95% CI (10,000 resamples, seed 123); "
                   "Diebold-Mariano t with year-clustered variance; n_eff cross-check",
    "acceptance": "the frozen model beats seed-pair rates if the 2021-2026 year-block 95% CI on Brier(frozen) - "
                  "Brier(seed-pair) lies entirely below 0; otherwise report no detectable edge with the MDE",
    "meaningful_edge": MEANINGFUL,
    "allowed_reruns": "none of the selection after the frozen file exists; the score stage is deterministic",
}


PRIOR_SOURCES = [
    ("cbb/ nine topic analyses (reports/*_metric_selection.csv, scripts/topic_inference.py)", "used",
     "each topic's headline metric entered as a team a minus team b difference in the logit_topics candidates"),
    ("cbb/src/pipelines/madness.py (port of madness.R)", "used",
     "candidate madness_bt and a comparison in every table; intercept restored, see Section 6"),
    ("cbb/src/pipelines/madness.R", "not run", "the Python port carries the same general + 14 SEED_MODELS design"),
    ("cbb/scripts/run_analysis.py, run_bracket_evaluation.py", "used", "the existing model, walked forward 2010-2026"),
    ("OneDrive Python Scripts/updated_25marchmadnesshistoricwithpred.csv", "used",
     "Trevor's 2025 forecasts (saved 2025-03-19, before the Round of 64), scored on the 2025 field"),
    ("OneDrive Python Scripts/25marchmadnesshistoricwithpred.csv", "skipped",
     "the 2025-03-17 save of the same forecasts; the 03-19 file is the last one before the Round of 64"),
    ("OneDrive Python Scripts/marchmadnesshistoricaldata.csv", "skipped",
     "2008-2024 only: no season before 2008, so no extra walk-forward years"),
    ("OneDrive Python Scripts/marchmadnesshistoricwithpred.csv", "skipped",
     "in-sample: models fit on 2008-2024 with random splits, then applied to the same rows; carries PASE/PAKE"),
    ("OneDrive Python Scripts/newmadness.ipynb", "ideas reviewed",
     "seed-group regressions are covered by madness_bt; per-round classifiers are not path-aware; tree "
     "ensembles on ~1,000 team rows break the 20-rows-per-parameter rule; WAB is not in the training data"),
    ("OneDrive Documents/madness24model.csv", "used",
     "Trevor's 2024 madness.R forecast (saved 2024-03-18, before the First Four), scored on the 2024 field; "
     "copy and manifest in data/external/"),
    ("OneDrive Documents/historiccbb.csv", "skipped",
     "896 rows, 2008-2022 (no 2020), 64 tournament teams a season: seasons the training table already covers"),
    ("OneDrive cbball.R, cbbworksp.RData, cbbtrainingdata31224.csv, cbb24foranalysis.csv, madness24model.xlsx, "
     "cbbxl.xlsx, Excel_csv_Files/20260224transfer/*", "unreadable",
     "cloud-only placeholders; every read on 2026-09-29 failed with 'The cloud file provider is not running'"),
    ("OneDrive 20260224transfer/CBBData/CBB (older repo copy)", "reviewed, skipped",
     "notebooks/ holds only .gitkeep; playground/ is generic playground templates with no CBB work; src and "
     "scripts are older copies of this repo's files; its data/external CSVs are cloud-only"),
]


INPUTS_BY_BASE = {"log5": ["Barthag"], "log5_scaled": ["Barthag"], "em_logistic": ["AdjOE", "AdjDE"],
                  "tempo_margin": ["AdjOE", "AdjDE", "Adj T."],
                  "logit_diffs": ["AdjOE", "AdjDE", "Adj T.", "Barthag", "seed"],
                  "logit_topics": list(dict.fromkeys(TEAM_COLS + TOPIC_COLS))}
SHORT_NAME = {"log5": "Torvik's regular-season Barthag rating turned into win probabilities with log5",
              "log5_scaled": "temperature-scaled Barthag log5", "em_logistic": "logistic on efficiency margin",
              "tempo_margin": "logistic on tempo-adjusted margin", "logit_diffs": "logistic on rating differences",
              "logit_topics": "logistic on rating and topic-metric differences",
              "ridge_bt": "ridge Bradley-Terry", "madness_bt": "seed-stratified ridge Bradley-Terry"}


def spec_name(spec: dict) -> str:
    name = spec["base"] + (f" C={spec['C']:g}" if "C" in spec else "")
    return name + (" + seed-pair blend" if spec.get("blend") else "")


def sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


# ---------------------------------------------------------------- inputs

def team_features(raw: pd.DataFrame) -> pd.DataFrame:
    """Pre-tournament inputs per (year, Team). Only TEAM_COLS and TOPIC_COLS are read, never an outcome."""
    cols = list(dict.fromkeys(TEAM_COLS + TOPIC_COLS))
    assert_prediction_time_contract(cols, label="game-level team inputs")
    t = raw[["year", "Team", *cols]].copy()
    t["tavg"] = t.groupby("year")["Adj T."].transform("mean")  # D1 mean tempo, same regular season
    t["em"] = t["AdjOE"] - t["AdjDE"]
    return t.set_index(["year", "Team"])


def pair_features(pairs: pd.DataFrame, teams: pd.DataFrame) -> pd.DataFrame:
    """Difference features for (year, team_a, team_b) rows; antisymmetric, so no intercept is needed."""
    out = pairs.copy()
    for side in ("a", "b"):
        f = teams.loc[list(zip(out["year"], out[f"team_{side}"]))]
        for col, src in (("seed", "seed"), ("em", "em"), ("bart", "Barthag"), ("t", "Adj T.")):
            out[f"{col}_{side}"] = f[src].to_numpy(dtype=float)
        topic = f[TOPIC_COLS].to_numpy(dtype=float)
        if side == "a":
            out["tavg"] = f["tavg"].to_numpy(dtype=float)
            topic_a = topic
    diffs = np.nan_to_num(topic_a - topic)  # a missing input carries no information: difference 0
    for i, name in enumerate(TOPIC_FEATURES):
        out[name] = diffs[:, i]
    out["em_diff"] = out["em_a"] - out["em_b"]
    out["tempo_margin"] = out["em_diff"] * out["t_a"] * out["t_b"] / out["tavg"] / 100.0
    lg = lambda p: np.log(p / (1 - p))  # noqa: E731
    out["log5_logit"] = lg(out["bart_a"]) - lg(out["bart_b"])
    out["seed_diff"] = out["seed_b"] - out["seed_a"]
    return out


def game_frame(games: pd.DataFrame, teams: pd.DataFrame) -> pd.DataFrame:
    og = oriented_games(games).sort_values(["year", "round", "team_a"], kind="stable").reset_index(drop=True)
    return pair_features(og, teams)


def results_frame(history: pd.DataFrame, games: pd.DataFrame) -> pd.DataFrame:
    cols = ["year", "round", "winner", "loser", "winner_seed", "loser_seed", "no_contest"]
    out = pd.concat([history[cols], games[cols]], ignore_index=True)
    out["no_contest"] = out["no_contest"].astype(bool)
    return out


def chalk(frame: pd.DataFrame, cap: float) -> np.ndarray:
    a, b = frame["seed_a"].to_numpy(), frame["seed_b"].to_numpy()
    return np.where(a < b, cap, np.where(a > b, 1 - cap, 0.5))


# ---------------------------------------------------------------- candidates

def seed_pair_fn(results: pd.DataFrame):
    rate = seed_pair_rates(results)
    return lambda f: np.array([rate(a, b) for a, b in zip(f["seed_a"], f["seed_b"])], dtype=float)


def fit_base(spec: dict, train: pd.DataFrame, strength_year: dict | None = None):
    """predict(frame) -> P(team_a wins), fit on `train` games only. Scaling is fit on `train` too."""
    base = spec["base"]
    if base == "log5":
        return lambda f: expit(f["log5_logit"].to_numpy())
    if base in SCALAR_BASES:
        col = SCALAR_BASES[base]
        x = train[col].to_numpy()
        k = fit_bradley_terry_k(np.where(train["a_won"].to_numpy() == 1, x, -x))
        return lambda f: expit(k * f[col].to_numpy())
    if base in LOGIT_BASES:
        feats = LOGIT_BASES[base]
        scale = train[feats].std().replace(0, 1.0)  # ponytail: no centring, features are differences
        m = LogisticRegression(C=spec["C"], fit_intercept=False, max_iter=5000)
        m.fit(train[feats] / scale, train["a_won"])
        return lambda f: m.predict_proba(f[feats] / scale)[:, 1]
    if base in STRENGTH_BASES:
        s, k = strength_year["strength_all"], strength_year["k"]
        return lambda f: np.array([p_win(s[a], s[b], k) for a, b in zip(f["team_a"], f["team_b"])])
    raise ValueError(base)


def blend_weight(train: pd.DataFrame, results: pd.DataFrame, spec: dict, strength_year: dict | None) -> float:
    """w in seed + w (model - seed), fit on leave-one-season-out predictions of the training seasons.

    Both the model and the seed-pair rates for held-out season s are fit without s, so w is
    learned from out-of-sample forecasts, all from seasons before the tournament being predicted.
    """
    pb, ps, y = [], [], []
    for s in sorted(train["year"].unique()):
        held = train[train["year"] == s]
        if spec["base"] in STRENGTH_BASES:  # out-of-fold strengths from the fold's own season-grouped CV
            oof, k = strength_year["strength_oof"], strength_year["k"]
            pb.append([p_win(oof[(s, a)], oof[(s, b)], k) for a, b in zip(held["team_a"], held["team_b"])])
        else:
            pb.append(fit_base(spec, train[train["year"] != s])(held))
        ps.append(seed_pair_fn(results[results["year"] != s])(held))
        y.append(held["a_won"].to_numpy())
    pb, ps, y = (np.concatenate(v).astype(float) for v in (pb, ps, y))
    res = minimize_scalar(lambda w: logloss(ps + w * (pb - ps), y).mean(), bounds=(0.0, 1.0), method="bounded")
    return float(res.x)


def fit_candidate(year: int, spec: dict, gl: pd.DataFrame, results: pd.DataFrame, fits: dict | None = None):
    """Fit `spec` for tournament `year` from games and results strictly before `year`.

    `fits[base][year]` holds the strength models (fit_year_model / madness_fit_year) for the
    strength-based candidates; each of those also sees only seasons before `year`.
    """
    train = gl[gl["year"] < year]
    hist = results[results["year"] < year]
    seed_fn = seed_pair_fn(hist)
    if spec["base"] == "seed_pair":
        return seed_fn, {}
    strength_year = (fits or {}).get(spec["base"], {}).get(year)
    base_fn = fit_base(spec, train, strength_year)
    if not spec.get("blend"):
        return base_fn, {}
    w = blend_weight(train, hist, spec, strength_year)
    return (lambda f: seed_fn(f) + w * (base_fn(f) - seed_fn(f))), {"w": w}


# ---------------------------------------------------------------- Trevor's prior family (madness.py)

MADNESS_FEATURES = sorted({f for _, _, fs in mad.SEED_MODELS for f in fs if f != "predround"} | set(mad.GEN_FEATURES))


def _madness_fit(df: pd.DataFrame, feats: list[str], poly: dict | None):
    """madness.py's ridge, tuned the same way (LOO when n <= 50, else shuffled 5-fold), with an intercept.

    madness._build_regression_model used fit_intercept=False on standardised inputs (fixed 2026-09-30), which centred
    every prediction on zero inside each seed band; see madness_as_implemented_zero_share().
    """
    X, y = mad._get_Xy(df, feats, poly=poly)
    build = lambda a: Pipeline([("scaler", StandardScaler()),  # noqa: E731
                                ("model", Ridge(alpha=a) if a > 0 else LinearRegression())])
    alpha = float(mad.DEFAULT_CONFIG["model"]["ridge_alpha"])
    if len(X) >= 3:
        cv = LeaveOneOut() if len(X) <= 50 else KFold(n_splits=5, shuffle=True, random_state=42)
        grid = mad.DEFAULT_CONFIG["model"]["ridge_alpha_grid"]
        mse = [-cross_val_score(build(float(a)), X, y, cv=cv, scoring="neg_mean_squared_error").mean() for a in grid]
        alpha = float(grid[int(np.argmin(mse))])
    return build(alpha).fit(X, y)


def madness_stack(train_rows: pd.DataFrame, score_rows: pd.DataFrame) -> np.ndarray:
    """General model, then the 14 SEED_MODELS that take its predround as an input (madness.py order).

    Same models as madness.train_general_model / train_seed_models; ponytail: their jackknife LOO
    only reports an MSE and does not change the fitted models, so it is skipped.
    """
    score_rows = score_rows.drop(columns=[c for c in OUTCOME_COLUMNS if c in score_rows.columns])
    gen = _madness_fit(train_rows, mad.GEN_FEATURES, mad.GEN_POLY)
    tr = train_rows.assign(predround=gen.predict(mad._get_Xy(train_rows, mad.GEN_FEATURES, poly=mad.GEN_POLY)[0]))
    seed_models = {}
    for i, (hist_seeds, _, feats) in enumerate(mad.SEED_MODELS, 1):
        sub = tr[tr["seed"].isin(hist_seeds)]
        avail = [f for f in feats if f in sub.columns]
        if len(sub) >= 3 and avail:
            seed_models[i] = (_madness_fit(sub, avail, None), avail)
    out = score_rows.assign(predround=gen.predict(mad._get_Xy(score_rows, mad.GEN_FEATURES, poly=mad.GEN_POLY)[0]))
    return mad._predict_with_seed_models(out, seed_models)["predround"].to_numpy(dtype=float)


def madness_fit_year(year: int, raw: pd.DataFrame, games: pd.DataFrame) -> dict:
    """Strengths for tournament `year` plus Bradley-Terry k on out-of-fold strengths, seasons < year."""
    assert_prediction_time_contract(MADNESS_FEATURES, label="madness.py features")
    seeded = pd.to_numeric(raw["seed"], errors="coerce").notna()
    tourn = raw[seeded & pd.to_numeric(raw["round"], errors="coerce").notna() & (raw["year"] != 2020)]
    train = tourn[tourn["year"] < year]
    oof: dict = {}
    for block in np.array_split(np.array(sorted(train["year"].unique())), min(5, train["year"].nunique())):
        held = train[train["year"].isin(block)]
        oof.update(zip(zip(held["year"].astype(int), held["Team"]), madness_stack(train[~train["year"].isin(block)], held)))
    hist = games[(games["year"] < year) & ~games["no_contest"]]
    k = fit_bradley_terry_k(np.array([oof[(y, w)] - oof[(y, lo)] for y, w, lo in zip(hist["year"], hist["winner"], hist["loser"])]))
    season = raw[(raw["year"] == year) & seeded]
    return {"strength_all": dict(zip(season["Team"], madness_stack(train, season))), "k": k, "strength_oof": oof}


def predict_games(year: int, spec: dict, gl: pd.DataFrame, results: pd.DataFrame, fits: dict | None = None):
    fn, info = fit_candidate(year, spec, gl, results, fits)
    test = gl[gl["year"] == year].drop(columns="a_won")
    return fn(test), fn, info


# ---------------------------------------------------------------- scoring

def brier(p, y):
    return (np.asarray(p, float) - np.asarray(y, float)) ** 2


def logloss(p, y, eps: float = 1e-15):
    q = np.clip(np.asarray(p, float), eps, 1 - eps)
    y = np.asarray(y, float)
    return -(y * np.log(q) + (1 - y) * np.log(1 - q))


LOSSES = {"brier": brier, "logloss": logloss}


def paired_stats(d: np.ndarray, years: np.ndarray, edge: float | None = None) -> dict:
    """Mean loss difference with year-block bootstrap CI, clustered DM test, MDE and n_eff check.

    `d` must be in chronological order (year, round) for the lag-1 autocorrelation.
    """
    d = np.asarray(d, float)
    uniq, inv = np.unique(np.asarray(years), return_inverse=True)
    g, n = len(uniq), len(d)
    sums, counts = np.bincount(inv, weights=d, minlength=g), np.bincount(inv, minlength=g)
    idx = np.random.default_rng(BOOT_SEED).integers(0, g, size=(N_BOOT, g))
    boot = sums[idx].sum(axis=1) / counts[idx].sum(axis=1)
    mean = float(d.mean())
    resid = np.bincount(inv, weights=d - mean, minlength=g)
    se = float(np.sqrt(g / (g - 1) * (resid ** 2).sum()) / n)
    df = g - 1
    t = mean / se if se > 0 else 0.0
    sd = float(d.std(ddof=1))
    r1 = float(np.corrcoef(d[:-1], d[1:])[0, 1]) if sd > 0 else 0.0
    n_eff = float(min(n, n * (1 - r1) / (1 + r1)))
    z = lambda num, den: num / den if den > 0 else 0.0  # noqa: E731  (a constant series has no test)
    out = {
        "mean": mean, "ci_low": float(np.quantile(boot, 0.025)), "ci_high": float(np.quantile(boot, 0.975)),
        "se": se, "t": t, "df": df, "p_dm": float(2 * stats.t.sf(abs(t), df)),
        "p_iid": float(2 * stats.norm.sf(abs(z(mean, sd / np.sqrt(n))))),
        "mde": float((stats.t.ppf(0.975, df) + stats.t.ppf(0.80, df)) * se),
        "r1": r1, "n_eff": n_eff, "p_neff": float(2 * stats.t.sf(abs(z(mean, sd / np.sqrt(n_eff))), max(n_eff - 1, 1))),
        "years": g, "n": n,
    }
    if edge is not None:
        out["power_at_edge"] = float(stats.t.cdf(z(edge, se) - stats.t.ppf(0.975, df), df)) if se > 0 else 1.0
    return out


def murphy(p: np.ndarray, y: np.ndarray) -> dict:
    """Brier = reliability - resolution + uncertainty on decile bins (0-1 Brier scale)."""
    p, y = np.asarray(p, float), np.asarray(y, float)
    bins = np.minimum((p * 10).astype(int), 9)
    ybar = y.mean()
    rel = res = 0.0
    for b in np.unique(bins):
        m = bins == b
        rel += m.sum() * (p[m].mean() - y[m].mean()) ** 2
        res += m.sum() * (y[m].mean() - ybar) ** 2
    n = len(p)
    return {"brier": float(brier(p, y).mean()), "reliability": rel / n, "resolution": res / n,
            "uncertainty": float(ybar * (1 - ybar))}


def score_table(frame: pd.DataFrame, cols: dict[str, str], by: str | None, y: str = "a_won") -> pd.DataFrame:
    groups = [("all", frame)] if by is None else list(frame.groupby(by))
    rows = []
    for key, g in groups:
        row = {by or "scope": key, "n": len(g)}
        for label, col in cols.items():
            row[f"brier_{label}"] = brier(g[col], g[y]).mean()
            row[f"logloss_{label}"] = logloss(g[col], g[y]).mean()
        rows.append(row)
    return pd.DataFrame(rows)


def advancement(bracket: pd.DataFrame, fn, year: int, teams: pd.DataFrame) -> pd.DataFrame:
    """Exact P(team wins its round-r game), r = 1..6, from the real bracket and pairwise P.

    P_r(i) = P_{r-1}(i) * sum over j in the other half of i's round-r block of P_{r-1}(j) * P(i beats j),
    so a team's chance sums over the opponents it can actually meet. Regions sorted A-D, so the
    national semifinals are A-B and C-D, as bracket_from_games() labels them.
    """
    slot = {t: "ABCD".index(r) * 16 + BRACKET_ORDER.index(int(s))
            for t, s, r in zip(bracket["Team"], bracket["Seed"], bracket["Region"])}
    order = sorted(slot, key=slot.get)
    n = len(order)
    ia, ib = np.triu_indices(n, 1)
    pairs = pair_features(pd.DataFrame({"year": year, "team_a": [order[i] for i in ia],
                                        "team_b": [order[j] for j in ib]}), teams)
    m = np.full((n, n), 0.5)
    pij = np.asarray(fn(pairs), float)
    m[ia, ib], m[ib, ia] = pij, 1 - pij
    reach = np.ones(n)
    rows = []
    pos = np.arange(n)
    for r in range(1, 7):
        nxt = np.zeros(n)
        for i in range(n):
            opp = (pos >> r == i >> r) & ((pos >> (r - 1)) != (i >> (r - 1)))
            nxt[i] = reach[i] * float((reach[opp] * m[i, opp]).sum())
        reach = nxt
        rows += [{"year": year, "Team": order[i], "round": r, "p": reach[i]} for i in range(n)]
    out = pd.DataFrame(rows)
    sums = out.groupby("round")["p"].sum().to_numpy()
    if not np.allclose(sums, [2 ** (6 - r) for r in range(1, 7)]):
        raise AssertionError(f"{year}: round probabilities do not sum to the number of winners: {sums}")
    return out


def load_inputs(args: argparse.Namespace) -> dict:
    training_csv, games_csv, history_csv = (REPO_ROOT / p for p in (args.training_csv, args.games_csv, args.history_csv))
    raw = pd.read_csv(training_csv)
    games = pd.read_csv(games_csv)
    games["no_contest"] = games["no_contest"].astype(bool)
    history = pd.read_csv(history_csv)
    if history["year"].max() >= games["year"].min():
        raise SystemExit("seed history overlaps the Torvik games; it must end before them")
    teams = team_features(raw)
    return {
        "raw": raw, "games": games, "teams": teams, "gl": game_frame(games, teams),
        "results": results_frame(history, games),
        "hashes": {"training_csv": sha(training_csv)[:16], "games_csv": sha(games_csv)[:16],
                   "history_csv": sha(history_csv)[:16]},
        "paths": {"training_csv": args.training_csv, "games_csv": args.games_csv, "history_csv": args.history_csv},
    }


def strength_fits(data: dict, years, args: argparse.Namespace) -> dict:
    """{base: {year: fit}} for the two strength-based families, each fit on seasons before `year`."""
    model_df, rank_cols = prepare_model_frame(data["raw"])
    out: dict = {b: {} for b in STRENGTH_BASES}
    for y in years:
        print(f"[strength fits] {y}", flush=True)
        out["ridge_bt"][y] = fit_year_model(y, data["raw"], model_df, rank_cols, data["games"], args.cv_folds,
                                            args.random_state)
        out["madness_bt"][y] = madness_fit_year(y, data["raw"], data["games"])
    return out


def madness_as_implemented_zero_share(raw: pd.DataFrame, year: int) -> float:
    """Share of the `year` field that madness.py's own models (no intercept) predict at exactly 0 wins."""
    seeded = pd.to_numeric(raw["seed"], errors="coerce").notna()
    tourn = raw[seeded & pd.to_numeric(raw["round"], errors="coerce").notna() & (raw["year"] < year) & (raw["year"] != 2020)]
    grid = list(mad.DEFAULT_CONFIG["model"]["ridge_alpha_grid"])
    legacy = mad._build_regression_model  # the pre-fix model: no intercept, tuned without one too

    def fit(df, feats, poly):
        with patch.object(mad, "_build_regression_model", lambda a: legacy(a, fit_intercept=False)):
            tuned = mad.tune_ridge_alpha(df, feats, poly, grid)
        X, y = mad._get_Xy(df, feats, poly=poly)
        return legacy(tuned["alpha_selected"] if tuned.get("tuned") else 1.0, fit_intercept=False).fit(X, y)

    gen = fit(tourn, mad.GEN_FEATURES, mad.GEN_POLY)
    tr = tourn.assign(predround=gen.predict(mad._get_Xy(tourn, mad.GEN_FEATURES, poly=mad.GEN_POLY)[0]))
    models = {i: (fit(tr[tr["seed"].isin(h)], [f for f in fs if f in tr.columns], None), [f for f in fs if f in tr.columns])
              for i, (h, _, fs) in enumerate(mad.SEED_MODELS, 1)}
    season = raw[(raw["year"] == year) & seeded]
    season = season.drop(columns=[c for c in OUTCOME_COLUMNS if c in season.columns])
    out = season.assign(predround=gen.predict(mad._get_Xy(season, mad.GEN_FEATURES, poly=mad.GEN_POLY)[0]))
    return float((mad._predict_with_seed_models(out, models)["predround"] == 0).mean())


# ---------------------------------------------------------------- stage 1: select

def run_select(args: argparse.Namespace) -> int:
    if FROZEN_PATH.exists() and not args.force:
        raise SystemExit(f"{FROZEN_PATH} exists; the selection is frozen. Delete it deliberately to re-select.")
    charter_locked_at = datetime.now(timezone.utc).isoformat(timespec="seconds")
    data = load_inputs(args)
    gl, results = data["gl"], data["results"]
    fits = strength_fits(data, SELECT_YEARS, args)
    sel = gl[gl["year"].isin(SELECT_YEARS)]
    rows, losses = [], {}
    for spec in CANDIDATES:
        ps, ws = [], []
        for y in SELECT_YEARS:
            p, _, info = predict_games(y, spec, gl, results, fits)
            ps.append(p)
            ws.append(info.get("w", np.nan))
        p = np.concatenate(ps)
        name = spec_name(spec)
        losses[name] = brier(p, sel["a_won"])
        per_year = pd.Series(losses[name]).groupby(sel["year"].to_numpy()).mean()
        rows.append({"candidate": name, "spec": spec, "hypothesis": BASE_TEXT[spec["base"]], "n_games": int(len(p)),
                     "brier": float(losses[name].mean()), "logloss": float(logloss(p, sel["a_won"]).mean()),
                     "mean_blend_w": None if np.isnan(ws).all() else float(np.nanmean(ws)),
                     "brier_by_year": {str(k): float(v) for k, v in per_year.items()}})
        print(f"[select] {name}: logloss {rows[-1]['logloss']:.4f} brier {rows[-1]['brier']:.4f}", flush=True)
    rows.sort(key=lambda r: (r["brier"], r["logloss"]))
    # the baseline stays in the log for reference but cannot be frozen: the test needs a real model
    chosen = next(r for r in rows if r["spec"]["base"] != "seed_pair")

    # search correction over every variant tried (seed-pair itself excluded: it is the null)
    years = sel["year"].to_numpy()
    base = losses[spec_name({"base": "seed_pair"})]
    tried = [r["candidate"] for r in rows if r["spec"]["base"] != "seed_pair"]
    m = len(tried)
    raw_p = paired_stats(losses[chosen["candidate"]] - base, years)["p_dm"]
    gains = np.array([base - losses[c] for c in tried])  # Brier gain; positive = better than seed-pair
    uniq, inv = np.unique(years, return_inverse=True)
    sums = np.stack([np.bincount(inv, weights=gk, minlength=len(uniq)) for gk in gains])
    counts = np.bincount(inv, minlength=len(uniq))
    means = gains.mean(axis=1)
    idx = np.random.default_rng(BOOT_SEED).integers(0, len(uniq), size=(N_BOOT, len(uniq)))
    boot_means = sums[:, idx].sum(axis=2) / counts[idx].sum(axis=1)
    v_star = (boot_means - means[:, None]).max(axis=0)
    rc_p = float((v_star >= means.max()).mean())

    frozen = {
        "frozen_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "charter": CHARTER, "charter_locked_at": charter_locked_at,
        "rule": "lowest pooled Brier (log loss breaks ties) among model candidates (seed-pair itself is logged "
                "for reference but cannot be frozen) over walk-forward tournaments 2010-2019; frozen "
                "before any 2021-2026 score of these candidates was computed; 2021-2026 is scored once by "
                "`run_powered_comparison.py score`",
        "selection_years": list(SELECT_YEARS), "test_years": list(TEST_YEARS),
        "chosen": chosen["spec"], "chosen_name": chosen["candidate"],
        "chosen_description": BASE_TEXT[chosen["spec"]["base"]]
                              + (" blended as seed + w (model - seed), w fit per fold on leave-one-season-out "
                                 "predictions" if chosen["spec"].get("blend") else ""),
        "team_inputs": TEAM_COLS, "pair_features": LOGIT_FEATURES,
        "seed_pair_history": "every tournament 1985 to Y-1 (Sports Reference 1985-2007, Torvik 2008 on)",
        "search_correction": {
            "variants_tried": m, "raw_p_chosen_vs_seed_pair_2010_2019": raw_p,
            "sidak_p_adj": float(1 - (1 - raw_p) ** m),
            "white_reality_check_p": rc_p,
            "reality_check": "H0: no candidate beats seed-pair on Brier; year-block bootstrap of the best "
                             f"candidate's centred mean gain, {N_BOOT} resamples, seed {BOOT_SEED}",
        },
        "variants": [{k: v for k, v in r.items()} for r in rows],
        "inputs": data["hashes"], "code_revision": run_provenance(REPO_ROOT / args.training_csv)["code_revision"],
        "random_state": args.random_state, "bootstrap_seed": BOOT_SEED,
    }
    FROZEN_PATH.write_text(json.dumps(frozen, indent=2) + "\n", encoding="utf-8")
    print(f"Frozen: {chosen['candidate']} -> {FROZEN_PATH}")
    return 0


# ---------------------------------------------------------------- stage 2: score

def fmt_ci(s: dict, digits: int = 4) -> str:
    return f"{s['mean']:+.{digits}f} [{s['ci_low']:+.{digits}f}, {s['ci_high']:+.{digits}f}]"


def verdict(s: dict) -> str:
    if s["ci_high"] < 0:
        return "better"
    if s["ci_low"] > 0:
        return "worse"
    return "not distinguishable"


def paired_row(label: str, metric: str, s: dict) -> dict:
    return {"comparison": label, "metric": metric, "years": s["years"], "n": s["n"], "mean_diff": s["mean"],
            "ci_low": s["ci_low"], "ci_high": s["ci_high"], "dm_t": s["t"], "p_dm_year_clustered": s["p_dm"],
            "p_iid": s["p_iid"], "lag1_r": s["r1"], "n_eff": s["n_eff"], "p_n_eff": s["p_neff"],
            "mde_80pct": s["mde"], "power_at_meaningful_edge": s.get("power_at_edge", np.nan)}


def run_score(args: argparse.Namespace) -> int:
    if not FROZEN_PATH.exists():
        raise SystemExit(f"{FROZEN_PATH} missing: run `select` first so the model is frozen before scoring")
    frozen = json.loads(FROZEN_PATH.read_text(encoding="utf-8"))
    frozen_sha = sha(FROZEN_PATH)[:16]
    data = load_inputs(args)
    if data["hashes"] != frozen["inputs"]:
        raise SystemExit(f"inputs changed since the selection was frozen: {data['hashes']} vs {frozen['inputs']}")
    set_project_theme()
    gl, results, games, teams = data["gl"], data["results"], data["games"], data["teams"]
    spec = frozen["chosen"]
    fname = frozen["chosen_name"]
    fits = strength_fits(data, EVAL_YEARS, args)
    ridge_spec = {"base": "ridge_bt"}
    madness_spec = {"base": "madness_bt"}
    seed_spec = {"base": "seed_pair"}
    results08 = results[results["year"] >= 2008]

    ev = gl[gl["year"].isin(EVAL_YEARS)].copy()
    cols = {"p_ridge": [], "p_madness": [], "p_seed": [], "p_seed08": [], "p_frozen": []}
    adv_rows, blend_w = [], {}
    for y in EVAL_YEARS:
        print(f"[score] {y}", flush=True)
        fns = {}
        for col, sp, res in (("p_ridge", ridge_spec, results), ("p_madness", madness_spec, results),
                             ("p_seed", seed_spec, results), ("p_seed08", seed_spec, results08),
                             ("p_frozen", spec, results)):
            p, fn, info = predict_games(y, sp, gl, res, fits)
            cols[col].append(p)
            fns[col] = fn
            if col == "p_frozen" and "w" in info:
                blend_w[y] = info["w"]
        bracket = bracket_from_games(games[games["year"] == y])
        wins = games[games["year"] == y]["winner"].value_counts().reindex(bracket["Team"], fill_value=0)
        fns["p_chalk70"] = lambda f: chalk(f, CHALK_CAPS["chalk70"])
        adv = None
        for col in ("p_ridge", "p_madness", "p_seed", "p_frozen", "p_chalk70"):
            a = advancement(bracket, fns[col], y, teams).rename(columns={"p": col})
            adv = a if adv is None else adv.merge(a, on=["year", "Team", "round"])
        adv["reached"] = (adv["Team"].map(wins) >= adv["round"]).astype(int)
        adv_rows.append(adv)
    for col, parts in cols.items():
        ev[col] = np.concatenate(parts)
    for col, cap in CHALK_CAPS.items():
        ev[f"p_{col}"] = chalk(ev, cap)
    tr = {(r.year, r.winner, r.loser): r.trank_p_winner for r in games.itertuples(index=False)}
    ev["p_trank"] = [tr.get((y, a, b)) if w else 1 - tr.get((y, b, a), np.nan)
                     for y, a, b, w in zip(ev["year"], ev["team_a"], ev["team_b"], ev["a_won"])]
    ev["p_trank"] = pd.to_numeric(ev["p_trank"], errors="coerce")
    adv = pd.concat(adv_rows, ignore_index=True).sort_values(["year", "round", "Team"], kind="stable")
    const = {y: float(gl.loc[(gl["year"] < y) & (gl["seed_a"] != gl["seed_b"]), "a_won"].mean()) for y in EVAL_YEARS}
    ev["p_const"] = np.where(ev["seed_a"] == ev["seed_b"], 0.5, ev["year"].map(const))
    ev["p_coin"] = 0.5

    y_ev = ev["a_won"].to_numpy()
    test = ev[ev["year"].isin(TEST_YEARS)]
    y_te = test["a_won"].to_numpy()

    def diff(frame, a, b, metric, yv):
        return LOSSES[metric](frame[a], yv) - LOSSES[metric](frame[b], yv)

    # (1) powered: existing model vs seed-pair, 2010-2026
    tests = {}
    for metric in ("brier", "logloss"):
        tests[("ridge", "seed", metric, "all")] = paired_stats(diff(ev, "p_ridge", "p_seed", metric, y_ev), ev["year"], MEANINGFUL[metric])
        tests[("ridge", "seed", metric, "test")] = paired_stats(diff(test, "p_ridge", "p_seed", metric, y_te), test["year"], MEANINGFUL[metric])
        tests[("frozen", "seed", metric, "test")] = paired_stats(diff(test, "p_frozen", "p_seed", metric, y_te), test["year"], MEANINGFUL[metric])
        tests[("frozen", "ridge", metric, "test")] = paired_stats(diff(test, "p_frozen", "p_ridge", metric, y_te), test["year"], MEANINGFUL[metric])
        tests[("seed08", "seed", metric, "all")] = paired_stats(diff(ev, "p_seed08", "p_seed", metric, y_ev), ev["year"])
        tests[("madness", "seed", metric, "all")] = paired_stats(diff(ev, "p_madness", "p_seed", metric, y_ev), ev["year"], MEANINGFUL[metric])
    tk = ev[ev["p_trank"].notna()]
    y_tk = tk["a_won"].to_numpy()
    for metric in ("brier", "logloss"):
        tests[("trank", "seed", metric, "trank")] = paired_stats(diff(tk, "p_trank", "p_seed", metric, y_tk), tk["year"])
        tests[("ridge", "trank", metric, "trank")] = paired_stats(diff(tk, "p_ridge", "p_trank", metric, y_tk), tk["year"])
    tk_te = tk[tk["year"].isin(TEST_YEARS)]
    for metric in ("brier", "logloss"):
        tests[("frozen", "trank", metric, "test")] = paired_stats(
            diff(tk_te, "p_frozen", "p_trank", metric, tk_te["a_won"].to_numpy()), tk_te["year"])
    # path-aware advancement
    adv_y = adv["reached"].to_numpy()
    adv_te = adv[adv["year"].isin(TEST_YEARS)]
    for metric in ("brier", "logloss"):
        tests[("adv_ridge", "adv_seed", metric, "all")] = paired_stats(diff(adv, "p_ridge", "p_seed", metric, adv_y), adv["year"])
        tests[("adv_frozen", "adv_seed", metric, "test")] = paired_stats(
            diff(adv_te, "p_frozen", "p_seed", metric, adv_te["reached"].to_numpy()), adv_te["year"])
    ew = adv.groupby(["year", "Team"])[["p_ridge", "p_seed", "p_frozen", "p_chalk70"]].sum()
    ew["wins"] = adv.groupby(["year", "Team"])["reached"].sum()
    ew = ew.reset_index()
    se = {c: (ew[c] - ew["wins"]) ** 2 for c in ("p_ridge", "p_seed", "p_frozen", "p_chalk70")}
    ew_all = paired_stats((se["p_ridge"] - se["p_seed"]).to_numpy(), ew["year"])
    ew_te_mask = ew["year"].isin(TEST_YEARS).to_numpy()
    ew_te = paired_stats((se["p_frozen"] - se["p_seed"]).to_numpy()[ew_te_mask], ew["year"][ew_te_mask])

    methods = {"model": "p_ridge", "madness": "p_madness", "seed_pair": "p_seed", "coin": "p_coin",
               "constant": "p_const", "chalk70": "p_chalk70", "chalk99": "p_chalk99"}
    by_round = score_table(ev, methods, "round")
    by_round.insert(1, "round_name", by_round["round"].map(ROUND_NAMES))
    overall = score_table(ev, methods, None).rename(columns={"scope": "round"}).assign(round_name="all")
    by_round = pd.concat([by_round, overall[by_round.columns]], ignore_index=True)
    by_year = score_table(ev, {"model": "p_ridge", "seed_pair": "p_seed"}, "year")
    by_year["brier_diff"] = by_year["brier_model"] - by_year["brier_seed_pair"]
    by_year["logloss_diff"] = by_year["logloss_model"] - by_year["logloss_seed_pair"]

    te_methods = {"frozen": "p_frozen", "model": "p_ridge", "madness": "p_madness", "seed_pair": "p_seed",
                  "trank": "p_trank", "chalk70": "p_chalk70"}
    te_round = score_table(test, te_methods, "round")
    te_round.insert(1, "round_name", te_round["round"].map(ROUND_NAMES))
    te_all = score_table(test, te_methods, None).rename(columns={"scope": "round"}).assign(round_name="all")
    te_round = pd.concat([te_round, te_all[te_round.columns]], ignore_index=True)
    te_year = score_table(test, {"frozen": "p_frozen", "seed_pair": "p_seed"}, "year")

    adv_methods = {"model": "p_ridge", "madness": "p_madness", "seed_pair": "p_seed", "chalk70": "p_chalk70"}
    adv_round = score_table(adv, adv_methods, "round", y="reached")
    adv_te_round = score_table(adv_te, {"frozen": "p_frozen", "seed_pair": "p_seed", "chalk70": "p_chalk70"},
                               "round", y="reached")
    reach_names = {1: "R32", 2: "S16", 3: "E8", 4: "F4", 5: "Final", 6: "Champion"}
    for t in (adv_round, adv_te_round):
        t.insert(1, "round_reached", t["round"].map(reach_names))

    tk_methods = {"trank": "p_trank", "model": "p_ridge", "seed_pair": "p_seed"}
    tk_tab = pd.concat([score_table(tk, tk_methods, None).assign(scope="all rounds"),
                        score_table(tk[tk["round"] == 1], tk_methods, None).assign(scope="R64 only")],
                       ignore_index=True)

    cal = {"frozen": calibration_table(test, "frozen"), "seed": calibration_table(test, "seed")}
    ece = {k: expected_calibration_error(v) for k, v in cal.items()}
    save_calibration_plot({"model": cal["frozen"], "seed_pair": cal["seed"]}, FIG_PATH,
                          f"Frozen model calibration, 2021-2026 ({len(test)} games)",
                          labels={"model": f"Frozen: {fname}", "seed_pair": "Seed-pair rates"})
    mur = pd.DataFrame([{"method": lab, **murphy(test[col], y_te)} for lab, col in
                        (("frozen", "p_frozen"), ("model", "p_ridge"), ("seed_pair", "p_seed"),
                         ("trank", "p_trank"))])

    up_rows = []
    unequal = test[test["seed_a"] != test["seed_b"]]
    upsets = unequal["a_won"] == 0
    for lab, col in (("frozen", "p_frozen"), ("model", "p_ridge"), ("seed_pair", "p_seed"), ("trank", "p_trank")):
        called = unequal[col] < 0.5
        up_rows.append({"method": lab, "upsets": int(upsets.sum()), "upsets_called_over_50pct": int((called & upsets).sum()),
                        "recall": float((called & upsets).sum() / upsets.sum()),
                        "underdog_picks": int(called.sum()),
                        "precision": float((called & upsets).sum() / called.sum()) if called.sum() else np.nan,
                        "mean_p_underdog_in_upsets": float((1 - unequal.loc[upsets, col]).mean())})
    up_tab = pd.DataFrame(up_rows)

    # ------------------------------------------------------------ Trevor's earlier forecasts
    zero25 = madness_as_implemented_zero_share(data["raw"], 2025)
    tv_path = REPO_ROOT / "data/external/trevor_2025_predictions_20250319.csv"
    tv_round = tv_ew = None
    tv_missing: list[str] = []
    if tv_path.exists():
        tv = pd.read_csv(tv_path).set_index("Team")
        a25 = adv[adv["year"] == 2025].copy()
        tv_missing = sorted(set(a25["Team"]) - set(tv.index))
        prob_cols = {1: "prob_R32", 2: "prob_S16", 3: "prob_E8", 4: "prob_F4", 5: "prob_F2", 6: "prob_Champ"}
        a25["p_trevor"] = [tv.at[t, prob_cols[r]] if t in tv.index else np.nan for t, r in zip(a25["Team"], a25["round"])]
        a25 = a25.dropna(subset=["p_trevor"])
        tv_round = score_table(a25, {"trevor_2025": "p_trevor", "frozen": "p_frozen", "model": "p_ridge",
                                     "seed_pair": "p_seed"}, "round", y="reached")
        tv_round.insert(1, "round_reached", tv_round["round"].map(reach_names))
        tv_round["sum_p_trevor"] = a25.groupby("round")["p_trevor"].sum().to_numpy()
        tv_round["teams_that_reach"] = [2 ** (6 - r) for r in tv_round["round"]]
        e25 = ew[ew["year"] == 2025].set_index("Team")
        e25 = e25[e25.index.isin(tv.index)]
        tv_ew = {"trevor_2025 (pred_round)": float(((tv["pred_round"].reindex(e25.index) - e25["wins"]) ** 2).mean()),
                 "frozen": float(((e25["p_frozen"] - e25["wins"]) ** 2).mean()),
                 "model": float(((e25["p_ridge"] - e25["wins"]) ** 2).mean()),
                 "seed_pair": float(((e25["p_seed"] - e25["wins"]) ** 2).mean()), "teams": int(len(e25))}
    tv24_path = REPO_ROOT / "data/external/trevor_2024_predictions_20240318.csv"
    tv24_ew = None
    if tv24_path.exists():
        # madness.R used Sports Reference names for two teams Torvik spells differently
        tv24 = (pd.read_csv(tv24_path).set_index("Team")["pred_round"]
                .rename(index={"North Carolina St.": "N.C. State", "College of Charleston": "Charleston"}))
        e24 = ew[ew["year"] == 2024].set_index("Team")
        tv24_missing = sorted(set(e24.index) - set(tv24.index))
        e24 = e24[e24.index.isin(tv24.index)]
        tv24_ew = {"trevor_2024 (pred_round)": float(((tv24.reindex(e24.index) - e24["wins"]) ** 2).mean()),
                   "frozen": float(((e24["p_frozen"] - e24["wins"]) ** 2).mean()),
                   "model": float(((e24["p_ridge"] - e24["wins"]) ** 2).mean()),
                   "seed_pair": float(((e24["p_seed"] - e24["wins"]) ** 2).mean()), "teams": int(len(e24)),
                   "missing": tv24_missing}

    cf = cal["frozen"][cal["frozen"]["n"] > 0]
    outside = int(((cf["mean_pred"] < cf["ci_low"]) | (cf["mean_pred"] > cf["ci_high"])).sum())
    cal_verdict = (f"no bin's 95% Wilson interval excludes its mean forecast ({len(cf)} occupied bins): no detectable "
                   "miscalibration at this sample size, which is not proof of calibration"
                   if outside == 0 else
                   f"{outside} of {len(cf)} occupied bins exclude their mean forecast: miscalibrated there")

    # ------------------------------------------------------------ report
    b_all, l_all = tests[("ridge", "seed", "brier", "all")], tests[("ridge", "seed", "logloss", "all")]
    b_fz, l_fz = tests[("frozen", "seed", "brier", "test")], tests[("frozen", "seed", "logloss", "test")]
    b_tk = tests[("trank", "seed", "brier", "trank")]
    from scripts.sensitivity_2026_inputs import PULL, frozen_test, refreshed_training  # lazy: it imports this module
    s26 = frozen_test(refreshed_training(2026, PULL / "trank_2026_through_20260315.csv"))
    b_md = tests[("madness", "seed", "brier", "all")]
    n_years = len(EVAL_YEARS)
    sc = frozen["search_correction"]
    ta = te_all.iloc[0]
    prov = run_provenance(REPO_ROOT / args.training_csv)
    pa, pf = tests[("adv_ridge", "adv_seed", "brier", "all")], tests[("adv_frozen", "adv_seed", "brier", "test")]

    def answer(s_b: dict, s_l: dict, who: str, window: str) -> str:
        if s_b["ci_high"] < 0:
            return (f"{who} beats seed-pair rates by {-s_b['mean']:.4f} Brier over {window}, 95% CI "
                    f"[{-s_b['ci_high']:.4f}, {-s_b['ci_low']:.4f}] (log loss {-s_l['mean']:.4f}, CI "
                    f"[{-s_l['ci_high']:.4f}, {-s_l['ci_low']:.4f}]).")
        if s_b["ci_low"] > 0:
            return (f"{who} is worse than seed-pair rates by {s_b['mean']:.4f} Brier over {window}, 95% CI "
                    f"[{s_b['ci_low']:.4f}, {s_b['ci_high']:.4f}].")
        return (f"No detectable edge for {who.lower()} over {window}: Brier difference {fmt_ci(s_b)} "
                f"(model minus seed-pair), log loss {fmt_ci(s_l)}. The test could have detected an edge of "
                f"{s_b['mde']:.4f} Brier ({s_l['mde']:.4f} log loss) with 80% power; its power against the "
                f"{MEANINGFUL['brier']} Brier edge fixed in advance was {s_b['power_at_edge']:.0%}.")

    def supports(s: dict, who: str) -> str:
        if s["ci_high"] < 0:
            return f"{who} is better than seed-pair rates; the whole interval is below zero"
        if s["ci_low"] > 0:
            return f"{who} is worse than seed-pair rates"
        return (f"{who} cannot be told apart from seed-pair rates; an edge smaller than {s['mde']:.4f} Brier is "
                f"neither shown nor ruled out, and the interval runs from {s['ci_low']:+.4f} to {s['ci_high']:+.4f}")

    L: list[str] = []
    L.append("# Model vs seed-pair rates: powered walk-forward test, 2010-2026")
    L.append("")
    L.append("## Executive result")
    L.append(f"- **Existing model, every tournament 2010-2026 ({n_years} tournaments, {b_all['n']} games):** "
             + answer(b_all, l_all, "The model", f"{n_years} tournaments"))
    L.append(f"- **Frozen game-level model, 2021-2026 ({b_fz['years']} tournaments, {b_fz['n']} games, scored once):** "
             f"`{fname}`, chosen on 2010-2019 only and frozen at {frozen['frozen_at']}. "
             + answer(b_fz, l_fz, "The frozen model", "2021-2026"))
    L.append(f"- **External reference:** T-Rank's archived pregame line, 2015-2026 ({b_tk['years']} tournaments, "
             f"{b_tk['n']} games), minus seed-pair: Brier {fmt_ci(b_tk)}. It is published the day of each game, "
             "so from the Round of 32 on it has seen earlier tournament games: a ceiling, not a like-for-like rival.")
    L.append(f"- **Path-aware round probabilities** (each team's chance of reaching each round, summed over the "
             f"opponents it could meet in the real bracket): existing model minus seed-pair, 2010-2026, Brier "
             f"{fmt_ci(pa)}; frozen model minus seed-pair, 2021-2026, Brier {fmt_ci(pf)}.")
    L.append(f"- **Trevor's earlier models:** the madness.py seed-stratified family (intercept restored) minus "
             f"seed-pair, 2010-2026, Brier {fmt_ci(b_md)}. His 2025 notebook forecasts are scored on 2025 only "
             "(Section 6).")
    L.append(f"- **Search correction on the selection years:** {sc['variants_tried']} variants tried on 2010-2019. "
             f"Chosen variant vs seed-pair there: raw p {sc['raw_p_chosen_vs_seed_pair_2010_2019']:.3f}, "
             f"Sidak-adjusted p {sc['sidak_p_adj']:.3f}, White's Reality Check p {sc['white_reality_check_p']:.3f}. "
             "The 2021-2026 test is one pre-registered comparison, so its p needs no search correction.")
    L.append("")
    L.append("### Publishable claim")
    if b_fz["ci_high"] < 0:
        L.append(f"> A game-level model ({SHORT_NAME.get(spec['base'], fname)}) chosen on 2010-2019 and frozen before scoring beat historical seed-pair "
                 f"win rates on every NCAA tournament game from 2021 to 2026 ({b_fz['n']} games) by "
                 f"{-b_fz['mean']:.3f} Brier, 95% CI {-b_fz['ci_high']:.3f} to {-b_fz['ci_low']:.3f}.")
    elif b_all["ci_high"] < 0:
        L.append(f"> Over {b_all['n']} NCAA tournament games from 2010 to 2026, predicted walk-forward, the model "
                 f"beat historical seed-pair win rates by {-b_all['mean']:.3f} Brier (95% CI "
                 f"{-b_all['ci_high']:.3f} to {-b_all['ci_low']:.3f}); a model frozen on 2010-2019 was not "
                 f"distinguishable from seed-pair rates on 2021-2026.")
    else:
        L.append(f"> Across {b_all['n']} NCAA tournament games from 2010 to 2026, the model's win probabilities "
                 f"were no more accurate than historical seed-pair rates (Brier difference {b_all['mean']:+.3f}, "
                 f"95% CI {b_all['ci_low']:+.3f} to {b_all['ci_high']:+.3f}); the test could have detected an "
                 f"edge of {b_all['mde']:.3f}.")
    L.append("")
    L.append("## Question and locked charter")
    L.append(f"Locked at {frozen['charter_locked_at']} before any fit; the selection froze at {frozen['frozen_at']}.")
    L.append("")
    for k, v in frozen["charter"].items():
        if isinstance(v, dict):
            v = ", ".join(f"{val} {key.replace('logloss', 'log loss').replace('brier', 'Brier')}" for key, val in v.items())
        L.append(f"- {k.replace('_', ' ')}: {v}")
    L.append("")
    L.append("## Data")
    L.append(f"- Games: {len(ev)} scored games in {n_years} tournaments 2010-2026 (2020 cancelled), 63 per year "
             "except 2021 (62: Oregon-VCU no-contest). Torvik `YYYY_super_sked.json`, cross-checked against "
             "recorded wins every season (`data/raw/tournament_games_manifest.json`).")
    L.append("- Seed-pair history: 1985-2007 from Sports Reference (`data/raw/seed_history_games_manifest.json`; "
             "parser matched Torvik game for game on 2008 and 2019), 2008 on from Torvik.")
    L.append(f"- T-Rank archived pregame line: {int(ev['p_trank'].notna().sum())} of the scored games (every game 2015-2026).")
    L.append("- Team inputs: Torvik regular-season table (type R, games through the conference tournaments) and bracket seed; "
             "2026 from the 2026-03-05 export, which predates the conference tournaments: 2026 tournament teams have a median 30 games against 32-33 in 2022-2025, so 2026 inputs are a few games staler than every other year. "
             f"Sensitivity (`scripts/sensitivity_2026_inputs.py`): with 2026 refreshed to Torvik's table for games through "
             f"2026-03-15, frozen minus seed-pair over 2021-2026 is Brier {fmt_ci(s26)}, against {fmt_ci(b_fz)} as scored; "
             "the same pull for 2025 reproduces the training rows.")
    L.append("")
    L.append("## Results")
    L.append("### 1. Existing model vs baselines, every game 2010-2026")
    L.append("Model = ridge on tournament wins with the feature set re-chosen each year on earlier seasons, "
             "Bradley-Terry scale k fit on earlier tournaments' out-of-fold strengths (`scripts/run_bracket_evaluation.py`). "
             "coin = 0.5; constant = training-fold better-seed win rate; chalk gives the better seed a fixed "
             "probability, 0.70 as a sane cap and 0.99 to show that chalk's old log loss was set by its cap. "
             "madness = Trevor's madness.py family. Equal seeds get 0.5 everywhere.")
    L.append("")
    L.append(markdown_table(by_round, float_digits=4))
    L.append("")
    L.append("#### By tournament (model minus seed-pair)")
    L.append(markdown_table(by_year, float_digits=4))
    L.append("")
    L.append("### 2. Paired tests")
    L.append("Per-game loss difference, first method minus second (negative favours the first). CI: 95% "
             f"percentile interval from {N_BOOT:,} bootstrap resamples of whole tournaments. DM: Diebold-Mariano "
             "t on the same series with year-clustered variance, (tournaments - 1) df. p_iid treats games as "
             "independent and is shown only to make the clustering visible. n_eff = n (1 - r1) / (1 + r1) from "
             "the lag-1 autocorrelation of the chronological series, a cross-check. MDE: the true difference "
             "detectable with 80% power at alpha 0.05 two-sided, (t_0.975 + t_0.80) x clustered SE. Power is "
             f"against the edge fixed before scoring ({MEANINGFUL['brier']} Brier, {MEANINGFUL['logloss']} log loss).")
    L.append("")
    labels = {
        ("ridge", "seed", "all"): "model - seed-pair, 2010-2026",
        ("ridge", "seed", "test"): "model - seed-pair, 2021-2026",
        ("frozen", "seed", "test"): "frozen - seed-pair, 2021-2026",
        ("frozen", "ridge", "test"): "frozen - model, 2021-2026",
        ("seed08", "seed", "all"): "seed-pair (2008+ history) - seed-pair (1985+), 2010-2026",
        ("madness", "seed", "all"): "madness.py family - seed-pair, 2010-2026",
        ("trank", "seed", "trank"): "T-Rank - seed-pair, 2015-2026",
        ("ridge", "trank", "trank"): "model - T-Rank, 2015-2026",
        ("frozen", "trank", "test"): "frozen - T-Rank, 2021-2026",
        ("adv_ridge", "adv_seed", "all"): "round probs: model - seed-pair, 2010-2026",
        ("adv_frozen", "adv_seed", "test"): "round probs: frozen - seed-pair, 2021-2026",
    }
    prow = [paired_row(labels[(a, b, w)], m, s) for (a, b, m, w), s in tests.items()]
    L.append(markdown_table(pd.DataFrame(prow), float_digits=4))
    L.append("")
    flips = [(labels[(a, b, w)], m, st) for (a, b, m, w), st in tests.items() if st["p_dm"] < 0.05 <= st["p_iid"]]
    if flips:
        L.append("Few-block caution: with this few tournaments the clustered variance can come out below the "
                 "game-level variance. These rows pass the clustered DM test but not the independent-games one: "
                 + "; ".join(f"{lab} {m} (clustered p {st['p_dm']:.3f}, iid p {st['p_iid']:.3f})" for lab, m, st in flips)
                 + ". Read the larger p: on that reading none of them is significant.")
        L.append("")
    L.append(f"Expected tournament wins from the path-aware bracket (squared error per team, 0-6 wins): model "
             f"minus seed-pair 2010-2026 {fmt_ci(ew_all)} over {ew_all['n']} team-seasons; frozen minus seed-pair "
             f"2021-2026 {fmt_ci(ew_te)} over {ew_te['n']}. This replaces the single 64-row round holdout.")
    L.append("")
    L.append("### 3. Frozen game-level model, 2021-2026")
    L.append(f"- Chosen: `{fname}`: {frozen['chosen_description']}.")
    inputs = ", ".join(INPUTS_BY_BASE.get(spec["base"], ["see its pipeline"]) + (["seed"] if spec.get("blend") else []))
    L.append(f"- Frozen model inputs: {inputs}; every candidate's inputs pass the prediction-time contract; no "
             "PASE, PAKE, finish or postseason field.")
    if blend_w:
        L.append("- Blend weight w fit inside each walk-forward fold: "
                 + ", ".join(f"{y} {w:.2f}" for y, w in blend_w.items() if y in TEST_YEARS) + ".")
    L.append(f"- Overall 2021-2026: Brier frozen {ta['brier_frozen']:.4f}, existing model {ta['brier_model']:.4f}, "
             f"madness family {ta['brier_madness']:.4f}, seed-pair {ta['brier_seed_pair']:.4f}, T-Rank "
             f"{ta['brier_trank']:.4f}; log loss {ta['logloss_frozen']:.4f} / {ta['logloss_model']:.4f} / "
             f"{ta['logloss_madness']:.4f} / {ta['logloss_seed_pair']:.4f} / {ta['logloss_trank']:.4f}.")
    L.append("")
    L.append("#### Selection log, 2010-2019 walk-forward (every variant tried, best first)")
    vt = pd.DataFrame([{"candidate": v["candidate"], "n_games": v["n_games"], "brier": v["brier"],
                        "logloss": v["logloss"], "mean_blend_w": v["mean_blend_w"]} for v in frozen["variants"]])
    L.append(markdown_table(vt, float_digits=4))
    L.append("")
    L.append("These numbers chose the model, so the winner's are optimistic; only 2021-2026 is a clean test. "
             "Per-year Brier for every variant is in `reports/model_selection_frozen.json`.")
    L.append("")
    L.append("#### By round, 2021-2026")
    L.append(markdown_table(te_round, float_digits=4))
    L.append("")
    L.append("#### By tournament, 2021-2026")
    L.append(markdown_table(te_year, float_digits=4))
    L.append("")
    L.append("#### Calibration and resolution, 2021-2026")
    L.append("Murphy decomposition on decile bins, 0-1 Brier scale (Tetlock's 0-2 scale doubles every number): "
             "Brier = reliability - resolution + uncertainty, up to a small within-bin term. Lower reliability is "
             "better calibration; higher resolution is more decisive. Games oriented to the better seed.")
    L.append("")
    L.append(markdown_table(mur, float_digits=4))
    L.append("")
    L.append(f"ECE (decile bins): frozen {ece['frozen']:.4f}, seed-pair {ece['seed']:.4f}. Verdict: {cal_verdict}.")
    L.append("")
    L.append(f"![Frozen model calibration, 2021-2026, {len(test)} games, with seed-pair rates](figures/{FIG_PATH.name})")
    L.append("")
    L.append("Reliability table, frozen model (x = P(better seed wins), 95% Wilson intervals, counts per bin):")
    L.append("")
    L.append(markdown_table(cal["frozen"], float_digits=3))
    L.append("")
    L.append("#### Upset detection, 2021-2026")
    L.append("Games between different seed lines where the worse seed won. recall = share of those upsets the "
             "method gave the underdog above 50%; precision = share of the method's underdog picks that came in.")
    L.append("")
    L.append(markdown_table(up_tab, float_digits=3))
    L.append("")
    L.append("### 4. Path-aware round probabilities")
    L.append("Each method's head-to-head probability for every possible pairing in the real bracket is pushed "
             "through the bracket exactly: P(reach round r+1) = P(reach r) x sum over opponents j in the other "
             "half of the block of P(j reaches r) x P(beats j). Games are assumed independent given the "
             "pre-tournament probabilities, which are frozen for the whole tournament. Scored against whether "
             "each of the 64 teams reached the round. No per-team regression on seed or stats.")
    L.append("")
    L.append("#### 2010-2026, existing model")
    L.append(markdown_table(adv_round, float_digits=4))
    L.append("")
    L.append("#### 2021-2026, frozen model")
    L.append(markdown_table(adv_te_round, float_digits=4))
    L.append("")
    L.append("### 5. External reference: T-Rank archived pregame line")
    L.append("Row position 54 of Torvik's `YYYY_super_sked.json`: the line published on the day of the game, "
             "integer percent (clipped to 0.5%-99.5%). The file's other prediction fields are recomputed from "
             "end-of-season ratings (each team carries one rating all season), so they include the tournament and "
             "are not used. The Round of 64 row is the closest to like-for-like.")
    L.append("")
    L.append(markdown_table(tk_tab, float_digits=4))
    L.append("")
    L.append("### 6. Trevor's earlier models")
    L.append(f"- madness.py family (port of madness.R: general ridge plus 14 seed-stratified ridge models): entered "
             f"as a candidate and scored in every table above as `madness`. As implemented it fits without an "
             f"intercept on standardised inputs, so predictions centre on zero inside each seed band and clip to 0: "
             f"{zero25:.0%} of the 2025 field is predicted at exactly 0 wins. The candidate restores the intercept.")
    if tv_round is not None:
        L.append(f"- 2025 notebook forecasts (`Python Scripts/newmadness.ipynb`, saved 2025-03-19 before the Round "
                 f"of 64; copy and manifest in `data/external/`): per-round classifiers give P(reach round) per "
                 f"team with no bracket path, so each round's probabilities do not sum to the teams that can reach "
                 f"it (`sum_p_trevor` vs `teams_that_reach`). Scored on {tv_ew['teams']} teams of the 2025 field"
                 + (f" ({len(tv_missing)} not matched: {', '.join(tv_missing)})" if tv_missing else "")
                 + ". One tournament: read it as an anecdote, not evidence.")
        L.append("")
        L.append(markdown_table(tv_round, float_digits=4))
        L.append("")
        L.append("Expected-wins squared error, 2025 field: " + ", ".join(
            f"{k} {v:.3f}" for k, v in tv_ew.items() if k != "teams") + ".")
    if tv24_ew is not None:
        L.append("")
        L.append(f"- 2024 madness.R forecast (`Documents/madness24model.csv`, saved 2024-03-18 before the First Four; "
                 f"copy and manifest in `data/external/`): expected tournament wins only, no per-round probabilities. "
                 f"Expected-wins squared error, {tv24_ew['teams']} teams of the 2024 field"
                 + (f" ({len(tv24_ew['missing'])} not matched: {', '.join(tv24_ew['missing'])})" if tv24_ew["missing"] else "")
                 + ": " + ", ".join(f"{k} {v:.3f}" for k, v in tv24_ew.items() if k not in ("teams", "missing"))
                 + ". One tournament: an anecdote, not evidence.")
    L.append("")
    L.append("## Interpretation")
    L.append(f"- Powered test: {supports(b_all, 'the existing model')}.")
    L.append(f"- Clean test: {supports(b_fz, 'the frozen model')}.")
    L.append(f"- Reference: {supports(b_tk, 'T-Rank game-day line')}. That is the size of edge a strong public "
             "rating reaches with game-day information.")
    if spec["base"] == "log5":
        L.append("- The frozen winner has no fitted parameter: it is Torvik's own regular-season Barthag pushed "
                 "through log5. So the edge over seed-pair rates is the edge of a good public rating over seeds; "
                 "none of the fitted candidates (ridge, madness.py, logistic, topic metrics, blends) beat it on "
                 "2010-2019.")
    L.append("- Prediction, not explanation: none of these coefficients is a causal claim about what wins games.")
    L.append("")
    L.append("## Method and validation")
    L.append("- Walk-forward: for tournament Y every fit (ridge feature-set choice and alpha, Bradley-Terry k, "
             "madness.py models, logistic coefficients and scaling, blend weight) uses seasons before Y only; "
             "seed-pair rates use tournaments 1985 to Y-1, Laplace smoothed. `tests/test_powered_walk_forward.py` "
             "flips every year-Y outcome and scrambles every later season, and requires identical year-Y predictions.")
    L.append("- Prediction moment: the published bracket. Games are scored conditional on the matchups that "
             "happened; round probabilities are pre-tournament and path-aware.")
    L.append("- Candidates were fixed before selection: Barthag log5 (raw and temperature-scaled), logistic on "
             "efficiency-margin difference, logistic on tempo-adjusted margin, L2 logistic on those differences "
             "plus seed difference, the same plus the nine topic-analysis metrics (C in 0.01, 0.1, 1, 10 for both), "
             "the existing ridge Bradley-Terry model, the madness.py family, and each of those blended as "
             "seed + w (model - seed) with w fit inside each fold on leave-one-season-out predictions.")
    L.append("- At most 13 coefficients on at least 126 games in any fold (the topic logistic in 2010, about 10 "
             "games per coefficient, which L2 shrinkage has to carry); the others keep to 4 or fewer.")
    L.append("")
    L.append("## Limits")
    L.append(f"- Tournaments, not games, are the independent unit: {n_years} blocks for the powered test and "
             f"{len(TEST_YEARS)} for the frozen test. A 6-block bootstrap is coarse; read its interval with the DM p.")
    L.append("- The existing model's feature list was written after 2021-2025 results were known (verification "
             "2026-09-29), so only 2026 is fully prospective for it. The frozen model's candidate list was fixed "
             "before its 2021-2026 scores, but it draws on well-known rating ideas and on topic analyses that "
             "had looked at every season, including 2021-2025, as descriptive work.")
    L.append("- The ridge model has 20 features; in 2010 it trains on 128 team rows, about 6 rows per feature, "
             "which is overfit territory.")
    L.append("- Games inside a tournament are treated as independent given the probabilities in the path "
             "calculation; ratings do not update during the tournament.")
    L.append("- T-Rank's line is rounded to whole percents and, after the Round of 64, sees tournament games.")
    L.append("")
    L.append("## Model card: frozen model")
    L.append(f"- Identity: `{fname}`, version = `reports/model_selection_frozen.json` sha256 `{frozen_sha}`, "
             f"frozen {frozen['frozen_at']}; owner Trevor White; status: evaluated, not approved for betting use.")
    L.append("- Intended use: pre-tournament game and round probabilities for NCAA men's tournament brackets and "
             "analysis posts. Prohibited: wagering decisions, in-tournament updating, women's or other tournaments.")
    L.append("- Target and timing: P(better seed wins) for each Round-of-64-onward game, from inputs known at the "
             "published bracket; horizon = the whole tournament, no updates.")
    L.append(f"- Data: {inputs} from the Torvik regular-season table (Section 3); seed-pair history 1985 to Y-1 "
             "for the baseline.")
    L.append("- Baselines: coin, constant, chalk 0.70 and 0.99, seed-pair rates, existing model, madness.py family, "
             "T-Rank line.")
    L.append(f"- Validation: walk-forward, selection on 2010-2019, one scoring pass on 2021-2026. Result: Brier "
             f"difference vs seed-pair {fmt_ci(b_fz)}; calibration: {cal_verdict}.")
    L.append("- Limits: Section Limits. Monitoring: re-score every new tournament with the frozen spec; retire it "
             "if the running Brier over the post-freeze tournaments is worse than seed-pair rates with the year-block "
             "CI above zero; any change to the spec is a new version with a new frozen file.")
    L.append("")
    L.append("## Deviations from the brief (stricter rule followed)")
    L.append("- One primary metric locked before fitting (Brier); selection runs on it, log loss is secondary "
             "(validation-design, sports-modeling-doctrine).")
    L.append("- Seed-pair history extended to 1985, so the baseline is not starved in the early walk-forward "
             "years; the 2008-onward version is reported as a sensitivity row.")
    L.append("- The baseline is logged in the selection but cannot be frozen, so the 2021-2026 test always tests a model.")
    L.append("- Coin-flip and constant baselines added (baseline-models Tier A).")
    L.append("- Round probabilities computed exactly instead of Monte Carlo, so no simulation seed is needed; the "
             f"bootstrap seed {BOOT_SEED} is recorded.")
    L.append("- No from-scratch Elo: Torvik's AdjOE/AdjDE (opponent-adjusted offense and defense ratings from "
             "regular-season games) and Barthag are the rating baselines; an Elo would need every regular-season "
             "game 2008-2026 and is the natural next candidate.")
    L.append("")
    L.append("## Appendix: prior CBB work, used or skipped")
    L.append("")
    L.append(markdown_table(pd.DataFrame(PRIOR_SOURCES, columns=["source", "status", "use or reason"])))
    L.append("")
    L.append("## Reproduction")
    L.append(f"- `python scripts/run_powered_comparison.py select` (charter locked {frozen['charter_locked_at']}, "
             f"frozen {frozen['frozen_at']}), then `python scripts/run_powered_comparison.py score` (run "
             f"{datetime.now(timezone.utc).isoformat(timespec='seconds')}); data refresh: "
             "`scripts/fetch_tournament_games.py`, `scripts/fetch_seed_history.py`.")
    L.append(f"- Frozen selection sha256 `{frozen_sha}`; code at selection `{frozen['code_revision']}`, at scoring "
             f"`{prov['code_revision']}`.")
    L.append(f"- Inputs (sha256 prefix, identical at selection and scoring): training `{data['paths']['training_csv']}` "
             f"`{data['hashes']['training_csv']}`; games `{data['paths']['games_csv']}` `{data['hashes']['games_csv']}`; "
             f"seed history `{data['paths']['history_csv']}` `{data['hashes']['history_csv']}`.")
    L.append(f"- Deterministic: bootstrap seed `{BOOT_SEED}`, `{N_BOOT:,}` year-block resamples; ridge random_state "
             f"`{args.random_state}`; madness.py KFold seed 42. Per-game and per-round predictions: "
             "`data/processed/powered_game_predictions_2010_2026.csv`, `data/processed/powered_round_probabilities_2010_2026.csv`.")
    REPORT_PATH.write_text("\n".join(L) + "\n", encoding="utf-8")
    ev.to_csv(REPO_ROOT / "data/processed/powered_game_predictions_2010_2026.csv", index=False)
    adv.to_csv(REPO_ROOT / "data/processed/powered_round_probabilities_2010_2026.csv", index=False)
    print(f"Report written: {REPORT_PATH}")
    print(f"model-seed Brier {fmt_ci(b_all)}; frozen-seed 2021-2026 Brier {fmt_ci(b_fz)}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("stage", choices=["select", "score"])
    parser.add_argument("--training-csv", default="data/processed/cbb_training_clean.csv")
    parser.add_argument("--games-csv", default="data/raw/tournament_games.csv")
    parser.add_argument("--history-csv", default="data/raw/seed_history_games.csv")
    parser.add_argument("--cv-folds", type=int, default=10)
    parser.add_argument("--random-state", type=int, default=42)
    parser.add_argument("--force", action="store_true", help="select only: overwrite a frozen selection")
    args = parser.parse_args()
    if set(TEAM_COLS) & OUTCOME_COLUMNS:
        raise SystemExit("outcome column in team inputs")
    return run_select(args) if args.stage == "select" else run_score(args)


if __name__ == "__main__":
    raise SystemExit(main())
