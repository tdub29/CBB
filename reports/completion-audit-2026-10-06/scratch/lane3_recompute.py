"""Lane 3 independent recompute. Read-only: reads frozen inputs and saved predictions, writes JSON beside itself.

Recomputes log5 and seed-pair from raw inputs with its own code, then scores, bootstraps,
calibrates and plays committed brackets. Only bracket_from_games/committed_bracket are reused
(structure only; they raise if the rebuilt bracket disagrees with the games played).
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
from scipy import stats
from scipy.special import expit, logit

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT))
from src.models.tournament import bracket_from_games, committed_bracket  # noqa: E402

OUT = Path(__file__).with_name("lane3_results.json")
POINTS = {1: 10, 2: 20, 3: 40, 4: 80, 5: 160, 6: 320}
TEST = [2021, 2022, 2023, 2024, 2025, 2026]

tr = pd.read_csv(ROOT / "data/processed/cbb_training_clean.csv")
games = pd.read_csv(ROOT / "data/raw/tournament_games.csv")
hist = pd.read_csv(ROOT / "data/raw/seed_history_games.csv")
saved = pd.read_csv(ROOT / "data/processed/powered_game_predictions_2010_2026.csv")
bart = {(y, t): b for y, t, b in zip(tr["year"], tr["Team"], tr["Barthag"])}

allg = pd.concat([hist, games[hist.columns]], ignore_index=True)
allg = allg[~allg["no_contest"].astype(bool)]


def orient(df):
    wf = (df.winner_seed < df.loser_seed) | ((df.winner_seed == df.loser_seed) & (df.winner < df.loser))
    return pd.DataFrame({"year": df.year.values, "round": df["round"].values,
                         "a": np.where(wf, df.winner, df.loser), "b": np.where(wf, df.loser, df.winner),
                         "sa": np.where(wf, df.winner_seed, df.loser_seed).astype(int),
                         "sb": np.where(wf, df.loser_seed, df.winner_seed).astype(int),
                         "y": wf.astype(int).values})


def seed_rates(year):
    o = orient(allg[allg.year < year])
    o = o[o.sa != o.sb]
    t = o.groupby(["sa", "sb"]).y.agg(["sum", "count"])
    return {k: (r["sum"] + 1) / (r["count"] + 2) for k, r in t.iterrows()}


def p_seed(rates, sa, sb):
    if sa == sb:
        return 0.5
    lo, hi = min(sa, sb), max(sa, sb)
    p = rates.get((lo, hi), 0.5)
    return p if sa == lo else 1 - p


def p_log5(y, a, b):
    return float(expit(logit(bart[(y, a)]) - logit(bart[(y, b)])))


g = orient(games[~games["no_contest"].astype(bool) & (games.year >= 2010)])
rates = {y: seed_rates(y) for y in sorted(g.year.unique())}
g["p_log5"] = [p_log5(y, a, b) for y, a, b in zip(g.year, g.a, g.b)]
g["p_seedpair"] = [p_seed(rates[y], sa, sb) for y, sa, sb in zip(g.year, g.sa, g.sb)]

# 1. reproduction against saved predictions
m = g.merge(saved, left_on=["year", "round", "a", "b"], right_on=["year", "round", "team_a", "team_b"],
            how="outer", indicator=True)
res = {"rows_mine": len(g), "rows_saved": len(saved), "merge": m["_merge"].value_counts().to_dict(),
       "label_mismatch": int((m.y != m.a_won).sum()),
       "max_abs_log5_vs_saved_p_frozen": float((m.p_log5 - m.p_frozen).abs().max()),
       "max_abs_seedpair_vs_saved_p_seed": float((m.p_seedpair - m.p_seed).abs().max())}


def brier(p, y):
    return (np.asarray(p, float) - y) ** 2


def ll(p, y):
    q = np.clip(np.asarray(p, float), 1e-15, 1 - 1e-15)
    return -(y * np.log(q) + (1 - y) * np.log(1 - q))


def paired(df, pa, pb, loss, B=10000, seed=123):
    d = loss(df[pa].values, df.y.values) - loss(df[pb].values, df.y.values)
    yrs = df.year.values
    uy = np.unique(yrs)
    sums = np.array([d[yrs == u].sum() for u in uy])
    cnts = np.array([(yrs == u).sum() for u in uy])
    mean = d.mean()
    idx = np.random.default_rng(seed).integers(0, len(uy), size=(B, len(uy)))
    boot = sums[idx].sum(1) / cnts[idx].sum(1)
    G, n = len(uy), len(d)
    se = np.sqrt(G / (G - 1) * ((sums - cnts * mean) ** 2).sum()) / n
    t = mean / se
    return {"n": int(n), "years": int(G), "mean_diff": round(float(mean), 5),
            "ci": [round(float(np.percentile(boot, 2.5)), 5), round(float(np.percentile(boot, 97.5)), 5)],
            "dm_t": round(float(t), 3), "p_dm": round(float(2 * stats.t.sf(abs(t), G - 1)), 4),
            "years_model_better": int((sums < 0).sum())}


tt = g[g.year.isin(TEST)]
sm = m[m.year.isin(TEST)]
res["test_scores_2021_2026"] = {k: {"brier": round(float(brier(tt[k], tt.y).mean()), 5),
                                    "logloss": round(float(ll(tt[k], tt.y).mean()), 5)} for k in ["p_log5", "p_seedpair"]}
res["test_scores_2021_2026"]["p_trank_saved"] = {"brier": round(float(brier(sm.p_trank, sm.y).mean()), 5),
                                                 "n": int(sm.p_trank.notna().sum())}
res["paired_brier_log5_minus_seed_2021_2026"] = paired(tt, "p_log5", "p_seedpair", brier)
res["paired_logloss_log5_minus_seed_2021_2026"] = paired(tt, "p_log5", "p_seedpair", ll)
res["paired_brier_log5_minus_seed_2010_2019_selection"] = paired(g[g.year <= 2019], "p_log5", "p_seedpair", brier)
res["paired_brier_log5_minus_seed_2010_2026_all"] = paired(g, "p_log5", "p_seedpair", brier)

# 2. leave-one-tournament-out sensitivity of the test result
dt = brier(tt.p_log5, tt.y) - brier(tt.p_seedpair, tt.y)
res["drop_one_year_brier_diff_2021_2026"] = {int(y): round(float(dt[tt.year.values != y].mean()), 5) for y in TEST}


# 3. calibration: logistic recalibration slope/intercept on logit(p)
def recal(df, col):
    import statsmodels.api as sm_
    x = sm_.add_constant(logit(np.clip(df[col].values, 1e-6, 1 - 1e-6)))
    f = sm_.GLM(df.y.values, x, family=sm_.families.Binomial()).fit()
    ci = np.asarray(f.conf_int())
    return {"intercept": round(float(f.params[0]), 3), "slope": round(float(f.params[1]), 3),
            "slope_ci": [round(float(ci[1][0]), 3), round(float(ci[1][1]), 3)]}


try:
    res["recalibration_2021_2026"] = {c: recal(tt, c) for c in ["p_log5", "p_seedpair"]}
    res["recalibration_2010_2026"] = {c: recal(g, c) for c in ["p_log5", "p_seedpair"]}
except ImportError:
    res["recalibration"] = "statsmodels missing"

# 4. bracket utility: committed greedy brackets, ESPN 10-20-40-80-160-320
rows = []
for y in sorted(g.year.unique()):
    gy = games[games.year == y]
    br = bracket_from_games(gy)
    seed = dict(zip(br.Team, br.Seed))
    wins = gy.winner.value_counts().to_dict()  # no-contest winner (2021 Oregon) did advance
    fns = {"log5": lambda a, b, y=y: p_log5(y, a, b),
           "seed_pair": lambda a, b, y=y, s=seed: p_seed(rates[y], s[a], s[b]),
           "chalk": lambda a, b, s=seed: 1.0 if s[a] < s[b] else (0.0 if s[a] > s[b] else 0.5)}
    for name, fn in fns.items():
        picks = committed_bracket(br, fn)
        pts, hits = 0.0, {}
        for r, slots in picks.items():
            h = sum(w for s in slots for t, w in s.items() if wins.get(t, 0) >= r)
            hits[r] = h
            pts += h * POINTS[r]
        champ = max(picks[6][0].items(), key=lambda kv: kv[1])[0]
        rows.append({"year": int(y), "method": name, "points": pts, "r64_correct": hits[1],
                     "f4_correct": hits[4], "champ_correct": hits[6], "champ_pick": champ})
bk = pd.DataFrame(rows)
piv = bk.pivot(index="year", columns="method", values="points")
res["bracket_points_by_year"] = piv.reset_index().to_dict("records")
res["bracket_champ_by_year"] = bk.pivot(index="year", columns="method", values="champ_pick").reset_index().to_dict("records")
for span, yrs in [("2021_2026", TEST), ("2010_2026", list(piv.index))]:
    p = piv.loc[yrs]
    dd = (p.log5 - p.seed_pair).values
    bt = np.random.default_rng(123).integers(0, len(dd), (10000, len(dd)))
    sub = bk[bk.year.isin(yrs)]
    res[f"bracket_{span}"] = {
        "mean_points": p.mean().round(1).to_dict(),
        "log5_minus_seed_pair_mean": round(float(dd.mean()), 1),
        "ci": [round(float(np.percentile(dd[bt].mean(1), 2.5)), 1), round(float(np.percentile(dd[bt].mean(1), 97.5)), 1)],
        "years_log5_more": int((dd > 0).sum()), "years_tied": int((dd == 0).sum()), "years": len(dd),
        "sum_champ_correct": sub.groupby("method").champ_correct.sum().to_dict(),
        "sum_f4_correct": sub.groupby("method").f4_correct.sum().to_dict(),
        "sum_r64_correct": sub.groupby("method").r64_correct.sum().to_dict(),
    }
dis = (tt.p_log5 > 0.5) != (tt.p_seedpair > 0.5)
res["pick_disagreement_share_2021_2026"] = round(float(dis.mean()), 3)

OUT.write_text(json.dumps(res, indent=1, default=str))
print(json.dumps(res, indent=1, default=str))
