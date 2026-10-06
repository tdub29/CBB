"""Selection Sunday log5 round probabilities for one bracket, before any game is played.

The frozen model (reports/model_selection_frozen.json) is log5 on Barthag:
P(a beats b) = expit(logit(Barthag_a) - logit(Barthag_b)). This command applies it to a bracket with the
same exact path math the powered comparison scored (run_powered_comparison.advancement).

Modes
  --bracket CSV, 68 teams   Selection Sunday. Each First Four pair shares a seed line and region, and either
                            team fills the slot with its log5 P(win play-in). The four play-ins are
                            independent, so the round probabilities are the weighted sum of advancement()
                            over the 16 possible 64-team fields. That is exact, not simulated.
  --bracket CSV, 64 teams   a field with no First Four left to play.
  --from-games              the 64-team field rebuilt from tournament_games.csv. It needs to know who won the
                            First Four, so its output is labelled post_first_four: these are the round
                            probabilities the powered report scored, not a Selection Sunday run.
  --check                   acceptance for --year: --from-games reproduces the saved p_frozen game rows and
                            round probabilities to 1e-12, and the 68-team run is a valid distribution that
                            equals the saved one once the First Four winners are fixed.

Inputs are pre-tournament only: Barthag from the training CSV (team_features enforces the prediction-time
contract) and Team/Seed/Region from the bracket CSV. No outcome column is read.

    python scripts/predict_selection_sunday.py --year 2026 \
        --bracket data/raw/torvik_2026/selection_sunday_bracket_2026.csv --semis East-South,West-Midwest
    python scripts/predict_selection_sunday.py --year 2026 --check \
        --bracket data/raw/torvik_2026/selection_sunday_bracket_2026.csv --semis East-South,West-Midwest
"""

from __future__ import annotations

import argparse
import itertools
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from scripts.run_powered_comparison import advancement, fit_base, pair_features, team_features  # noqa: E402
from src.frozen_inputs import write_csv  # noqa: E402
from src.models.tournament import bracket_from_games  # noqa: E402

FROZEN_JSON = REPO_ROOT / "reports/model_selection_frozen.json"
SAVED_ROUNDS = REPO_ROOT / "data/processed/powered_round_probabilities_2010_2026.csv"
SAVED_GAMES = REPO_ROOT / "data/processed/powered_game_predictions_2010_2026.csv"
ROUND_COLS = [f"p_round{r}" for r in range(7)]  # round 0 = the First Four (1.0 for a team with a bye)
TOL = 1e-12


def frozen_log5():
    """P(team_a wins) for a pair_features frame. Refuses to run if the frozen choice is not log5."""
    frozen = json.loads(FROZEN_JSON.read_text())
    if frozen["chosen_name"] != "log5":
        raise SystemExit(f"frozen model is {frozen['chosen_name']!r}; this command implements log5 only")
    return fit_base(frozen["chosen"], train=None)  # log5 has no parameters, so nothing is fit


def read_bracket(path: str | Path) -> pd.DataFrame:
    b = pd.read_csv(path, usecols=["Team", "Seed", "Region"])  # structure only, never an outcome column
    return b.assign(Team=b["Team"].str.strip(), Region=b["Region"].str.strip(), Seed=b["Seed"].astype(int))


def check_shape(b: pd.DataFrame) -> None:
    """64 teams, or 68 with exactly four (region, seed) slots held by two teams."""
    full = pd.MultiIndex.from_product([sorted(b["Region"].unique()), range(1, 17)])
    counts = b.groupby(["Region", "Seed"]).size().reindex(full, fill_value=0)
    if (b["Team"].duplicated().any() or len(full) != 64 or len(b) not in (64, 68)
            or not counts.between(1, 2).all() or (counts == 2).sum() != len(b) - 64):
        raise ValueError(f"not a 64- or 68-team bracket: {len(b)} teams, odd slots {counts[counts != 1].to_dict()}")


def label_regions(b: pd.DataFrame, semis: str) -> tuple[pd.DataFrame, dict[str, str]]:
    """Map region names to A-D so the national semifinals are A-B and C-D, as advancement() expects."""
    pairs = [tuple(s.strip() for s in p.split("-")) for p in semis.split(",")]
    order = [r for p in pairs for r in p]
    if len(pairs) != 2 or any(len(p) != 2 for p in pairs) or sorted(order) != sorted(b["Region"].unique()):
        raise ValueError(f"--semis {semis!r} must pair the bracket's four regions {sorted(b['Region'].unique())}")
    return b.assign(Region=b["Region"].map(dict(zip(order, "ABCD")))), dict(zip("ABCD", order))


def check_teams(b: pd.DataFrame, year: int, teams: pd.DataFrame) -> None:
    """Every bracket team joins to exactly one training row for `year` with a usable Barthag."""
    known = teams.xs(year, level="year") if year in teams.index.get_level_values("year") else teams.iloc[:0]
    missing = sorted(set(b["Team"]) - set(known.index))
    if missing:
        raise ValueError(f"{year}: bracket teams not in the training CSV: {missing}")
    if known.index.duplicated().any():
        raise ValueError(f"{year}: duplicate team rows in the training CSV")
    bart = known.loc[b["Team"], "Barthag"]
    if not bart.between(0, 1, inclusive="neither").all():
        raise ValueError(f"{year}: Barthag missing or outside (0, 1): {bart[~bart.between(0, 1)].to_dict()}")


def predict(b: pd.DataFrame, year: int, teams: pd.DataFrame, fn) -> pd.DataFrame:
    """P(team wins its round-r game) for an A-D labelled 64- or 68-team bracket, r = 0..6."""
    check_shape(b)
    check_teams(b, year, teams)
    first_four = [ts for ts in b.groupby(["Region", "Seed"])["Team"].apply(list) if len(ts) == 2]
    p_play = {}
    for a, c in first_four:
        p = float(fn(pair_features(pd.DataFrame({"year": [year], "team_a": [a], "team_b": [c]}), teams))[0])
        p_play[a], p_play[c] = p, 1 - p
    total = None
    for winners in itertools.product(*first_four):  # one empty tuple when there is no First Four
        losers = {t for pair in first_four for t in pair} - set(winners)
        w = float(np.prod([p_play[t] for t in winners]))
        adv = advancement(b[~b["Team"].isin(losers)], fn, year, teams).set_index(["Team", "round"])["p"]
        adv = adv if not first_four else adv * w  # 64 teams: the saved numbers, untouched
        total = adv if total is None else total.add(adv, fill_value=0.0)
    wide = total.unstack("round").rename(columns=lambda r: f"p_round{r}")
    wide.insert(0, "p_round0", [p_play.get(t, 1.0) for t in wide.index])
    out = b.merge(wide, left_on="Team", right_index=True, how="left")
    out.insert(0, "year", year)
    validate(out)
    return out


def validate(out: pd.DataFrame) -> None:
    p = out[ROUND_COLS].to_numpy()
    want = [64] + [2 ** (6 - r) for r in range(1, 7)]
    if np.isnan(p).any() or (p < 0).any() or (p > 1).any():
        raise ValueError("round probability missing or outside [0, 1]")
    if not np.allclose(p.sum(0), want, rtol=0, atol=1e-9):
        raise ValueError(f"round sums {p.sum(0)} != winners per round {want}")
    if (np.diff(p, axis=1) > 1e-15).any():
        raise ValueError("a team's chance rises from one round to the next")


def selection_sunday(b: pd.DataFrame, semis: str, year: int, teams: pd.DataFrame, fn) -> pd.DataFrame:
    labelled, names = label_regions(b, semis)
    out = predict(labelled, year, teams, fn)
    out["Region"] = out["Region"].map(names)
    return out.assign(timing="selection_sunday" if len(out) == 68 else "64_team_field")


def post_first_four(games: pd.DataFrame, year: int, teams: pd.DataFrame, fn) -> pd.DataFrame:
    """The bracket the powered report scored. Uses who won the First Four, so it is not pre-tournament."""
    out = predict(bracket_from_games(games[games["year"] == year]), year, teams, fn)
    return out.assign(timing="post_first_four")


def run_check(args, teams, fn) -> int:
    games = pd.read_csv(REPO_ROOT / args.games_csv)
    post = post_first_four(games, args.year, teams, fn)
    saved = pd.read_csv(SAVED_ROUNDS)
    saved = saved[saved["year"] == args.year]
    long = post.melt(id_vars="Team", value_vars=ROUND_COLS[1:], var_name="round", value_name="p")
    long["round"] = long["round"].str[-1].astype(int)
    m = saved.merge(long, on=["Team", "round"], how="outer", indicator=True)
    sg = pd.read_csv(SAVED_GAMES)
    sg = sg[sg["year"] == args.year]
    p_games = fn(pair_features(sg[["year", "team_a", "team_b"]].reset_index(drop=True), teams))
    rep = {
        "round_rows_saved": len(saved), "round_rows_joined": int((m["_merge"] == "both").sum()),
        "max_abs_round_vs_saved_p_frozen": float((m["p"] - m["p_frozen"]).abs().max()),
        "game_rows_saved": len(sg),
        "max_abs_game_vs_saved_p_frozen": float(np.abs(p_games - sg["p_frozen"].to_numpy()).max()),
    }
    ok = (len(saved) == 64 * 6 and rep["round_rows_joined"] == len(saved) and len(sg) > 0
          and rep["max_abs_round_vs_saved_p_frozen"] <= TOL and rep["max_abs_game_vs_saved_p_frozen"] <= TOL)
    if args.bracket:
        field = read_bracket(args.bracket)
        sel = selection_sunday(field, args.semis, args.year, teams, fn)
        # fix the First Four at its real winners: must give the saved post-First-Four numbers
        fixed = selection_sunday(field[field["Team"].isin(post["Team"])], args.semis, args.year, teams, fn)
        d = fixed.set_index("Team")[ROUND_COLS[1:]] - post.set_index("Team")[ROUND_COLS[1:]]
        rep.update({"selection_sunday_teams": len(sel), "first_four_slots": int((sel["p_round0"] < 1).sum() // 2),
                    "max_abs_fixed_first_four_vs_post_first_four": float(d.abs().max().max())})
        ok = ok and len(sel) == 68 and rep["first_four_slots"] == 4 and len(d) == 64 \
            and rep["max_abs_fixed_first_four_vs_post_first_four"] <= TOL
    print(json.dumps(rep, indent=1))
    print("CHECK PASS" if ok else "CHECK FAIL")
    return 0 if ok else 1


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--year", type=int, required=True)
    ap.add_argument("--bracket", help="Team,Seed,Region CSV (64 or 68 teams); other columns are ignored")
    ap.add_argument("--semis", help="national semifinal pairing of region names, e.g. East-South,West-Midwest")
    ap.add_argument("--from-games", action="store_true", help="post-First-Four field rebuilt from the games")
    ap.add_argument("--check", action="store_true", help="acceptance check against the saved p_frozen")
    ap.add_argument("--training-csv", default="data/processed/cbb_training_clean.csv")
    ap.add_argument("--games-csv", default="data/raw/tournament_games.csv")
    ap.add_argument("--out", help="output CSV (default data/processed/selection_sunday/log5_<year>_<timing>.csv)")
    args = ap.parse_args(argv)
    if args.bracket and not args.semis:
        ap.error("--bracket needs --semis")
    if not (args.bracket or args.from_games or args.check):
        ap.error("give --bracket, --from-games or --check")

    teams = team_features(pd.read_csv(REPO_ROOT / args.training_csv))
    fn = frozen_log5()
    if args.check:
        return run_check(args, teams, fn)
    if args.from_games:
        out = post_first_four(pd.read_csv(REPO_ROOT / args.games_csv), args.year, teams, fn)
    else:
        out = selection_sunday(read_bracket(args.bracket), args.semis, args.year, teams, fn)
    timing = out["timing"].iloc[0]
    path = write_csv(out, args.out or REPO_ROOT / f"data/processed/selection_sunday/log5_{args.year}_{timing}.csv")
    top = out.sort_values("p_round6", ascending=False).head(8)
    print(f"{args.year} log5 round probabilities ({timing}, {len(out)} teams) -> {path}")
    print(top[["Team", "Seed", "Region", "p_round4", "p_round6"]].to_string(index=False, float_format="%.3f"))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
