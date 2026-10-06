"""Sensitivity: the frozen model's 2021-2026 test with 2026 inputs refreshed to Selection Sunday.

The scored 2026 rows come from a 2026-03-05 export that predates the conference tournaments, while
every earlier season's row runs through them. This re-scores the frozen model (log5 on Barthag, no
fitted parameter) with 2026 Barthag/AdjOE/AdjDE/WINS/GAMES replaced by Torvik's table for games through
2026-03-15. The same pull for 2025 (through 2025-03-16) reproduces the training rows (games identical,
Barthag within 0.002), which is the check that the pull matches how every other season was built.
It does not touch the frozen selection or the pre-registered result; it is reported beside it.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path
from types import SimpleNamespace

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT))
import scripts.run_powered_comparison as rpc  # noqa: E402

PULL = REPO_ROOT / "data/external/torvik_2026_selection_sunday"
COLS = {1: "AdjOE", 2: "AdjDE", 3: "Barthag", 5: "WINS", 6: "GAMES"}  # trank.php csv=1 positions


def refreshed_training(year: int, pull: Path) -> pd.DataFrame:
    raw = pd.read_csv(REPO_ROOT / "data/processed/cbb_training_clean.csv")
    t = pd.read_csv(pull, header=None).set_index(0)
    rows = raw["year"] == year
    missing = sorted(set(raw.loc[rows & pd.to_numeric(raw["seed"], errors="coerce").notna(), "Team"]) - set(t.index))
    if missing:
        raise SystemExit(f"seeded {year} teams missing from the pull: {missing}")
    for pos, col in COLS.items():
        raw.loc[rows, col] = raw.loc[rows, "Team"].map(t[pos]).fillna(raw.loc[rows, col]).to_numpy()
    return raw


def frozen_test(raw: pd.DataFrame) -> dict:
    out = REPO_ROOT / "data/interim/_sensitivity_training.csv"
    raw.to_csv(out, index=False)
    try:
        args = SimpleNamespace(training_csv=str(out), games_csv="data/raw/tournament_games.csv",
                               history_csv="data/raw/seed_history_games.csv", cv_folds=10, random_state=42)
        data = rpc.load_inputs(args)
    finally:
        out.unlink()
    spec = json.loads(rpc.FROZEN_PATH.read_text(encoding="utf-8"))["chosen"]
    gl, res = data["gl"], data["results"]
    test = gl[gl["year"].isin(rpc.TEST_YEARS)].copy()
    test["p_frozen"] = pd.concat([pd.Series(rpc.predict_games(y, spec, gl, res)[0]) for y in rpc.TEST_YEARS]).to_numpy()
    test["p_seed"] = pd.concat([pd.Series(rpc.predict_games(y, {"base": "seed_pair"}, gl, res)[0]) for y in rpc.TEST_YEARS]).to_numpy()
    y = test["a_won"].to_numpy()
    d = rpc.LOSSES["brier"](test["p_frozen"], y) - rpc.LOSSES["brier"](test["p_seed"], y)
    return rpc.paired_stats(d, test["year"], rpc.MEANINGFUL["brier"])


def main() -> int:
    base = frozen_test(pd.read_csv(REPO_ROOT / "data/processed/cbb_training_clean.csv"))
    fresh = frozen_test(refreshed_training(2026, PULL / "trank_2026_through_20260315.csv"))
    for name, s in (("as scored (2026-03-05 export)", base), ("2026 refreshed to Selection Sunday", fresh)):
        print(f"{name}: Brier diff {s['mean']:+.4f} [{s['ci_low']:+.4f}, {s['ci_high']:+.4f}]")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
