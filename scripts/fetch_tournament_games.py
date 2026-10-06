#!/usr/bin/env python3
"""Fetch NCAA tournament game results (Round of 64 onward) from Bart Torvik's bulk schedule.

One request per season to https://barttorvik.com/YYYY_super_sked.json (a documented bulk
endpoint, see docs/data_sources.md). Keeps only postseason games (type code 3) where both
teams are in that season's 64-team field from data/processed/cbb_training_clean.csv, which
drops First Four, NIT and CBI games. Round is the game's position in each team's tournament
run, and every season is checked against the field's recorded wins before it is written.

From 2015 the rows also carry T-Rank's archived pregame line (row position 54), kept as an
external reference: `trank_p_winner` is its probability that the eventual winner wins. It is
the line published the day of the game, so from the Round of 32 on it has seen earlier
tournament games. The live prediction fields (positions 4, 52, 53) are NOT used: they are
recomputed from end-of-season ratings, which include the tournament.

Output: data/raw/tournament_games.csv plus data/raw/tournament_games_manifest.json.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import time
import urllib.request
from datetime import datetime, timezone
import sys
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))
from src.frozen_inputs import assert_writable, write_csv  # noqa: E402

URL = "https://barttorvik.com/{year}_super_sked.json"
# data/raw/tournament_games.csv and its manifest are frozen inputs of the log5 test (src/frozen_inputs.py);
# a refetch lands in a dated folder for comparison and never replaces them.
OUT_DIR = REPO_ROOT / "data/raw/refetch" / datetime.now(timezone.utc).strftime("%Y%m%d")
OUT_CSV = OUT_DIR / "tournament_games.csv"
OUT_MANIFEST = OUT_DIR / "tournament_games_manifest.json"
POSTSEASON_TYPE = 3
# super_sked row layout (positional): 1 date, 6 game type, 8 team1, 14 team2,
# 27 team1 points, 28 team2 points, 29 winner, 30 loser
I_DATE, I_TYPE, I_T1, I_T2, I_P1, I_P2, I_WIN, I_LOSE = 1, 6, 8, 14, 27, 28, 29, 30
I_ARCHIVE = 54  # [key, date, "Fav -5.8, 77-71 (71%)", team1 predicted pts, team2 predicted pts, ...]
LINE_RE = re.compile(r"^(.*) -?\d+(?:\.\d+)?, \d+-\d+ \((\d+)%\)$")


def archived_p_team1(g: list) -> tuple[float, str]:
    """T-Rank's archived pregame P(team1 wins) and the raw line; (nan, "") when absent.

    The favourite is the side with the higher predicted score; the stated percent is an
    integer, so it is clipped to [0.005, 0.995].
    """
    arch = g[I_ARCHIVE] if len(g) > I_ARCHIVE and isinstance(g[I_ARCHIVE], list) else []
    m = LINE_RE.match(str(arch[2]).strip()) if len(arch) > 4 else None
    if not m:
        return float("nan"), ""
    p_fav = min(max(int(m.group(2)) / 100, 0.005), 0.995)
    t1_fav = float(arch[3]) >= float(arch[4])
    if m.group(1) in (g[I_T1], g[I_T2]) and (m.group(1) == g[I_T1]) != t1_fav and p_fav != 0.5:
        raise ValueError(f"archived line names {m.group(1)} but predicted scores favour the other side: {arch[:5]}")
    return (p_fav if t1_fav else 1 - p_fav), str(arch[2])


def field_by_year(training_csv: Path) -> dict[int, pd.DataFrame]:
    df = pd.read_csv(training_csv, usecols=["Team", "year", "seed", "round"])
    df = df.dropna(subset=["seed", "round"])
    return {int(y): g.set_index("Team") for y, g in df.groupby("year")}


def extract_games(rows: list[list], field: pd.DataFrame, year: int) -> pd.DataFrame:
    teams = set(field.index)
    games = []
    for g in rows:
        if g[I_TYPE] != POSTSEASON_TYPE or g[I_T1] not in teams or g[I_T2] not in teams:
            continue
        if not g[I_WIN]:
            continue  # scheduled, not played
        winner, loser = g[I_WIN], g[I_LOSE]
        p1, p2 = float(g[I_P1]), float(g[I_P2])
        w_pts, l_pts = (p1, p2) if g[I_T1] == winner else (p2, p1)
        p_t1, line = archived_p_team1(g)
        games.append({
            "year": year,
            "date": datetime.strptime(g[I_DATE], "%m/%d/%y").date().isoformat(),
            "winner": winner, "loser": loser,
            "winner_seed": int(field.at[winner, "seed"]), "loser_seed": int(field.at[loser, "seed"]),
            "winner_pts": w_pts, "loser_pts": l_pts,
            "trank_p_winner": p_t1 if g[I_T1] == winner else 1 - p_t1, "trank_pregame_line": line,
        })
    out = pd.DataFrame(games).sort_values("date", kind="stable").reset_index(drop=True)
    out["no_contest"] = False
    played: dict[str, int] = {}
    rounds, walkovers = [], []
    for r in out.itertuples(index=False):
        nw, nl = played.get(r.winner, 0), played.get(r.loser, 0)
        if nw != nl:
            # 2021 Oregon-VCU was declared a no-contest (COVID); Oregon advanced without playing.
            behind = r.winner if nw < nl else r.loser
            idle = sorted(teams - set(out["winner"]) - set(out["loser"]))
            if abs(nw - nl) != 1 or len(idle) != 1:
                raise ValueError(f"{year}: {r.winner} and {r.loser} met with different games played")
            walkovers.append({**out.iloc[0].to_dict(), "round": min(nw, nl) + 1, "winner": behind,
                              "loser": idle[0], "winner_seed": int(field.at[behind, "seed"]),
                              "loser_seed": int(field.at[idle[0], "seed"]),
                              "winner_pts": float("nan"), "loser_pts": float("nan"), "no_contest": True,
                              "trank_p_winner": float("nan"), "trank_pregame_line": ""})
            played[behind] = max(nw, nl)
        n = max(nw, nl)
        rounds.append(n + 1)
        played[r.winner] = played[r.loser] = n + 1
    out.insert(1, "round", rounds)
    if walkovers:
        out = pd.concat([out, pd.DataFrame(walkovers)[out.columns]], ignore_index=True)
        out = out.sort_values(["round", "date"], kind="stable").reset_index(drop=True)
    return out


def check_season(games: pd.DataFrame, field: pd.DataFrame, year: int) -> None:
    counts = games["round"].value_counts().to_dict()
    expected = {1: 32, 2: 16, 3: 8, 4: 4, 5: 2, 6: 1}
    if counts != expected:
        raise ValueError(f"{year}: round counts {counts} != {expected}")
    wins = games["winner"].value_counts().reindex(field.index, fill_value=0)
    bad = wins[wins != field["round"].astype(int)]
    if not bad.empty:
        raise ValueError(f"{year}: wins disagree with recorded round for {list(bad.index)[:5]}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--training-csv", default=str(REPO_ROOT / "data/processed/cbb_training_clean.csv"))
    parser.add_argument("--delay", type=float, default=2.0, help="Seconds between requests.")
    args = parser.parse_args()

    fields = field_by_year(Path(args.training_csv))
    frames, per_year = [], {}
    for year in sorted(fields):
        url = URL.format(year=year)
        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req, timeout=120) as resp:
            rows = json.loads(resp.read())
        games = extract_games(rows, fields[year], year)
        check_season(games, fields[year], year)
        frames.append(games)
        per_year[year] = len(games)
        print(f"{year}: {len(games)} games ok", flush=True)
        time.sleep(args.delay)

    out = pd.concat(frames, ignore_index=True)
    assert_writable(OUT_MANIFEST)
    write_csv(out, OUT_CSV)
    OUT_MANIFEST.write_text(json.dumps({
        "source": URL.format(year="YYYY"),
        "retrieved_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "filter": "game type 3 with both teams in the 64-team field of cbb_training_clean.csv",
        "trank_p_winner": "row position 54, T-Rank's archived pregame line (published the day of the game; "
                          "present from 2015); positions 4/52/53 are end-of-season recomputations and unused",
        "trank_lines_per_year": {str(y): int(n) for y, n in out.groupby("year")["trank_p_winner"].count().items()},
        "rows": int(len(out)),
        "games_per_year": per_year,
        "sha256": hashlib.sha256(OUT_CSV.read_bytes()).hexdigest(),
    }, indent=2), encoding="utf-8")
    print(f"Wrote {OUT_CSV} ({len(out)} rows) and {OUT_MANIFEST}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
