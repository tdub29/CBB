#!/usr/bin/env python3
"""Fetch 1985-2007 NCAA tournament results (Round of 64 onward) for seed-pair baselines.

Torvik's schedule files start in 2008, so the seed-pair baseline used to learn from 2008 onward
only, which leaves it starved in the early walk-forward years (two seasons of history in 2010).
Real seed-pair tables start in 1985, when the field went to 64. This pulls each year's bracket
from Sports Reference (one page per season, key-free) and keeps what the baseline needs: year,
round, date, seeds, teams and score. Play-in games (16 vs 16, 2001 onward) are dropped.

The parser is checked against Torvik on overlap seasons (default 2008 and 2019): every game's
round, winner seed and loser seed must match data/raw/tournament_games.csv, or nothing is written.

Output: data/raw/seed_history_games.csv plus data/raw/seed_history_games_manifest.json.
Team names are Sports Reference's; the file feeds seed-pair rates only, never team features.
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

URL = "https://www.sports-reference.com/cbb/postseason/men/{year}-ncaa.html"
# data/raw/seed_history_games.csv and its manifest are frozen inputs of the log5 test (src/frozen_inputs.py);
# a refetch lands in a dated folder for comparison and never replaces them.
OUT_DIR = REPO_ROOT / "data/raw/refetch" / datetime.now(timezone.utc).strftime("%Y%m%d")
OUT_CSV = OUT_DIR / "seed_history_games.csv"
OUT_MANIFEST = OUT_DIR / "seed_history_games_manifest.json"
TORVIK_GAMES = REPO_ROOT / "data/raw/tournament_games.csv"
TEAM_RE = re.compile(
    r'<span>(\d+)</span>\s*<a href="/cbb/schools/[^"]+">([^<]+)</a>\s*'
    r'<a href="/cbb/boxscores/((\d{4}-\d{2}-\d{2})-[^"]+)\.html">(\d+)</a>'
)


def parse_bracket(html: str, year: int) -> pd.DataFrame:
    teams: dict[str, dict[str, tuple[int, float]]] = {}
    dates: dict[str, str] = {}
    for seed, name, box, date, pts in TEAM_RE.findall(html):
        teams.setdefault(box, {})[name.strip()] = (int(seed), float(pts))
        dates[box] = date
    games = []
    for box, side in teams.items():
        if len(side) != 2:
            raise ValueError(f"{year}: game {box} has {len(side)} teams")
        (t1, (s1, p1)), (t2, (s2, p2)) = side.items()
        if s1 == s2 == 16:
            continue  # play-in / opening round
        if p1 == p2:
            raise ValueError(f"{year}: tie score in {box}")
        (w, ws, wp), (lo, ls, lp) = sorted([(t1, s1, p1), (t2, s2, p2)], key=lambda r: -r[2])
        games.append({"year": year, "date": dates[box], "winner": w, "loser": lo, "winner_seed": ws,
                      "loser_seed": ls, "winner_pts": wp, "loser_pts": lp})
    out = pd.DataFrame(games).sort_values(["date", "winner"], kind="stable").reset_index(drop=True)
    played: dict[str, int] = {}
    rounds = []
    for r in out.itertuples(index=False):
        nw, nl = played.get(r.winner, 0), played.get(r.loser, 0)
        if nw != nl:
            raise ValueError(f"{year}: {r.winner} and {r.loser} met with different games played")
        rounds.append(nw + 1)
        played[r.winner] = played[r.loser] = nw + 1
    out.insert(1, "round", rounds)
    out["no_contest"] = False
    counts = out["round"].value_counts().to_dict()
    if counts != {1: 32, 2: 16, 3: 8, 4: 4, 5: 2, 6: 1}:
        raise ValueError(f"{year}: round counts {counts}")
    return out


def fetch(year: int) -> str:
    req = urllib.request.Request(URL.format(year=year), headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=120) as resp:
        return resp.read().decode("utf-8", "replace")


def seed_signature(games: pd.DataFrame) -> list[tuple[int, int, int]]:
    return sorted(zip(games["round"].astype(int), games["winner_seed"].astype(int), games["loser_seed"].astype(int)))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--first-year", type=int, default=1985)
    parser.add_argument("--last-year", type=int, default=2007)
    parser.add_argument("--check-years", type=int, nargs="*", default=[2008, 2019])
    parser.add_argument("--delay", type=float, default=4.0, help="Seconds between requests (site limit 20/min).")
    args = parser.parse_args()

    torvik = pd.read_csv(TORVIK_GAMES)
    checks = {}
    for year in args.check_years:
        mine = parse_bracket(fetch(year), year)
        theirs = torvik[(torvik["year"] == year) & ~torvik["no_contest"].astype(bool)]
        ok = seed_signature(mine) == seed_signature(theirs)
        checks[str(year)] = "match" if ok else "MISMATCH"
        print(f"check {year}: {checks[str(year)]}", flush=True)
        if not ok:
            raise SystemExit(f"parser disagrees with Torvik on {year}; nothing written")
        time.sleep(args.delay)

    frames, per_year = [], {}
    for year in range(args.first_year, args.last_year + 1):
        games = parse_bracket(fetch(year), year)
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
        "filter": "Round of 64 onward; 16 vs 16 play-in games dropped",
        "use": "seed-pair baseline history only (team names are Sports Reference's)",
        "overlap_check_vs_torvik": checks,
        "rows": int(len(out)),
        "games_per_year": per_year,
        "sha256": hashlib.sha256(OUT_CSV.read_bytes()).hexdigest(),
    }, indent=2), encoding="utf-8")
    print(f"Wrote {OUT_CSV} ({len(out)} rows) and {OUT_MANIFEST}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
