#!/usr/bin/env python3
"""Layer the 2026 NCAA tournament into the raw data (seeds, finish, postseason record).

Why this exists: on 2026-09-29 every Torvik .php page (trank.php, team-tables_each.php,
team-history.php) returned the JS "Verifying Browser" interstitial to requests and
403 Forbidden to headless Playwright, so the madness.py scraper returned 0 rows for 2026.
The static Torvik files still serve, so this script uses:

  * data/raw/torvik_2026/2026_super_sked.csv   (barttorvik.com/2026_super_sked.csv)
      game-level 2026 schedule/results; game type 2 = conference tournament,
      3 = postseason (NCAA, NIT). Used for postseason record and to cross-check every
      NCAA game result.
  * data/raw/torvik_2026/wikipedia_2026_ncaa_tournament.wikitext
      the official bracket (seed line, region, every score), from Wikipedia raw wikitext.

Regular-season features stay the on-disk Torvik team-tables export
data/raw/trank_team_table_data2026.csv (type R, pulled 2026-03-05, before Selection Sunday).

Writes (all idempotent):
  data/raw/torvik_2026/ncaa_2026_results.csv       one row per 2026 NCAA team
  data/raw/torvik_2026/postseason_rec_2026.csv      Team, Year, Postseason rec (P-table meaning)
  data/raw/team_history_cache.csv                   2026 rows get seed + finish
  data/raw/2026_expected_seeds_update.csv           actual seed line (was bracketology)
  data/raw/2026 expected seeds.xlsx                 actual seed line (was bracketology)
  data/raw/torvik_2026/bracketology_archive/        the replaced bracketology files, kept
"""
from __future__ import annotations

import re
import shutil
import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
SRC = RAW / "torvik_2026"
WIKI = SRC / "wikipedia_2026_ncaa_tournament.wikitext"
SKED = SRC / "2026_super_sked.csv"
CACHE = RAW / "team_history_cache.csv"
SEEDS_CSV = RAW / "2026_expected_seeds_update.csv"
SEEDS_XLSX = RAW / "2026 expected seeds.xlsx"
ARCHIVE = SRC / "bracketology_archive"
YEAR = 2026

# Wikipedia display name -> Torvik name, where the "State" -> "St." rule is not enough.
NAME_MAP = {
    "UConn": "Connecticut",
    "NC State": "N.C. State",
    "Miami (OH)": "Miami OH",
    "Miami (FL)": "Miami FL",
    "Hawaiʻi": "Hawaii",
    "Hawai'i": "Hawaii",
    "Hawai’i": "Hawaii",
    "McNeese": "McNeese St.",
    "California Baptist": "Cal Baptist",
    "Ole Miss": "Mississippi",
    "Saint Mary's": "Saint Mary's",
    "St. Mary's": "Saint Mary's",
    "Long Island": "LIU",
}
# Main-bracket wins -> Torvik finish label (First Four losers are R68).
FINISH_BY_WINS = {0: "R64", 1: "R32", 2: "Sweet Sixteen", 3: "Elite Eight", 4: "Final Four", 5: "Finals", 6: "CHAMPS"}


def _clean_team(raw: str) -> str:
    s = re.sub(r"\{\{[^}]*\}\}|<[^>]+>|'''?", "", raw).strip()
    m = re.search(r"\[\[(?:[^|\]]*\|)?([^\]]+)\]\]", s)
    return (m.group(1) if m else s).strip()


def _to_torvik(name: str, known: set[str]) -> str:
    for cand in (NAME_MAP.get(name, name), re.sub(r" State$", " St.", name)):
        if cand in known:
            return cand
    raise ValueError(f"No Torvik name for bracket team {name!r}")


def parse_bracket(text: str) -> tuple[list[dict], list[dict]]:
    """Return (entries, games). entries: team/seed/region from the first round; games: winner/loser/scores."""
    body = text[text.index("==Tournament bracket=="): text.index("==Record by conference==")]
    entries, games = [], []
    region = None
    for block in re.finditer(r"====?\s*([^=\n]+?)\s*====?\n|\{\{(2|4|16)TeamBracket(.*?)\n\}\}", body, flags=re.S):
        if block.group(1):
            head = block.group(1)
            region = "First Four" if head.startswith("First Four") else head.split(" ")[0]
            continue
        kind, inner = block.group(2), block.group(3)
        slots: dict[tuple[int, int], dict] = {}
        for rd, field, idx, val in re.findall(r"RD(\d)-(seed|team|score)(\d+)\s*=\s*((?:\[\[[^\]]*\]\]|[^\n|])*)", inner):
            slots.setdefault((int(rd), int(idx)), {})[field] = val.strip()
        for (rd, idx), s in sorted(slots.items()):
            if idx % 2 == 0:
                continue
            a, b = s, slots[(rd, idx + 1)]
            ta, tb = _clean_team(a["team"]), _clean_team(b["team"])
            sa, sb = (int(re.match(r"\d+",_clean_team(x["score"])).group()) for x in (a, b))
            ff = kind == "2"
            games.append({"round": "First Four" if ff else (f"{region} RD{rd}" if kind == "16" else f"Final Four RD{rd}"),
                          "winner": ta if sa > sb else tb, "loser": tb if sa > sb else ta,
                          "w_score": max(sa, sb), "l_score": min(sa, sb), "first_four": ff})
            if (kind == "16" and rd == 1) or ff:
                for t in (a, b):
                    entries.append({"bracket_name": _clean_team(t["team"]), "seed": int(_clean_team(t["seed"])),
                                    "region": region if kind == "16" else None, "first_four": ff})
    return entries, games


def load_sked() -> pd.DataFrame:
    import csv
    rows = list(csv.reader(SKED.open(encoding="utf-8")))
    # cols: 1 date, 6 game type, 8 team1, 14 team2, 28/27 not needed; 24 "Winner, W-L" text
    out = pd.DataFrame({"date": [r[1] for r in rows], "gtype": [r[6] for r in rows],
                        "t1": [r[8] for r in rows], "t2": [r[14] for r in rows], "result": [r[24] for r in rows]})
    parsed = out["result"].str.extract(r"^(?P<winner>.+), (?P<w>\d+)-(?P<l>\d+)$")
    return pd.concat([out, parsed], axis=1)


def main() -> int:
    cache = pd.read_csv(CACHE)
    known = set(cache.loc[cache["year"] == YEAR, "team"].astype(str))
    entries, games = parse_bracket(WIKI.read_text(encoding="utf-8"))
    for g in games:
        g["winner"], g["loser"] = _to_torvik(g["winner"], known), _to_torvik(g["loser"], known)
    field = pd.DataFrame(entries)
    field["team"] = field["bracket_name"].map(lambda n: _to_torvik(n, known))
    ff = field[field["first_four"]].set_index("team")
    main_field = field[~field["first_four"]].drop_duplicates("team").set_index("team")
    # First Four winners appear in RD1 of their region; First Four losers need their region from the winner's slot.
    ff_games = [g for g in games if g["first_four"]]
    for g in ff_games:
        main_field.loc[g["loser"]] = {"bracket_name": ff.loc[g["loser"], "bracket_name"], "seed": int(ff.loc[g["loser"], "seed"]),
                                      "region": main_field.loc[g["winner"], "region"], "first_four": True}
        main_field.loc[g["winner"], "first_four"] = True
    res = main_field.reset_index()
    wins = pd.Series([g["winner"] for g in games if not g["first_four"]]).value_counts()
    res["ncaa_wins"] = res["team"].map(wins).fillna(0).astype(int)
    ff_losers = {g["loser"] for g in ff_games}
    res["finish"] = [("R68" if t in ff_losers else FINISH_BY_WINS[w]) for t, w in zip(res["team"], res["ncaa_wins"])]
    res["year"] = YEAR

    # ---- checks on the bracket itself ----
    assert len(res) == 68 and res["team"].is_unique, f"expected 68 unique teams, got {len(res)}"
    assert len(games) == 67, f"expected 67 games, got {len(games)}"
    assert (res["seed"].value_counts().sort_index() >= 4).all() and res["seed"].between(1, 16).all()
    assert (res["finish"] == "CHAMPS").sum() == 1 and (res["finish"] == "R68").sum() == 4

    # ---- cross-check every NCAA game against Torvik's own 2026 schedule ----
    sked = load_sked()
    post = sked[sked["gtype"] == "3"]
    mismatches = []
    for g in games:
        pair = post[((post["t1"] == g["winner"]) & (post["t2"] == g["loser"])) | ((post["t1"] == g["loser"]) & (post["t2"] == g["winner"]))]
        ok = len(pair) == 1 and pair["winner"].iloc[0] == g["winner"] and int(pair["w"].iloc[0]) == g["w_score"] and int(pair["l"].iloc[0]) == g["l_score"]
        if not ok:
            mismatches.append(g)
    if mismatches:
        print("Bracket vs Torvik super_sked mismatches:", mismatches)
        return 1
    print(f"Bracket parsed: 68 teams, 67 games; all 67 match Torvik 2026_super_sked.csv (winner and score).")

    # ---- postseason record (Torvik P-table meaning: conference tournament + postseason games) ----
    ps = sked[sked["gtype"].isin(["2", "3"]) & sked["winner"].notna()]
    teams = pd.concat([ps["t1"], ps["t2"]]).unique()
    rec = []
    for t in sorted(teams):
        g = ps[(ps["t1"] == t) | (ps["t2"] == t)]
        w = int((g["winner"] == t).sum())
        rec.append({"Team": t, "Year": YEAR, "Postseason rec": f"{w}-{len(g) - w}"})
    pd.DataFrame(rec).to_csv(SRC / "postseason_rec_2026.csv", index=False)

    # ---- write results + update cache seed/finish for 2026 ----
    res = res.sort_values(["seed", "team"])[["team", "year", "seed", "region", "finish", "ncaa_wins", "first_four", "bracket_name"]]
    res.to_csv(SRC / "ncaa_2026_results.csv", index=False)
    missing = set(res["team"]) - known
    assert not missing, f"teams missing from 2026 team-history cache: {missing}"
    m = cache["year"] == YEAR
    cache.loc[m, ["seed", "finish"]] = pd.NA
    lookup = res.set_index("team")
    hit = m & cache["team"].isin(lookup.index)
    cache.loc[hit, "seed"] = cache.loc[hit, "team"].map(lookup["seed"]).astype(float)
    cache.loc[hit, "finish"] = cache.loc[hit, "team"].map(lookup["finish"])
    assert int(hit.sum()) == 68, f"cache 2026 rows matched {int(hit.sum())} of 68"
    cache.to_csv(CACHE, index=False)

    # ---- replace bracketology seed files with the actual seed line (archive the originals once) ----
    ARCHIVE.mkdir(parents=True, exist_ok=True)
    for f in (SEEDS_CSV, SEEDS_XLSX):
        dest = ARCHIVE / f.name
        if f.exists() and not dest.exists():
            shutil.copy2(f, dest)
    conf = cache[m].set_index("team")["conf"]
    seeds = pd.DataFrame({"Seed": res["seed"].astype(int), "Team": res["team"], "Conf": res["team"].map(conf),
                          "Region": res["region"], "Finish": res["finish"], "Source": "official 2026 bracket"})
    seeds.to_csv(SEEDS_CSV, index=False)
    seeds.to_excel(SEEDS_XLSX, index=False)
    print(f"Wrote {SRC / 'ncaa_2026_results.csv'}, {SRC / 'postseason_rec_2026.csv'} ({len(rec)} teams),")
    print(f"updated 68 2026 rows in {CACHE.name}, replaced bracketology in {SEEDS_CSV.name} and {SEEDS_XLSX.name}.")
    print(res["finish"].value_counts().to_string())
    return 0


if __name__ == "__main__":
    sys.exit(main())
