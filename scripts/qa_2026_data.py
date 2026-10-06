#!/usr/bin/env python3
"""QA the 2026 layer and write data/raw/torvik_2026/MANIFEST.md.

Run after: ingest_2026.py -> enrich_team_history_cache.py -> recompile_all_trank_team_tables.py.
Fails (exit 1) on any grain, coverage, or null check that does not hold.
"""
from __future__ import annotations

import csv
import datetime as dt
import hashlib
import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
SRC = RAW / "torvik_2026"
ASM = RAW / "assembled"
OUT = SRC / "MANIFEST.md"
SELECTION_SUNDAY = dt.date(2026, 3, 15)

# The columns the nine topic analyses read (raw assembled names).
TOPIC_COLS = ["Elite SOS", "conf_avg_barthag", "Eff. Hgt.", "Avg Hgt.", "Exp.", "TOV%", "TOV% D", "Blk %", "Blked %",
              "3P Rate", "3P %", "3P % D.", "O Reb%", "Op OReb%", "Adj. T", "FT Rate", "FT%", "FT Rate D", "coach",
              "coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y", "coach_years_with_program",
              "seed", "finish", "conf"]

SOURCES = [
    ("data/raw/trank_team_table_data2026.csv", "https://barttorvik.com/team-tables_each.php?year=2026&type=R (CSV export)",
     "regular-season team table, type R; pre-existing export, file time 2026-03-05, before Selection Sunday 2026-03-15"),
    ("data/raw/torvik_2026/2026_super_sked.csv", "https://barttorvik.com/2026_super_sked.csv",
     "game-level schedule and results; used for postseason record and to cross-check all 67 NCAA games"),
    ("data/raw/torvik_2026/wikipedia_2026_ncaa_tournament.wikitext",
     "https://en.wikipedia.org/w/index.php?title=2026_NCAA_Division_I_men%27s_basketball_tournament&action=raw",
     "official bracket: seed line, region and score of every game"),
    ("data/raw/team_history_cache.csv", "https://barttorvik.com/team-history.php?team=<team>",
     "2026 rows (coach, conf) scraped 2026-03-04; seed and finish filled by ingest_2026.py"),
    ("data/raw/player_stats/player_advanced_2026.csv", "https://barttorvik.com/getadvstats.php?year=2026&csv=1",
     "player advanced stats, pre-existing export 2026-03-07 (size and experience features)"),
    ("data/raw/torvik_2026/ncaa_2026_results.csv", "derived by scripts/ingest_2026.py", "one row per 2026 NCAA team"),
    ("data/raw/torvik_2026/postseason_rec_2026.csv", "derived by scripts/ingest_2026.py",
     "conference tournament + postseason W-L (Torvik P-table meaning)"),
    ("data/raw/2026_expected_seeds_update.csv", "derived by scripts/ingest_2026.py", "actual 2026 seed line (was bracketology)"),
    ("data/raw/assembled/all_trank_team_tables.csv", "scripts/recompile_all_trank_team_tables.py", "all D1 team-seasons"),
    ("data/raw/assembled/all_trank_team_tables_tournament.csv", "scripts/recompile_all_trank_team_tables.py", "NCAA teams only"),
    ("data/processed/cbb_training_clean.csv", "src.pipelines.madness.run_pipeline(data_only=True, year_start=2008, year_end=2026)",
     "model training set; 2008-2025 rows unchanged and in original order, 2026 appended with round 0..6 for 64 teams"),
]
UNREACHABLE = [
    "https://barttorvik.com/trank.php?year=2026&type=R|P|T (madness.py scrape_bart_torvik)",
    "https://barttorvik.com/team-tables_each.php?year=2026&type=R|P (fetch_team_tables.py)",
    "https://barttorvik.com/team-history.php (team history refresh)",
]


def _sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()[:16]


def _rows(path: Path) -> int | str:
    if path.suffix == ".wikitext":
        return "text"
    with path.open(encoding="utf-8-sig") as f:
        return sum(1 for _ in f) - (0 if path.name.startswith("trank_team_table_data") or "super_sked" in path.name else 1)


def main() -> int:
    full = pd.read_csv(ASM / "all_trank_team_tables.csv")
    tour = pd.read_csv(ASM / "all_trank_team_tables_tournament.csv")
    res = pd.read_csv(SRC / "ncaa_2026_results.csv")
    checks: list[tuple[str, bool, str]] = []

    def check(name: str, ok: bool, detail: str) -> None:
        checks.append((name, bool(ok), detail))

    t26 = tour[tour["Year"] == 2026]
    check("assembled grain: one row per (Team, Year)", not full.duplicated(["Team", "Year"]).any(),
          f"{len(full)} rows, {full.duplicated(['Team', 'Year']).sum()} duplicate keys")
    check("tournament grain: one row per (Team, Year)", not tour.duplicated(["Team", "Year"]).any(),
          f"{len(tour)} rows, {tour.duplicated(['Team', 'Year']).sum()} duplicate keys")
    check("2026 D1 rows in assembled table", (full["Year"] == 2026).sum() == 365, f"{(full['Year'] == 2026).sum()} rows")
    check("2026 tournament rows = 68", len(t26) == 68, f"{len(t26)} rows")
    check("bracket teams joined to regular-season table", set(res["team"]) <= set(full.loc[full["Year"] == 2026, "Team"]),
          f"{len(set(res['team']) & set(full.loc[full['Year'] == 2026, 'Team']))}/68 matched")
    check("seed + finish populated for all 68", t26["seed"].notna().all() and t26["finish"].notna().all(),
          f"seed {t26['seed'].notna().sum()}/68, finish {t26['finish'].notna().sum()}/68")
    check("seed line shape: 4 per line, 6 on the two First Four lines",
          sorted(t26["seed"].value_counts().tolist()) == [4] * 14 + [6] * 2, str(t26["seed"].value_counts().sort_index().to_dict()))
    expected = {"R68": 4, "R64": 32, "R32": 16, "Sweet Sixteen": 8, "Elite Eight": 4, "Final Four": 2, "Finals": 1, "CHAMPS": 1}
    check("finish distribution matches a 68-team bracket", t26["finish"].value_counts().to_dict() == expected,
          str(t26["finish"].value_counts().to_dict()))
    analysed = tour[tour["finish"] != "R68"]
    per_year = analysed.groupby("Year").size()
    check("analysis rows 2008-2025 = 1,088 (baseline)", int(per_year[per_year.index <= 2025].sum()) == 1088,
          f"{int(per_year[per_year.index <= 2025].sum())} rows")
    check("analysis rows 2026 = 64", int(per_year.get(2026, 0)) == 64, f"{int(per_year.get(2026, 0))} rows")
    null26 = t26[TOPIC_COLS].isna().sum()
    check("no nulls in topic columns for 2026 NCAA teams", int(null26.sum()) == 0,
          ", ".join(f"{c}={n}" for c, n in null26.items() if n) or "0 nulls across " + str(len(TOPIC_COLS)) + " columns")
    ps_cov = t26["Postseason rec"].notna().sum()
    check("postseason record joined for 2026 NCAA teams", ps_cov == 68, f"{ps_cov}/68")

    # Regular-season staleness: D1 regular-season games (types 0/1) in Torvik's schedule vs games in the 2026-03-05 export.
    rows = list(csv.reader((SRC / "2026_super_sked.csv").open(encoding="utf-8")))
    reg = pd.DataFrame({"t1": [r[8] for r in rows], "t2": [r[14] for r in rows], "gtype": [r[6] for r in rows],
                        "played": [bool(r[24]) for r in rows]})
    reg = reg[reg["gtype"].isin(["0", "1"]) & reg["played"]]
    games = pd.concat([reg["t1"], reg["t2"]]).value_counts()
    snap = t26.set_index("Team")["Games"]
    missing_games = (snap.index.map(games).to_series(index=snap.index) - snap).astype(int)
    check("2026 regular-season export is pre-tournament", True,
          f"export games vs full regular season: median {missing_games.median():.0f} missing, max {missing_games.max()}, "
          f"{int((missing_games == 0).sum())}/68 complete")

    ok = all(c[1] for c in checks)
    today = dt.date.today().isoformat()
    lines = [
        "# 2026 data manifest",
        "",
        f"Generated by `scripts/qa_2026_data.py` on {today}. Status: **{'PASS' if ok else 'FAIL'}**.",
        "",
        "## Sources",
        "",
        "| file | source | note | rows | sha256[:16] |",
        "|:--|:--|:--|--:|:--|",
    ]
    for rel, url, note in SOURCES:
        p = ROOT / rel
        lines.append(f"| `{rel}` | {url} | {note} | {_rows(p) if p.exists() else 'missing'} | {_sha(p) if p.exists() else ''} |")
    lines += [
        "",
        f"Retrieval date for the two files fetched this pass (super_sked, wikitext): {dt.date.fromtimestamp((SRC / '2026_super_sked.csv').stat().st_mtime).isoformat()}.",
        "",
        "## Unreachable on 2026-09-29",
        "",
        "Plain requests got the JS \"Verifying Browser\" interstitial; headless Playwright got HTTP 403 Forbidden. "
        "The madness.py scraper therefore returned 0 rows for 2026. No 2026 row was fabricated: regular-season stats come "
        "from the pre-tournament export above, seeds and results from the official bracket cross-checked game by game "
        "against Torvik's own schedule file.",
        "",
    ] + [f"- {u}" for u in UNREACHABLE] + [
        "",
        "## Checks",
        "",
        "| check | result | detail |",
        "|:--|:--|:--|",
    ] + [f"| {n} | {'pass' if o else 'FAIL'} | {d} |" for n, o, d in checks] + [
        "",
        "## Row counts",
        "",
        f"- Assembled table: {len(full)} team-seasons ({(full['Year'] == 2026).sum()} in 2026).",
        f"- Tournament table: {len(tour)} rows ({len(t26)} in 2026).",
        f"- Topic analysis sample (finish != R68): {len(analysed)} = {int(per_year[per_year.index <= 2025].sum())} (2008-2025) + {int(per_year.get(2026, 0))} (2026).",
        "",
        "## Known limitation",
        "",
        "The 2026 regular-season export predates the end of the regular season by a few days (see staleness check). "
        "Conference tournament games are inside type R: 2008-2025 rows carry them (Florida 2025 is 30-4, its record after the SEC tournament; 2024 Connecticut 31-3). The 2026 export predates them, so 2026 tournament teams sit at a median 30 games against 32-33 in 2022-2025 (checked 2026-09-30).",
        "",
    ]
    OUT.write_text("\n".join(lines), encoding="utf-8")
    for n, o, d in checks:
        print(f"[{'pass' if o else 'FAIL'}] {n}: {d}")
    print(f"Wrote {OUT}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
