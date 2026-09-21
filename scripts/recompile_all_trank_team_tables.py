#!/usr/bin/env python3
"""
Official raw-data assembly: build all_trank_team_tables.csv from per-year team-table
CSVs, postseason records, and the enriched team-history cache.

Steps:
  1. Concatenate data/raw/trank_team_table_data08..25.csv (skip 20) → base table
  2. LEFT JOIN Postseason rec from data/raw/trank_postseason_final.csv on (Team, Year)
  3. LEFT JOIN coach/seed/finish/conf + coach experience from
     data/raw/team_history_cache_enriched.csv on (Team, Year)
  4. Write data/raw/assembled/all_trank_team_tables.csv (full) and
     data/raw/assembled/all_trank_team_tables_tournament.csv (finish IS NOT NULL)

Prerequisite scripts (run first if data is stale):
  python scripts/populate_team_history_cache.py   → data/raw/team_history_cache.csv
  python scripts/enrich_team_history_cache.py     → data/raw/team_history_cache_enriched.csv

Years 2008-2025 excluding 2020.
"""
from __future__ import annotations

from pathlib import Path

import pandas as pd

project_root = Path(__file__).resolve().parent.parent
RAW_DIR = project_root / "data" / "raw"
SOURCES_DIR = RAW_DIR / "sources"  # per-year trank_team_table_dataNN.csv

POSTSEASON_PATH = RAW_DIR / "trank_postseason_final.csv"
ENRICHED_CACHE_PATH = RAW_DIR / "team_history_cache_enriched.csv"
ASSEMBLED_DIR = RAW_DIR / "assembled"  # final outputs (sources + postseason + cache flow into these)
STAGING_DIR = RAW_DIR / "_staging"
OUT_PATH = ASSEMBLED_DIR / "all_trank_team_tables.csv"
OUT_PATH_ALT = STAGING_DIR / "all_trank_team_tables_recompiled.csv"  # fallback when assembled/ file is locked
OUT_TOURNAMENT_PATH = ASSEMBLED_DIR / "all_trank_team_tables_tournament.csv"
OUT_2026_ENRICHED_PATH = ASSEMBLED_DIR / "trank_team_table_data2026_enriched.csv"

BASE_HEADERS = [
    "Team", "Adj OE", "Adj DE", "Barthag", "Record", "Wins", "Games",
    "eFG", "eFG D.", "FT Rate", "FT Rate D", "TOV%", "TOV% D", "O Reb%", "Op OReb%",
    "Raw T", "2P %", "2P % D.", "3P %", "3P % D.", "Blk %", "Blked %", "Ast %", "Op Ast %",
    "3P Rate", "3P Rate D", "Adj. T", "Avg Hgt.", "Eff. Hgt.", "Exp.", "Year",
    "PAKE", "PASE", "Talent", "", "FT%", "Op. FT%", "PPP Off.", "PPP Def.", "Elite SOS",
]

ENRICHED_JOIN_COLS = [
    "team", "year", "coach", "seed", "finish", "conf",
    "coach_tourn_appearances_5y", "coach_tourn_wins_5y",
    "coach_tourn_losses_5y", "coach_years_with_program",
]

YEARS = list(range(2008, 2020)) + list(range(2021, 2027))


def _write_csv(df: pd.DataFrame, primary: Path, fallback: Path, label: str) -> Path:
    """Write to primary; fall back to _staging if locked."""
    fallback.parent.mkdir(parents=True, exist_ok=True)
    for target in [primary, fallback]:
        try:
            df.to_csv(target, index=False)
            if target == primary:
                print(f"Wrote {label}: {target} ({len(df)} rows, {len(df.columns)} cols)")
            else:
                print(f"Target busy; wrote {label}: {target}. Copy over {primary.name} when closed.")
            return target
        except PermissionError:
            continue
    import datetime
    ts = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    last = fallback.parent / f"{primary.stem}_{ts}.csv"
    df.to_csv(last, index=False)
    print(f"All targets busy; wrote {label}: {last}")
    return last


def _attach_conf_avg_barthag(df: pd.DataFrame) -> pd.DataFrame:
    """Add conf/year average Barthag using the full field for each conference-season."""
    out = df.copy()
    out["conf"] = out["conf"].astype(str).str.strip()
    out.loc[out["conf"] == "", "conf"] = pd.NA

    barthag_numeric = pd.to_numeric(out["Barthag"], errors="coerce")
    out["conf_avg_barthag"] = (
        barthag_numeric.groupby([out["conf"], out["Year"]]).transform("mean")
    )
    return out


def _find_team_table_path(year: int) -> Path | None:
    candidates = [
        SOURCES_DIR / f"trank_team_table_data{str(year)[-2:]}.csv",
        SOURCES_DIR / f"trank_team_table_data{year}.csv",
        RAW_DIR / f"trank_team_table_data{str(year)[-2:]}.csv",
        RAW_DIR / f"trank_team_table_data{year}.csv",
    ]
    for path in candidates:
        if path.exists():
            return path
    return None


def main() -> int:
    # --- Step 1: concat per-year regular-season CSVs (from raw/sources/ or raw root) ---
    dfs = []
    for year in YEARS:
        path = _find_team_table_path(year)
        if path is None:
            print(f"Missing team-table CSV for {year}")
            return 1
        df = pd.read_csv(path, header=None)
        if len(df.columns) != len(BASE_HEADERS):
            print(f"{path.name}: got {len(df.columns)} cols, expected {len(BASE_HEADERS)}")
            return 1
        dfs.append(df)

    combined = pd.concat(dfs, ignore_index=True)
    combined.columns = BASE_HEADERS
    print(f"Step 1: Concatenated {len(YEARS)} files -> {len(combined)} rows, {len(BASE_HEADERS)} cols")

    # --- Step 2: join Postseason rec ---
    if POSTSEASON_PATH.exists():
        post = pd.read_csv(POSTSEASON_PATH, header=None)
        post_rec = post[[0, 30, 4]].copy()
        post_rec.columns = ["Team", "Year", "Postseason rec"]
        combined = combined.merge(post_rec, on=["Team", "Year"], how="left")
        matched = combined["Postseason rec"].notna().sum()
        print(f"Step 2: Joined Postseason rec -> {matched}/{len(combined)} rows matched")
    else:
        combined["Postseason rec"] = ""
        print(f"Step 2: {POSTSEASON_PATH.name} not found; Postseason rec left blank")

    # --- Step 3: join enriched team-history cache ---
    if ENRICHED_CACHE_PATH.exists():
        enriched = pd.read_csv(ENRICHED_CACHE_PATH)
        enriched_sub = enriched[ENRICHED_JOIN_COLS].copy()
        enriched_sub = enriched_sub.rename(columns={"team": "Team", "year": "Year"})
        enriched_sub["Year"] = pd.to_numeric(enriched_sub["Year"], errors="coerce")
        combined["Year"] = pd.to_numeric(combined["Year"], errors="coerce")
        combined = combined.merge(enriched_sub, on=["Team", "Year"], how="left")
        coach_matched = combined["coach"].notna().sum()
        finish_matched = combined["finish"].notna().sum()
        print(f"Step 3: Joined enriched cache -> coach: {coach_matched}, finish: {finish_matched}")
    else:
        for col in ENRICHED_JOIN_COLS[2:]:
            combined[col] = ""
        print(f"Step 3: {ENRICHED_CACHE_PATH.name} not found; history columns left blank")

    combined = _attach_conf_avg_barthag(combined)
    conf_avg_matched = combined["conf_avg_barthag"].notna().sum()
    print(f"Step 3b: Added conf_avg_barthag -> {conf_avg_matched}/{len(combined)} rows populated")

    # --- Write full table (assembled/ = final level; sources flow into this) ---
    ASSEMBLED_DIR.mkdir(parents=True, exist_ok=True)
    _write_csv(combined, OUT_PATH, OUT_PATH_ALT, "all_trank_team_tables")

    year_2026 = combined[pd.to_numeric(combined["Year"], errors="coerce") == 2026].copy()
    if not year_2026.empty:
        try:
            year_2026.to_csv(OUT_2026_ENRICHED_PATH, index=False)
            print(f"Step 3c: Wrote enriched 2026 table -> {OUT_2026_ENRICHED_PATH} ({len(year_2026)} rows)")
        except PermissionError:
            alt_2026 = STAGING_DIR / "trank_team_table_data2026_enriched.csv"
            STAGING_DIR.mkdir(parents=True, exist_ok=True)
            year_2026.to_csv(alt_2026, index=False)
            print(f"Step 3c: Target busy; wrote {alt_2026}. Copy over {OUT_2026_ENRICHED_PATH.name} when closed.")

    # --- Step 4: filter to tournament teams ---
    mask = combined["finish"].notna() & (combined["finish"].astype(str).str.strip() != "")
    tournament = combined[mask].copy()
    try:
        tournament.to_csv(OUT_TOURNAMENT_PATH, index=False)
        print(f"Step 4: Wrote tournament filter -> {OUT_TOURNAMENT_PATH} ({len(tournament)} rows)")
    except PermissionError:
        STAGING_DIR.mkdir(parents=True, exist_ok=True)
        alt = STAGING_DIR / "all_trank_team_tables_tournament_recompiled.csv"  # fallback when assembled/ is locked
        tournament.to_csv(alt, index=False)
        print(f"Step 4: Target busy; wrote {alt}. Copy over {OUT_TOURNAMENT_PATH.name} when closed.")

    print("\nDone. Raw data assembly complete.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
