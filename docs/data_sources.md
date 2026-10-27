# Data Sources

## Data collection runtimes (estimated)

| Script / step | Scope | Estimated runtime | Notes |
|---------------|-------|-------------------|-------|
| `scripts/fetch_team_tables.py` | Year range (default 2008–2025, ~18 years) | **~2–5 min** (HTTP) or **~5–15 min** (Playwright fallback) | Optional: fetches per-year CSVs from Bart Torvik. Not required if raw CSVs already exist. |
| `scripts/populate_team_history_cache.py` | ~367 teams × all years | **~1.5–2 hours** | Scrapes team-history.php per team. Playwright required. |
| `scripts/enrich_team_history_cache.py` | Local computation on cache | **~20 seconds** | Adds 4 coach-experience columns from existing cache data. |
| `scripts/recompile_all_trank_team_tables.py` | Concat + 2 joins + filter | **~20 seconds** | Assembles all_trank_team_tables.csv from raw CSVs + enriched cache. |
| Main pipeline (`run_pipeline.py` / `madness.py`) | Training data + models | **~10–25 min** with warm cache | Reads `all_trank_team_tables.csv` + `team_history_cache.csv`. |
| **Full data refresh** (history cache rebuild + pipeline) | All teams × all years in cache | **~1.5–2.5 hours** | Run `python scripts/run_pipeline.py --refresh-team-history`. |
| **Raw data prep only** (enrich + recompile) | Local | **~40 seconds** | Run `python scripts/run_pipeline.py --prep-raw`. |

---

## Reference: Sacred Data (Bart Torvik data access)

**Forum**: [Adam's WI Sports Blog – Sacred Data](https://adamcwisports.blogspot.com/p/data.html)

Bart Torvik and others document how to access T-Rank data without scraping. Summary of useful patterns:

- **Bulk CSV/JSON**: Add `&csv=1` or `&json=1` to URL parameters (before any `#`). Many pages (T-Rank, team tables, SOS, etc.) support this.
- **Team results (full season)**: `https://barttorvik.com/YYYY_team_results.csv` (or `.json`) — final/updated team stats by year.
- **Team tables (height, experience, etc.)**: [team-tables_each.php](https://barttorvik.com/team-tables_each.php) — add `?csv=1` to download.
- **Pre-tournament / time machine**: `/timemachine/team_results/YYYYMMDD_team_results.json.gz` for ratings as of a given date (e.g. Selection Sunday).
- **Schedules**: `YYYY_super_sked.csv` / `YYYY_master_sked.csv`; game-level stats in `getgamestats.php?year=YYYY&csv=1`.
- **Player stats**: `getadvstats.php?year=YYYY&csv=1`; column key: [pstatheaders.xlsx (Dropbox)](https://www.dropbox.com/s/ryugeykvntto5ji/pstatheaders.xlsx?dl=0). 2008–2009 may have different columns (play-by-play stats skipped).

Avoid mass scraping; use these bulk endpoints and reasonable request rates. If blocked unintentionally, contact via the forum or Bart’s channels.

---

## Primary: Bart Torvik

**URL**: [https://barttorvik.com](https://barttorvik.com)

The pipeline uses two Bart Torvik sources:

| Source | Path / URL pattern | Description |
|------|-------------|-------------|
| Precompiled team tables | `data/raw/assembled/all_trank_team_tables.csv` | Base regular-season stats for all teams and years, assembled from the per-year files below |
| Team history | `https://barttorvik.com/team-history.php?team=Team+Name` | Year, coach, seed, finish, conf, records, and efficiency/four-factor history |

The assembled table is built from these per-year Bart Torvik team-table exports (headerless, 40 columns; see [data_lineage.md](data_lineage.md)):

| Files | Location | Export type | Notes |
|-------|----------|-------------|-------|
| `trank_team_table_data08.csv` … `trank_team_table_data25.csv` (no 20) | `data/raw/sources/` | `type=R` (regular season) | Read by `scripts/recompile_all_trank_team_tables.py` |
| `trank_team_table_data2026.csv` | `data/raw/` | Current season | Read by the same script |
| `trank_postseason_2008.csv` … `trank_postseason_2025.csv` (no 2020) | `data/raw/trank_postseason/` | `type=P` (postseason) | One season per file, named by the season's `YEAR` column. `scripts/append_postseason_final.py` concatenates them into `data/raw/trank_postseason_final.csv` and skips any file whose `YEAR` does not match its name |

**Year range**: 2008–2025 with 2020 excluded (configurable in `configs/default.yaml`).

**Key columns (examples)**: Rk, Team, Conf, AdjOE, AdjDE, Barthag, EFG%, EFGD%, TOR, TORD, ORB, DRB, FTR, FTRD, 2P%, 2P%D, 3P%, 3P%D, Adj T., Rec, G, plus team-history columns like coach, seed, finish, ov_rec, conf_rec.

**Team-history cache**: The pipeline writes/reads `data/raw/team_history_cache.csv`. To force rebuild, run:

```bash
python scripts/run_pipeline.py --refresh-team-history
```

**Usage constraints**: Scraping for personal/research use. Respect Bart Torvik's terms of use and rate limits.

---

## Optional: External data (`data/external/`)

The default pipeline does not read anything from `data/external/`. `run_pipeline.py` builds training data from
`data/raw/assembled/all_trank_team_tables.csv`, and that table already carries WINS, BLK%, BLKED%, AST%, 3PRATE,
EFFHGT, EXP., TALENT, ELITESOS and the other team-table columns. They come from the per-year files in
`data/raw/sources/`, which `scripts/recompile_all_trank_team_tables.py` assembles (see
[data/raw/README.md](../data/raw/README.md)). In the default build, `_finalize_training_frame` runs with the
external and legacy-coach merges switched off.

### 1. `trank_team_table_data*.csv` (fetched team tables)

- **Written by**: `scripts/fetch_team_tables.py`, to `data/external/sources/` by default (`--out-dir` to change).
  Pattern: `trank_team_table_data_YYYY.csv` (one season per file) or `trank_team_table_data.csv` (combined).
- **Source**: Bart Torvik team-tables export from [team-tables_each.php](https://barttorvik.com/team-tables_each.php). The fetch uses `type=R` (regular season) by default; configurable via `--type` or `ingestion.team_tables_type` in `configs/default.yaml`.
- **Read by**: nothing in the default pipeline. The legacy rebuild path (`scripts/rebuild_from_raw.py` → `build_features`) globs `data/external/trank_team_table_data*.csv` without recursing, so it only sees files placed directly in `data/external/`, not in `sources/`. To use a fetched season in the main pipeline, add it to `data/raw/sources/` and rerun the recompile step.
- **Committed files**: the tables in `data/external/sources/` hold postseason-sized records (at most 7-11 wins per team; 2020, which had no postseason, holds full-season records). They look like `type=P` exports and are not interchangeable with `data/raw/sources/`.

#### Automated ingestion

Optional maintenance workflow (not required for normal pipeline runs):

```bash
python scripts/fetch_team_tables.py
```

The script tries HTTP first, then falls back to Playwright if the site blocks requests (bot protection). Options:

- `--year-start YYYY` / `--year-end YYYY` – limit year range
- `--no-playwright` – skip Playwright fallback
- `--out-dir PATH` – output directory (default: `data/external/sources/`)

**Estimated runtime**: ~2–5 min (HTTP) or ~5–15 min (Playwright fallback) for full year range.

Install Playwright for the fallback: `pip install playwright && playwright install chromium`

#### Manual fallback

If automation fails:

1. Open [https://barttorvik.com/team-tables_each.php](https://barttorvik.com/team-tables_each.php)
2. Select the year(s) if the page supports it
3. Copy the table or use the site’s export, and save as CSV
4. Save to `data/external/sources/` as `trank_team_table_data_YYYY.csv` (rename browser downloads such as `trank_team_table_data (3).csv` to the season they contain)
5. Ensure columns align with: TEAM, ADJOE, ADJDE, BARTHAG, RECORD, WINS, GAMES, EFG, EFGD., FTRATE, FTRATED, TOV%, TOV%D, OREB%, OPOREB%, RAWT, 2P%, 2P%D, 3P%, 3P%D, BLK%, BLKED%, AST%, OPAST%, 3PRATE, 3PRATED, ADJ.T, AVGHGT, EFFHGT, EXP., YEAR, PAKE, PASE, TALENT, blank, FT%, OPFT%, PPPOFF, PPPDEF, ELITESOS

### 2. `cleanteam.xlsx`

- **Source**: Prepared Excel of 2023 tournament teams (or holdout year).
- **Provides**: Pre-cleaned team roster/performance for holdout validation.
- **Usage**: R pipeline only (`src/pipelines/madness.R`), read from `data/external/` when both holdout files exist; loaded as `cbb2023` for prediction and validation. Not committed.

### 3. `teamdata23.csv`

- **Source**: Team-level data for 2023 (aligned with cleanteam.xlsx).
- **Provides**: Same structure as `trank_team_table_data` for the holdout year.
- **Usage**: R pipeline only; joined to `cbb2023` for the full feature set. Not committed.

### 4. Coach tournament experience

Coach experience is now computed locally from `data/raw/team_history_cache.csv` (no external fetch needed):

```bash
python scripts/enrich_team_history_cache.py
```

This reads the team-history cache and writes `data/raw/team_history_cache_enriched.csv` with four columns:

- **`coach_tourn_appearances_5y`**: NCAA tournaments the head coach appeared in over the prior 5 tournament years (2020 excluded).
- **`coach_tourn_wins_5y`** / **`coach_tourn_losses_5y`**: Tournament W-L for that coach over those same seasons.
- **`coach_years_with_program`**: Consecutive years (including current) the head coach has been at this program.

The enriched cache is joined into `all_trank_team_tables.csv` by `recompile_all_trank_team_tables.py`. **Estimated runtime**: ~20 seconds (local computation, no network).

**Derived (in pipeline)**: `coach_tourn_win_pct_5y`, `coach_tourn_games_5y`, and `coach_avg_wins_per_appearance`.

**Legacy**: Only the legacy rebuild path (`scripts/rebuild_from_raw.py`) still merges any `coach_tournament_experience*.csv` found under `data/external/` (e.g. `data/external/legacy/`) when the coach columns are missing; the default pipeline uses the enriched cache.

#### Legacy fetch (deprecated)

- **Pattern (legacy)**: `coach_tournament_experience.csv` or `coach_tournament_experience_YYYY.csv`
- **Source**: Optional. Head coach NCAA tournament experience over the **last 5 years** (including the current season). **Easiest:** use the fetch script with `--fetch` (defaults to Bart Torvik [team-history.php](https://www.torvik.dev/reference/bart_team_history.php), same data as toRvik’s `bart_team_history`). Alternatively: [Sports Reference](https://www.sports-reference.com/cbb/coaches/) coach pages, or fill manually.
- **Columns**: `Team`, `Year`, `coach_tourn_appearances_5y`, `coach_tourn_wins_5y`, `coach_tourn_losses_5y`, `coach_years_with_program`
  - **Team**: Must match pipeline team names (same as in training data).
  - **Year**: Season year (e.g. 2024 for 2023–24).
  - **coach_tourn_appearances_5y**: Number of NCAA tournaments the team’s head coach appeared in over the previous 5 tournament years (e.g. for 2024, count 2019, 2021, 2022, 2023, 2024 — 2020 excluded per project convention).
  - **coach_tourn_wins_5y** / **coach_tourn_losses_5y**: Tournament W–L for that coach over those same seasons.
  - **coach_years_with_program**: Number of consecutive years (including current) the head coach has been at this program.
- **Derived (in pipeline)**: The pipeline computes `coach_tourn_win_pct_5y` (wins/(wins+losses)), `coach_tourn_games_5y` (wins+losses), and `coach_avg_wins_per_appearance` (wins/appearances) from the above.
- **Usage**: Joined to master table by (Team, year). Missing rows or missing file → features filled with 0.

#### Building the file

```bash
# Template only (zeros)
python scripts/fetch_coach_experience.py

# Fetch from Bart Torvik (same source as pipeline; team names must match)
python scripts/fetch_coach_experience.py --fetch

# Fetch from Sports Reference instead
python scripts/fetch_coach_experience.py --fetch --source sr
```

Torvik uses [team-history.php](https://barttorvik.com/team-history.php?team=Team+Name); team name must be an exact match (e.g. 'North Carolina', 'St.' for State). If fetch fails, fill manually from coach pages (e.g. [Sports Reference Coaches](https://www.sports-reference.com/cbb/coaches/)).

**Estimated runtime**: ~1.5–2 hours for full fetch (~367 teams). See [COACH_FETCH_README.md](../data/external/COACH_FETCH_README.md).

---

## Player advanced stats

**Path**: `data/raw/player_stats/player_advanced_YYYY.csv` (per-year) and `player_advanced_all.csv` (combined)

Individual player-level advanced season statistics from Bart Torvik, matching the `bart_player_season(stat='advanced')` output from the [toRvik R package](https://torvik.dev/reference/bart_player_season.php).

- **Source (2008–2023)**: Pre-built parquet files from the [toRvik-data GitHub repo](https://github.com/andreweatherman/toRvik-data)
- **Source (2024+)**: Bart Torvik CSV endpoint (`getadvstats.php?year=YYYY&csv=1`)
- **Columns** (29): `player`, `pos`, `exp`, `team`, `conf`, `g`, `min`, `porpag`, `dporpag`, `ortg`, `adj_oe`, `drtg`, `adj_de`, `stops`, `obpm`, `dbpm`, `bpm`, `oreb`, `dreb`, `ast`, `to`, `blk`, `stl`, `ftr`, `pfr`, `rec`, `pick`, `year`, `id`
- **Year range**: 2008–2025 (2020 excluded per project convention)
- **Total records**: ~81,000 players across all years

#### Fetching

```bash
python scripts/fetch_player_stats.py
python scripts/fetch_player_stats.py --year-start 2024 --year-end 2025  # specific years only
```

**Estimated runtime**: ~3–4 minutes for all years (parquet downloads for 2008–2023, CSV for 2024+).

---

## Clean training dataset (pipeline output)

**Path**: `data/processed/cbb_training_clean.csv`

Single prepped dataset for training and testing: one row per (Team, year), no duplicate column names, no redundant variables. Built automatically by the pipeline from the full training export.

- **Variables**: All Bart Torvik base stats (AdjOE, AdjDE, Barthag, TOR, ORB, DRB, FTR, 2P%, 3P%, etc.), optional external (WINS, BLK%, EFFHGT, TALENT, …), coach experience derived from team history (coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_tourn_losses_5y, coach_tourn_win_pct_5y, coach_tourn_games_5y, coach_avg_wins_per_appearance, coach_years_with_program), conference flags (Power6, B12, B10, …), seed.
- **Outcomes**: `result`, `round`, and binary `Round2`, `S16`, `E8`, `F4`, `Ship`, `Champ`.
- **Redundant columns removed**: Duplicate column names (first kept); year-specific "3P Rate *_3PR" / "3P Rate *_3PRD" columns are dropped (canonical `3P%`, `3PRATE`, `3PRATED` kept).

To regenerate from an existing full export: `python scripts/build_clean_training_data.py [path/to/cbbtrainingdata_YYYYMMDD.csv]`

---

## `holdout_actual_results` (optional)

A manually maintained data frame or CSV with actual tournament results for the holdout year (Team, round). Used to compute out-of-sample RMSE for 2023 predictions. If absent, that validation step is skipped.
