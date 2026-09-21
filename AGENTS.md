# AGENTS.md

Instruction file for AI coding agents working on this project. See [README.md](README.md) for human-oriented overview.

## Project Overview

NCAA Men's March Madness prediction pipeline: predicts tournament round advancement using Bart Torvik team stats and seed-stratified linear models with jackknife leave-one-out cross-validation. Key outputs: `data/processed/cbbtrainingdata_YYYYMMDD.csv`, **`data/processed/cbb_training_clean.csv`** (single clean dataset: no duplicate vars/rows, all outcomes), and in-memory model artifacts.

## Setup and Run Commands

**Python (primary):**
```bash
pip install -r requirements.txt
python scripts/run_pipeline.py
```

**Raw data prep** (rebuild `all_trank_team_tables.csv` from per-year CSVs + enriched cache):
```bash
python scripts/run_pipeline.py --prep-raw
# Or step by step:
python scripts/enrich_team_history_cache.py           # cache → enriched cache
python scripts/recompile_all_trank_team_tables.py     # CSVs + enriched cache → final tables
```

**Rare / first-time data fetch** (only needed when raw sources are stale or missing):
```bash
python scripts/fetch_team_tables.py                   # fetch per-year team-table CSVs (~2-15 min)
python scripts/populate_team_history_cache.py          # fetch team history (~1.5-2 hours)
python scripts/fetch_player_stats.py                   # fetch player advanced stats (~3-4 min)
```

See [docs/data_sources.md](docs/data_sources.md) for estimated runtimes.

**R:**
```bash
Rscript scripts/install_deps.R
Rscript scripts/run_pipeline.R
```

**Tests:** Run from project root: `pytest`

## Configuration

- Primary config: [configs/default.yaml](configs/default.yaml) — year range, paths, model params
- Local overrides: `configs/local.yaml` (gitignored)

## Conventions

- Data frames: `snake_case` (e.g., `training_data`, `holdout_2023`, `regular_season_raw`)
- Directories: lowercase hyphenated (`data/raw`, `reports/figures`)
- Scripts: snake_case (`run_pipeline.py`, `fetch_team_tables.py`)

Full variable and column naming: [docs/naming_conventions.md](docs/naming_conventions.md)

## Important Paths

| Path | Purpose |
|------|---------|
| `src/pipelines/madness.py` | Python main pipeline (modeling + training data) |
| `src/pipelines/madness.R` | R main pipeline |
| `scripts/recompile_all_trank_team_tables.py` | Official raw-data assembly (concat + joins + tournament filter) |
| `scripts/enrich_team_history_cache.py` | Enrich team-history cache with coach experience columns |
| `scripts/populate_team_history_cache.py` | Fetch/rebuild team-history cache from Bart Torvik |
| `data/raw/trank_team_table_data08..25.csv` | Per-year regular-season team tables (source CSVs) |
| `data/raw/trank_postseason_final.csv` | Concatenated postseason records |
| `data/raw/team_history_cache.csv` | Scraped team history (coach, seed, finish, conf per team-year) |
| `data/raw/team_history_cache_enriched.csv` | Cache + coach experience columns |
| `data/raw/assembled/all_trank_team_tables.csv` | Final assembled master table (49 cols); sources flow into this |
| `data/raw/assembled/all_trank_team_tables_tournament.csv` | Tournament teams only (finish IS NOT NULL) |
| `data/raw/player_stats/player_advanced_YYYY.csv` | Per-year player advanced stats (from toRvik-data + barttorvik) |
| `data/raw/player_stats/player_advanced_all.csv` | Combined player advanced stats across all years |
| `scripts/fetch_player_stats.py` | Fetch player advanced season stats per year |
| `data/processed/cbb_training_clean.csv` | Single clean training set: no duplicate vars/rows, all outcomes |
| `configs/default.yaml` | Year range, file paths, model params |
| `docs/data_sources.md` | Bart Torvik access, bulk CSV/JSON patterns, rate limits |
| `docs/data_lineage.md` | Raw data prep pipeline diagram and column order |
| `docs/modeling_plan.md` | Predictive model strategy, architecture, evaluation, improvement roadmap |

## Do's and Don'ts

- **Do** respect Bart Torvik rate limits; prefer bulk endpoints (`&csv=1`, `&json=1`) over mass scraping
- **Do** exclude year 2020 from analysis (COVID, no NCAA tournament)
- **Do** assume optional external data (EFFHGT, TALENT, etc.) is filled with 0 when absent — pipeline runs without it
- **Do** treat historical logo or champion badges as title-year data points, not current-team points; if a chart shows former champion markers, compute their coordinates from the championship season's metrics relative to that season's seed baseline
- **Don't** mass scrape; use documented bulk endpoints (see [docs/data_sources.md](docs/data_sources.md))
