# AGENTS.md

Instruction file for AI coding agents working on this project. See [README.md](README.md) for human-oriented overview.

## Project Overview

NCAA men's tournament win probabilities on Bart Torvik team stats, tested against historical seed-pair win rates under a pre-registered charter. The frozen model is `log5` on Barthag (no parameters, frozen 2026-09-29T16:22:27Z in `reports/model_selection_frozen.json`). Its 2021-2026 edge over seed-pair rates is **not established**: Brier -0.0059 [-0.0123, +0.0004], power 15%, MDE 0.0126 (`reports/model_vs_seeds_powered.md`). Never describe it as beating seeds or as a betting edge.

## Hard rules

- **Never run `python scripts/run_powered_comparison.py select`.** The selection is frozen; a rerun is a second look at the test data.
- **Never write a frozen input.** `src/frozen_inputs.py` lists them with full sha256: `data/processed/cbb_training_clean.csv`, `data/raw/tournament_games.csv`, `data/raw/seed_history_games.csv`, their two `*_manifest.json` files and `reports/model_selection_frozen.json`. Write CSVs through `src.frozen_inputs.write_csv`, which refuses those paths; builds and re-fetches go to run-tagged paths (`cbb_training_clean_<YYYYMMDD>.csv`, `data/raw/refetch/<date>/`). `tests/test_frozen_inputs.py` fails if any hash moves.
- `cbb_training_clean.csv` cannot be rebuilt from live Torvik (`data/raw/torvik_2026/MANIFEST.md`). Treat it as source data.
- `score` rewrites `reports/model_vs_seeds_powered.md`, `data/processed/powered_*_2010_2026.csv` and `reports/figures/frozen_model_calibration_2021_2026.png`. Run it to reproduce, then confirm only the timestamps changed.
- Label timing honestly. `powered_round_probabilities_2010_2026.csv` is **post-First-Four** (the 64-team field after the play-in games). Retrospective scores are not prospective evidence.

## Setup and Run Commands

**Supported path (Python 3.12):**
```bash
pip install -r requirements.txt
pytest
python scripts/run_powered_comparison.py score        # hash guard, then the frozen test
python scripts/predict_selection_sunday.py --year 2026 --check --bracket data/raw/torvik_2026/selection_sunday_bracket_2026.csv --semis East-South,West-Midwest
```

`predict_selection_sunday.py` gives log5 round probabilities from a `Team,Seed,Region` bracket. A 68-team bracket weights each First Four slot by its log5 play-in probability (`timing=selection_sunday`); `--from-games` rebuilds the post-First-Four field the powered report scored (`timing=post_first_four`). `--semis` names the national semifinal pairing. `--check` must print `CHECK PASS` (saved 2026 `p_frozen` reproduced to 1e-12). Output goes to `data/processed/selection_sunday/`.

**Legacy seed-stratified pipeline** (topic analyses and raw-data build; superseded for prediction; writes run-tagged files only):
```bash
python scripts/run_pipeline.py
```
`scripts/run_bracket_evaluation.py` is the older ridge walk-forward, superseded by `score`; its outputs carry `--out-tag` (default today's date) so the 2026-09-29 `bracket_simulation_2025.md` and `round_predictions_<Y>.csv` stay as recorded.

**2026 data layer** (already done; recorded in `data/raw/torvik_2026/MANIFEST.md`): `scripts/ingest_2026.py`, then `scripts/enrich_team_history_cache.py`, `scripts/recompile_all_trank_team_tables.py`, then `scripts/qa_2026_data.py`, which writes the manifest. Game files re-fetch with `scripts/fetch_tournament_games.py` and `scripts/fetch_seed_history.py` into `data/raw/refetch/<date>/` for a diff, never over the frozen copies.

**Raw data prep** (rebuild `all_trank_team_tables.csv` from per-year CSVs + enriched cache):
```bash
python scripts/run_pipeline.py --prep-raw
# Or step by step:
python scripts/enrich_team_history_cache.py           # cache to enriched cache
python scripts/recompile_all_trank_team_tables.py     # CSVs + enriched cache to final tables
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

- Primary config: [configs/default.yaml](configs/default.yaml): year range, paths, model params
- Local overrides: `configs/local.yaml` (gitignored)

## Conventions

- Data frames: `snake_case` (e.g., `training_data`, `holdout_2023`, `regular_season_raw`)
- Directories: lowercase hyphenated (`data/raw`, `reports/figures`)
- Scripts: snake_case (`run_pipeline.py`, `fetch_team_tables.py`)

Full variable and column naming: [docs/naming_conventions.md](docs/naming_conventions.md)

## Important Paths

| Path | Purpose |
|------|---------|
| `scripts/run_powered_comparison.py` | Pre-registered test: `score` only (never `select`) |
| `scripts/predict_selection_sunday.py` | Frozen log5 bracket probabilities, Selection Sunday or post-First-Four |
| `src/frozen_inputs.py` | Frozen-input hashes and the guarded `write_csv` |
| `src/pipelines/madness.py` | Legacy Python pipeline (seed-stratified models + training-data build) |
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
| `data/processed/cbb_training_clean.csv` | **Frozen** training set of the log5 test (2008-2026); not regenerable |
| `configs/default.yaml` | Year range, file paths, model params |
| `docs/data_sources.md` | Bart Torvik access, bulk CSV/JSON patterns, rate limits |
| `docs/data_lineage.md` | Raw data prep pipeline diagram and column order |
| `docs/modeling_plan.md` | Predictive model strategy, architecture, evaluation, improvement roadmap |

## Do's and Don'ts

- **Do** respect Bart Torvik rate limits; prefer bulk endpoints (`&csv=1`, `&json=1`) over mass scraping
- **Do** exclude year 2020 from analysis (COVID, no NCAA tournament)
- **Do** assume optional external data (EFFHGT, TALENT, etc.) is filled with 0 when absent; the pipeline runs without it
- **Do** treat historical logo or champion badges as title-year data points, not current-team points; if a chart shows former champion markers, compute their coordinates from the championship season's metrics relative to that season's seed baseline
- **Don't** write a frozen input, run `select`, or relabel a retrospective result as prospective
- **Don't** mass scrape; use documented bulk endpoints (see [docs/data_sources.md](docs/data_sources.md))
