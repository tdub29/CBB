# External Data

Optional and legacy data. The default pipeline does not read this folder; its team-table columns come from `data/raw/assembled/all_trank_team_tables.csv` (see [docs/data_sources.md](../../docs/data_sources.md)).

## Layout

| Path | Role | Contents |
|------|------|----------|
| **sources/** | Fetched team tables | `trank_team_table_data_YYYY.csv`, `trank_team_table_data.csv` — per-year or combined from [fetch_team_tables.py](../../scripts/fetch_team_tables.py). Default output dir for fetch. |
| **tests/** | Test / reference CSVs | Connecticut and other small test files used to validate pipeline outputs: `team_history_cache_connecticut_test.csv`, `coach_tournament_experience_connecticut_test.csv`, `test_connecticut.csv`, `test_teams.csv`. |
| **legacy/** | Legacy coach experience | Superseded by `data/raw/team_history_cache_enriched.csv`. `coach_tournament_experience.csv` and `coach_2025_temp/` live here. Only the legacy [rebuild_from_raw.py](../../scripts/rebuild_from_raw.py) path still merges `coach_tournament_experience*.csv` from `data/external/` when coach columns are missing. |
| **team_logos/espn_ncaam/** | Logo cache | `<espn_team_id>.png` images drawn on the 2026-field figures by the eight `scripts/*_analysis_05_07.py` scripts. Those scripts fetch ESPN's public team catalog (`site.api.espn.com`) on every run to map team names to ids, so they need network access even when every logo is cached; images missing from the cache are downloaded on the same run. |
| **templates/** | Bracket and one-off inputs | `bracket_template.csv` (for `--bracket`), `temp_2025_teams.csv` (for fetch_coach_experience 2025 merge). |

## File roles

| File / pattern | Purpose |
|----------------|---------|
| `trank_team_table_data*.csv` | Team depth metrics fetched by `scripts/fetch_team_tables.py` into `sources/`. Not read by the default pipeline, which uses the per-year CSVs in `data/raw/sources/`. The committed files hold postseason-sized records (see docs/data_sources.md). |
| `legacy/coach_tournament_experience*.csv` | Legacy coach experience file. Prefer `data/raw/team_history_cache_enriched.csv` (built by `scripts/enrich_team_history_cache.py`). |
| `templates/bracket_template.csv` | Example bracket CSV for simulation: `--bracket data/external/templates/bracket_template.csv`. |
| `cleanteam.xlsx`, `teamdata23.csv` | Optional holdout-year data for the R pipeline (place at root if used; not committed). |

See [docs/data_sources.md](../../docs/data_sources.md) for details and where to obtain files.
