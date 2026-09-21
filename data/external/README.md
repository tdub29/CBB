# External Data

Optional and legacy data. The main pipeline runs without these; optional columns are filled with 0 when absent.

## Layout

| Path | Role | Contents |
|------|------|----------|
| **sources/** | Fetched team tables | `trank_team_table_data_YYYY.csv`, `trank_team_table_data.csv` — per-year or combined from [fetch_team_tables.py](../../scripts/fetch_team_tables.py). Default output dir for fetch. |
| **tests/** | Test / reference CSVs | Connecticut and other small test files used to validate pipeline outputs: `team_history_cache_connecticut_test.csv`, `coach_tournament_experience_connecticut_test.csv`, `test_connecticut.csv`, `test_teams.csv`. |
| **legacy/** | Legacy coach experience | Superseded by `data/raw/team_history_cache_enriched.csv`. `coach_tournament_experience.csv` and `coach_2025_temp/` live here; [madness.py](../../src/pipelines/madness.py) still merges any `coach_tournament_experience*.csv` found under `data/external/` if coach columns are missing from the base table. |
| **templates/** | Bracket and one-off inputs | `bracket_template.csv` (for `--bracket`), `temp_2025_teams.csv` (for fetch_coach_experience 2025 merge). |

## File roles

| File / pattern | Purpose |
|----------------|---------|
| `trank_team_table_data*.csv` | Team depth metrics; fetched by `scripts/fetch_team_tables.py`. Pipeline primary source is `data/raw/` per-year CSVs; these are an alternative source when present. |
| `legacy/coach_tournament_experience*.csv` | Legacy coach experience file. Prefer `data/raw/team_history_cache_enriched.csv` (built by `scripts/enrich_team_history_cache.py`). |
| `templates/bracket_template.csv` | Example bracket CSV for simulation: `--bracket data/external/templates/bracket_template.csv`. |
| `cleanteam.xlsx`, `teamdata23.csv` | Optional holdout-year data (place at root if used). |

See [docs/data_sources.md](../../docs/data_sources.md) for details and where to obtain files.
