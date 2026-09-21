# External Data

Optional data files placed here extend the pipeline with richer features. **The pipeline runs without them**; optional columns are filled with 0.

**Save location:** All fetched data lives in `data/external/`. Team-tables CSVs are written here by `scripts/fetch_team_tables.py`.

## Naming conventions

| File | Purpose |
|------|---------|
| `trank_team_table_data*.csv` | Team depth metrics (EFFHGT, TALENT, etc.); joined to scraped data. Fetched with regular-season filter. |
| `cleanteam.xlsx` | 2023 (or holdout year) tournament teams for validation |
| `teamdata23.csv` | 2023 team-level data to join with cleanteam.xlsx |

See [docs/data_sources.md](../../docs/data_sources.md) for details and where to obtain these files.
