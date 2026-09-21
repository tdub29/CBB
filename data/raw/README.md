# Raw Data

Source inputs and assembled tables for the data-prep pipeline. See [docs/data_lineage.md](../../docs/data_lineage.md) for the full flow.

## Roles

| Role | Path(s) | Description |
|------|---------|-------------|
| **Source — regular season** | `sources/trank_team_table_data08.csv` … `trank_team_table_data25.csv` (no 20) | Per-year Bart Torvik team tables (type=R). [recompile_all_trank_team_tables.py](../../scripts/recompile_all_trank_team_tables.py) reads from `sources/` then raw root. |
| **Source — postseason** | `trank_postseason/*.csv` | Per-year postseason CSVs (type=P). Concatenated → `trank_postseason_final.csv`. |
| **Derived — concat** | `trank_postseason_final.csv` | Single file of all postseason rows (no header). Input to recompile script. |
| **Source — team history** | `team_history_cache.csv` | Scraped coach/seed/finish/conf per team-year from [populate_team_history_cache.py](../../scripts/populate_team_history_cache.py). |
| **Derived — enriched cache** | `team_history_cache_enriched.csv` | Cache + 4 coach-experience columns from [enrich_team_history_cache.py](../../scripts/enrich_team_history_cache.py). Input to recompile. |
| **Output — assembled** | `assembled/all_trank_team_tables.csv` | **Final** master table: base + Postseason rec + history + experience (49 cols). All sources flow into this; primary input for the modeling pipeline. |
| **Output — tournament only** | `assembled/all_trank_team_tables_tournament.csv` | Same table filtered to rows where `finish` is not null (NCAA tournament teams only). |
| **Output (fallback)** | `assembled/all_trank_team_tables_recompiled.csv` | When the main file is locked, recompile writes here (or to `_staging/`). Same content as the primary table. |
| **Validation** | `validation/finish_counts.txt`, `year_counts.txt`, `missing_seeds_by_year.txt` | Check/seed-count outputs (manual or from validation scripts). |
| **Samples** | `samples/trank_page_sample.html`, `samples/trank_table_sample.csv` | Written by [inspect_trank_page.py](../../scripts/inspect_trank_page.py) for manual T-Rank inspection. |
| **Staging** | `_staging/` | Recompile fallbacks (when `assembled/` is locked), merge temps, test outputs. Gitignored. |
| **Pipeline run artifacts** | `pipeline_runs/trank_regular_YYYYMMDD.csv`, `trank_postseason_YYYYMMDD.csv`, `trank_tournament_YYYYMMDD.csv` | Written by [madness.py](../../src/pipelines/madness.py) to `pipeline_runs/`. Read by [rebuild_from_raw.py](../../scripts/rebuild_from_raw.py) from `pipeline_runs/` or raw root. |

## Pipeline order

1. **Sources** must exist: per-year CSVs in `raw/` and `raw/trank_postseason/`, and `team_history_cache.csv`.
2. Run `enrich_team_history_cache.py` → `team_history_cache_enriched.csv`.
3. Run `recompile_all_trank_team_tables.py` → writes to `assembled/all_trank_team_tables.csv` and `assembled/all_trank_team_tables_tournament.csv`.

Or run both steps via: `python scripts/run_pipeline.py --prep-raw`.

**Hierarchy:** Inputs (sources, cache, postseason) flow into the **final** outputs in **`assembled/`**; that folder is the only place that holds the combined tables.
