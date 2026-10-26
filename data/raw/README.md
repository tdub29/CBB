# Raw Data

Source inputs and assembled tables for the data-prep pipeline. See [docs/data_lineage.md](../../docs/data_lineage.md) for the full flow.

## Roles

| Role | Path(s) | Description |
|------|---------|-------------|
| **Source — regular season** | `sources/trank_team_table_data08.csv` … `trank_team_table_data25.csv` (no 20), `trank_team_table_data2026.csv` | Per-year Bart Torvik team tables (type=R for 2008–2025; 2026 is the current-season table). [recompile_all_trank_team_tables.py](../../scripts/recompile_all_trank_team_tables.py) reads from `sources/` then raw root (2026 lives in the raw root). |
| **Source — postseason** | `trank_postseason/trank_postseason_2008.csv` … `trank_postseason_2025.csv` (no 2020) | Per-year postseason CSVs (type=P), one season per file, named by the season in the `YEAR` column. |
| **Derived — concat** | `trank_postseason_final.csv` | Single file of all postseason rows (no header), built by [append_postseason_final.py](../../scripts/append_postseason_final.py). Input to recompile script. |
| **Source — team history** | `team_history_cache.csv` | Scraped coach/seed/finish/conf per team-year from [populate_team_history_cache.py](../../scripts/populate_team_history_cache.py). |
| **Derived — enriched cache** | `team_history_cache_enriched.csv` | Cache + 4 coach-experience columns from [enrich_team_history_cache.py](../../scripts/enrich_team_history_cache.py). Input to recompile. |
| **Output — assembled** | `assembled/all_trank_team_tables.csv` | **Final** master table: base + Postseason rec + history + experience (49 cols). All sources flow into this; primary input for the modeling pipeline. |
| **Output — tournament only** | `assembled/all_trank_team_tables_tournament.csv` | Same table filtered to rows where `finish` is not null (NCAA tournament teams only). |
| **Output — 2026 slice** | `assembled/trank_team_table_data2026_enriched.csv` | `Year == 2026` rows of the assembled table (current season; no postseason or tournament results). |
| **Output (fallback)** | `_staging/all_trank_team_tables_recompiled.csv` | When the main file is locked, recompile writes here instead. Same content as the primary table. |
| **Validation** | `validation/finish_counts.txt`, `year_counts.txt`, `missing_seeds_by_year.txt` | Check/seed-count outputs (manual or from validation scripts). |
| **Samples** | `samples/trank_page_sample.html`, `samples/trank_table_sample.csv` | Written by [inspect_trank_page.py](../../scripts/inspect_trank_page.py) for manual T-Rank inspection. |
| **Staging** | `_staging/` | Recompile fallbacks (when `assembled/` is locked), merge temps, test outputs. Gitignored. |
| **Pipeline run artifacts** | `pipeline_runs/trank_regular_YYYYMMDD.csv`, `trank_postseason_YYYYMMDD.csv`, `trank_tournament_YYYYMMDD.csv` | Written by [madness.py](../../src/pipelines/madness.py) to `pipeline_runs/`. Read by [rebuild_from_raw.py](../../scripts/rebuild_from_raw.py) from `pipeline_runs/` or raw root. |

## Pipeline order

1. **Sources** must exist: per-year CSVs in `raw/sources/` (plus `raw/trank_team_table_data2026.csv`), `trank_postseason_final.csv` (rebuild from `raw/trank_postseason/` with `python scripts/append_postseason_final.py`), and `team_history_cache.csv`.
2. Run `enrich_team_history_cache.py` → `team_history_cache_enriched.csv`.
3. Run `recompile_all_trank_team_tables.py` → writes to `assembled/all_trank_team_tables.csv` and `assembled/all_trank_team_tables_tournament.csv`.

Or run both steps via: `python scripts/run_pipeline.py --prep-raw`.

**Hierarchy:** Inputs (sources, cache, postseason) flow into the **final** outputs in **`assembled/`**; that folder is the only place that holds the combined tables.
