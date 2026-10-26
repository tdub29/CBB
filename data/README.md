# Data Directory

| Subdirectory | Purpose |
|--------------|---------|
| `raw/` | **Source + assembled.** Per-year team/postseason CSVs, team-history cache, enriched cache, and the assembled master tables. Pipeline reads `all_trank_team_tables.csv` from here. See [raw/README.md](raw/README.md). |
| `interim/` | Intermediate data after light cleaning or joins (currently unused). |
| `processed/` | **Pipeline outputs.** Full training export, canonical clean set, and research slice. See [processed/README.md](processed/README.md). |
| `external/` | **Optional / legacy.** Fetched team-table CSVs, bracket templates, coach-experience legacy files, and test/reference CSVs. See [external/README.md](external/README.md). |

## File roles at a glance

| Role | Location | Used by |
|------|----------|---------|
| Per-year regular-season tables | `raw/sources/trank_team_table_data08..25.csv`, `raw/trank_team_table_data2026.csv` | recompile_all_trank_team_tables.py |
| Per-year postseason tables | `raw/trank_postseason/trank_postseason_YYYY.csv` | append_postseason_final.py → trank_postseason_final.csv |
| Postseason concat | `raw/trank_postseason_final.csv` | recompile script (Postseason rec join) |
| Team-history cache | `raw/team_history_cache.csv` | populate_team_history_cache.py (write); enrich + pipeline (read) |
| Enriched cache | `raw/team_history_cache_enriched.csv` | recompile script (history + experience join) |
| Master table (final) | `raw/assembled/all_trank_team_tables.csv` | madness.py (main pipeline); all sources flow into this |
| Tournament subset | `raw/assembled/all_trank_team_tables_tournament.csv` | Validation / analysis scripts |
| Clean training set | `processed/cbb_training_clean.csv` | Modeling, research |
| Optional coach experience (legacy) | `external/legacy/coach_tournament_experience*.csv` | madness.py fallback merge |
| Fetched team tables (optional) | `external/sources/trank_team_table_data*.csv` | fetch_team_tables.py output |
| Staging / temp (raw) | `raw/_staging/` | Recompile fallbacks, merge temps, test outputs (gitignored) |

## Data lifecycle

```
raw sources (per-year CSVs, team-history cache)
  → enrich_team_history_cache.py        → team_history_cache_enriched.csv
  → recompile_all_trank_team_tables.py  → all_trank_team_tables.csv + tournament filter
  → madness.py (run_pipeline.py)        → processed/cbbtrainingdata_YYYYMMDD.csv
                                        → processed/cbb_training_clean.csv
```

See [docs/data_lineage.md](../docs/data_lineage.md) for the full diagram.
