# Processed Data

Outputs from the main modeling pipeline ([run_pipeline.py](../../scripts/run_pipeline.py) → [madness.py](../../src/pipelines/madness.py)). Used for training, research, and validation.

## Files (current)

| File | Role | Description |
|------|------|-------------|
| `cbb_training_clean.csv` | **Canonical training set** | Single clean dataset: no duplicate columns, no redundant vars, all outcomes. Primary input for modeling. |

## Archive

| Location | Contents |
|----------|----------|
| `archive/` | Older date-stamped exports: `cbbtrainingdata_YYYYMMDD.csv` (full) and `cbbtrainingdata_last2seasons_YYYYMMDD.csv` (research slice). Kept for reference; current pipeline writes the same patterns but only the latest run is kept in the root; older runs are archived here. |

## Flow

- Pipeline reads `data/raw/assembled/all_trank_team_tables.csv` and `data/raw/team_history_cache.csv`. It reads nothing from `data/external/`. Only the legacy `scripts/rebuild_from_raw.py` path reads that folder: top-level `data/external/trank_team_table_data*.csv`, and `data/external/**/coach_tournament_experience*.csv` when coach columns are missing (see `docs/data_sources.md`).
- Writes `cbbtrainingdata_YYYYMMDD.csv` and `cbb_training_clean.csv` here.
- To regenerate clean from an existing export:  
  `python scripts/build_clean_training_data.py [path/to/cbbtrainingdata_YYYYMMDD.csv]`
