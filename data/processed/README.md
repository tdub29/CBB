# Processed Data

Training data, prediction outputs and topic samples. The legacy pipeline ([run_pipeline.py](../../scripts/run_pipeline.py) → [madness.py](../../src/pipelines/madness.py)) writes run-tagged builds here; it never writes the frozen training CSV.

## Files (current)

| File | Role | Description |
|------|------|-------------|
| `cbb_training_clean.csv` | **Frozen input of the log5 test** | 2008-2025 rows from the March 2026 build plus the 2026 layer from `scripts/ingest_2026.py`. Hashed in [src/frozen_inputs.py](../../src/frozen_inputs.py); every writer refuses this path, and it cannot be rebuilt from live Torvik ([data/raw/torvik_2026/MANIFEST.md](../raw/torvik_2026/MANIFEST.md)). |
| `cbb_training_clean_YYYYMMDD.csv` | Run-tagged clean build | Written by `run_pipeline.py` and by `scripts/build_clean_training_data.py`. None is committed. |
| `cbbtrainingdata_YYYYMMDD.csv`, `cbbtrainingdata_last2seasons_YYYYMMDD.csv` | Run-tagged full export and research slice | Written by `run_pipeline.py`. The committed pair is dated 2026-03-08. |
| `powered_game_predictions_2010_2026.csv`, `powered_round_probabilities_2010_2026.csv` | Powered comparison predictions | Per-game and per-round predictions written by `scripts/run_powered_comparison.py score`. The round probabilities are post-First-Four. `scripts/predict_selection_sunday.py --check` compares its 2026 output against both files. |
| `round_predictions_YYYY.csv` (2021-2025) | Older ridge walk-forward | The 2026-09-29 run of `scripts/run_bracket_evaluation.py`; later runs write `round_predictions_YYYY_<tag>.csv`. |
| `*_sample.csv` (experience, free_throw_rate, rebounding, size, sos, tempo, three_point, turnovers) | Topic analysis inputs | Written by `scripts/build_<topic>_sample.py`, mostly from `data/raw/assembled/all_trank_team_tables.csv`. Read by the `scripts/<topic>_analysis_*.py` scripts. |
| `team_avg_top3_guard_height_2026.csv` | 2026 guard height | Written by `scripts/build_team_avg_top3_guard_height_2026.py`; read by `scripts/size_analysis_05_07.py`. |

## Archive

| Location | Contents |
|----------|----------|
| `archive/` | Older date-stamped exports: `cbbtrainingdata_YYYYMMDD.csv` (full), `cbbtrainingdata_last2seasons_YYYYMMDD.csv` (research slice) and earlier copies of the clean set. Kept for reference. |

## Flow

- The pipeline reads `data/raw/assembled/all_trank_team_tables.csv` and `data/raw/team_history_cache.csv` (and optional `data/external/coach_tournament_experience*.csv`).
- It writes `cbbtrainingdata_YYYYMMDD.csv`, `cbbtrainingdata_last2seasons_YYYYMMDD.csv` and `cbb_training_clean_YYYYMMDD.csv` here, tagged with the run date.
- To build a clean set from an existing export (writes `cbb_training_clean_YYYYMMDD.csv` unless `--output` names another path):
  `python scripts/build_clean_training_data.py [path/to/cbbtrainingdata_YYYYMMDD.csv]`
