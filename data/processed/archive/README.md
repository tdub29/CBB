# Processed archive

Date-stamped pipeline outputs from past runs:

- **cbbtrainingdata_YYYYMMDD.csv** — full training export (one row per Team/year; all features and outcomes).
- **cbbtrainingdata_last2seasons_YYYYMMDD.csv** — research slice (last N seasons only).

The canonical current training set is `../cbb_training_clean.csv` in the parent directory.

These exports predate the coach-tenure fix (commit e2f5b9f) and later fixes. In the two newest full exports here (2026-03-04 and 2026-03-08), `coach_years_with_program` is 1 for all 68 seeded 2021 teams because tenure restarted after the 2020 season. Use the export in the parent directory instead.
