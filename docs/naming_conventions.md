# Naming Conventions

Consistent, descriptive names for data frames and key variables used across the pipelines.

## Data Frames (ingestion → modeling)

| Variable | Description |
|----------|-------------|
| `regular_season_raw` | Raw regular-season scrape from Bart Torvik (type=R) |
| `postseason_raw` | Raw postseason scrape (type=P, conference tournaments) |
| `ncaa_tournament_raw` | Raw NCAA tournament scrape (type=T) |
| `conf_tourney_with_ncaa` | Postseason joined with NCAA tourney W/L |
| `teams_with_conf_tourney_wl` | Regular season + conference tournament W/L |
| `teams_annotated` | Teams with seed, result, round extracted from Team strings |
| `training_data` | Cleaned data for modeling (features, no NAs) |
| `training_data_full` | Full feature set before dropping cols (for output CSV) |
| `external_team_depth` | Optional team depth metrics from external CSVs |

## Holdout / validation

| Variable | Description |
|----------|-------------|
| `holdout_2023` | 2023 tournament teams for validation |
| `holdout_2023_team_depth` | 2023 team-level data (optional join) |
| `holdout_2023_merged` | holdout_2023 joined with team depth |
| `holdout_actual_results` | Actual 2023 round results (optional, for RMSE) |
| `holdout_predictions_combined` | All seed predictions for 2023 |
| `historical_predictions_combined` | All seed predictions on training data |

## Models and seed subsets

| Variable | Description |
|----------|-------------|
| `general_model` | Best general linear model (jackknife LOO) |
| `training_seed_N` | Training subset for seed-tier N (e.g. seeds 1–2) |
| `holdout_seed_N` | Holdout subset for seed-tier N |
| `model_seed_N` | Fitted model for seed-tier N |
| `best_general_mse` | MSE of best general model |
| `best_mse_seed_N` | MSE of best seed-tier N model |

## Column names (kept short for formulas)

| Column | Description |
|--------|-------------|
| `predround` | General model predicted round |
| `seedpred` | Seed-model predicted round |
