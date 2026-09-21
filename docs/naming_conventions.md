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

## Raw Data Prep (pre-pipeline assembly)

| Variable / File | Description |
|----------------|-------------|
| `team_history_cache` | Scraped team history: team, year, coach, seed, finish, conf per team-year |
| `team_history_cache_enriched` | Cache + 4 coach experience columns (appearances_5y, wins_5y, losses_5y, years_with_program) |
| `trank_postseason_final` | Concatenated postseason records (all years, headerless) |
| `all_trank_team_tables` | Master assembled table: base stats + postseason rec + enriched history (49 cols) |
| `all_trank_team_tables_tournament` | Filtered to tournament teams only (finish IS NOT NULL) |

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
| `coach_tourn_appearances_5y` | Number of NCAA tournaments head coach was in (prior 5 years) |
| `coach_tourn_wins_5y` | Coach’s NCAA tournament wins in prior 5 years |
| `coach_tourn_losses_5y` | Coach’s NCAA tournament losses in prior 5 years |
| `coach_years_with_program` | Consecutive years head coach has been at this program |
| `coach_tourn_win_pct_5y` | Coach tournament win rate (prior 5y); 0 when no games |
| `coach_tourn_games_5y` | Coach tournament games played (prior 5y): wins + losses |
| `coach_avg_wins_per_appearance` | Avg wins per tournament appearance (prior 5y); 0 when no appearances |