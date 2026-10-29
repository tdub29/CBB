# Model Optimization Report (20260929)

> **Vintage note (added 2026-10-06; numbers below unchanged).** Run before the 2026-09-29 charter on an
> earlier training CSV (sha256 prefix `8565be043d11155d`), not the frozen `a35137c1619032e6` the log5 test
> uses. This evaluates the legacy round-reached model on one holdout season; it is not the pre-registered
> test. Current result: `reports/model_vs_seeds_powered.md` (no detectable edge over seed-pair rates).

## Headline
- Leakage assertion: **PASSED** on all 8 feature sets (fails the run at r >= 0.9; strongest candidate feature `eff_ratio` scores 0.5938). Negative controls PASE, PAKE, round and result are all rejected on the real training rows.
- 2025 holdout, refit on seasons before 2025: model (`baseline_raw_poly` / `ridge`) MSE 0.5441, MAE 0.5550; historical seed lookup MSE 0.5839, MAE 0.5874. The model beats the seed line on point estimate only. One 64-row season cannot separate them (independent check, 2026-09-29: 95% CI on the MSE difference -0.198 to +0.109), so read it as no worse than the seed line, not better.
- 5 of 8 honest candidate models beat the seed lookup on 2025 MSE (table in Section 5; reported only, never used for selection).
- Outcome-free scoring: the 2025 season re-scored with every outcome column deleted gives identical predictions (max abs diff 0.0e+00), so the model can score a field whose tournament has not been played.

## Run Configuration
- command: `python scripts/run_pipeline.py --holdout 2025` (or `python scripts/run_analysis.py --holdout-year 2025`)
- input_csv: `data/processed/cbb_training_clean.csv` (sha256 prefix `8565be043d11155d`)
- code_revision: `2ce8199 + uncommitted changes`
- holdout_year: `2025`
- cv: season-grouped `GroupKFold`, `10` folds over 16 training seasons
- search_depth: `balanced`
- random_state: `42`
- selection metric: season-grouped CV MSE on the 0-6 wins target. MSE is kept for continuity only; bracket-level selection uses Brier, log loss and calibration (`reports/bracket_simulation_*.md`).

## Section 1: Data Profile
- dataset_shape: `5971 rows x 118 columns`
- year_range: `2008-2025`
- tournament_rows (seed+round non-null): `1088`
- non_tournament_rows: `4883`
- duplicate (Team, year) rows: `0`
- modeled_rows_excluding_2020: `1088`
- pre_holdout_rows (<2025): `1024`
- holdout_rows (2025): `64`

### Distribution Summary
| feature | mean | std | min | 25% | 50% | 75% | max |
| --- | --- | --- | --- | --- | --- | --- | --- |
| AdjOE | 102.8510 | 7.4716 | 72.1000 | 97.8000 | 102.5000 | 107.7000 | 129.5000 |
| AdjDE | 102.8534 | 6.5015 | 81.8000 | 98.3000 | 103.0000 | 107.5000 | 125.3000 |
| Barthag | 0.4940 | 0.2565 | 0.0051 | 0.2781 | 0.4724 | 0.7155 | 0.9833 |
| seed | 8.7698 | 4.6715 | 1.0000 | 5.0000 | 9.0000 | 13.0000 | 16.0000 |
| round | 0.9844 | 1.3410 | 0.0000 | 0.0000 | 0.5000 | 1.2500 | 6.0000 |

![Missingness Heatmap](figures/missingness_heatmap.png)

![Correlation Matrix](figures/correlation_matrix.png)

- corr(Barthag, AdjOE) = `0.8594`, corr(Barthag, AdjDE) = `-0.8258`, corr(AdjOE, AdjDE) = `-0.4603`. Barthag is built from the two efficiencies, so high collinearity is expected.

## Leakage Register
Prediction moment: the published bracket on Selection Sunday, before any NCAA tournament game.

| risk | control | status |
| --- | --- | --- |
| Post-tournament values | 15 outcome columns in OUTCOME_COLUMNS, excluded from every set (PAKE, PASE, round dummies, result, finish, postseason W-L, final record) | pass |
| Feature reconstructs the target | assert_no_target_leakage on every set, fails at r >= 0.9 (seed-residual Pearson or raw Spearman) | pass, max 0.5938 |
| Guard cannot fail | negative controls injected on real rows | pass, all rejected |
| Unmapped or unavailable column | assert_prediction_time_contract on every set; unmapped columns fail the run | pass |
| Full-sample aggregates | ranks and conf_avg_barthag are within-season, from pre-tournament columns; seed lookup fit on training seasons only | pass by construction |
| Preprocessing before split | imputer and scaler inside the sklearn Pipeline, refit in every fold | pass by construction |
| Repeated team-season rows | duplicate (Team, year) count | pass, 0 |
| Season overlap train/validation | GroupKFold by season; holdout season later than every training season | pass |
| Coach history window | prior 5 tournaments only, current season excluded (tests/test_team_history_pipeline.py) | pass |

### Guard negative controls
| injected_feature | leak_score | guard |
| --- | --- | --- |
| PASE | 0.9935 | rejected |
| PAKE | 0.9560 | rejected |
| round | 1.0000 | rejected |
| result | 1.0000 | rejected |

### Prediction-time feature inventory
`leak_score` is the guard statistic on pre-holdout tournament rows. Rank columns (`*_rank`, within-season ranks of the listed stats) and polynomial terms inherit their base column's source.

| column | source | pre_tournament | in_candidate_sets | leak_score |
| --- | --- | --- | --- | --- |
| AdjOE | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.4739 |
| AdjDE | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.4348 |
| Barthag | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.5938 |
| WINS | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.3677 |
| GAMES | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.2153 |
| EFG% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1346 |
| EFGD% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1953 |
| FTR | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0843 |
| FTRD | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1369 |
| TOR | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1851 |
| TORD | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0603 |
| ORB | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1964 |
| DRB | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0355 |
| Adj T. | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0106 |
| 2P% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1288 |
| 2P%D | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1772 |
| 3P% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0702 |
| 3P%D | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1217 |
| BLK% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1753 |
| BLKED% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1135 |
| AST% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0901 |
| OPAST% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0210 |
| 3PRATE | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0282 |
| 3PRATED | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0121 |
| AVGHGT | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.2711 |
| EFFHGT | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.2305 |
| EXP. | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.1152 |
| year | Identifier / empty | NO | no | 0.0000 |
| PAKE | Torvik tournament result vs expectation (PAKE, PASE) | NO | no | 0.9560 |
| PASE | Torvik tournament result vs expectation (PAKE, PASE) | NO | no | 0.9935 |
| TALENT | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.4735 |
| blank | Identifier / empty | NO | yes |  |
| FT% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0411 |
| OPFT% | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.0529 |
| PPPOFF | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.3163 |
| PPPDEF | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.2305 |
| ELITESOS | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.4427 |
| _extra_42 | Selection Sunday bracket seed | yes | yes | 0.5876 |
| _extra_45 | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.2713 |
| _extra_46 | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.3438 |
| _extra_47 | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.2591 |
| _extra_48 | Team history, consecutive seasons with current coach | yes | yes | 0.1149 |
| _extra_49 | Conference membership / same-season mean regular-season Barthag | yes | yes | 0.4078 |
| W | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.3677 |
| L | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.2898 |
| G | Torvik team table type=R (games through conference tournaments) | yes | yes | 0.2153 |
| Rk | Within-season rank of regular-season Barthag | yes | yes | 0.5898 |
| seed | Selection Sunday bracket seed | yes | yes |  |
| result | Tournament outcome / target encodings | NO | no | 1.0000 |
| round | Tournament outcome / target encodings | NO | no | 1.0000 |
| coach_tourn_appearances_5y | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.3212 |
| coach_tourn_wins_5y | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.4004 |
| coach_tourn_losses_5y | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.3123 |
| coach_years_with_program | Team history, consecutive seasons with current coach | yes | yes | 0.0846 |
| coach_tourn_games_5y | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.3657 |
| coach_tourn_win_pct_5y | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.3886 |
| coach_avg_wins_per_appearance | Team history, prior 5 tournaments only (current season excluded) | yes | yes | 0.3884 |
| Power6 | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| B12 | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| B10 | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| SEC | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| ACC | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| P12 | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| BE | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| MWC | Conference membership / same-season mean regular-season Barthag | yes | yes |  |
| Round2 | Tournament outcome / target encodings | NO | no |  |
| S16 | Tournament outcome / target encodings | NO | no |  |
| E8 | Tournament outcome / target encodings | NO | no |  |
| F4 | Tournament outcome / target encodings | NO | no |  |
| Ship | Tournament outcome / target encodings | NO | no |  |
| Champ | Tournament outcome / target encodings | NO | no |  |
| eff_ratio | Engineered from pre-tournament columns | yes | yes | 0.5938 |
| seed_barthag | Engineered from pre-tournament columns | yes | yes | 0.4715 |
| coach_seed | Engineered from pre-tournament columns | yes | yes | 0.2285 |
| _extra_43 | Tournament outcome / target encodings | NO | no |  |
| ov_rec | Tournament outcome / target encodings | NO | no |  |
| _extra_40 | Tournament outcome / target encodings | NO | no |  |
| finish | Tournament outcome / target encodings | NO | no |  |

## Section 2: Baseline Evaluation (season-grouped CV)
- baseline_feature_count: `20`
- baseline_best_alpha_by_cv: `0.1`
- baseline_cv_mse: `1.088995`
- baseline_cv_mae: `0.738563`
- baseline_cv_fold_mse_std: `0.199899`
- seed_lookup_leave_one_season_out_mse: `1.135846`
- seed_lookup_leave_one_season_out_mae: `0.748307`

| alpha | cv_mse | cv_mae | cv_mse_std |
| --- | --- | --- | --- |
| 0.010000 | 1.089971 | 0.738701 | 0.202336 |
| 0.100000 | 1.088995 | 0.738563 | 0.199899 |
| 0.300000 | 1.089140 | 0.739272 | 0.197084 |
| 1.000000 | 1.092654 | 0.741241 | 0.193998 |
| 3.000000 | 1.097622 | 0.742372 | 0.191033 |
| 10.000000 | 1.103057 | 0.742679 | 0.184763 |
| 30.000000 | 1.114532 | 0.744476 | 0.174183 |

## Section 3: Feature Optimization
| feature_set | n_features | best_alpha | cv_mse | cv_mae | cv_mse_std | cv_splits | leak_guard_max_r | leak_guard_top_feature |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline_raw_poly | 20 | 0.100000 | 1.088995 | 0.738563 | 0.199899 | 10 | 0.593781 | Barthag |
| engineered_interactions | 23 | 0.100000 | 1.092742 | 0.738401 | 0.198384 | 10 | 0.593787 | eff_ratio |
| baseline_plus_unused | 25 | 0.100000 | 1.096721 | 0.741144 | 0.199916 | 10 | 0.593781 | Barthag |
| drop_barthag_keep_adjoe_adjde | 18 | 0.300000 | 1.104959 | 0.746927 | 0.191757 | 10 | 0.473937 | AdjOE |
| raw_plus_rank | 41 | 0.300000 | 1.107672 | 0.744558 | 0.197670 | 10 | 0.593781 | Barthag |
| kitchen_sink_numeric_no_leakage | 95 | 30.000000 | 1.168444 | 0.765507 | 0.179159 | 10 | 0.593787 | eff_ratio |
| rank_only | 21 | 3.000000 | 1.201859 | 0.789053 | 0.154117 | 10 | 0.589627 | Barthag_rank |

- selected_feature_set: `baseline_raw_poly`
- selected_feature_count: `20`
- selected_ridge_alpha: `0.1`

### VIF (Top 20)
> **Note (added 2026-10-29; values below unchanged).** These come from auxiliary regressions without an
> intercept, which misstate the VIF of uncentred columns; `compute_vif` in `scripts/run_analysis.py` now fits
> one. The pre-2025 rows of the frozen `a35137c1619032e6` CSV reproduce every value below to four decimals
> with the old formula. With the intercept, AdjDE is 1004.95 and AdjDE_2 987.69 (listed as 37.41 and 31.62);
> no other value moves by more than 14 (largest: Barthag_2, 213.42 to 226.84).

| feature | vif |
| --- | --- |
| seed_2 | 1099.6722 |
| coach_tourn_games_5y | 767.5386 |
| seed_3 | 444.2399 |
| coach_tourn_wins_5y | 396.6781 |
| seed | 218.1813 |
| Barthag_2 | 213.4202 |
| Barthag | 111.1276 |
| coach_tourn_appearances_5y | 91.0799 |
| AdjDE | 37.4101 |
| AdjOE | 34.2934 |
| AdjDE_2 | 31.6164 |
| coach_avg_wins_per_appearance | 14.3600 |
| coach_tourn_win_pct_5y | 11.9705 |
| TALENT | 2.9325 |
| EFGD% | 2.4534 |
| TOR | 2.2838 |
| 3P% | 1.8182 |
| ORB | 1.8047 |
| coach_years_with_program | 1.4671 |
| AST% | 1.1976 |

### Scaled Ridge Coefficients (Top 25)
| feature | coef | abs_coef |
| --- | --- | --- |
| seed_2 | 3.813824 | 3.813824 |
| seed | -2.198708 | 2.198708 |
| seed_3 | -1.921280 | 1.921280 |
| coach_tourn_games_5y | 1.642235 | 1.642235 |
| coach_tourn_wins_5y | -1.125524 | 1.125524 |
| AdjOE | 0.891530 | 0.891530 |
| AdjDE | -0.780596 | 0.780596 |
| Barthag | -0.594109 | 0.594109 |
| coach_tourn_appearances_5y | -0.564704 | 0.564704 |
| Barthag_2 | -0.368382 | 0.368382 |
| coach_tourn_win_pct_5y | -0.114042 | 0.114042 |
| TALENT | 0.105327 | 0.105327 |
| AdjDE_2 | -0.104798 | 0.104798 |
| EFGD% | 0.097615 | 0.097615 |
| coach_avg_wins_per_appearance | 0.079297 | 0.079297 |
| AST% | -0.058974 | 0.058974 |
| ORB | 0.042817 | 0.042817 |
| TOR | -0.040459 | 0.040459 |
| 3P% | -0.024947 | 0.024947 |
| coach_years_with_program | 0.014432 | 0.014432 |

## Section 4: Algorithm Search
| model_id | algorithm | params | cv_mse | cv_mae | cv_mse_std | cv_splits |
| --- | --- | --- | --- | --- | --- | --- |
| 2 | ridge | alpha=0.1 | 1.088995 | 0.738563 | 0.199899 | 10 |
| 3 | ridge | alpha=0.3 | 1.089140 | 0.739272 | 0.197084 | 10 |
| 1 | ridge | alpha=0.01 | 1.089971 | 0.738701 | 0.202336 | 10 |
| 4 | ridge | alpha=1.0 | 1.092654 | 0.741241 | 0.193998 | 10 |
| 5 | ridge | alpha=3.0 | 1.097622 | 0.742372 | 0.191033 | 10 |
| 6 | ridge | alpha=10.0 | 1.103057 | 0.742679 | 0.184763 | 10 |
| 15 | elasticnet | alpha=0.01, l1_ratio=0.25 | 1.105215 | 0.742583 | 0.184483 | 10 |
| 16 | elasticnet | alpha=0.01, l1_ratio=0.5 | 1.109092 | 0.742603 | 0.182621 | 10 |
| 17 | elasticnet | alpha=0.01, l1_ratio=0.75 | 1.112154 | 0.742820 | 0.180886 | 10 |
| 8 | lasso | alpha=0.01 | 1.113971 | 0.743638 | 0.178898 | 10 |
| 7 | ridge | alpha=30.0 | 1.114532 | 0.744476 | 0.174183 | 10 |
| 44 | rf | n_estimators=100, max_depth=5 | 1.124922 | 0.747355 | 0.217329 | 10 |
| 47 | rf | n_estimators=200, max_depth=5 | 1.125079 | 0.747108 | 0.214067 | 10 |
| 36 | gbr | n_estimators=100, max_depth=3, learning_rate=0.05 | 1.160177 | 0.762425 | 0.209599 | 10 |
| 18 | elasticnet | alpha=0.1, l1_ratio=0.25 | 1.182713 | 0.772085 | 0.145937 | 10 |
| 48 | rf | n_estimators=200, max_depth=10 | 1.186970 | 0.771851 | 0.199472 | 10 |
| 45 | rf | n_estimators=100, max_depth=10 | 1.191712 | 0.772026 | 0.203601 | 10 |
| 49 | rf | n_estimators=200, max_depth=None | 1.198949 | 0.776929 | 0.186240 | 10 |
| 46 | rf | n_estimators=100, max_depth=None | 1.202994 | 0.778037 | 0.187658 | 10 |
| 40 | gbr | n_estimators=200, max_depth=3, learning_rate=0.05 | 1.210515 | 0.772198 | 0.209422 | 10 |
| 19 | elasticnet | alpha=0.1, l1_ratio=0.5 | 1.217688 | 0.784796 | 0.135509 | 10 |
| 20 | elasticnet | alpha=0.1, l1_ratio=0.75 | 1.223200 | 0.783042 | 0.128572 | 10 |
| 9 | lasso | alpha=0.1 | 1.230787 | 0.782462 | 0.121326 | 10 |
| 37 | gbr | n_estimators=100, max_depth=3, learning_rate=0.1 | 1.231926 | 0.779862 | 0.206540 | 10 |
| 21 | elasticnet | alpha=0.3, l1_ratio=0.25 | 1.233293 | 0.787716 | 0.123700 | 10 |
| 38 | gbr | n_estimators=100, max_depth=5, learning_rate=0.05 | 1.256219 | 0.786231 | 0.231981 | 10 |
| 22 | elasticnet | alpha=0.3, l1_ratio=0.5 | 1.259542 | 0.797176 | 0.102753 | 10 |
| 41 | gbr | n_estimators=200, max_depth=3, learning_rate=0.1 | 1.297505 | 0.803570 | 0.187824 | 10 |
| 23 | elasticnet | alpha=0.3, l1_ratio=0.75 | 1.299955 | 0.822118 | 0.081509 | 10 |
| 42 | gbr | n_estimators=200, max_depth=5, learning_rate=0.05 | 1.309719 | 0.802862 | 0.225892 | 10 |

![Model Comparison](figures/model_comparison.png)

- winning_algorithm: `ridge`
- winning_params: `alpha=0.1`
- winning_cv_mse: `1.088995`
- winning_cv_mae: `0.738563`

## Section 5: Holdout vs Seed Lookup
- holdout_year: `2025`
- holdout_mse: `0.544140`
- holdout_mae: `0.554999`
- seed_lookup_holdout_mse: `0.583893`
- seed_lookup_holdout_mae: `0.587402`
- model_beats_seed_lookup_mse: `true`
- model_beats_seed_lookup_mae: `true`
- holdout_rows_scored: `64`
- outcome_free_scoring_max_abs_diff: `0.0e+00`
- seasons after holdout in input: `none`
- final_retrain_rows (all seasons, for future fields): `1088`
- model_artifact: `models/analysis_best_model_20260929.joblib`

### Every candidate on the holdout (reported only)
| feature_set | algorithm | holdout_mse | holdout_mae |
| --- | --- | --- | --- |
| raw_plus_rank | ridge | 0.5348 | 0.5402 |
| engineered_interactions | ridge | 0.5417 | 0.5544 |
| baseline_raw_poly | ridge (winner) | 0.5441 | 0.5550 |
| baseline_raw_poly | ridge | 0.5441 | 0.5550 |
| baseline_plus_unused | ridge | 0.5655 | 0.5713 |
| seed only | historical mean wins by seed | 0.5839 | 0.5874 |
| drop_barthag_keep_adjoe_adjde | ridge | 0.5924 | 0.5987 |
| kitchen_sink_numeric_no_leakage | ridge | 0.7102 | 0.6381 |
| rank_only | ridge | 0.8279 | 0.6760 |

![Holdout Actual vs Predicted](figures/holdout_2025_actual_vs_pred.png)

### Holdout Prediction Table (first 25)
| Team | year | seed | actual_round | pred_round | seed_baseline_round |
| --- | --- | --- | --- | --- | --- |
| Mount St. Mary's | 2025 | 16.0000 | 0.0000 | -0.3486 | 0.0312 |
| Auburn | 2025 | 1.0000 | 4.0000 | 3.8782 | 3.2188 |
| Duke | 2025 | 1.0000 | 4.0000 | 4.0687 | 3.2188 |
| Alabama | 2025 | 2.0000 | 3.0000 | 3.0890 | 2.1719 |
| Florida | 2025 | 1.0000 | 6.0000 | 3.4441 | 3.2188 |
| Missouri | 2025 | 6.0000 | 0.0000 | 1.6407 | 0.7344 |
| Texas Tech | 2025 | 3.0000 | 3.0000 | 2.2535 | 1.9062 |
| Purdue | 2025 | 4.0000 | 2.0000 | 2.0493 | 1.6094 |
| Houston | 2025 | 1.0000 | 5.0000 | 4.5361 | 3.2188 |
| BYU | 2025 | 6.0000 | 2.0000 | 1.4935 | 0.7344 |
| Kentucky | 2025 | 3.0000 | 2.0000 | 1.9644 | 1.9062 |
| Gonzaga | 2025 | 8.0000 | 1.0000 | 1.8943 | 0.7656 |
| Arizona | 2025 | 4.0000 | 2.0000 | 2.0933 | 1.6094 |
| Wisconsin | 2025 | 3.0000 | 1.0000 | 1.9276 | 1.9062 |
| Illinois | 2025 | 6.0000 | 1.0000 | 1.7426 | 0.7344 |
| Utah St. | 2025 | 10.0000 | 0.0000 | 0.6436 | 0.5625 |
| Connecticut | 2025 | 8.0000 | 1.0000 | 0.3964 | 0.7656 |
| Baylor | 2025 | 9.0000 | 1.0000 | 1.0416 | 0.6562 |
| Iowa St. | 2025 | 3.0000 | 1.0000 | 2.1654 | 1.9062 |
| Tennessee | 2025 | 2.0000 | 3.0000 | 2.5928 | 2.1719 |
| Maryland | 2025 | 4.0000 | 2.0000 | 2.0058 | 1.6094 |
| Mississippi St. | 2025 | 8.0000 | 0.0000 | 1.1645 | 0.7656 |
| Vanderbilt | 2025 | 10.0000 | 0.0000 | 0.8991 | 0.5625 |
| North Carolina | 2025 | 11.0000 | 0.0000 | 1.2221 | 0.9062 |
| Louisville | 2025 | 8.0000 | 0.0000 | 1.1920 | 0.7656 |

### Winner Feature Importance
![Feature Importance](figures/feature_importance.png)

| feature | importance |
| --- | --- |
| seed_2 | 3.813824 |
| seed | 2.198708 |
| seed_3 | 1.921280 |
| coach_tourn_games_5y | 1.642235 |
| coach_tourn_wins_5y | 1.125524 |
| AdjOE | 0.891530 |
| AdjDE | 0.780596 |
| Barthag | 0.594109 |
| coach_tourn_appearances_5y | 0.564704 |
| Barthag_2 | 0.368382 |
| coach_tourn_win_pct_5y | 0.114042 |
| TALENT | 0.105327 |
| AdjDE_2 | 0.104798 |
| EFGD% | 0.097615 |
| coach_avg_wins_per_appearance | 0.079297 |
| AST% | 0.058974 |
| ORB | 0.042817 |
| TOR | 0.040459 |
| 3P% | 0.024947 |
| coach_years_with_program | 0.014432 |

## Recommendation
The `baseline_raw_poly` / `ridge` candidate beats the historical seed lookup on the 2025 holdout on both MSE (0.5441 vs 0.5839) and MAE (0.5550 vs 0.5874) on point estimate. One season is 64 rows and cannot separate them, so treat this as provisional until the walked-forward bracket evaluation agrees.
