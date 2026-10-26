# Modeling Plan

Strategic plan for NCAA Men's March Madness prediction models. This covers the current architecture, what's working, what to try next, and how we evaluate progress.

## Objective

Predict how far each tournament team advances (round 0–6) and convert those predictions into game-level win probabilities for bracket simulation.

| Round code | Meaning |
|------------|---------|
| 0 | R64 (lost first game) |
| 1 | R32 |
| 2 | Sweet 16 |
| 3 | Elite 8 |
| 4 | Final Four |
| 5 | Finals |
| 6 | Champion |

---

## Training Data

**Source:** `data/processed/cbb_training_clean.csv`
- 5,971 team-seasons (2008–2025, excluding 2020)
- Models train on the 1,088 tournament rows (64 per season × 17 seasons, `seed` and `round` non-null), selected by `tournament_model_frame()` in `src/pipelines/madness.py`. Both `run_pipeline` and `scripts/run_analysis.py` use it, so the evaluated model is the shipped one.
- 70+ columns: Bart Torvik efficiency stats, team-table metrics, coach experience features, conference flags, outcome variables

**Key feature groups:**

| Group | Columns | Notes |
|-------|---------|-------|
| Efficiency | AdjOE, AdjDE, Barthag | Core Torvik ratings |
| Four Factors | EFG%, EFGD%, TOR, TORD, ORB, DRB, FTR, FTRD | Offensive/defensive factors |
| Shooting | 2P%, 2P%D, 3P%, 3P%D, 3PRATE, 3PRATED | Shot selection & accuracy |
| Physical | BLK%, BLKED%, AVGHGT, EFFHGT | Size/athleticism proxies |
| Roster | TALENT, EXP., AST%, OPAST% | Recruiting rank & experience |
| Schedule | ELITESOS | Strength of schedule metric |
| **Outcome-derived, NEVER features** | PAKE, PASE | Wins minus expectation, so they are computed from the result. Filing them under Schedule is what put them in the feature sets. Enforced by `assert_no_target_leakage` in `src/pipelines/madness.py`, which both `run_pipeline` and `scripts/run_analysis.py` call on every feature set. On the 1,088 tournament rows, plain corr(round, PASE) is only 0.767, but after residualising round on seed it is 0.9948 for PASE and 0.9553 for PAKE, against 0.058 for AdjOE as a clean control. With them in, the void 2026-03-04 report scored a 2025 holdout MAE of 0.060 on a 0–6 target. |
| Coach experience | coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_tourn_losses_5y, coach_years_with_program, coach_tourn_games_5y, coach_tourn_win_pct_5y, coach_avg_wins_per_appearance | Rolling 5-year tournament track record |
| Conference | Power6, B12, B10, SEC, ACC, P12, BE, MWC | One-hot flags |
| Seed | seed | NCAA committee ranking (1–16) |
| Outcomes | round, Round2, S16, E8, F4, Ship, Champ, result | Target & binary milestones |

---

## Current Model Architecture

### Phase 1: General Model
- **Algorithm:** Ridge regression (StandardScaler → Ridge, with an intercept)
- **Target:** `round` (0–6)
- **Features (10 + 6 coach + 4 polynomial):**
  - Base: TOR, Barthag, AdjOE, ORB, EFGD%, 3P%, AdjDE, seed, TALENT, AST%
  - Coach: coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_years_with_program, coach_tourn_win_pct_5y, coach_tourn_games_5y, coach_avg_wins_per_appearance
  - Polynomial: Barthag², AdjDE², seed², seed³
- **Provenance:** without the coach features this is the `predround` model in `madness.R` at commit 21a1e96 (2025-01-16) minus `WAB`, which the current data does not have. The coach features came later (absent at commit 0f6eb0c, 2026-01-31; see the Decision Log), after the 2025 tournament, and walk-forward does not show that they help (see Results).
- **Output:** `predround`, a continuous predicted-round value. It is the team strength for bracket simulation and an input to the seed models.
- **Intercept fix (2026-10):** the Python port used `fit_intercept=False` after `StandardScaler`. `madness.R` fits `lm(... + 0)` on raw columns, where the level can ride on the uncentred features, but once the scaler centres them every prediction averages 0. Measured on the 1,088 rows (ridge, alpha = 1): general model mean prediction 0.00 vs mean round 0.98; the seeds 1–2 band 0.00 vs 2.75. Both pipelines now fit an intercept.

### Phase 2: 14 Seed-Stratified Models (the "stack")
Each model trains on a 2–3 seed band. All include the general model's `predround` as a feature.
- **Out-of-fold `predround`:** seed-model training rows get `predround` from a general model refit without that row's season (`oof_predround()`, leave-one-season-out). `train_seed_models()` refuses a frame without the `predround_oof` column. The old pipeline fed them the in-sample fit, which had already seen each row's result; on seasons before 2025 that flattered the seed models' own CV MSE by +0.023 on average (12 of 14 bands), and by +0.117 for the seeds 1–2 band.
- **Known second-order leak (disclosed, not fixed):** the out-of-fold column is built once over all training seasons. When a seed model's own leave-one-season-out CV holds out season s, its training rows' `predround` came from general models that saw s. So a seed model's `season_cv_mse` is for picking alpha only, and the optimism figure above may be understated. Walk-forward and holdout scores rebuild the column inside each training window and are not affected.
- **New-season prediction:** the general model fit on every training season produces `predround`; when the stack is enabled, the seed models then overwrite it for seeds 1–15.
- **Which band scores which seed:** bands overlap and are applied in order, so the last band holding a seed wins. Seed s (1–13) is scored by the 3-seed band that starts at s, seeds 14–15 by [13, 14, 15], and seed 16 by the general model. Band 1 ([1, 2]) never scores anything.
- **Provenance:** the lists were hand-picked in `madness.R`, not selected out of sample. 13 of 14 match the file as committed on 2025-01-16 (commit 21a1e96) once `WAB` terms are removed; band 9 has since dropped `W`. So they predate the 2025 tournament but not the 2013–2024 walk-forward seasons.
- **Switch:** `model.use_seed_models` in `configs/default.yaml`. Default `false`: on walk-forward MSE the stack is not distinguishable from the general model alone (see Results).

| # | Train seeds | Features (besides predround) |
|---|-------------|------|
| 1 | 1,2 | Barthag, AdjOE, OPAST%, EXP., AdjDE, seed |
| 2 | 1,2,3 | Barthag, AdjOE, AdjDE, seed |
| 3 | 2,3,4 | Barthag, AdjOE, AdjDE, seed |
| 4 | 3,4,5 | AST%, Barthag, AdjOE, 2P%, TORD, AdjDE, seed |
| 5 | 4,5,6 | Barthag, AdjOE, AdjDE, seed |
| 6 | 5,6,7 | FTRD, AdjOE, AdjDE, seed |
| 7 | 6,7,8 | Barthag, AdjOE, TOR, Adj T., 2P%, seed |
| 8 | 7,8,9 | Barthag, AdjOE, AdjDE, TOR, BLKED%, AST%, OPAST%, seed |
| 9 | 8,9,10 | Barthag, AdjOE, AdjDE, seed, TOR, BLKED%, AST%, 3PRATE, ELITESOS |
| 10 | 9,10,11 | Barthag, AdjOE, AdjDE, seed, 2P%D, WINS, 3PRATED, TALENT, ELITESOS |
| 11 | 10,11,12 | FTR, AST%, OPAST%, ELITESOS, Barthag, AdjDE, AdjOE, seed |
| 12 | 11,12,13 | BLK%, AST%, OPAST%, EFFHGT, TALENT, Barthag, AdjOE, AdjDE, seed |
| 13 | 12,13,14 | W, FTR, FTRD, TOR, TORD, DRB, TALENT, ELITESOS, Barthag, AdjOE, AdjDE, seed |
| 14 | 13,14,15 | FTR, TORD, DRB, EXP., ELITESOS, Barthag, AdjOE, AdjDE, seed |

### Phase 3: Bracket Simulation
- Bradley-Terry model converts `predround` into pairwise win probabilities
- Monte Carlo simulation (default 10,000 runs) produces per-team probabilities for each round (R64 through Champion)
- Controlled by `k_bradley_terry` exponent (currently 1.0)

### Regularization
- Ridge `alpha` selected per model via CV grid search: `[0.0, 0.01, 0.1, 0.3, 1.0, 3.0, 10.0]`
- CV scheme: leave-one-season-out (`LeaveOneGroupOut` on `year`). The old scheme (shuffled KFold(5), or row-level LOO for n ≤ 50) put teams from the same bracket in train and test.
- Training metric: leave-one-season-out MSE per model (`reports/model_optimization_YYYYMMDD.md`). This is model selection inside the training years, not an accuracy claim.

---

## Validation

All of this is in `scripts/run_analysis.py`, with the splitters, baseline and stack in `src/pipelines/madness.py`.

- **The season is the unit.** One season is one 64-team bracket of 63 games: the rounds in a season always sum to 63, and a team's finish depends on who is in its path. No split ever puts a season on both sides (`season_group_splits()`, `walk_forward_splits()`; unit-tested in `tests/test_validation_and_stacking.py`).
- **Grouped CV:** `GroupKFold(10)` on year over the 16 seasons before the holdout. Used to pick ridge alphas, the feature set, and the algorithm.
- **Walk-forward (expanding window):** for each test season Y in 2013–2024 (11 seasons; 2020 had no tournament), fit on every season before Y and predict Y. Ridge alphas are re-tuned inside each window. The feature set and the search winner's hyperparameters were picked by grouped CV over seasons that include these test seasons, so those two candidates' walk-forward scores are mildly optimistic.
- **Holdout:** 2025, fit on 2008–2024, scored once. Nothing in `run_analysis.py` is selected on it, but it is not blind to feature design: the coach features were added to the general model after the 2025 tournament. The report therefore also scores the general model without them (`general_ridge_no_coach`).
- **Seed-only baselines, one per metric:** every model is fit by least squares, so RMSE is compared with the per-seed **mean** round (the best seed-only predictor under squared error). MAE is minimised by the median, and most seeds 9–16 win 0 games, so MAE is compared with the per-seed **median**. Both are fit on the training seasons only and scored on identical folds.
- **Metrics:** RMSE (headline) and MAE on round (0–6), plus mean within-season Spearman between predicted and actual round. Predictions are clipped to [0, 6] for every model.
- **Uncertainty:** every gap is the mean of 11 per-season differences, with a 95% Student-t interval (10 df), a t-test p-value, a Holm adjustment within its family, and a two-sided sign test on the season count. Primary family (7 tests): each model's MSE vs the seed mean, stack vs general, and general with vs without the coach features. Secondary family: the same on MAE vs the seed median. This replaced a percentile bootstrap over seasons, which gives intervals that are too narrow with 11 seasons.
- **Winner's curse:** the lowest walk-forward MSE is the best of 5 candidates on the same 11 seasons, so it is optimistic. The 2025 holdout is the clean number for whichever model is picked.
- **Leakage guard:** `assert_no_target_leakage()` (now in `madness.py`) runs on every feature set in both pipelines. Table-parse artifacts (`blank`, `_extra_*`) are excluded from the kitchen-sink set.

## Results (reports/predictive_analysis_20261025.md)

Training data: `data/processed/cbb_training_clean.csv` after the coach-tenure fix was regenerated into it.

| Model | Walk-forward 2013–2024 RMSE | MAE | 2025 holdout RMSE | MAE |
|-------|------|------|------|------|
| Seed only, per-seed mean (RMSE reference) | 1.0914 | 0.7626 | 0.7641 | 0.5874 |
| Seed only, per-seed median (MAE reference) | 1.1500 | 0.7230 | 0.8385 | 0.5469 |
| **General ridge (pipeline default)** | **1.0675** | **0.7477** | **0.7365** | **0.5493** |
| General ridge without the 6 coach features | 1.0592 | 0.7454 | 0.7389 | 0.5480 |
| Seed-stratified stack (`use_seed_models: true`) | 1.0579 | 0.7197 | 0.7105 | 0.5159 |
| Search winner (ridge, alpha=0.3, `baseline_raw_poly`) | 1.0640 | 0.7444 | 0.7431 | 0.5537 |

- **Against seed alone, on RMSE (headline):** the 5 candidates are 2.2% to 3.1% better than the per-seed mean in walk-forward. The 95% interval on the per-season MSE gap excludes zero for two of them: the stack (−0.0721, interval −0.1308 to −0.0133) and the general model without coach features (−0.0693, −0.1212 to −0.0173). For the pipeline default it includes zero (−0.0518, −0.1098 to +0.0062; lower in 8 of 11 seasons). After Holm adjustment over the 7 primary comparisons, none has adjusted p < 0.05 (smallest: 0.098).
- **Against seed alone, on MAE:** no model beats the per-seed median with an interval below zero. The stack is level with it (+0.5%, gap −0.0033, −0.0419 to +0.0352). The general ridge is worse (−3.4%, gap +0.0247, −0.0160 to +0.0653; better in only 4 of 11 seasons). Scoring MAE against the per-seed mean instead, as an earlier version of this report did, makes the same models look 2.0% (general ridge) to 5.6% (stack) better than seed.
- **Do the seed models help? Not shown.** Stack minus general ridge, walk-forward MSE gap −0.0203 (−0.0644 to +0.0238), lower in 6 of 11 seasons. Its MAE edge, −0.0280 (−0.0519 to −0.0042; Holm p 0.180), comes 63% from seeds 13–16. There the stack predicts exactly 0 for 30% of teams (general: 23%), which helps MAE because 87% of those teams win 0 games; the per-seed median predicts 0 for all of them and has lower MAE there (0.1648 vs 0.2326). The stack is worse than the general model on seeds 5–8 MSE (1.3431 vs 1.2870). So `model.use_seed_models` now defaults to `false`.
- **Rule used:** keep the stack only if its walk-forward MSE is lower than the general model's and the 95% interval on the per-season MSE gap lies below zero. MSE is the loss every model is fit with and the metric candidates are ranked by. This rule was written after the walk-forward results had been seen; an earlier MAE-based rule kept the stack.
- **Winner's curse:** the stack has the lowest walk-forward MSE of the 5 candidates, so its walk-forward RMSE (1.0579) is optimistic. Its clean number is the 2025 holdout: RMSE 0.7105, 7.0% better than the per-seed mean (general ridge: 0.7365, 3.6%).
- **Do the coach features help? Not shown.** General ridge minus the no-coach version, walk-forward MSE gap +0.0175 (−0.0041 to +0.0391); the coach features lower the MSE in only 2 of 11 seasons. On the 2025 holdout, RMSE is 0.7365 with them and 0.7389 without. `GEN_FEATURES` is unchanged; see roadmap item 5c.
- **Feature engineering and other algorithms did not help.** Under grouped CV no feature set improved on the plain `baseline_raw_poly` set (CV MSE 1.0858; engineered interactions 1.0894, kitchen sink 1.1562, rank-only 1.1927), and ridge beat the other algorithms (best of each: elastic net 1.1028, lasso 1.1119, random forest 1.1238, gradient boosting 1.1551).
- **Scale of the effect:** the stats-based models improve on seed alone by a few percent of RMSE, not by multiples, and on MAE not at all against the right baseline.
- The 2025 holdout is one bracket of 64 teams, so it is a single noisy draw; read it next to the walk-forward numbers.

---

## Evaluation Framework

| Metric | Scope | Purpose |
|--------|-------|---------|
| Walk-forward RMSE vs per-seed mean; MAE vs per-seed median; Spearman | Each model, 2013–2024 | Primary out-of-sample comparison |
| Student-t interval and Holm-adjusted p on per-season gaps | Each model, stack vs general, coach ablation | Whether a gap is distinguishable from season noise |
| Holdout RMSE / MAE | 2025 season | Clean check for the picked model; not blind to post-2025 feature design (coach features) |
| Grouped-CV MSE | Each model, seasons < 2025 | Alpha, feature set and algorithm selection |
| Leave-one-season-out MSE | Each pipeline model | In-pipeline alpha selection evidence |
| Bracket accuracy | Full simulation | Downstream validation against actual outcomes (not built yet) |

Reports: `reports/predictive_analysis_YYYYMMDD.md` (out-of-sample, from `scripts/run_analysis.py`) and `reports/model_optimization_YYYYMMDD.md` (in-pipeline selection, from `scripts/run_pipeline.py`). The pre-fix reports from 2026-03-04 and 2026-03-06 are void and live in `reports/archive/`.

---

## Improvement Roadmap

### Near-term (ready to test now)

1. **Feature selection audit**
   - Profile feature importance in the general model (Ridge coefficients after scaling)
   - Check for multicollinearity (Barthag is derived from AdjOE/AdjDE — including all three may be redundant)
   - Test dropping low-signal features to reduce noise in small seed-band samples

2. **Interaction and ratio features**
   - Offensive-to-defensive efficiency ratio: `AdjOE / AdjDE`
   - Seed × Barthag interaction (strong low-seeds vs weak high-seeds)
   - Coach experience × seed interaction (experienced coaches matter more for underdogs?)

3. **Non-linear models (benchmark)**
   - Random Forest / Gradient Boosted Trees as comparison baselines
   - Would let us measure how much predictive signal the linear models are leaving on the table
   - Start with a single model (no seed stratification) to compare fairly against the general Ridge model

4. **Better target encoding**
   - Current round encoding (0–6) is ordinal but treats the gap between rounds as equal
   - Test log-transformed result codes, or model each binary milestone (Round2, S16, E8, F4, Ship, Champ) separately with logistic regression
   - Ensemble the binary classifiers for a composite advancement probability

5. **Temporal validation** (done 2026-10, see Validation)
   - Walk-forward by season and season-grouped CV replaced row-level LOO and shuffled KFold

5a. **Is the stack just per-band calibration?** (next)
   - The stack's MAE gain sits mostly in seeds 13–16, where it pushes predictions to 0. Test whether the general model plus a per-seed offset (one parameter per seed, no hand-picked lists) recovers the same walk-forward gain; if so, drop the 14 lists for good.
   - In a 2008–2025 fit of the seed models, 13 of 14 bands picked alpha = 10, the top of the grid; extend the grid before trusting those fits.
   - Nest the out-of-fold `predround` inside each seed-model CV fold, to remove the second-order leak described under Phase 2.

5c. **Coach features**
   - Walk-forward does not show that the 6 coach features help the general model (see Results). Decide whether to drop them by the same rule as the seed models, or to keep them on prior grounds and say so.

5b. **Search-winner nesting**
   - The algorithm search's hyperparameters are picked on all pre-holdout seasons, so its walk-forward score is mildly optimistic. Nest the search inside each walk-forward window if a non-ridge model ever comes close.

### Medium-term (requires some new work)

6. **Bradley-Terry `k` calibration**
   - Run bracket simulation against multiple historical tournaments
   - Grid-search `k` values (0.5, 0.75, 1.0, 1.25, 1.5) minimizing Brier score on historical outcomes
   - Currently k=1.0 is a guess

7. **Ordinal regression**
   - Replace Ridge on round (0–6) with ordinal logistic regression (proportional odds model)
   - Respects that round is ordered but not interval-scaled
   - `mord` or `statsmodels` OrderedModel in Python

8. **Conference strength features**
   - Annual conference-level aggregate stats (mean Barthag, median AdjOE for conference)
   - Captures year-to-year variation in conference quality beyond binary Power6 flag

9. **Momentum / trend features**
   - Late-season performance trend (e.g., last-10-games winning percentage)
   - Requires game-level data (not currently in pipeline)

### Long-term (exploratory)

10. **Game-level matchup modeling**
    - Move from team-level round prediction to game-level win prediction
    - Feature each matchup as (team_A_stats - team_B_stats) or similar
    - Allows direct win-probability estimation without the Bradley-Terry layer

11. **Ensemble stacking**
    - Train multiple model families (Ridge, GBT, ordinal regression) and combine with a meta-learner
    - Use out-of-fold predictions as meta-features (the seed stack already does this for `predround`)

12. **Player-level features**
    - Returning production metrics (% of minutes, points, assists returning)
    - Transfer portal impact
    - Requires roster-level data source

---

## How to Run

```bash
# Full pipeline (data + models + report)
python scripts/run_pipeline.py

# Data only (no modeling)
python scripts/run_pipeline.py --data-only

# With bracket simulation
python scripts/run_pipeline.py --bracket data/external/templates/bracket_template.csv --n-sims 20000 --k 1.0
```

```bash
# Out-of-sample evaluation: grouped CV, walk-forward and 2025 holdout vs the seed-only baselines
python scripts/run_analysis.py --holdout-year 2025
```

Model artifacts are saved to `models/` (joblib, not committed). Reports go to `reports/`.

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2026-03 | Start from Ridge + seed stratification | Interpretable baseline; matches R pipeline; low risk of overfitting on ~70 rows per seed band |
| 2026-03 | Include coach experience features | Tournament coaching experience is a known predictor; 5-year rolling window balances recency and sample size |
| 2026-03 | Bradley-Terry for simulation | Standard pairwise comparison model; simple, well-understood, converts continuous strength to win probability |
| 2026-10 | Season-grouped CV + walk-forward; seed-only baseline in every comparison | A season is one bracket, so row-level splits leak; an accuracy number means little without the seed baseline beside it |
| 2026-10 | Seed models train on out-of-fold `predround` | The in-sample fit had already seen each row's result |
| 2026-10 | Fit an intercept in every ridge | `fit_intercept=False` after scaling forced every model's predictions to average 0 |
| 2026-10 | Train on `tournament_model_frame(clean_df)` in `run_pipeline` | The old `training_data` frame dropped AdjOE/AdjDE and then every row (all-NaN `blank` column), so modeling crashed on an unfitted model |
| 2026-10 | Score RMSE against the per-seed mean and MAE against the per-seed median; Student-t intervals over seasons with Holm adjustment | Least-squares models against a mean baseline overstate MAE gains; a percentile bootstrap over 11 seasons is too narrow |
| 2026-10 | Seed stack off by default (`model.use_seed_models: false`) | On walk-forward MSE, the selection metric, the stack is not distinguishable from the general model; its MAE edge is mostly seeds 13–16 pushed to 0. Replaces an earlier MAE-based decision to keep it |
| 2026-10 | Regenerate `cbb_training_clean.csv` after the coach-tenure fix | `coach_years_with_program` had reset across 2020; only that column changed (1,044 rows, 2021–2025) |
