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
| **Outcome-derived, NEVER features** | PAKE, PASE | Wins minus expectation, so they are computed from the result. Filing them under Schedule is what put them in the feature sets. Enforced by `assert_no_target_leakage` in `src/pipelines/madness.py`, which both `run_pipeline` and `scripts/run_analysis.py` call on every feature set. |
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
- **Output:** `predround`, a continuous predicted-round value. It is the team strength for bracket simulation and an input to the seed models.
- **Intercept fix (2026-10):** the Python port used `fit_intercept=False` after `StandardScaler`. `madness.R` fits `lm(... + 0)` on raw columns, where the level can ride on the uncentred features, but once the scaler centres them every prediction averages 0. Measured on the 1,088 rows: general model mean prediction 0.00 vs mean round 0.98; the seeds 1–2 band 0.00 vs 2.75. Both pipelines now fit an intercept.

### Phase 2: 14 Seed-Stratified Models (the "stack")
Each model trains on a 2–3 seed band. All include the general model's `predround` as a feature.
- **Out-of-fold `predround`:** seed-model training rows get `predround` from a general model refit without that row's season (`oof_predround()`, leave-one-season-out). `train_seed_models()` refuses a frame without the `predround_oof` column. The old pipeline fed them the in-sample fit, which had already seen each row's result; on seasons before 2025 that flattered the seed models' own CV MSE by +0.024 on average (12 of 14 bands), and by +0.117 for the seeds 1–2 band.
- **New-season prediction:** the general model fit on every training season produces `predround`, then the seed models overwrite it for seeds 1–15.
- **Which band scores which seed:** bands overlap and are applied in order, so the last band holding a seed wins. Seed s (1–13) is scored by the 3-seed band that starts at s, seeds 14–15 by [13, 14, 15], and seed 16 by the general model. Band 1 ([1, 2]) never scores anything.
- **Provenance:** the lists were hand-picked in `madness.R`, not selected out of sample. 13 of 14 match the file as committed on 2025-01-16 (commit 21a1e96) once `WAB` terms are removed; band 9 has since dropped `W`. So they predate the 2025 tournament but not the 2013–2024 walk-forward seasons.
- **Switch:** `model.use_seed_models` in `configs/default.yaml` (default `true`, see Results).

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
- **Walk-forward (expanding window):** for each test season Y in 2013–2024 (11 seasons; 2020 had no tournament), fit on every season before Y and predict Y. Ridge alphas are re-tuned inside each window.
- **Blind holdout:** 2025, fit on 2008–2024, scored once, nothing selected on it.
- **Seed-only baseline:** predicted round = mean round of that seed in the training seasons only. Every model is reported against it on identical folds.
- **Metrics:** MAE and RMSE on round (0–6), plus mean within-season Spearman between predicted and actual round. Predictions are clipped to [0, 6] for every model.
- **Uncertainty:** gaps to the baseline get a 95% interval from resampling walk-forward seasons with replacement (10,000 draws), and a count of seasons where the model wins.
- **Leakage guard:** `assert_no_target_leakage()` (now in `madness.py`) runs on every feature set in both pipelines.

## Results (reports/predictive_analysis_20261025.md)

| Model | Walk-forward 2013–2024 MAE | RMSE | 2025 holdout MAE | RMSE |
|-------|------|------|------|------|
| Seed-only baseline | 0.7626 | 1.0914 | 0.5874 | 0.7641 |
| General ridge (pipeline) | 0.7463 | 1.0672 | 0.5484 | 0.7363 |
| **Seed-stratified stack (pipeline)** | **0.7195** | **1.0585** | **0.5152** | **0.7101** |
| Search winner (ridge, alpha=0.3, `baseline_raw_poly`) | 0.7426 | 1.0637 | 0.5529 | 0.7428 |

- **Best model:** the stack, chosen on pre-holdout walk-forward MSE. Against the baseline it is 5.7% better on walk-forward MAE and 3.0% on RMSE, and 12.3% / 7.1% on the 2025 holdout. Walk-forward gap to the baseline: MAE −0.0431 (95% interval −0.0714 to −0.0161), RMSE −0.0329 (−0.0567 to −0.0096). It beats the baseline's MAE in 9 of 11 seasons.
- **Do the seed models help?** Modestly. Stack minus general ridge, walk-forward: MAE −0.0268 (interval −0.0474 to −0.0080, excludes zero), RMSE −0.0087 (−0.0274 to +0.0087, includes zero); lower MAE in 9 of 11 seasons. The gain is in seeds 9–16 (MAE 0.6653 vs 0.6939 for seeds 9–12, 0.2318 vs 0.2995 for 13–16); for seeds 1–4 the general model is slightly better (1.1504 vs 1.1530).
- **Rule used:** keep the stack only if it beats the general model on both walk-forward MAE and MSE and at least one interval excludes zero. The rule was written after a first look at walk-forward results; the 2025 holdout, scored afterwards, points the same way.
- **Feature engineering and other algorithms did not help.** Under grouped CV no feature set improved on the plain `baseline_raw_poly` set (CV MSE 1.0862; engineered interactions 1.0898, kitchen sink 1.1585, rank-only 1.1915), and ridge beat lasso, elastic net, random forest and gradient boosting (best non-ridge: elastic net 1.1028, random forest 1.1249, gradient boosting 1.1602).
- **Scale of the effect:** the stats-based models improve on seed alone by a few percent, not by multiples.
- The 2025 holdout is one bracket of 64 teams, so it is a single noisy draw; read it next to the walk-forward numbers.

---

## Evaluation Framework

| Metric | Scope | Purpose |
|--------|-------|---------|
| Walk-forward MAE / RMSE / Spearman vs seed baseline | Each model, 2013–2024 | Primary out-of-sample comparison |
| Season-bootstrap interval of the gap to baseline | Each model | Whether a gap is distinguishable from season noise |
| Holdout MAE / RMSE | 2025 season | One blind check, never used for selection |
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
   - The stack's gain sits in seeds 9–16, where the general model is worse than seed alone. Test whether the general model plus a per-seed offset (one parameter per seed, no hand-picked lists) recovers the same walk-forward gain; if so, drop the 14 lists.
   - In a 2008–2025 `run_pipeline` run, 13 of 14 seed bands picked alpha = 10, the top of the grid; extend the grid before trusting those fits.

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
# Out-of-sample evaluation: grouped CV, walk-forward and 2025 holdout vs the seed baseline
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
| 2026-10 | Keep the seed stack on (`model.use_seed_models: true`) | Beats the general model on walk-forward MAE with an interval that excludes zero; RMSE gain is nominal. Revisit with item 5a |
