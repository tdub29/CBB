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
- ~5,972 team-seasons (2008–2025, excluding 2020)
- Tournament teams only (~1,100 rows have non-null `round`); remaining rows are non-tournament teams with `round = 0` or NaN
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
| **Outcome-derived, NEVER features** | PAKE, PASE | Wins minus expectation, so they are computed from the result. Filing them under Schedule is what put them in the feature sets. Enforced by `assert_no_target_leakage` in `scripts/run_analysis.py`. |
| Coach experience | coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_tourn_losses_5y, coach_years_with_program, coach_tourn_games_5y, coach_tourn_win_pct_5y, coach_avg_wins_per_appearance | Rolling 5-year tournament track record |
| Conference | Power6, B12, B10, SEC, ACC, P12, BE, MWC | One-hot flags |
| Seed | seed | NCAA committee ranking (1–16) |
| Outcomes | round, Round2, S16, E8, F4, Ship, Champ, result | Target & binary milestones |

---

## Current Model Architecture

### Phase 1: General Model
- **Algorithm:** Ridge regression (StandardScaler → Ridge, `fit_intercept=False`)
- **Target:** `round` (0–6)
- **Features (10 + 6 coach + 3 polynomial):**
  - Base: TOR, Barthag, AdjOE, ORB, EFGD%, 3P%, AdjDE, seed, TALENT, AST%
  - Coach: coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_years_with_program, coach_tourn_win_pct_5y, coach_tourn_games_5y, coach_avg_wins_per_appearance
  - Polynomial: Barthag², AdjDE², seed², seed³
- **Output:** `predround` — a continuous predicted-round value used as a feature in seed models

### Phase 2: 14 Seed-Stratified Models
Each model trains on a 2–3 seed band and applies to the same band. All include `predround` from the general model as a feature.

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
- CV scheme: KFold(5) when n > 50, LeaveOneOut when n ≤ 50
- Final evaluation: Leave-One-Out MSE per model

---

## Evaluation Framework

| Metric | Scope | Purpose |
|--------|-------|---------|
| LOO MSE | Per model | Primary error metric; comparable across runs |
| CV MSE grid | Per model | Ridge alpha selection evidence |
| Holdout MSE | 2023 season (if held out) | Out-of-sample sanity check |
| Bracket accuracy | Full simulation | Downstream validation against actual outcomes |

Reports auto-generated at `reports/model_optimization_YYYYMMDD.md`.

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

5. **Temporal validation**
   - Replace LOO with walk-forward (train on years < Y, test on year Y) to respect time ordering
   - LOO treats 2008 and 2024 as exchangeable — walk-forward doesn't

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
    - Use out-of-fold predictions as meta-features

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

Model artifacts saved to `models/` (joblib). Reports to `reports/`.

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2026-03 | Start from Ridge + seed stratification | Interpretable baseline; matches R pipeline; low risk of overfitting on ~70 rows per seed band |
| 2026-03 | Include coach experience features | Tournament coaching experience is a known predictor; 5-year rolling window balances recency and sample size |
| 2026-03 | Bradley-Terry for simulation | Standard pairwise comparison model; simple, well-understood, converts continuous strength to win probability |
