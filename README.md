# March Madness Model

Predicts how many rounds each NCAA men's tournament team wins (0 to 6) from [Bart Torvik](https://barttorvik.com) team ratings, 2008-2025 (no tournament in 2020). It also tests common "what wins in March" claims once seed is accounted for. Seed is hard to beat, so every result here is measured against a seed-only baseline.

## Headline result

Out of sample, adding Torvik team stats to seed lowers RMSE about 3% below a seed-only baseline. The 95% interval on that gap excludes zero. It does not survive a Holm correction across the seven comparisons tested.

| Model | Walk-forward RMSE (2013-2024) | Walk-forward MAE | 2025 holdout RMSE | 2025 holdout MAE |
|---|---|---|---|---|
| Seed only, per-seed mean (RMSE baseline) | 1.0914 | 0.7626 | 0.7641 | 0.5874 |
| Seed only, per-seed median (MAE baseline) | 1.1500 | 0.7230 | 0.8385 | 0.5469 |
| **General ridge (pipeline default)** | **1.0592** | **0.7454** | **0.7389** | **0.5480** |
| Seed-stratified stack (off by default) | 1.0532 | 0.7183 | 0.7069 | 0.5192 |

Error is in rounds won. Every number in this section comes from [reports/predictive_analysis_20261025.md](reports/predictive_analysis_20261025.md).

- **RMSE vs the per-seed mean.** The default model is 3.0% lower in walk-forward and 3.3% lower on the 2025 holdout.
  - Mean per-season MSE gap: -0.0693, 95% interval [-0.1212, -0.0173]. The model is lower in 9 of 11 seasons.
  - Holm-adjusted p over the 7 primary comparisons: 0.084. None of the 7 is below 0.05.
- **MAE vs the per-seed median.** No model beats it with an interval below zero. The default is 3.1% worse in walk-forward (0.7454 vs 0.7230) and 0.2% worse on the holdout.
- **The seed stack is off.** The stack is 14 per-seed-band ridges on top of the general model.
  - It has the lowest walk-forward error, but it is the best of 5 candidates scored on the same seasons.
  - Its gain over the general model is not distinguishable from zero: MSE gap -0.0127 [-0.0696, +0.0442], lower in 6 of 11 seasons.
- **These gaps are optimistic.** The feature lists were hand-tuned on seasons that include the walk-forward test seasons. Only the seed baselines are free of that selection.
  - The 2025 holdout is the least-biased check. The default model's whole feature list predates that tournament.
  - The holdout is still a single 64-team bracket.

![Walk-forward RMSE and MAE by test season](reports/figures/predictive/walk_forward_error_by_season.png)

## Methodology

- **The season is the unit.** A season is one 64-team bracket whose rounds always sum to 63, so no split puts a season on both sides.
  - Selection: ridge alphas, feature sets and the algorithm are chosen by `GroupKFold(10)` on year over the 16 pre-holdout seasons.
  - Walk-forward: for each test season Y from 2013 to 2024 (11 seasons), the model is fit on every season before Y. Alpha is re-tuned inside each window.
  - Holdout: 2025 is excluded from every fit and selection step and scored once.
- **One seed baseline per metric.** The models are fit by least squares, so RMSE is compared with the per-seed mean and MAE with the per-seed median.
  - Both baselines are fit on training seasons only and scored on identical folds.
  - Scoring MAE against the mean instead would make the default look 2.3% better than seed, not 3.1% worse.
- **Out-of-fold stacking.** Seed-band models take the general model's prediction as an input.
  - Each training row gets that input from a general model refit without the row's season.
  - The old in-sample version had already seen each row's result. It flattered the seed models' CV MSE by +0.0196 on average, in 11 of 14 bands.
- **Season-level uncertainty.** Every comparison is a mean of 11 per-season differences. Each gets a 95% Student-t interval, a Holm adjustment within its family of 7 tests, and a sign test.
- **A component stays only if it earns it.** The rule: lower walk-forward MSE, and a 95% interval on the per-season gap below zero.
  - The seed stack fails it, and so do the 6 coach-experience features (MSE gap +0.0175 [-0.0041, +0.0391], lower in only 2 of 11 seasons). `configs/default.yaml` turns both off.
  - The rule was written after the walk-forward results were seen, and the report says so.
- **Search.** Under grouped CV, no engineered or expanded feature set beat the base list. Ridge (best CV MSE 1.0858) beat elastic net (1.1028), lasso (1.1119), random forest (1.1238) and gradient boosting (1.1551).

### How I caught a target leak

An earlier version of this analysis reported a 2025 holdout MAE of 0.060 rounds on a 0-6 target. That was too good to be true.

- **The cause.** Two Torvik columns, `PAKE` and `PASE`, measure tournament wins above expectation. They are computed from the result being predicted.
- **Why a plain screen would miss them.** `PASE` correlates 0.767 with round: high, but believable for a strong predictor.
- **The test that exposed them.** I residualized round on seed. `PASE` then correlates 0.9948 with what is left, and `PAKE` 0.9553, against 0.058 for `AdjOE` as a clean control.
- **The fix.** `assert_no_target_leakage()` in [src/evaluation/leakage.py](src/evaluation/leakage.py) now runs that check on every feature set before fitting. It fails at |r| >= 0.90.

That version's row-level CV also split teams from one bracket across folds. A later pass replaced it with the season-grouped scheme above. The old reports are kept, marked void, in [reports/archive/](reports/archive/).

## What the descriptive analyses show

The per-topic scripts correlate team stats with finish inside seed lines and seed bands. The topics are strength of schedule (SOS), size, experience, three-point shooting, turnovers, rebounding, tempo and free-throw rate.

[scripts/robustness_checks.py](scripts/robustness_checks.py) re-derives each headline number and tests it with:

- bootstrap intervals;
- permutation tests that shuffle only within an exact seed line;
- Benjamini-Hochberg (BH) correction across all the band cells.

Full results are in [reports/robustness_summary.md](reports/robustness_summary.md). Under the pooled correction, none of the earlier seed-band or conference findings survives, and the SOS result is borderline.

- **SOS within seed line: borderline.** Spearman +0.061 (n = 1,088), 95% CI [-0.001, +0.123], permutation p = 0.041.
  - The permutation test and the bootstrap interval disagree at 5%.
  - Shuffling only within seed line and season gives p = 0.290.
- **Seed-band correlations: none survives.** There are 131 band-level correlations across 9 topic tables.
  - 9 have nominal p < 0.05, about 6.6 would be expected by chance, and 0 survive BH at q < 0.05. The smallest q is 0.367.
  - That includes the effective-height result this README used to quote: +0.108 for seeds 1-2 and -0.077 for seeds 7-8, p = 0.347 each.
  - Two SOS cells for seeds 11-13 (rho +0.313 and +0.303) survive only under narrower BH families (per table or per statistic). Neither is significant against the seed-and-season null (p = 0.071 and 0.138).
- **Conference overperformance: no effect beyond seed.**
  - The Horizon League's +0.420 mean (n = 18) comes from Butler. Its 4 appearances sum to +7.588, more than the conference's +7.559 total. Without Butler the mean is -0.002.
  - Horizon and the Mountain West (-0.342, n = 45) have nominal p = 0.026 and 0.018, but q = 0.422 across 32 conferences.
  - Cochran's Q permutation p = 0.271. Empirical-Bayes shrinkage puts every conference within 0.023 of zero.
- **A bug the checks caught.** The player-experience band correlations paired the wrong rows, because of a pandas index-alignment bug.
  - Seeds 1-2 was published as +0.554. Corrected, it is +0.092 (n = 136, CI [-0.071, +0.254], p = 0.284).
  - The corrected bands run from -0.048 to +0.108.

The per-topic summaries in [reports/](reports/) are descriptive tables. Read their numbers next to the robustness summary, which tests them.

## Models

- **General model.** Ridge regression on `round`, standardized, with an intercept and a season-grouped alpha.
  - Base features: `TOR`, `Barthag`, `AdjOE`, `ORB`, `EFGD%`, `3P%`, `AdjDE`, `seed`, `TALENT`, `AST%`.
  - Polynomial terms: `Barthag`^2, `AdjDE`^2, `seed`^2 and `seed`^3.
  - Its output, `predround`, is each team's strength.
- **Seed stack** (`model.use_seed_models`, off). 14 ridges, each on a band of 2-3 seed lines, with `predround` as an input.
- **Bracket simulation.** A Bradley-Terry model turns `predround` into win probabilities. A Monte Carlo run (10,000 brackets by default) then gives each team's odds of reaching each round.

Design notes, provenance and the roadmap are in [docs/modeling_plan.md](docs/modeling_plan.md) and [docs/model_optimization.md](docs/model_optimization.md).

## Run it

Run every command from the repository root.

```bash
pip install -r requirements.txt
python -m pytest -q
python scripts/run_analysis.py --holdout-year 2025    # grouped CV, walk-forward, holdout -> reports/predictive_analysis_<date>.md
python scripts/robustness_checks.py                   # tests the descriptive claims -> reports/robustness_*
python scripts/run_pipeline.py --data-only            # rebuild data/processed/ from the committed raw tables
python scripts/run_pipeline.py                        # also fit the default model -> reports/model_optimization_<date>.md
python scripts/run_pipeline.py --bracket data/external/templates/bracket_template.csv --n-sims 20000
```

`scripts/run_analysis.py --holdout-year 2025 --run-tag 20261025` rebuilds the committed report and figures byte for byte. The per-topic analyses are the `scripts/<topic>_analysis_*.py` and `scripts/build_<topic>_sample.py` scripts. They write to `reports/`.

## Data

| What | Where |
|---|---|
| Bart Torvik regular-season team tables, postseason tables and team-history pages | `data/raw/` ([README](data/raw/README.md)) |
| Assembled base table, all teams and seasons | `data/raw/assembled/all_trank_team_tables.csv` |
| Modeling table: 5,971 team-seasons by 82 columns, 1,088 of them tournament teams | `data/processed/cbb_training_clean.csv` |

More detail:

- Sources and lineage: [docs/data_sources.md](docs/data_sources.md) and [docs/data_lineage.md](docs/data_lineage.md).
- Paths, year range and model switches: [configs/default.yaml](configs/default.yaml).

## Layout

```
configs/default.yaml   paths, year range, model switches
src/config.py          config loading and validation
src/ingestion/         Torvik fetch and parse, team tables, team-history cache
src/features/          outcome encodings, coach stats, training-frame build
src/modeling/          feature lists, ridge with season-grouped alpha, out-of-fold stack, seed baseline
src/evaluation/        season splits, leakage guard, report writers
src/pipelines/         madness.py (pipeline entry point); madness.R (original R script, see below)
src/models/            bracket simulation: Bradley-Terry win probability, Monte Carlo
src/visualization/     shared chart palette
scripts/               run_pipeline.py, run_analysis.py, robustness_checks.py, per-topic analyses, data prep
tests/                 pytest suite
reports/               predictive report, robustness checks, per-topic summaries, figures; archive/ holds void reports
data/                  raw/, processed/, external/
docs/                  modeling plan, model optimization, data sources and lineage
models/                joblib artifacts, rebuilt on each run and not committed
```

`src/modeling/` fits the round model. `src/models/` simulates brackets from its output.

## Known limitations

- **Small sample.** There are 17 tournaments. Walk-forward rests on 11 seasons and the holdout on one 64-team bracket, so a few percent is within season-to-season noise.
- **Selection optimism.** The feature lists were tuned on 2008-2024, which includes the walk-forward test seasons. Re-running selection inside each window is roadmap item 5b in [docs/modeling_plan.md](docs/modeling_plan.md).
- **One data source.** The features are Torvik regular-season tables exported after the fact, not Selection Sunday snapshots. Any later revision of a rating is in the data.
- **Target and simulation.** The target is an ordinal 0-6 round count fit by least squares. The Bradley-Terry scale `k` = 1.0 is not calibrated, and bracket-level accuracy is not evaluated.
- **The R port lags the Python pipeline.** `src/pipelines/madness.R` is the original R research script. It fits OLS without an intercept, selects by row-level jackknife, and feeds the seed models an in-sample `predround`. None of the validation above is in it. It is kept for provenance, because the feature lists came from it (entry point `scripts/run_pipeline.R`, dependencies in `scripts/install_deps.R`).
- **2026 field figures need the network.** The `*_2026_*` figures come from `scripts/*_analysis_05_07.py`, which fetch ESPN's team-logo catalog on every run. They cannot be regenerated offline.
