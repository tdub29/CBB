# Model Optimization (General + Seed Models)

This project trains one general ridge model, plus, when `model.use_seed_models` is true, 14 seed-stratified models, to predict tournament advancement (`round`, 0..6). "Optimized" here means:

- Features are standardized inside each CV fold (no leakage across folds).
- Regularization is Ridge regression, with an intercept (OLS when `alpha=0`).
- Ridge `alpha` is selected per model by season-grouped cross-validation on an `alpha` grid.
- Each model reports its CV error, so changes are measurable.

This is model selection inside the training years. It is not an accuracy claim. Out-of-sample accuracy against seed-only baselines (walk-forward 2013-2024 plus a held-out 2025 season) comes from `scripts/run_analysis.py` and is written to `reports/predictive_analysis_YYYYMMDD.md`; see `docs/modeling_plan.md` (Validation, Results).

## Where The Evidence Lives

Every pipeline run that trains models writes a per-run report:

- `reports/model_optimization_YYYYMMDD.md`

For the general model and, if enabled, each seed model, that report includes:

- The selected `alpha`
- Baseline CV MSE at `alpha=0` (if present in the grid)
- Best CV MSE from the grid
- Leave-one-season-out MSE for the selected `alpha` (`season_cv_mse` for seed models)

If you run bracket simulation, the report also links to:

- `data/processed/round_predictions_YYYYMMDD.csv`

## How Alpha Tuning Works

- For each model, the code evaluates `model.ridge_alpha_grid` and selects the `alpha` with the lowest mean CV MSE.
- CV scheme: leave-one-season-out (`LeaveOneGroupOut` with `year` as the group), in `season_group_splits()` in `src/pipelines/madness.py`. One season is one 64-team bracket, so a season is never split between train and test. This replaced `KFold(5, shuffle)` and row-level `LeaveOneOut`, which put teams from the same bracket on both sides.
- After selecting `alpha`, the model is scored once more with leave-one-season-out CV to give the reported MSE.

## Seed Models

- Off by default (`model.use_seed_models: false`). In walk-forward validation the stack's MSE is not distinguishable from the general model alone; the evidence is Section 5 of the latest `reports/predictive_analysis_*.md`.
- When enabled, seed models train on out-of-fold `predround`: each row's value comes from a general model refit without that row's season (`oof_predround()`). `train_seed_models()` refuses a frame without it.
- A seed model's `season_cv_mse` is for picking `alpha` only. The out-of-fold column is built once over all training seasons, so when a seed model's CV holds out season s, its training rows' `predround` came from general models that saw s. Walk-forward scores in `scripts/run_analysis.py` rebuild the column inside each training window and do not have this problem.

## Configuration

In `configs/default.yaml`:

- `model.optimize_ridge_alpha`: enables per-model alpha selection
- `model.ridge_alpha_grid`: candidate alphas (include `0.0` for an OLS baseline)
- `model.use_seed_models`: adds the 14 seed-stratified models on top of the general model

## Commands

Run pipeline (writes report automatically):

```bash
python scripts/run_pipeline.py
```

Out-of-sample evaluation (walk-forward and held-out season against seed-only baselines):

```bash
python scripts/run_analysis.py --holdout-year 2025
```

Run pipeline with bracket simulation:

```bash
python scripts/run_pipeline.py --bracket data/external/templates/bracket_template.csv --n-sims 20000 --k 1.0
```

Validate a bracket CSV:

```bash
python scripts/validate_bracket.py data/external/templates/bracket_template.csv
```

Evaluate predictions (requires actual outcomes):

```bash
python scripts/evaluate_predictions.py --predictions data/processed/round_predictions_YYYYMMDD.csv --actuals path/to/actuals.csv
```

## Next Optimization Targets (Optional)

- Calibrate `model.k_bradley_terry` using historical brackets + actual outcomes (requires per-year bracket CSV with regions).
- Add interaction features for matchup strength (requires game-level or matchup-level data).
- Widen the seed-model alpha grid if seed models are re-enabled: most bands pick the largest value in the current grid (see `docs/modeling_plan.md`, roadmap 5a).
