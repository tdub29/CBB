# Model Optimization (General + Seed Models)

This project trains one general model plus 14 seed-stratified models to predict tournament advancement (`round`, 0..6). "Optimized" here means:

- Features are standardized inside CV folds (no leakage).
- Regularization is applied via Ridge regression (optional OLS baseline when `alpha=0`).
- Ridge `alpha` is selected by cross-validation on an `alpha` grid per model.
- Each model reports CV error so improvements are measurable.

## Where The Evidence Lives

Every pipeline run writes a per-run report:

- `reports/model_optimization_YYYYMMDD.md`

That report includes, for the general model and each seed model:

- The selected `alpha`
- Baseline CV MSE at `alpha=0` (if present in grid)
- Best CV MSE from the grid
- Final Leave-One-Out (LOO) MSE for the selected `alpha`

If you run bracket simulation, the report also links to:

- `data/processed/round_predictions_YYYYMMDD.csv`

## How Alpha Tuning Works

- For each model, the code evaluates `model.ridge_alpha_grid` and selects the `alpha` with the lowest mean CV MSE.
- CV scheme:
  - `KFold(5, shuffle)` when training rows > 50
  - `LeaveOneOut` when training rows <= 50
- After selecting `alpha`, the model is evaluated with Leave-One-Out CV to produce `loo_mse` (comparable across runs).

## Configuration

In `configs/default.yaml`:

- `model.optimize_ridge_alpha`: enables per-model alpha selection
- `model.ridge_alpha_grid`: list of candidate alphas (include `0.0` for OLS baseline)

## Commands

Run pipeline (writes report automatically):

```bash
python scripts/run_pipeline.py
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
