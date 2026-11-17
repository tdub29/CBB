# Recalibration Rules

## Choose the map by sample size

| Calibration cases | Map | Why |
|---|---|---|
| below about 1,000 | logistic intercept and slope on logit(p) | two parameters, stable on small samples |
| above about 1,000 | isotonic is an option | flexible, but overfits small samples |
| any, when only sharpness is off | temperature scaling | one parameter |

The scikit-learn calibration guide says isotonic beats sigmoid only with
"enough data (greater than ~ 1000 samples)", is "more prone to overfitting,
especially on small datasets", and that the calibrator must be fit on data
disjoint from the model's training data
(https://scikit-learn.org/stable/modules/calibration.html [V]). scikit-learn
1.8 added `method="temperature"` to `CalibratedClassifierCV` [V].

## Allowed
- Fit the map on out-of-fold predictions from the training window only, then
  apply it to the next forward fold.
- Nested walk-forward recalibration experiments.
- Pool small segments (positions, competitions) with offsets when each alone
  is too small to fit.

## Forbidden
- Fit any map on the final test fold and call it validated.
- Isotonic on a few hundred cases.
- Recalibrate on the same small validation set used for model selection
  (double dipping).
- Manual probability edits after outcomes are known.

## After recalibration
Recompute the whole probability block on true forward folds (proper scores with
skill, intercept and slope with an interval at the replication unit, CORP,
tails) and report raw and recalibrated side by side. Then apply the display
rule in SKILL.md.

## Sketch: logistic recalibration inside a fold

```python
import numpy as np
from sklearn.linear_model import LogisticRegression

def _logit(p):
    p = np.clip(np.asarray(p, dtype=float), 1e-12, 1 - 1e-12)
    return np.log(p / (1 - p))

def fit_recalibrator(p_oof_train, y_train):
    """Logistic intercept and slope on logit(p), fit on out-of-fold training predictions only."""
    lr = LogisticRegression(C=1e6, max_iter=1000).fit(_logit(p_oof_train).reshape(-1, 1), y_train)
    return lambda p: lr.predict_proba(_logit(p).reshape(-1, 1))[:, 1]

# Per outer fold: recalibrate = fit_recalibrator(oof_p[train_idx], y[train_idx])
#                 p_test_recalibrated = recalibrate(p[test_idx])
```
