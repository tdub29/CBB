# Few-Cluster and Conformal Intervals

Use when an interval, a p-value or a promotion gate rests on a handful of
independent units (seasons, draft classes, tournaments, sites, instruments), or
when a continuous output or a displayed rate needs a range.

Tags: [V] verified against the primary paper, abstract or official docs; [S]
secondary summary only; [K] background knowledge, a lead to verify before citing.

## 1. State the estimand and G first

Before any interval, write down:

- the estimand (for example, the mean per-season difference in log loss between
  candidate and baseline);
- the replication unit and G, the number of independent units;
- the method, chosen from the table below before the result is seen.

Print G beside every interval, share and p-value. Rows inside a unit (games in
a season, players in a class) are not independent evidence; with few clusters,
use small-sample-aware methods and weaken the claim (SKILL.md, group comparison
examples).

## 2. Choose the method by G

| Situation | Method |
|---|---|
| Mean of unit-level differences, any G | t on G-1 df |
| G below about 20 | add an exact sign-flip or permutation test over units |
| Regression coefficient with few clusters | wild cluster bootstrap with the null imposed |
| Many nearly independent units | cluster-robust or block methods; t and bootstrap then agree |
| A rate shown as "k of n" | Wilson or exact interval |
| A continuous output shown to people | split conformal or conformalized quantile regression, calibrated on a later time block |

### t on G-1 df for unit-level differences

```python
import itertools
import numpy as np
from scipy import stats

def t_interval(diffs, level=0.95):
    """Interval for the mean of unit-level differences on G-1 df; returns (low, high, G)."""
    d = np.asarray(diffs, dtype=float)
    g = d.size
    half = stats.t.ppf(0.5 + level / 2, g - 1) * d.std(ddof=1) / np.sqrt(g)
    return d.mean() - half, d.mean() + half, g
```

### Sign-flip permutation over units

Under the null that unit-level differences are symmetric around zero, every
sign pattern is equally likely, so the p-value is the share of sign patterns
whose mean is at least as extreme as the observed one. The smallest attainable
two-sided p is 2 / 2^G: at G = 5 it is 0.0625, so no result can reach p < 0.05
two-sided. State that before running, not after. The permutation idea is the
one `evidence-based-technical-analysis` uses for searched rules.

```python
def sign_flip_p(diffs, two_sided=True, max_exact=16, draws=200_000, seed=7):
    """Sign-flip permutation p over units: exact up to max_exact units, sampled beyond."""
    d = np.asarray(diffs, dtype=float)
    g = d.size
    if g <= max_exact:
        signs = np.array(list(itertools.product((1.0, -1.0), repeat=g)))
    else:
        signs = np.random.default_rng(seed).choice((1.0, -1.0), size=(draws, g))
    means = (signs * d).mean(axis=1)
    if two_sided:
        return float(np.mean(np.abs(means) >= abs(d.mean()) - 1e-12))
    return float(np.mean(means >= d.mean() - 1e-12))
```

### Wild cluster bootstrap for regression coefficients

Fit the model with the null imposed, multiply each cluster's residual vector by
a random sign (Rademacher weight), rebuild the outcome, refit, and compare the
observed cluster-robust t with its bootstrap distribution (Cameron, Gelbach and
Miller, Review of Economics and Statistics 90(3), 2008 [K]; not in the research
notes this skill was built from, so verify before citing). With very few
clusters, Rademacher weights give only 2^G distinct draws and the p-value is
coarse; report that.

```python
def _cluster_t(X, y, groups, k):
    """OLS coefficient k over its CR1 cluster-robust standard error."""
    xtx_inv = np.linalg.inv(X.T @ X)
    beta = xtx_inv @ X.T @ y
    u = y - X @ beta
    meat = np.zeros((X.shape[1], X.shape[1]))
    for g in np.unique(groups):
        s = X[groups == g].T @ u[groups == g]
        meat += np.outer(s, s)
    n, p = X.shape
    n_groups = np.unique(groups).size
    v = xtx_inv @ meat @ xtx_inv * (n_groups / (n_groups - 1)) * ((n - 1) / (n - p))
    return beta[k] / np.sqrt(v[k, k])

def wild_cluster_p(X, y, groups, k, reps=9999, seed=7):
    """Wild cluster bootstrap-t p for H0: beta_k = 0, null imposed, Rademacher weights."""
    X, y, groups = np.asarray(X, float), np.asarray(y, float), np.asarray(groups)
    t_obs = _cluster_t(X, y, groups, k)
    x_restricted = np.delete(X, k, axis=1)
    fitted = x_restricted @ np.linalg.lstsq(x_restricted, y, rcond=None)[0]
    resid = y - fitted
    labels, inverse = np.unique(groups, return_inverse=True)
    rng = np.random.default_rng(seed)
    hits = 0
    for _ in range(reps):
        w = rng.choice((-1.0, 1.0), size=labels.size)[inverse]
        hits += abs(_cluster_t(X, fitted + w * resid, groups, k)) >= abs(t_obs)
    return hits / reps, t_obs
```

## 3. Resamples are not clusters

More bootstrap resamples add Monte Carlo precision, not clusters: 200,000
resamples of 8 seasons still describe 8 seasons. A percentile bootstrap over
few units is too narrow, so a bootstrap share (the fraction of resamples in
which the candidate wins) is never a gate below about 20 units. Gate on t with
G-1 df or an exact permutation; report the share only as a sensitivity.

### Coverage simulation: measure a gate's false-pass rate before trusting it

Draw unit gains with the observed spread and a true gain of zero, apply the gate
exactly as written, and record how often it passes. A false-pass rate well above
the nominal level means the gate is anti-conservative before multiplicity is
even counted.

```python
rng = np.random.default_rng(7)
R, B = 4000, 2000
for G in (5, 8, 12):
    passes90 = passes95 = t95 = 0
    for _ in range(R):
        gains = rng.normal(0.0, 1.0, G)            # true mean gain is zero; use your spread
        idx = rng.integers(0, G, size=(B, G))
        share = (gains[idx].mean(axis=1) > 0).mean()
        passes90 += share >= 0.90
        passes95 += share >= 0.95
        tstat = gains.mean() / (gains.std(ddof=1) / np.sqrt(G))
        t95 += stats.t.sf(tstat, G - 1) <= 0.05     # one-sided t at 5%
    print(G, passes90 / R, passes95 / R, t95 / R)
```

Synthetic illustration, computed with this code (normal unit gains, true gain
zero, seed 7): a "bootstrap share at least 95%" gate passed 10.4% of the time
at G = 5, 8.6% at G = 8 and 7.1% at G = 12 against a nominal 5%; a "share at
least 90%" gate passed 15.2%, 13.6% and 11.7% against a nominal 10%; a
one-sided t test on G-1 df held 4.8%, 4.9% and 5.1%. Rerun it with your own
spread, G and gate: heavier tails change the numbers, and a family of several
candidates multiplies them.

## 4. Cross-validation intervals

Cross-validation estimates the average error of models fit on other training
sets from the same population, not the error of the model in hand, and naive CV
standard errors are too small because folds share data; nested CV gives
intervals near nominal coverage (Bates, Hastie and Tibshirani, JASA
119(546):1434-1445, 2024 [V]). Read a "plus or minus fold SD" band as a floor
on uncertainty: model differences inside it are noise.

## 5. Conformal intervals for continuous outputs

Conformal prediction gives distribution-free intervals with finite-sample
marginal coverage when calibration and new points are exchangeable (Angelopoulos
and Bates, "Conformal Prediction: A Gentle Introduction", Foundations and Trends
in Machine Learning 16(4):494-591, 2023 [V]).

```python
def conformal_quantile(scores, alpha):
    """Finite-sample split-conformal quantile of nonconformity scores."""
    scores = np.sort(np.asarray(scores, dtype=float))
    n = scores.size
    rank = int(np.ceil((n + 1) * (1 - alpha)))
    if rank > n:
        return np.inf          # too few calibration points for this alpha
    if rank < 1:
        return -np.inf         # alpha >= 1: empty interval
    return scores[rank - 1]

# Split conformal around a point model, calibrated on the latest block before the scored period:
#   q = conformal_quantile(np.abs(y_cal - yhat_cal), alpha=0.1)
#   interval = (yhat_new - q, yhat_new + q)
# Conformalized quantile regression around lower and upper quantile models lo and hi:
#   q = conformal_quantile(np.maximum(lo_cal - y_cal, y_cal - hi_cal), alpha=0.1)
#   interval = (lo_new - q, hi_new + q)
```

- **Conformalized quantile regression** adapts the width to heteroscedasticity
  and keeps finite-sample marginal coverage (Romano, Patterson and Candes,
  NeurIPS 2019, arXiv 1905.03222 [V]).
- **Time order.** Exchangeability fails in time, so calibrate on the latest block
  before the scored period and expect approximate or long-run coverage only.
  Adaptive conformal inference moves the miscoverage level online after each
  hit or miss (Gibbs and Candes, NeurIPS 2021 [V]); weighted conformal handles
  drift (Barber, Candes, Ramdas and Tibshirani, arXiv 2202.13415 [V preprint];
  Annals of Statistics 2023 [K]); EnbPI and ACI ship in MAPIE v1's
  `TimeSeriesRegressor` (`method="enbpi"` or `"aci"`) [V].
- **Mondrian (per-segment) calibration.** Marginal coverage can hide poor
  coverage in segments. Calibrate one quantile per segment and check coverage
  per segment on a later block. A segment needs at least (1 - alpha) / alpha
  calibration points for a finite interval: 9 for a 90% interval.
- **Coverage check.** Report empirical coverage on a later block, overall and by
  segment, with counts.

```python
rng = np.random.default_rng(5)
T = 3000
x = rng.uniform(0, 1, T)
y = 2 * x + rng.normal(0, 0.2 + x, T)          # noise grows with x
yhat = 2 * x
cal, test = slice(1500, 2000), slice(2000, 3000)
seg = (x > 0.5).astype(int)
q = conformal_quantile(np.abs(y[cal] - yhat[cal]), alpha=0.1)
q_seg = {g: conformal_quantile(np.abs(y[cal][seg[cal] == g] - yhat[cal][seg[cal] == g]), 0.1) for g in (0, 1)}
print("overall", np.mean(np.abs(y[test] - yhat[test]) <= q))
for g in (0, 1):
    m = seg[test] == g
    miss = np.abs(y[test][m] - yhat[test][m])
    print(g, np.mean(miss <= q), np.mean(miss <= q_seg[g]))
```

Synthetic illustration, computed with this code: the pooled 90% interval
covered 90.6% of the later block overall, but 98.8% of the low-noise segment and
82.8% of the high-noise one; per-segment quantiles gave 89.9% and 94.0%.

## 6. Small counts on displayed rates

A displayed rate "k of n" carries a Wilson or exact (Clopper-Pearson) interval.
Example (computed): 5 of 20 gives Wilson 95% [11%, 47%] and exact [9%, 49%];
0 of 12 gives an exact upper bound of 26%. Prefer Wilson when n is small or the
rate is near 0 or 1 (`katz-system-significance-test`).

```python
# scipy >= 1.7
from scipy.stats import binomtest
binomtest(5, 20).proportion_ci(confidence_level=0.95, method="wilson")   # or method="exact"
```

## 7. Reporting line

```text
Estimand: ... ; unit: ... ; G = ...
Interval: t(G-1) 95% [ ..., ... ]  |  exact sign-flip p = ... (smallest attainable p = ...)
Method chosen before the result: yes/no; bootstrap share (sensitivity only): ...
Displayed numbers: interval method per number (Wilson/exact, conformal with coverage by segment)
```
