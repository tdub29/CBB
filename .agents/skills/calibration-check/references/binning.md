# Binning for Calibration

Bins serve the secondary views: bin tables and ECE. The moderate-calibration
view is the CORP diagram, which needs no bins (calibration_metrics.md).

## Equal-mass (quantile): the default for any binned number
Each bin holds about the same n, so no bin is empty or tiny by construction
(`--bins-strategy quantile`). Equal-mass bins bias binned ECE less than
equal-width bins (Roelofs et al., AISTATS 2022 [V abstract, venue K]).

## Equal-width
10 bins on [0, 1] draw a readable picture when probabilities spread out. Do not
headline a number computed from them.

## Tails
Always inspect the decision-relevant extremes (for example 0 to 0.2 and 0.8 to
1.0, or the top decile) separately when quoting strong favorites or long shots.

## Counts and intervals
- Report n per bin; do not interpret empty bins.
- A displayed rate "k of n" carries a Wilson or exact (Clopper-Pearson)
  interval. Example (computed): 5 of 20 gives Wilson 95% [11%, 47%] and exact
  [9%, 49%]. Prefer Wilson when n is small or the rate is near 0 or 1
  (`katz-system-significance-test`).

```python
import math

def wilson(k, n, z=1.959964):
    p = k / n
    den = 1 + z * z / n
    center = (p + z * z / (2 * n)) / den
    half = z / den * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))
    return max(0.0, center - half), min(1.0, center + half)

# scipy >= 1.7: scipy.stats.binomtest(k, n).proportion_ci(method="wilson" or "exact")
```

## Rules
- Pre-declare bins and tail cut-offs before looking at curves.
- Report n per bin.
- Never pick the bin count that makes a number look best.
