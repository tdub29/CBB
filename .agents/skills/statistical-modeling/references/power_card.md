# Power Card

Write the power card before fitting candidates, before freezing a promotion
gate, and before starting a forward record. It says what the data can resolve,
by when, and what happens if they cannot. A power card written after the result
is a description, not a plan.

Tags: [V] verified against the primary paper, abstract or official docs; [S]
secondary summary only; [K] background knowledge, a lead to verify before citing.

## 1. The card

```text
Power card
Decision it can move:          (what changes, for whom, if the result is positive)
Replication unit and count G:  (season, class, tournament, site, instrument; G now)
Practical edge:                (smallest effect that would change the decision, in the metric's unit)
Unit-level SD:                 (spread of unit-level differences; source: pilot, history, literature)
MDE at 80% power, alpha 0.05:  (at the G available now)
Time to reach the edge:        (units needed for MDE <= practical edge; the calendar date they exist)
Horizon:                       (set by the decision this serves)
Past the horizon:              stop | pool | redesign | monitor-only (pick one before the result)
Sensitivity row:               (pass probability at three true effect sizes, from simulation)
Kill rule check:               (false-kill rate at the planned effect, half of it, and the null; if a kill rule exists)
Null label rule:               "no effect larger than the MDE" when MDE <= practical edge, else "underpowered"
```

### Admission line, one per candidate family, before the test window opens

```text
family | mechanism | expected effect (source or prior) | MDE at available n | trials it adds
```

Admit a family only if its expected effect is at least the MDE. Otherwise it is
exploratory and carries no claim. A family first named after the test window was
seen gets the next unseen window as its test. Count every configuration it will
evaluate (grid cells, seeds, feature sets) in "trials it adds"; they enter the
look budget (`experiment-log`).

## 2. MDE for paired unit-level differences

For the mean of G unit-level differences (candidate minus baseline, one number
per unit), two-sided alpha and power 1 - beta:

```text
MDE ~ (t(1 - alpha/2, G-1) + t(1 - beta, G-1)) * sd_unit / sqrt(G)
```

| G | multiplier at alpha 0.05, power 0.80 | MDE per unit-level SD |
|---|---|---|
| 5 | 3.717 | 1.662 |
| 8 | 3.261 | 1.153 |
| 12 | 3.077 | 0.888 |
| 20 | 2.954 | 0.661 |
| 30 | 2.899 | 0.529 |

Computed with scipy; the exact noncentral-t power at this approximate MDE is
79% at G = 5 and 80% from G = 8 up. Use simulation (section 4) for anything
other than a plain paired mean.

```python
import math
import numpy as np
from scipy import stats

def mde_paired(sd_unit, g, alpha=0.05, power=0.80):
    """Approximate MDE for the mean of g unit-level differences (two-sided t on g-1 df)."""
    return (stats.t.ppf(1 - alpha / 2, g - 1) + stats.t.ppf(power, g - 1)) * sd_unit / math.sqrt(g)

def units_needed(sd_unit, practical_edge, alpha=0.05, power=0.80, g_max=10_000):
    """Smallest G whose MDE reaches the practical edge, or None."""
    for g in range(3, g_max):
        if mde_paired(sd_unit, g, alpha, power) <= practical_edge:
            return g
    return None
```

## 3. From sample size to calendar time and a horizon

Start from a practical significance boundary, translate the required sample into
calendar duration, and remember that segment reads need their own power
(`kohavi-controlled-experiments`, its power and duration reference). Units that
arrive once a year turn "units needed" into years.

Synthetic example (computed): unit-level SD 0.010 and practical edge 0.005. At
G = 8 the MDE is 0.0115, more than twice the edge; reaching the edge needs 34
units. With one unit a year no forward test resolves the edge inside any normal
decision horizon, so the card says pool, redesign or monitor-only now, before a
ledger is frozen.

## 4. Simulation power for compound gates

Promotion gates are rarely one test: a margin on the point estimate, a
significance test, a calibration band, a stability condition. Simulate the
whole gate.

```python
def pass_probability(gate, simulate, effects, reps=4000, seed=7):
    """Share of simulated studies that pass `gate` at each true effect size."""
    rng = np.random.default_rng(seed)
    return {e: float(np.mean([gate(simulate(e, rng)) for _ in range(reps)])) for e in effects}

def simulate(effect, rng, g=8, sd=0.010):
    return rng.normal(effect, sd, g)                  # per-unit gains of one synthetic study

def gate(gains):
    g = gains.size
    t = gains.mean() / (gains.std(ddof=1) / math.sqrt(g))
    return gains.mean() >= 0.005 and stats.t.sf(t, g - 1) <= 0.05

# pass_probability(gate, simulate, effects=(0.0, 0.005, 0.010))
```

Synthetic result of this code (computed, seed 7): the gate "mean gain at least
0.005 and one-sided t p at most 0.05" passes 4.5% of the time at a true gain of
0, 34.3% at a true gain equal to the 0.005 edge, and 81.1% at 0.010. A gate
that demands the point estimate clear the edge misses a true edge-sized effect
about two times in three at this G. These three numbers are the sensitivity row.

### Sensitivity row for a deflated-Sharpe gate

For a DSR gate, report the pass probability at true annual Sharpe 0.3, 0.6 and
0.9 for the study's own T (observations), N (trials) and trial-Sharpe
dispersion. DSR is the probabilistic Sharpe ratio measured against the expected
maximum Sharpe of N skill-less trials (Bailey and Lopez de Prado, J Portfolio
Management 40(5):94-107, 2014 [V]; `quant-strategy-tear-sheet` section 3.2).

```python
EULER = 0.5772156649015329

def expected_max_sr(n_trials, var_trial_sr):
    """Expected maximum per-period Sharpe of n_trials skill-less trials."""
    z1 = stats.norm.ppf(1 - 1 / n_trials)
    z2 = stats.norm.ppf(1 - 1 / (n_trials * np.e))
    return np.sqrt(var_trial_sr) * ((1 - EULER) * z1 + EULER * z2)

def psr(returns, sr_star):
    """Probabilistic Sharpe ratio against sr_star, per-period units."""
    t = len(returns)
    sr = returns.mean() / returns.std(ddof=1)
    g3 = stats.skew(returns)
    g4 = stats.kurtosis(returns, fisher=False)        # normal = 3
    den = np.sqrt(1 - g3 * sr + (g4 - 1) / 4 * sr ** 2)
    return stats.norm.cdf((sr - sr_star) * np.sqrt(t - 1) / den)

def dsr_pass_rate(true_annual_sr, t_obs, n_trials, var_trial_sr, periods=252, bar=0.95, reps=2000, seed=7):
    """Share of simulated track records whose DSR clears the bar; normal returns, unit volatility."""
    rng = np.random.default_rng(seed)
    sr0 = expected_max_sr(n_trials, var_trial_sr)
    mu = true_annual_sr / np.sqrt(periods)
    return float(np.mean([psr(rng.normal(mu, 1.0, t_obs), sr0) >= bar for _ in range(reps)]))

# for s in (0.3, 0.6, 0.9): dsr_pass_rate(s, t_obs=T, n_trials=N, var_trial_sr=V)
```

Draw fat-tailed returns (for example a scaled Student t) when the strategy's
returns are fat-tailed. A sensitivity row near zero at every plausible true
Sharpe means the gate cannot pass a real strategy of that size, so a failure is
"inconclusive at this sensitivity", not evidence of no edge.

## 5. Kill rules: measure their operating characteristics before the freeze

A kill rule (stop the strategy or model when a condition hits) is a test too.
Before freezing it, simulate how often it kills a real effect at the planned
size, at half that size, and under the null, using the same `pass_probability`
pattern with "killed" as the outcome. A rule that kills a real effect most of
the time is replaced before the freeze, not after the first kill. A kill-only
test whose failure is informative at any power is fine, provided these rates
were measured first.

## 6. Null labels and observed power

- A null from a study whose MDE was at or below the practical edge: "no effect
  larger than the MDE (value, unit)", with the interval.
- A null from a study whose MDE was above the practical edge: "underpowered".
  It is not evidence of no effect.
- Never report observed (post hoc) power: it is a function of the p-value and
  adds nothing ([effect_sizes_and_power.md](effect_sizes_and_power.md), post hoc
  power). Report the interval, the MDE and the sensitivity row instead.
- Read a forward record only at the dates on the card. An interim mark is not a
  win; an unplanned early read counts as a look in the look budget
  (`experiment-log`).

## 7. Track records and few independent bets

- **Minimum track record length** (Bailey and Lopez de Prado 2012, as used in
  `quant-strategy-tear-sheet` section 3.4), in observations and per-period
  Sharpe units:
  `MinTRL = 1 + (1 - skew * SR + (kurt - 1) / 4 * SR^2) * (z(1 - alpha) / (SR - SR_star))^2`.
  Example (computed): an annual Sharpe of 1.0 on daily data with normal returns,
  against 0 at 95%, needs about 684 trading days (2.7 years); with skew -1 and
  kurtosis 6, about 729. State MinTRL beside the actual length.
- Count independent bets, not years; a short record is "unproven, not
  disproven" (`randomness-in-markets`).
- Below about 30 trades, report insufficient data rather than a precise-looking
  p-value (`katz-system-significance-test`).

## 8. Sample size for a prediction model by expected shrinkage

Size a regression-based prediction model by overfitting criteria, such as
expected shrinkage of 10% or less and a precisely estimated intercept, not by
events-per-variable folklore (Riley et al., BMJ 368:m441, 2020 [V]; R package
`pmsampsize`). Two of the criteria [K, formula detail; `pmsampsize` implements
them]:

```text
Shrinkage:  n >= p / ((S - 1) * ln(1 - R2_cs / S))     p = candidate parameters, S = 0.9, R2_cs = anticipated Cox-Snell R2
Intercept:  n >= (1.96 / delta)^2 * phi * (1 - phi)    phi = outcome proportion, delta = 0.05
```

Example (computed): p = 10, S = 0.9, R2_cs = 0.10 gives n of about 849; phi = 0.2
gives about 246 for the intercept. A per-segment fit (one per position, one per
competition) needs this per segment, which is the usual case for pooling with
offsets.
