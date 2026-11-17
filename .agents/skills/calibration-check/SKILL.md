---
name: calibration-check
description: "Check whether sports-model probabilities can be trusted and shown: Brier and log loss with skill, calibration intercept and slope, CORP reliability, tails, ECE as secondary, recalibration."
metadata:
  version: "0.13.0"
---

# Calibration Check (Sports)

## Overview

A model can rank teams well and still be miscalibrated. If the model says 30%,
about 30% of those cases should hit.

This skill scores probability forecasts with proper scoring rules, measures
calibration at three levels, and supports a human-reviewed verdict grounded in
evaluation provenance, uncertainty at the replication unit, and segment
stability. It also decides whether a probability may be shown at all.

Discrimination metrics (AUC, accuracy) do **not** replace calibration, and
calibration does not replace a proper score.

Citation tags used in this bundle: **[V]** verified against the primary paper,
abstract or official docs; **[S]** checked only through a secondary summary;
**[K]** background knowledge, a lead to verify before citing it to others.

---

## When to Use This Skill

Use when:

- Model outputs win/event probabilities
- User asks how confident or how reliable the probabilities are
- After walk-forward evaluation of a probability model
- Before any probability is displayed, claimed, quoted in results or used to size a bet or allocation
- Comparing raw vs recalibrated probabilities
- sport- and competition-specific probability artifacts

Do **not** use when:

| Need | Go instead |
|---|---|
| Pure ranking tasks with no probabilistic interpretation | ranking/eval skills |
| Hard labels only with no probability outputs | classification metrics only |
| Designing splits from scratch | `validation-design` |
| Feature legality review | `leakage-audit` |
| Margin-only models without probs | MAE/RMSE reporting |

---

## Installation

The bundled scripts require pandas (numpy installs with it):

```bash
python -m pip install pandas
```

Parquet input also needs `pyarrow` or `fastparquet`. Both scripts accept CSV,
Parquet, JSON, JSONL, and NDJSON and expose `--help` without importing pandas.
`calibration_report.py --selftest` needs numpy only and reads no files.

---

## What to Measure

Lead with proper scores, then calibration. Binned ECE comes last and never
headlines.

1. **Proper scores with skill.** Log loss and the Brier score are strictly
   proper: they reward honest probabilities (Gneiting and Raftery 2007 [K]).
   Report each with a skill score, `1 - score / reference_score`, against a
   reference forecaster: the base rate, and the strongest baseline rung or the
   market price at decision time wherever one exists (`baseline-models`).
2. **Calibration in the large.** Mean predicted vs observed rate, and the
   calibration intercept: a logistic fit of the outcome with logit(p) as an
   offset (slope fixed at 1). Target 0. Necessary, never sufficient.
3. **Weak calibration: intercept and slope.** A logistic fit of the outcome on
   logit(p). Slope target 1; below 1 the forecasts are too extreme, above 1 too
   timid. Give the slope an interval at the replication unit (a cluster
   bootstrap over seasons, classes or tournaments, or t on G-1 df for a
   unit-level estimate) and print G beside it (Van Calster et al. 2016 [K];
   2019 [V]).
4. **Moderate calibration: a CORP reliability diagram.** Recalibrate by
   isotonic regression (pool-adjacent-violators) and plot the recalibrated rate
   against the forecast as a step function, with the count in each block and
   uncertainty bands (Dimitriadis, Gneiting and Jordan 2021 [V]). CORP needs no
   bin choice, so no bin count can be tuned after looking.
5. **Brier decomposition (CORP).** Brier = miscalibration - discrimination +
   uncertainty, from the same PAV fit. Report the three terms.
6. **Tails apart.** Report the decision-relevant extremes (for example p < 0.2
   and p > 0.8, or the top decile) separately with counts. A pooled curve hides
   them.
7. **Segments and sharpness.** Season, home/away, competition; and the spread of
   the forecasts (all near the base rate is calibrated and useless).
8. **Binned ECE, secondary only.** Equal-mass bins with counts shown, never a
   headline: binned ECE can under- or over-estimate calibration error
   depending on the miscalibration type, sample size and bin count, and
   equal-mass bins are less biased than equal-width (Roelofs et al., AISTATS
   2022 [V abstract, venue K]). SmoothECE (Blasiok and Nakkiran, ICLR 2024 [V];
   the `relplot` package) is a smoothed alternative.

Read [calibration_metrics.md](references/calibration_metrics.md) for the
definitions, formulas and the replication-unit interval. Read
[binning.md](references/binning.md) before choosing bins or quoting a binned
rate. Read [recalibration.md](references/recalibration.md) before fitting any
probability correction.

---

## Workflow

1. Confirm predictions come from time-safe, walk-forward folds (`validation-design`).
2. Validate probabilities in `[0, 1]`; account for every excluded row.
3. Name the replication unit (season, class, tournament) and its count G;
   pre-declare the tail cut-offs and, for any binned number, equal-mass bins.
4. Compute log loss and Brier with skill against the base rate and the
   strongest rung or market, on identical rows.
5. Compute the intercept and the slope with an interval at the unit, the CORP
   diagram with block counts, and the Brier decomposition.
6. Slice by season, segment and tails.
7. Issue a verdict (table below), then apply the display rule.
8. If recalibrating, follow the recalibration rules: never fit on the final
   test fold and call it validated.
9. Write the probability block into the experiment log or results writeup.

---

## Verdict Scale

| Verdict | Meaning |
|---|---|
| `well-calibrated` | Reliability acceptable for quoting probabilities |
| `usable-with-caveats` | Some miscalibration; the caveat is recorded in the model card and registry (never on a public page), and/or recalibrate properly |
| `poorly-calibrated` | Probability numbers not trustworthy as probabilities |
| `invalid-eval` | Leakage/split issues block judgment |

These verdicts belong to the full methodology, not to a single ECE threshold.
Before issuing one, verify that predictions are genuinely held out and
walk-forward, read the slope interval at the replication unit and the CORP
block counts, compare seasons or folds, check decision-relevant segments and
tails, and document the prediction source. No universal ECE cutoff establishes
that probabilities are trustworthy.

---

## Display Rule

A probability is shown, claimed or used to size anything only when both hold:

1. the verdict is `usable-with-caveats` or better; and
2. the Brier-skill interval against the base rate, computed at the replication
   unit, excludes zero (its lower bound is above zero).

Otherwise show the base rate or a coarser tier instead of the model's
probability. The caveat behind `usable-with-caveats` lives in the model card
and registry, never on a public page. `calibration_report.py` prints the
interval and whether its lower bound is above zero under `display_gate`; the
verdict word stays a manual call.

---

## Run on Held-Out Prediction Artifacts

The input must contain one row per evaluated decision, a binary outcome, and a
probability in `[0, 1]`.

Preferred portable names from this pack's baseline helper:

- outcome: `y_true`
- probability: `p_pred` (also emitted as `logistic_probability`)
- training-window base rate: `constant_probability`

```bash
# After baseline-models/scripts/run_baselines.py --predictions-out ...
python /path/to/calibration-check/scripts/calibration_report.py \
  --input baseline-predictions.csv --target y_true --probability p_pred \
  --reference-col constant_probability --cluster-col season \
  --group-col season --bins 10 --bins-strategy quantile --out calibration.json

python /path/to/calibration-check/scripts/calibration_report.py \
  --input predictions.csv --target won --probability win_probability \
  --reference-col market_probability --cluster-col season \
  --group-col season --bins-strategy quantile --out calibration.json

python /path/to/calibration-check/scripts/segment_calibration.py \
  --input predictions.csv --target won --probability win_probability \
  --segment-col is_home
```

Use `--filter-col is_home --filter-value 1` for a symmetric team-game artifact
when one home perspective per game is the actual evaluation unit. Do not
double-count both sides of one event. For independent rows, pass a row id as
`--cluster-col`; leaving it out gives point estimates with no interval.

`calibration_report.py` returns Brier score, log loss, ECE, reliability-bin
counts, row accounting and optional group metrics as before, plus:

- `proper_scores`: log loss and Brier with skill against the evaluated rows'
  base rate and, with `--reference-col`, against that forecaster;
- `calibration_intercept_slope`: intercept (slope fixed at 1), slope, joint
  intercept, G, and 95% cluster-bootstrap intervals for intercept, slope and
  Brier skill (`--boot` default 1000, `--seed` default 7);
- `corp`: PAV blocks (forecast range, recalibrated rate, n) for the CORP diagram;
- `brier_decomposition`: miscalibration, discrimination, uncertainty;
- `display_gate`: the Brier-skill interval and whether its lower bound is above zero.

Skill against the evaluated rows' own base rate is conservative (that constant
knows the test-period rate); pass the training-window rate as `--reference-col`
for the rate known at decision time. The helper's `verdict` stays
`manual-review-required`: it cannot prove held-out provenance or issue a
verdict word. `segment_calibration.py` prints all-row, categorical-segment,
probability-tail metrics, and sparse-bin counts.

---

## Binning Guidance

| Strategy | Use when |
|---|---|
| CORP (no bins) | the moderate-calibration view; the default |
| Equal-mass (`--bins-strategy quantile`) | any binned number, ECE included |
| Equal-width (10 bins on [0, 1]) | a picture of spread-out probabilities |
| Tail focus (0 to 0.2, 0.8 to 1.0) | decisions live in extremes |

Always report **bin counts**. Empty bins are not evidence. A displayed bin rate
"k of n" carries a Wilson or exact interval ([binning.md](references/binning.md)).

---

## Recalibration Rules

**Allowed**
- Below about 1,000 calibration cases: logistic recalibration of intercept and
  slope on logit(p), fit inside training folds only and applied to the next
  forward fold.
- Above about 1,000 cases: isotonic is an option, fit on data disjoint from the
  model's training data (scikit-learn calibration guide [V]).
- Temperature scaling when only sharpness is off (scikit-learn 1.8
  `CalibratedClassifierCV(method="temperature")` [V]).
- Nested walk-forward recalibration.

**Not allowed**
- Any recalibration map fit on the final test fold and called validated
- Isotonic on a few hundred cases
- Recalibrating on the same small validation set used for model selection
- Hand-edited probabilities after seeing outcomes

After recalibration, re-report the whole probability block on true forward
folds, raw and recalibrated side by side.

---

## Hard Constraints

1. Never evaluate calibration on training rows used to fit the same model without nested disclosure.
2. Never present raw scores as probabilities without checking calibration.
3. Never hide segment or tail failures behind a pooled "looks fine."
4. If a bin or block is tiny, say so: widen bins, pool, or reduce claim strength.
5. Accuracy is not calibration.
6. Leakage-invalid evaluations cannot be "well-calibrated."
7. Never headline binned ECE; lead with proper scores and the slope interval.
8. Never show, claim or size with a probability that fails the display rule.

---

## Anti-Patterns

- "56% correct, so calibrated"
- One reliability plot with no sample sizes
- Holdout isotonic theater
- Average prob near the base rate, therefore calibrated (necessary, not sufficient)
- Ignoring the 0.05 and 0.95 tails
- Quoting NBA/MLB percents without sport-specific calibration
- ECE as the headline number
- Isotonic recalibration on a few hundred cases
- A slope or skill interval from row resampling when the season is the unit
- Skill against a strawman (a 0.5 coin) when a base rate, strongest baseline or market exists

---

## Reporting Template

```text
Calibration report
Sport/model:
Eval: walk-forward seasons ...
n / replication unit / G:
Log loss, skill vs base rate, skill vs strongest rung or market:
Brier, skill vs base rate (95% interval at the unit), skill vs strongest rung or market:
Calibration intercept (slope fixed at 1):
Calibration slope (95% interval, G):
CORP diagram path; Brier decomposition (miscalibration, discrimination, uncertainty):
ECE (equal-mass bins=...), secondary:
Notes by season:
Tail behavior (with counts):
Verdict: well-calibrated | usable-with-caveats | poorly-calibrated | invalid-eval
Display decision: show probability | show base rate | show tier (reason)
Actions:
Reproduce:
```

---

## Output Contract

Done means:

- [ ] Walk-forward probabilities used
- [ ] Log loss and Brier with skill vs the base rate and the strongest rung or market
- [ ] Intercept, and slope with a 95% interval and G at the replication unit
- [ ] CORP diagram path with block counts, and the Brier decomposition
- [ ] Tails reported apart with counts; segment notes present
- [ ] ECE, if reported, from equal-mass bins and labelled secondary
- [ ] Verdict issued
- [ ] Display decision stated from the display rule
- [ ] Actions stated

---

## Worked Example

```bash
python /path/to/calibration-check/scripts/calibration_report.py \
  --input held-out-predictions.csv --target y_true \
  --probability logistic_probability --reference-col constant_probability \
  --cluster-col season --group-col season --bins-strategy quantile \
  --out calibration.json

python /path/to/calibration-check/scripts/segment_calibration.py \
  --input held-out-predictions.csv --target y_true \
  --probability logistic_probability --segment-col is_home
```

Example finding: predictions near 0.70 occur in sparse blocks and realize near
0.60. Label the tail unstable or overconfident according to its uncertainty;
do not infer a durable defect from a handful of rows.

Example finding: the slope interval sits below 1 (forecasts too extreme). The
`--selftest` overconfident generator, logits doubled, gives slope 0.50 with an
interval excluding 1. Recalibrate by logistic intercept and slope inside
training folds, re-score forward, and apply the display rule to the result.

---

## Bundled Resources

### references/
| File | Contents |
|---|---|
| [calibration_metrics.md](references/calibration_metrics.md) | proper scores and skill, intercept and slope, CORP, Brier decomposition, ECE as secondary |
| [binning.md](references/binning.md) | equal-mass bins, counts, tails, Wilson and exact intervals |
| [recalibration.md](references/recalibration.md) | sample-size floor, disjoint data, allowed and forbidden patterns |

### scripts/
| File | Contents |
|---|---|
| `calibration_report.py` | proper scores and skill, intercept and slope with cluster-bootstrap intervals, CORP points, Brier decomposition, ECE, `--selftest`; provenance must be verified externally |
| `segment_calibration.py` | home/away and tail slices |


---

## Related Skills

| Need | Skill |
|---|---|
| Validation design | `validation-design` |
| Reference forecasters (base rate, strongest rung, market) | `baseline-models` |
| Predictive models | `predictive-modeling` |
| Few-cluster intervals and power | `statistical-modeling` |
| Results writeup | `results-reporting` |
| Model card | `model-card` |

---

## Quick Command Card

```bash
python /path/to/calibration-check/scripts/calibration_report.py \
  --input predictions.csv --target won --probability win_probability \
  --reference-col market_probability --cluster-col season \
  --group-col season --bins-strategy quantile --out calibration.json

python /path/to/calibration-check/scripts/calibration_report.py --selftest

python /path/to/calibration-check/scripts/segment_calibration.py \
  --input predictions.csv --target won --probability win_probability \
  --segment-col is_home
```

---
