# Calibration Metrics (Sports)

Tags: [V] verified against the primary paper, abstract or official docs; [S]
secondary summary only; [K] background knowledge, a lead to verify before citing.

## Proper scores first

- **Brier score**: mean((p - y)^2). Lower is better. Strictly proper (Gneiting
  and Raftery, JASA 2007 [K]).
- **Log loss**: -mean(y log p + (1 - y) log(1 - p)), with p clipped away from 0
  and 1. Strictly proper; punishes confident misses hard.
- **Skill score**: 1 - score / reference_score. Above 0 beats the reference.
  - Base rate: the constant forecast. The evaluated rows' own rate is an oracle
    constant (it knows the test-period rate), so skill against it is
    conservative; the training-window rate is the one known at decision time.
  - Strongest rung: the best baseline on the same rows, or the de-vigged market
    price at decision time wherever a market prices the event.
- Aim for maximum sharpness subject to calibration (Gneiting, Balabdaoui and
  Raftery, JRSS-B 2007 [K]). Accuracy and AUC are not proper scores; AUC
  measures ranking only.

## Calibration at three levels

Van Calster et al., J Clin Epidemiol 2016 [K]; Van Calster et al., "Calibration:
the Achilles heel of predictive analytics", BMC Medicine 17:230, 2019 [V]
(calibration is assessed far less often than discrimination, and curves need
large samples).

| Level | Measure | Target | Reads as |
|---|---|---|---|
| In the large | mean(p) vs mean(y); intercept a0 in logit P(y=1) = a0 + offset(logit p) | a0 = 0 | a0 > 0 under-predicts on average; a0 < 0 over-predicts |
| Weak | slope b in logit P(y=1) = a + b logit p | b = 1 | b < 1: forecasts too extreme (overfit or overconfident); b > 1: too timid |
| Moderate | CORP reliability curve | the diagonal | where, and how far, the curve leaves the diagonal |

Calibration in the large is necessary, never sufficient: a mean forecast equal
to the base rate can hide a slope far below 1. Report the intercept from the
offset model (slope fixed at 1); the intercept of the joint fit depends on the
slope and is not a calibration-in-the-large measure.

### Interval at the replication unit

Rows within one season, class or tournament are not independent evidence.
Resample whole units (cluster bootstrap) for the intercept, slope and
Brier-skill intervals and print G beside each interval. Below about 20 units a
bootstrap interval is optimistic; for a unit-level mean difference use t on G-1
df or a sign-flip permutation over units (`statistical-modeling`, its
few-cluster interval reference). `calibration_report.py --cluster-col <unit>`
runs the cluster bootstrap (`--boot`, `--seed`).

## CORP reliability diagram

Dimitriadis, Gneiting and Jordan, PNAS 118(8):e2016191118, 2021 [V]:
consistent, optimally binned, reproducible, with uncertainty bands and a Brier
decomposition.

1. Sort by forecast and pool tied forecasts.
2. Run pool-adjacent-violators (isotonic regression of y on p): each forecast's
   recalibrated value is the event rate of its PAV block, non-decreasing in p.
3. Plot recalibrated against forecast as a step function, with each block's count.
4. Add uncertainty bands [K, method detail]: consistency bands (draw outcomes
   from Bernoulli(p), refit PAV, and show where the curve falls when the forecast
   is calibrated) or confidence bands (bootstrap the rows, by unit for clustered
   data). The authors' R package is `reliabilitydiag` [K].

`calibration_report.py` writes the PAV blocks under `corp.points` (forecast
range, recalibrated rate, n).

## Brier decomposition (CORP)

With x_rc the PAV-recalibrated forecasts and r the base rate:

- MCB, miscalibration = Brier(p) - Brier(x_rc), at least 0
- DSC, discrimination = Brier(r) - Brier(x_rc), at least 0
- UNC, uncertainty = Brier(r) = r (1 - r)
- Brier(p) = MCB - DSC + UNC, exactly

## Binned ECE, a secondary diagnostic

1. Equal-mass bins (`--bins-strategy quantile`), counts shown.
2. For each bin: |mean(p) - mean(y)|; average weighted by bin size.

Binned ECE can under- or over-estimate the true calibration error depending on
the miscalibration type, sample size and bin count, and equal-mass bins are
less biased than equal-width (Roelofs et al., arXiv 2012.08668, AISTATS 2022
[V abstract, venue K]). SmoothECE kernel-smooths before measuring (Blasiok and
Nakkiran, ICLR 2024 [V]; `relplot` package). Never headline either.

## Sharpness

The spread of the forecasts. A forecast that always says the base rate is
calibrated and useless; sharpness counts only subject to calibration.

## Pitfalls

- Tiny blocks or bins look noisy; show counts.
- Good log loss can coexist with local miscalibration, especially in the tails.
- Pooling seasons can hide regime breaks; slice by season.
- Row-level intervals on clustered data are too narrow.
- A probability whose Brier-skill interval includes zero is not shown (SKILL.md, Display Rule).
