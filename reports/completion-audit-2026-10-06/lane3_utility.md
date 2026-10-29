# Lane 3: metrics, calibration, baselines and bracket utility

Lead auditor, 2026-10-06. Independent code: `scratch/lane3_recompute.py` (own log5, own seed-pair
rates, own bootstrap and DM); output `scratch/lane3_results.json`. Only the bracket-structure helpers
(`bracket_from_games`, `committed_bracket`) were reused; they raise if the bracket disagrees with the games.

## U1. Reproduction: exact

- 1,007 games rebuilt from raw inputs; 1,007/1,007 match the saved predictions, 0 label mismatches.
- log5 from Barthag vs saved `p_frozen`: max abs diff 2e-16. Own seed-pair (1985 to Y-1, Laplace) vs saved `p_seed`: 1e-16.
- Headline reproduced to the digit: frozen minus seed-pair, 2021-2026, Brier **-0.00594 [-0.01225, +0.00037]**, DM t -1.651, p 0.160; log loss -0.0190 [-0.0392, +0.0012], p 0.157. Report says -0.0059 [-0.0123, +0.0004], p 0.1597.
- Path-aware round probabilities: every round sums to 64/2^r (max dev 4e-15). Expected-wins error frozen minus seed-pair 2021-2026: -0.0861 [-0.1791, +0.0119], matching the report.

## U2. Predictive edge

| Comparison (Brier, log5 minus seed-pair) | n games / years | mean | 95% year-block CI | DM p | years log5 better |
|---|---|---|---|---|---|
| 2021-2026 clean test (pre-registered) | 377 / 6 | -0.0059 | [-0.0123, +0.0004] | 0.160 | 4 of 6 |
| 2010-2019 selection window (optimistic) | 630 / 10 | -0.0120 | [-0.0205, -0.0011] | 0.053 | 9 of 10 |
| 2010-2026 pooled (contains selection years) | 1007 / 16 | -0.0097 | [-0.0160, -0.0025] | 0.017 | 13 of 16 |

- Drop-one-tournament: the 2021-2026 mean stays negative whichever year is removed (-0.0039 to -0.0079). Direction is stable; the interval is wide because there are 6 blocks.
- The point estimate (0.0059) exceeds the pre-set meaningful edge (0.005), but power at that edge was 15% and the MDE 0.0126. Charter outcome: **no detectable edge**. Correct reading: consistent direction, underpowered.
- The pooled 2010-2026 row is significant but is not a clean test: 2010-2019 picked log5 out of 28 variants (Sidak 0.78, White's RC 0.099). It must not be quoted as evidence of edge.
- T-Rank game-day line, same 377 games: Brier 0.1838 vs frozen 0.1813. The Selection Sunday formula matches the game-day public line here.

## U3. Calibration

Logistic recalibration of outcome on logit(p) (slope 1 = calibrated; below 1 = overconfident):

| | 2021-2026 slope [95% CI] | 2010-2026 slope [95% CI] | intercept 2010-2026 |
|---|---|---|---|
| log5 (frozen) | 0.91 [0.61, 1.21] | 0.96 [0.77, 1.15] | 0.02 |
| seed-pair | 0.80 [0.48, 1.11] | **0.73 [0.55, 0.91]** | 0.21 |

log5 needs no recalibration; seed-pair rates are measurably overconfident over 16 years. This matches the
report's Murphy table (reliability 0.0032 vs 0.0048) and is a genuine, if modest, advantage of the frozen model.

## U4. Practical bracket utility (ESPN 10-20-40-80-160-320, one committed bracket per year)

| | 2021-2026 mean pts | 2010-2026 mean pts | champions hit 2021-26 | Final Four hits 2021-26 | R64 correct 2021-26 |
|---|---|---|---|---|---|
| log5 | 923 | 884 | 1 of 6 (2026 Michigan) | 12 | 142 |
| seed-pair | 922 | 866 | 1.25 (coin-split 1 seeds) | 11 | 145 |
| chalk | 912 | 868 | 1.25 | 11 | 139 |

- log5 minus seed-pair: +1.7 pts/yr 2021-2026 [-110, +127]; +18 pts/yr 2010-2026 [-67, +114]; log5 scored more in 3 of 6 test years.
- Results are lumpy: log5 bracket gains come from champion calls (2012 +480, 2019 +330, 2026 +280) and give back points in other years (2017 -230, 2022 -200). Houston was log5's champion pick three years running (2023-2025) and won none of them.
- log5 and seed-pair disagree on 13% of 2021-2026 games.
- Not measured: pool-win probability against a field of human entries. A committed max-likelihood bracket is not a pool strategy, and no claim about winning pools is supported.

## Lane 3 verdict

- Numbers: **reproduced exactly** from raw inputs with independent code.
- Improvement over seed-pair: **NOT ESTABLISHED** on the clean test (CI touches 0, power 15%). Better calibration is real but small.
- Bracket use: **LIMITED**. Fine as a probability sheet that is calibrated and at least as good as seeds; no evidence it scores more bracket points or wins pools.
