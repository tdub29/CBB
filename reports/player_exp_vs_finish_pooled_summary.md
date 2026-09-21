# Experience Analysis 2: Player Experience vs Finish (Pooled)

- Raw season-wide `Exp` rank relationship with finish depth: `+0.015`.
- Exact-seed-controlled season-wide `Exp` rank edge relationship: `+0.019`.
- Champion average `Exp.` vs R64 average: `1.69` vs `1.89` (`-0.20`).
- Champion average season-wide `Exp` rank vs R64 average: `188.8` vs `145.7` (`+43.2`, where lower rank means more experience).
- Interpretation: the visuals now use season-wide Exp rank, with the controlled panel expressed as rank edge versus the exact-seed baseline.

## Table

| metric          |   raw_spearman |   seed_adjusted_spearman |   field_avg_exp |   champ_avg_exp |   r64_avg_exp |   champ_minus_r64_exp |   field_avg_exp_rank |   champ_avg_exp_rank |   r64_avg_exp_rank |   champ_minus_r64_exp_rank |
|:----------------|---------------:|-------------------------:|----------------:|----------------:|--------------:|----------------------:|---------------------:|---------------------:|-------------------:|---------------------------:|
| exp_rank_season |          0.015 |                    0.019 |           1.838 |           1.687 |         1.889 |                -0.202 |              157.001 |              188.824 |            145.667 |                     43.156 |