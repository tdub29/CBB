# Experience Analysis 2: Player Experience vs Finish (Pooled)

- Sign convention: both rhos (and the table's `raw_spearman` / `seed_adjusted_spearman`) use the negated season-wide `Exp` rank, so higher = more experienced.
- Raw relationship between player experience and finish depth: `-0.122` (more-experienced teams tended to finish shallower).
- Within exact seed line (experience edge vs exact-seed baseline against finish vs exact-seed expectation): `+0.030` (teams more experienced than their seed peers tended to finish above seed expectation).
- Champion average `Exp.` vs R64 average: `1.69` vs `1.89` (`-0.20`).
- Champion average season-wide `Exp` rank vs R64 average: `188.8` vs `145.7` (`+43.2`, where lower rank means more experience).
- Interpretation: the controlled panel expresses each team's season-wide Exp rank as an edge versus its exact-seed baseline.

## Table

| metric          |   raw_spearman |   seed_adjusted_spearman |   field_avg_exp |   champ_avg_exp |   r64_avg_exp |   champ_minus_r64_exp |   field_avg_exp_rank |   champ_avg_exp_rank |   r64_avg_exp_rank |   champ_minus_r64_exp_rank |
|:----------------|---------------:|-------------------------:|----------------:|----------------:|--------------:|----------------------:|---------------------:|---------------------:|-------------------:|---------------------------:|
| exp_rank_season |         -0.122 |                     0.03 |           1.838 |           1.687 |         1.889 |                -0.202 |              157.001 |              188.824 |            145.667 |                     43.156 |