# Turnovers Analysis 2: Offensive Ball Security vs Finish

- Lead offense metric: `TOR` (`Offensive turnover rate`).
- Raw pooled relationship: `+0.181`.
- Exact-seed-controlled TOR relationship: `+0.065`.
- Exact-seed-controlled TORD relationship: `+0.047`.
- Exact-seed-controlled BLK% relationship: `+0.002`.
- Exact-seed-controlled BLKED% relationship: `+0.073`.
- Champions averaged `16.84` TOR vs `18.12` for R64 exits.
- Interpretation: the unadjusted gradient mixes in seed strength; the seed-controlled panel compares teams within the same seed line.

## Metric selection

| category   | metric   | label                          |   seed_adjusted_spearman | offense_primary_metric   | defense_primary_metric   | selection_note                                                                                                     |
|:-----------|:---------|:-------------------------------|-------------------------:|:-------------------------|:-------------------------|:-------------------------------------------------------------------------------------------------------------------|
| offense    | TOR      | Offensive turnover rate        |                    0.065 | TOR                      | TORD                     | TOR remains the default lead offense metric unless another metric leads by >= 0.05; observed lead margin = 0.007.  |
| offense    | BLKED%   | Offensive shots blocked rate   |                    0.073 | TOR                      | TORD                     | TOR remains the default lead offense metric unless another metric leads by >= 0.05; observed lead margin = 0.007.  |
| defense    | TORD     | Defensive turnover rate forced |                    0.047 | TOR                      | TORD                     | TORD remains the default lead defense metric unless another metric leads by >= 0.05; observed lead margin = 0.046. |
| defense    | BLK%     | Block rate                     |                    0.002 | TOR                      | TORD                     | TORD remains the default lead defense metric unless another metric leads by >= 0.05; observed lead margin = 0.046. |

## Table

| metric   |   raw_spearman |   seed_adjusted_spearman |   tord_seed_adjusted_spearman |   blk_seed_adjusted_spearman |   blked_seed_adjusted_spearman |   champ_mean_tor |   r64_mean_tor |   champ_minus_r64 |   field_mean_tor |
|:---------|---------------:|-------------------------:|------------------------------:|-----------------------------:|-------------------------------:|-----------------:|---------------:|------------------:|-----------------:|
| TOR      |          0.181 |                    0.065 |                         0.047 |                        0.002 |                          0.073 |           16.841 |         18.124 |            -1.283 |           17.767 |