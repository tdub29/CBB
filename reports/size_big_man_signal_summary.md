# Size Analysis 5A: Dominant Big-Man Signal

- Dominant big-team threshold: `max_c_porpag >= 4.0`.
- Raw pooled relationship with finish depth: `+0.180` Spearman rho.
- Overall exact-seed split: dominant big teams average `+0.050` vs `-0.009` for the rest of the field.
- Largest band gap (dominant minus other overperformance): `5-6` at `+0.468`.
- Smallest band gap: `9-10` at `-0.201`.
- Tagged bands: `5-6` (primary support pocket, `+0.468`), `7-8` (primary support pocket, `+0.177`), `11-13` (secondary support pocket, `+0.183`). Bands tagged `context only`: `1-2`, `3-4`, `9-10`, `14-16`.
- Interpretation: the gap changes sign across seed bands, and each band holds only `7`-`41` dominant teams. The `signal_band` tags were chosen from this same table and no band gap carries an uncertainty test here, so the tags are a hypothesis, not a validated signal.

## Table

| segment   |   threshold |   dominant_team_count |   other_team_count |   dominant_share |   dominant_mean_overperformance |   other_mean_overperformance |   overperformance_gap |   dominant_adv_rate |   other_adv_rate |   adv_rate_gap | signal_band              |
|:----------|------------:|----------------------:|-------------------:|-----------------:|--------------------------------:|-----------------------------:|----------------------:|--------------------:|-----------------:|---------------:|:-------------------------|
| Overall   |           4 |                   167 |                921 |            0.153 |                           0.05  |                       -0.009 |                 0.059 |               0.449 |            0.374 |          0.076 | overall                  |
| 1-2       |           4 |                    41 |                 95 |            0.301 |                          -0.102 |                        0.044 |                -0.146 |               0.39  |            0.432 |         -0.041 | context only             |
| 3-4       |           4 |                    35 |                101 |            0.257 |                           0.042 |                       -0.015 |                 0.057 |               0.571 |            0.564 |          0.007 | context only             |
| 5-6       |           4 |                    20 |                116 |            0.147 |                           0.399 |                       -0.069 |                 0.468 |               0.45  |            0.405 |          0.045 | primary support pocket   |
| 7-8       |           4 |                    26 |                110 |            0.191 |                           0.143 |                       -0.034 |                 0.177 |               0.577 |            0.555 |          0.022 | primary support pocket   |
| 9-10      |           4 |                    18 |                118 |            0.132 |                          -0.174 |                        0.027 |                -0.201 |               0.389 |            0.449 |         -0.06  | context only             |
| 11-13     |           4 |                    20 |                184 |            0.098 |                           0.165 |                       -0.018 |                 0.183 |               0.4   |            0.37  |          0.03  | secondary support pocket |
| 14-16     |           4 |                     7 |                197 |            0.034 |                          -0.113 |                        0.004 |                -0.117 |               0     |            0.086 |         -0.086 | context only             |