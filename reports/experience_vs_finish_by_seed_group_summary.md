# Experience Analysis: Overperformance by Seed Band

- Sign convention: `player_exp_rho` correlates finish vs exact-seed expectation with the negated season-wide `Exp` rank edge, and `coach_exp_rho` with the coach metric's edge, so a positive rho means teams more experienced than their seed peers tended to finish further above seed expectation.
- Strongest player Exp. seed band: `3-4` at `+0.108`.
- Strongest coach Exp. seed band: `14-16` at `+0.208` (`Coach tourney games (5y)`).
- Weakest player Exp. seed band: `14-16` at `-0.048`.
- Player Exp. positive seed bands: `1-2`, `3-4`, `5-6`, `7-8`, `9-10`.
- Coach Exp. positive seed bands: `1-2`, `5-6`, `7-8`, `9-10`, `11-13`, `14-16`.
- Player Exp. band rhos run from `-0.048` to `+0.108`; coach Exp. band rhos from `-0.058` to `+0.208`.
- Coach Exp. uses `2013+` seasons only, so its band n is `coach_team_count`, not `team_count`.
- Interpretation: these are descriptive band correlations; `reports/robustness_summary.md` tests each one against a within-seed permutation null.

## Table

| seed_group   |   expected_finish_depth |   team_count |   coach_team_count |   player_exp_rho |   coach_exp_rho |
|:-------------|------------------------:|-------------:|-------------------:|-----------------:|----------------:|
| 1-2          |                   3.75  |          136 |                 96 |            0.092 |           0.125 |
| 3-4          |                   2.757 |          136 |                 96 |            0.108 |          -0.058 |
| 5-6          |                   1.934 |          136 |                 96 |            0.007 |           0.169 |
| 7-8          |                   1.831 |          136 |                 96 |            0.021 |           0.018 |
| 9-10         |                   1.61  |          136 |                 96 |            0.015 |           0.038 |
| 11-13        |                   1.554 |          204 |                144 |           -0.035 |           0.146 |
| 14-16        |                   1.108 |          204 |                144 |           -0.048 |           0.208 |