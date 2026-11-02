# Experience Analysis: Overperformance by Seed Band

- Sign convention: `player_exp_rho` correlates finish vs exact-seed expectation with the negated season-wide `Exp` rank edge, and `coach_exp_rho` with the coach metric's edge, so a positive rho means teams more experienced than their seed peers tended to finish further above seed expectation.
- Strongest player Exp. seed band: `3-4` at `+0.108`.
- Strongest coach Exp. seed band: `11-13` at `+0.184` (`Coach tourney games (5y)`).
- Weakest player Exp. seed band: `14-16` at `-0.048`.
- Player Exp. positive seed bands: `1-2`, `3-4`, `5-6`, `7-8`, `9-10`.
- Coach Exp. positive seed bands: `1-2`, `5-6`, `7-8`, `9-10`, `11-13`, `14-16`.
- Player Exp. band rhos run from `-0.048` to `+0.108`; coach Exp. band rhos from `-0.028` to `+0.184`.
- Coach Exp. uses `2013+` seasons only, so its band n is `coach_team_count`, not `team_count`.
- These are descriptive band correlations with no significance test or multiple-comparison correction, so a band's sign alone is not evidence of an effect.

## Table

| seed_group   |   expected_finish_depth |   team_count |   coach_team_count |   player_exp_rho |   coach_exp_rho |
|:-------------|------------------------:|-------------:|-------------------:|-----------------:|----------------:|
| 1-2          |                   3.75  |          136 |                 96 |            0.092 |           0.14  |
| 3-4          |                   2.757 |          136 |                 96 |            0.108 |          -0.028 |
| 5-6          |                   1.934 |          136 |                 96 |            0.007 |           0.158 |
| 7-8          |                   1.831 |          136 |                 96 |            0.021 |           0.022 |
| 9-10         |                   1.61  |          136 |                 96 |            0.015 |           0.095 |
| 11-13        |                   1.554 |          204 |                144 |           -0.035 |           0.184 |
| 14-16        |                   1.108 |          204 |                144 |           -0.048 |           0.054 |