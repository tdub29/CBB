# Size Analysis 4: Size vs Finish by Seed Band

- Metric used here: `EFFHGT` (`Effective height`).
- Strongest seed band: `1-2` at `+0.108`.
- Weakest seed band: `7-8` at `-0.077`.
- Positive seed bands: `1-2`, `9-10`, `11-13`.
- Interpretation: band rhos are descriptive and each rests on one seed band's teams; `reports/robustness_summary.md` tests every band against a within-seed permutation null with a multiple-testing correction.

## Table

| seed_group   |   expected_finish_depth |   team_count |   effhgt_rho |
|:-------------|------------------------:|-------------:|-------------:|
| 1-2          |                   3.75  |          136 |        0.108 |
| 3-4          |                   2.757 |          136 |       -0.032 |
| 5-6          |                   1.934 |          136 |       -0.067 |
| 7-8          |                   1.831 |          136 |       -0.077 |
| 9-10         |                   1.61  |          136 |        0.077 |
| 11-13        |                   1.554 |          204 |        0.017 |
| 14-16        |                   1.108 |          204 |       -0.03  |