# SOS Analysis 2: SOS vs Finish (Seed-Controlled Pooled View)

- Primary metric: `Elite SOS`
- Method: each team is centered against its exact seed-line average finish depth and average SOS.
- Measured: with SOS and finish depth both centered on the seed line, the within-seed Spearman relationship is `0.061`.
- No significance test here; see `reports/robustness_summary.md` for its bootstrap interval and permutation p-value.

## Seed Baselines

|   seed_int |   seed_expected_finish |   seed_avg_metric |   team_count |
|-----------:|-----------------------:|------------------:|-------------:|
|          1 |                  4.309 |            30.86  |           68 |
|          2 |                  3.191 |            31.673 |           68 |
|          3 |                  2.897 |            31.244 |           68 |
|          4 |                  2.618 |            30.35  |           68 |
|          5 |                  2.103 |            28.256 |           68 |
|          6 |                  1.765 |            29.258 |           68 |
|          7 |                  1.912 |            26.935 |           68 |
|          8 |                  1.75  |            26.756 |           68 |
|          9 |                  1.647 |            28.737 |           68 |
|         10 |                  1.574 |            27.865 |           68 |
|         11 |                  1.868 |            24.59  |           68 |
|         12 |                  1.544 |            17.069 |           68 |
|         13 |                  1.25  |            13.658 |           68 |
|         14 |                  1.118 |            13.219 |           68 |
|         15 |                  1.176 |            11.733 |           68 |
|         16 |                  1.029 |            11.129 |           68 |