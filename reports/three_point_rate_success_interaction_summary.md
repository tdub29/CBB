# Three-Point Analysis 5: Rate and Success Interaction

- Best profile: `high success + high volume` at `+0.029` overperformance points.
- Interpretation: the useful comparison is how volume and success combine, not either metric in isolation.
- Figure structure: ranked profile view on the left, 3PRATE x 3P% overperformance heatmap on the right.

## Table

| success_bucket   | volume_bucket   | profile                    |   team_count |   mean_overperformance |   median_overperformance |   avg_three_prate_edge |   avg_three_pct_edge |   avg_three_pct_d_edge |
|:-----------------|:----------------|:---------------------------|-------------:|-----------------------:|-------------------------:|-----------------------:|---------------------:|-----------------------:|
| high success     | high volume     | high success + high volume |          308 |                  0.029 |                   -0.118 |                  4.553 |                2.06  |                 -0.464 |
| high success     | low volume      | high success + low volume  |          228 |                 -0.011 |                   -0.147 |                 -3.802 |                2.091 |                 -0.175 |
| low success      | high volume     | low success + high volume  |          226 |                 -0.039 |                   -0.118 |                  4.232 |               -1.818 |                  0.455 |
| low success      | low volume      | low success + low volume   |          326 |                  0.007 |                   -0.118 |                 -4.577 |               -2.149 |                  0.246 |