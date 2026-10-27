# SOS Analysis 2 Support: Unadjusted Pooled View

- Primary metric: `Elite SOS`
- Metric rule: Elite SOS led by only 0.029 rho, below the 0.05 switch threshold.
- Champions averaged `31.95` Elite SOS versus `19.99` for Round-of-64 exits.
- Baseline: the field-wide average was `23.96` Elite SOS.
- Interpretation: stronger schedules mapped to deeper runs overall, but much of this pooled signal is seed structure.

## Pooled Finish Table

| finish   |   n |   elite_sos_mean |   elite_sos_median |   conf_avg_barthag_mean |   conf_avg_barthag_median |
|:---------|----:|-----------------:|-------------------:|------------------------:|--------------------------:|
| R64      | 544 |           19.991 |             17.772 |                   0.561 |                     0.546 |
| R32      | 272 |           26.289 |             28.218 |                   0.705 |                     0.772 |
| Sweet 16 | 136 |           28.998 |             30.138 |                   0.753 |                     0.786 |
| Elite 8  |  68 |           30.372 |             30.964 |                   0.783 |                     0.8   |
| Final 4  |  34 |           29.569 |             30.902 |                   0.766 |                     0.792 |
| Finals   |  17 |           28.415 |             29.682 |                   0.738 |                     0.776 |
| Champ    |  17 |           31.952 |             31.735 |                   0.812 |                     0.811 |

## Metric Correlations

| metric           |   spearman_rho | is_primary_metric   |
|:-----------------|---------------:|:--------------------|
| Elite SOS        |          0.452 | True                |
| conf_avg_barthag |          0.424 | False               |