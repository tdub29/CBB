# Turnovers Analysis 5: Archetypes

- Best archetype: `clean offense only` at `+0.099` overperformance points.
- Archetypes split teams at their exact seed line's average on TOR edge and defensive disruption edge (TORD edge + 0.35 x BLK% edge; the 0.35 weight is ad hoc).
- These are descriptive group means of finish minus exact-seed expectation; no significance test is applied.

## Archetypes

| archetype                    |   team_count |   mean_overperformance |   median_overperformance |   avg_tor_edge |   avg_tord_edge |   avg_blk_pct_edge |   avg_adjt_edge |
|:-----------------------------|-------------:|-----------------------:|-------------------------:|---------------:|----------------:|-------------------:|----------------:|
| clean offense + live defense |          215 |                  0.051 |                   -0.029 |          1.347 |           1.878 |              0.888 |           0.413 |
| clean offense only           |          341 |                  0.099 |                   -0.118 |          1.74  |          -2.027 |             -1.148 |           0.106 |
| live defense only            |          306 |                 -0     |                   -0.176 |         -1.932 |           2.11  |              1.188 |          -0.193 |
| below both baselines         |          226 |                 -0.197 |                   -0.176 |         -1.291 |          -1.586 |             -0.722 |          -0.292 |