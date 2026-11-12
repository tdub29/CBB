# Experience Analysis 5: Combined Experience Archetypes

- Best archetype: `above coach experience only` at `+0.129` mean finish vs exact-seed expectation.
- Worst archetype: `below both seed baselines` at `-0.109` mean finish vs exact-seed expectation.
- The best archetype also carried an average coach tournament win rate of `0.502`.
- Seasons: `2013+` only, as in every coach analysis. The team-history cache starts in `2008`, so 5-year coach counts before `2013` are left-censored; both seed baselines are averages over the same seasons.
- Interpretation: each team is split at its exact seed line's average on both metrics; these are descriptive group means with no significance test.

## Table

| archetype                    |   team_count |   mean_overperformance |   median_overperformance |   adv_rate |   avg_exp |   avg_exp_rank |   avg_coach_primary |   avg_coach_win_pct_5y |
|:-----------------------------|-------------:|-----------------------:|-------------------------:|-----------:|----------:|---------------:|--------------------:|-----------------------:|
| above both seed baselines    |          146 |                  0.075 |                   -0.146 |      0.39  |     2.125 |         85     |               7.493 |                  0.422 |
| above player experience only |          255 |                 -0.045 |                   -0.146 |      0.337 |     2.189 |         73.608 |               1.984 |                  0.193 |
| above coach experience only  |          169 |                  0.129 |                   -0.042 |      0.467 |     1.478 |        255.716 |               8.84  |                  0.502 |
| below both seed baselines    |          198 |                 -0.109 |                   -0.208 |      0.273 |     1.609 |        235.369 |               1.939 |                  0.175 |