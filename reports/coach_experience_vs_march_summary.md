# Experience Analysis 4: Coach Experience vs March Success

- Lead coach metric: `coach_tourn_games_5y` (`Coach tourney games (5y)`) with seed-adjusted rho `+0.074` on `2013+` seasons.
- `coach_years_with_program` seed-adjusted rho: `+0.059`.
- Support metric `coach_tourn_win_pct_5y` seed-adjusted rho: `+0.064`.
- Interpretation: coach experience belongs in the Day 3 argument as a co-headline track, but recent March reps and tenure do not carry the exact same signal strength.

## Table

| metric                   |   raw_spearman |   seed_adjusted_spearman |   field_mean |   champ_mean |   r64_mean |   champ_minus_r64 |
|:-------------------------|---------------:|-------------------------:|-------------:|-------------:|-----------:|------------------:|
| coach_tourn_games_5y     |          0.375 |                    0.074 |        3.701 |        8.25  |      2.198 |             6.052 |
| coach_years_with_program |          0.174 |                    0.059 |        6.283 |        8.25  |      5.596 |             2.654 |
| coach_tourn_win_pct_5y   |          0.366 |                    0.064 |        0.277 |        0.478 |      0.174 |             0.304 |