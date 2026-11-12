# Experience Analysis 4: Coach Experience vs March Success

- Lead coach metric: `coach_tourn_games_5y` (`Coach tourney games (5y)`) with seed-adjusted rho `+0.101` on `2013+` seasons.
- `coach_years_with_program` seed-adjusted rho: `+0.059`. Tenure is counted from `2008`, the first season in the team-history cache, so it is capped (`6` in `2013`, `18` in `2025`) and understates longer tenures.
- Support metric `coach_tourn_win_pct_5y` seed-adjusted rho: `+0.054`.
- Interpretation: descriptive rank correlations on `2013+` seasons; no significance test is applied.

## Table

| metric                   |   raw_spearman |   seed_adjusted_spearman |   field_mean |   champ_mean |   r64_mean |   champ_minus_r64 |
|:-------------------------|---------------:|-------------------------:|-------------:|-------------:|-----------:|------------------:|
| coach_tourn_games_5y     |          0.415 |                    0.101 |        4.529 |        9.25  |      2.581 |             6.669 |
| coach_years_with_program |          0.174 |                    0.059 |        6.283 |        8.25  |      5.596 |             2.654 |
| coach_tourn_win_pct_5y   |          0.409 |                    0.054 |        0.3   |        0.492 |      0.184 |             0.308 |