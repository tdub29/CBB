# Experience Analysis 1: Experience Context Over Time

- Player metric: `exp_raw` (`Exp.`).
- Lead coach metric: `coach_tourn_games_5y` (`Coach tourney games (5y)`), using `2013+` seasons only.
- Coach metric rule: coach_tourn_games_5y led by only 0.015, below the 0.05 switch threshold, so coach_tourn_games_5y remains the default.
- Field-average `Exp.` moved from `1.68` in `2008` to `1.91` in `2026`.
- Field-average `Coach tourney games (5y)` moved from `1.43` in `2013` to `1.34` in `2026`.
- Interpretation: this is context only. It shows how experienced the field and the average head coach looked by season before testing whether either signal matters in March.

## Table

|   year |   field_team_count |   field_avg_exp |   field_avg_coach_primary |
|-------:|-------------------:|----------------:|--------------------------:|
|   2008 |                341 |           1.684 |                   nan     |
|   2009 |                344 |           1.666 |                   nan     |
|   2010 |                347 |           1.706 |                   nan     |
|   2011 |                345 |           1.731 |                   nan     |
|   2012 |                345 |           1.66  |                   nan     |
|   2013 |                347 |           1.699 |                     1.427 |
|   2014 |                351 |           1.724 |                     1.39  |
|   2015 |                351 |           1.699 |                     1.47  |
|   2016 |                351 |           1.7   |                     1.425 |
|   2017 |                351 |           1.691 |                     1.481 |
|   2018 |                351 |           1.692 |                     1.433 |
|   2019 |                353 |           1.663 |                     1.496 |
|   2021 |                347 |           1.729 |                     1.205 |
|   2022 |                358 |           1.979 |                     1.168 |
|   2023 |                363 |           1.99  |                     1.044 |
|   2024 |                362 |           1.991 |                     1.149 |
|   2025 |                364 |           2.015 |                     1.148 |
|   2026 |                365 |           1.907 |                     1.337 |