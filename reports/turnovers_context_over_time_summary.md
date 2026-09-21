# Turnovers Analysis 1: Context Over Time

- Field-average offensive turnover rate moved from `21.23` in 2008 to `16.82` in 2026.
- Field-average defensive turnover pressure moved from `21.16` to `16.81` over the same span.
- The field-level gap between those two turnover series is tiny on average (`0.05`), so they should be shown as one shared environment story rather than two separate headline panels.
- Block pressure context: `BLK%` moved from `9.03` to `9.47`, while `BLKED%` moved from `9.22` to `9.53`.
- Interpretation: the field environment changed enough that turnover edges should be interpreted against modern baselines, not early-sample assumptions.

## Table

|   year |   field_team_count |   avg_tor |   avg_tord |   avg_blk_pct |   avg_blked_pct |   tournament_team_count |
|-------:|-------------------:|----------:|-----------:|--------------:|----------------:|------------------------:|
|   2008 |                341 |    21.235 |     21.162 |         9.03  |           9.221 |                      64 |
|   2009 |                344 |    20.761 |     20.684 |         8.708 |           8.888 |                      64 |
|   2010 |                347 |    20.383 |     20.331 |         9.094 |           9.283 |                      64 |
|   2011 |                345 |    20.117 |     20.071 |         9.115 |           9.258 |                      64 |
|   2012 |                345 |    20.272 |     20.206 |         9.128 |           9.261 |                      64 |
|   2013 |                347 |    20.033 |     20.002 |         9.276 |           9.423 |                      64 |
|   2014 |                351 |    18.385 |     18.321 |         9.65  |           9.782 |                      64 |
|   2015 |                351 |    19.164 |     19.109 |         9.572 |           9.71  |                      64 |
|   2016 |                351 |    18.196 |     18.156 |         9.078 |           9.202 |                      64 |
|   2017 |                351 |    18.61  |     18.584 |         9.104 |           9.201 |                      64 |
|   2018 |                351 |    18.462 |     18.418 |         9.213 |           9.294 |                      64 |
|   2019 |                353 |    18.633 |     18.582 |         9.251 |           9.343 |                      64 |
|   2021 |                347 |    18.979 |     18.907 |         8.702 |           8.825 |                      64 |
|   2022 |                358 |    18.453 |     18.423 |         8.954 |           9.037 |                      64 |
|   2023 |                363 |    18.264 |     18.218 |         8.796 |           8.899 |                      64 |
|   2024 |                362 |    17.137 |     17.101 |         9.24  |           9.325 |                      64 |
|   2025 |                364 |    17.294 |     17.254 |         9.397 |           9.471 |                      64 |
|   2026 |                365 |    16.824 |     16.81  |         9.47  |           9.525 |                     nan |