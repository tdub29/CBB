# Size Analysis 1: Size Context Over Time

- Primary size metric: `EFFHGT` (`Effective height`).
- Full-field `EFFHGT` moved from `79.71` in `2008` to `80.32` in `2026` (`+0.60`); the `2026` point is the full D1 field before the tournament.
- Dominant-big-man count starts in `2010` because there is no positional data for `2008-2009`; it moved from `12` in `2010` to `8` in `2026` (`-4`), using `max_c_porpag >= 4.0`. The `2026` point uses the projected field.
- Outcome contamination: `max_c_porpag` comes from season player stats that include the team's NCAA tournament games, so any 2008-2025 split of tournament results on it is partly measured on the outcome it is compared with. Read it as outcome-contaminated description, not as a pre-tournament signal. The `2026` count uses a pre-tournament player export, so it is not like-for-like with the earlier seasons.
- Interpretation: this chart is field-level context only; it does not relate size to tournament results.

## Table

|   year |   field_team_count |   field_avg_effhgt |   tournament_team_count |   dominant_big_man_count |
|-------:|-------------------:|-------------------:|------------------------:|-------------------------:|
|   2008 |                341 |             79.712 |                      64 |                      nan |
|   2009 |                344 |             79.654 |                      64 |                      nan |
|   2010 |                347 |             79.673 |                      64 |                       12 |
|   2011 |                345 |             79.674 |                      64 |                       15 |
|   2012 |                345 |             79.661 |                      64 |                       10 |
|   2013 |                347 |             79.712 |                      64 |                       11 |
|   2014 |                351 |             79.762 |                      64 |                        8 |
|   2015 |                351 |             79.742 |                      64 |                        7 |
|   2016 |                351 |             79.836 |                      64 |                        9 |
|   2017 |                351 |             79.84  |                      64 |                       10 |
|   2018 |                351 |             79.886 |                      64 |                        8 |
|   2019 |                353 |             79.857 |                      64 |                       11 |
|   2021 |                347 |             79.899 |                      64 |                       11 |
|   2022 |                358 |             79.921 |                      64 |                       15 |
|   2023 |                363 |             80.028 |                      64 |                       12 |
|   2024 |                362 |             80.169 |                      64 |                       17 |
|   2025 |                364 |             80.144 |                      64 |                       11 |
|   2026 |                365 |             80.315 |                      68 |                        8 |