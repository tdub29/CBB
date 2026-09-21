# UConn (Connecticut) — data used by experience plots and team history cache

This file summarizes the data used for Connecticut in the experience visuals and the raw team history cache for review.

---

## 1. Data used by experience plots (all years)

**Source:** `data/processed/experience_sample.csv` (built from `data/raw/assembled/all_trank_team_tables.csv`).  
The experience scripts load this, then add derived columns (`coach_tourn_games_5y`, `coach_tourn_win_pct_5y`, `exp_rank_season`, `exp_pct_season`) and optionally filter to **tournament** rows (non-null `seed` and `finish` in R64 → CHAMPS) for coach-vs-March and seed-group plots.

|   year | coach       |   seed | finish     | Conf   |   exp_raw |   Barthag |   coach_tourn_appearances_5y |   coach_tourn_wins_5y |   coach_tourn_losses_5y |   coach_years_with_program |   coach_tourn_games_5y |   coach_tourn_win_pct_5y |   exp_rank_season |   exp_pct_season |
|-------:|:------------|-------:|:-----------|:-------|----------:|----------:|-----------------------------:|----------------------:|------------------------:|---------------------------:|-----------------------:|-------------------------:|------------------:|-----------------:|
|   2008 | Jim Calhoun |      4 | R64        | BE     |  1.41683  |    0.8998 |                            0 |                     0 |                       0 |                          1 |                      0 |                 0        |               258 |        0.244118  |
|   2009 | Jim Calhoun |      1 | Final Four | BE     |  2.16691  |    0.9595 |                            1 |                     0 |                       1 |                          2 |                      1 |                 0        |                36 |        0.897959  |
|   2010 | Jim Calhoun |    nan | nan        | BE     |  1.80985  |    0.8383 |                            2 |                     4 |                       2 |                          3 |                      6 |                 0.666667 |               143 |        0.589595  |
|   2011 | Jim Calhoun |      3 | CHAMPS     | BE     |  0.943011 |    0.9012 |                            2 |                     4 |                       2 |                          4 |                      6 |                 0.666667 |               334 |        0.0319767 |
|   2012 | Jim Calhoun |      9 | R64        | BE     |  0.8018   |    0.8584 |                            3 |                    10 |                       2 |                          5 |                     12 |                 0.833333 |               337 |        0.0232558 |
|   2013 | Kevin Ollie |    nan | nan        | BE     |  1.33933  |    0.8417 |                            0 |                     0 |                       0 |                          1 |                      0 |                 0        |               278 |        0.199422  |
|   2014 | Kevin Ollie |      7 | CHAMPS     | Amer   |  2.05501  |    0.893  |                            0 |                     0 |                       0 |                          2 |                      0 |                 0        |                78 |        0.78      |
|   2015 | Kevin Ollie |    nan | nan        | Amer   |  1.30812  |    0.7135 |                            1 |                     6 |                       0 |                          3 |                      6 |                 1        |               295 |        0.16      |
|   2016 | Kevin Ollie |      9 | R32        | Amer   |  1.96781  |    0.8738 |                            1 |                     6 |                       0 |                          4 |                      6 |                 1        |                92 |        0.74      |
|   2017 | Kevin Ollie |    nan | nan        | Amer   |  1.60272  |    0.6794 |                            2 |                     7 |                       1 |                          5 |                      8 |                 0.875    |               215 |        0.388571  |
|   2018 | Kevin Ollie |    nan | nan        | Amer   |  1.47489  |    0.4586 |                            2 |                     7 |                       1 |                          6 |                      8 |                 0.875    |               251 |        0.285714  |
|   2019 | Dan Hurley  |    nan | nan        | Amer   |  1.69896  |    0.6957 |                            2 |                     2 |                       2 |                          1 |                      4 |                 0.5      |               170 |        0.519886  |
|   2021 | Dan Hurley  |      7 | R64        | BE     |  1.7846   |    0.9182 |                            2 |                     2 |                       2 |                          3 |                      4 |                 0.5      |               164 |        0.528902  |
|   2022 | Dan Hurley  |      5 | R64        | BE     |  2.11224  |    0.8786 |                            3 |                     2 |                       3 |                          4 |                      5 |                 0.4      |               141 |        0.607843  |
|   2023 | Dan Hurley  |      4 | CHAMPS     | BE     |  1.72423  |    0.9472 |                            3 |                     1 |                       3 |                          5 |                      4 |                 0.25     |               274 |        0.245856  |
|   2024 | Dan Hurley  |      1 | CHAMPS     | BE     |  1.70986  |    0.9686 |                            3 |                     6 |                       2 |                          6 |                      8 |                 0.75     |               283 |        0.218837  |
|   2025 | Dan Hurley  |      8 | R32        | BE     |  1.64107  |    0.8746 |                            4 |                    12 |                       2 |                          7 |                     14 |                 0.857143 |               300 |        0.176309  |
|   2026 | Dan Hurley  |    nan | nan        | BE     |  2.005    |    0.9612 |                            5 |                    13 |                       3 |                          8 |                     16 |                 0.8125   |               158 |        0.568681  |

**Notes:**
- `coach_tourn_appearances_5y` now counts only **actual NCAA tournament bids** in the 5 years prior (current season excluded, 2020 excluded).
- Dan Hurley 2019: **2** appearances = Rhode Island 2017 (11-seed R32) + Rhode Island 2018 (7-seed R32). His non-tournament Rhode Island years (2013–2016) no longer inflate the count.
- Rows with `seed`/`finish` NaN are non-tournament seasons; they appear in context/overview plots but are dropped for coach-vs-March and seed-group plots.

---

## 2. Team history cache — all years (UConn)

**Source:** `data/raw/team_history_cache.csv`.

| team        |   year |   t_rk | coach       |   seed | finish     | conf   | ov_rec   | conf_rec   |   adj_o |   adj_d |   efg_o |   efg_d |   to_percent |   tod_percent |   or_percent |   dr_percent |   ftr |   ftrd |   ft_percent |   two_pct |   two_pct_d |   three_pct |   adj_t |
|:------------|-------:|-------:|:------------|-------:|:-----------|:-------|:---------|:-----------|--------:|--------:|--------:|--------:|-------------:|--------------:|-------------:|-------------:|------:|-------:|-------------:|----------:|------------:|------------:|--------:|
| Connecticut |   2008 |     24 | Jim Calhoun |      4 | R64        | BE     | 24-9     | 13-5       |   115.6 |    95.9 |    50.6 |    44.2 |         19.4 |          17.3 |         39.5 |         34.2 |  48.1 |   23.5 |         72   |      49.5 |        40.5 |        36.1 |    67.3 |
| Connecticut |   2009 |      3 | Jim Calhoun |      1 | Final Four | BE     | 31-5     | 15-3       |   116.3 |    87.1 |    51.3 |    42.4 |         18.1 |          16.8 |         39.8 |         30.6 |  46.4 |   19.5 |         67.8 |      51.4 |        40.9 |        34.1 |    67.4 |
| Connecticut |   2010 |     56 | Jim Calhoun |    nan | nan        | BE     | 18-16    | 7-11       |   107.6 |    93.8 |    48.2 |    44.2 |         21.8 |          18.1 |         38.7 |         35.2 |  44.2 |   26.6 |         68.9 |      48.6 |        42.8 |        31   |    67.2 |
| Connecticut |   2011 |     13 | Jim Calhoun |      3 | CHAMPS     | BE     | 32-9     | 9-9        |   115.8 |    93.6 |    48.2 |    44.5 |         17.1 |          17.4 |         38.7 |         34.1 |  33.8 |   26.8 |         76.3 |      47.7 |        42.4 |        32.9 |    64.2 |
| Connecticut |   2012 |     36 | Jim Calhoun |      9 | R64        | BE     | 20-14    | 8-10       |   111   |    96.2 |    50   |    44.6 |         19   |          16.9 |         36.3 |         34.8 |  34.6 |   25.3 |         66.2 |      50.1 |        40.9 |        33.2 |    63   |
| Connecticut |   2013 |     49 | Kevin Ollie |    nan | nan        | BE     | 20-10    | 10-8       |   109.8 |    95.5 |    50   |    45.3 |         18.2 |          20.1 |         28.5 |         36   |  36.9 |   31   |         74.3 |      49.5 |        44.4 |        34   |    64   |
| Connecticut |   2014 |     14 | Kevin Ollie |      7 | CHAMPS     | Amer   | 32-8     | 12-6       |   113   |    92   |    51.5 |    44.6 |         17.3 |          19.6 |         30.4 |         32.8 |  38.3 |   35.7 |         77.7 |      48.1 |        42.2 |        38.7 |    62.8 |
| Connecticut |   2015 |     85 | Kevin Ollie |    nan | nan        | Amer   | 20-15    | 10-8       |   107.1 |    99.4 |    50   |    45.2 |         18.6 |          17.9 |         30.2 |         30.5 |  32.3 |   32.6 |         70.8 |      48.4 |        42.8 |        35.4 |    61.9 |
| Connecticut |   2016 |     33 | Kevin Ollie |      9 | R32        | Amer   | 25-11    | 11-7       |   111.4 |    94.6 |    51.5 |    44.5 |         16.3 |          18.2 |         28.6 |         29.1 |  29.6 |   35.4 |         79.3 |      49.9 |        42   |        36.5 |    65.9 |
| Connecticut |   2017 |     90 | Kevin Ollie |    nan | nan        | Amer   | 16-17    | 9-9        |   106.3 |    99.2 |    48.5 |    45.6 |         18.7 |          16.1 |         29.9 |         30.5 |  34.5 |   32   |         71.6 |      46.1 |        43   |        35.4 |    64.8 |
| Connecticut |   2018 |    185 | Kevin Ollie |    nan | nan        | Amer   | 14-18    | 7-11       |   104.4 |   106   |    47.5 |    51.1 |         18.5 |          18.3 |         29.4 |         32.7 |  32.4 |   33.9 |         74.5 |      46.6 |        48.5 |        32.6 |    66.9 |
| Connecticut |   2019 |     96 | Dan Hurley  |    nan | nan        | Amer   | 16-17    | 6-12       |   108.8 |   102.1 |    51   |    49.6 |         18.5 |          20   |         32.3 |         30.2 |  32.7 |   39.2 |         68   |      50.7 |        49.2 |        34.4 |    68.9 |
| Connecticut |   2020 |     56 | Dan Hurley  |    nan | nan        | Amer   | 19-12    | 10-8       |   108.6 |    96.3 |    47.8 |    46.4 |         19.9 |          21.9 |         34.1 |         29.4 |  34.6 |   34.6 |         74.2 |      47.4 |        45   |        32.4 |    68.3 |
| Connecticut |   2021 |     15 | Dan Hurley  |      7 | R64        | BE     | 15-8     | 11-6       |   114.1 |    93.5 |    49   |    46.7 |         17.5 |          19.6 |         37.4 |         29.5 |  29   |   36.8 |         72.2 |      48.3 |        45.8 |        33.5 |    65.2 |
| Connecticut |   2022 |     25 | Dan Hurley  |      5 | R64        | BE     | 23-10    | 13-6       |   113.2 |    95.9 |    49.6 |    46   |         17.3 |          18   |         37.9 |         27.6 |  30.5 |   31.7 |         75.3 |      48   |        42.6 |        35.2 |    65   |
| Connecticut |   2023 |      1 | Dan Hurley  |      4 | CHAMPS     | BE     | 31-8     | 13-7       |   121.5 |    91.2 |    53.9 |    44.4 |         18.9 |          18.5 |         38.5 |         26.1 |  30.8 |   37.8 |         76.1 |      53.6 |        44.4 |        36.3 |    66.7 |
| Connecticut |   2024 |      1 | Dan Hurley  |      1 | CHAMPS     | BE     | 37-3     | 18-2       |   128   |    91.3 |    56.9 |    44.4 |         14.5 |          16   |         36.7 |         26.7 |  31.9 |   31.1 |         74.3 |      59.1 |        43.2 |        35.8 |    64.6 |
| Connecticut |   2025 |     36 | Dan Hurley  |      8 | R32        | BE     | 24-11    | 14-6       |   120.8 |   101.2 |    55   |    48.1 |         17   |          15.7 |         35.5 |         27.8 |  30.8 |   41   |         79.1 |      57   |        45.8 |        34.8 |    64.1 |
| Connecticut |   2026 |      8 | Dan Hurley  |    nan | nan        | BE     | 27-3     | 17-2       |   124.4 |    94.8 |    56.1 |    45.3 |         16.2 |          18.2 |         34.6 |         28.9 |  31.2 |   39.8 |         70.3 |      57.1 |        44.9 |        36.5 |    64.9 |

**Column key (cache):** `t_rk` = Torvik rank; `ov_rec` / `conf_rec` = overall / conference record; `adj_o` / `adj_d` = adjusted offense/defense; `efg_o` / `efg_d` = eFG%; `to_percent` / `tod_percent` = turnover%; `or_percent` / `dr_percent` = offensive/defensive rebounding%; `ftr` / `ftrd` = free throw rate; `adj_t` = adjusted tempo.

---

## 3. Dan Hurley tournament history (cross-team, from cache)

For reference, here are all Dan Hurley rows across all teams in `team_history_cache.csv`:

| team         |   year |   seed | finish | conf |
|:-------------|-------:|-------:|:-------|:-----|
| Wagner       |   2011 |    nan | nan    | NEC  |
| Wagner       |   2012 |    nan | nan    | NEC  |
| Rhode Island |   2013 |    nan | nan    | A10  |
| Rhode Island |   2014 |    nan | nan    | A10  |
| Rhode Island |   2015 |    nan | nan    | A10  |
| Rhode Island |   2016 |    nan | nan    | A10  |
| Rhode Island |   2017 |     11 | R32    | A10  |
| Rhode Island |   2018 |      7 | R32    | A10  |
| Connecticut  |   2019 |    nan | nan    | Amer |
| Connecticut  |   2020 |    nan | nan    | Amer |
| Connecticut  |   2021 |      7 | R64    | BE   |
| Connecticut  |   2022 |      5 | R64    | BE   |
| Connecticut  |   2023 |      4 | CHAMPS | BE   |
| Connecticut  |   2024 |      1 | CHAMPS | BE   |
| Connecticut  |   2025 |      8 | R32    | BE   |
| Connecticut  |   2026 |    nan | nan    | BE   |

Dan Hurley made the NCAA tournament in **6** total seasons: Rhode Island 2017, Rhode Island 2018, UConn 2021, UConn 2022, UConn 2023, UConn 2024. Wagner 2011-12 and Rhode Island 2013-16 had no tournament bids.
