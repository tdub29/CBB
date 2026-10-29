# SOS Analysis 7: Projected 2026 Field vs Seed-Line Schedule Strength

The March 2026 bracketology projection (`data/raw/torvik_2026/bracketology_archive/2026_expected_seeds_update.csv`) is joined to the `2026` SOS sample. Each team's `Elite SOS` is compared with the average of past tournament teams on the same seed line, over `2008-2025` and over `2022-2025`. This is a pre-tournament snapshot; it uses no 2026 results.

## Historical relationship (2008-2025)

- Seed-adjusted Spearman rho of `Elite SOS` with finish vs exact-seed expectation: `+0.061`.
- `Elite SOS` was positively correlated with raw finish depth in `17/17` seasons. Raw finish is not seed-adjusted, and stronger teams also play tougher schedules.
- Seed-band rho of `Elite SOS` with finish vs the band's average, `2008-2025` / `2022-2025`: `1-2` +0.060 (n=136) / +0.428 (n=32); `3-4` -0.008 (n=136) / -0.129 (n=32); `5-6` +0.051 (n=136) / +0.002 (n=32); `7-8` -0.022 (n=136) / +0.043 (n=32); `9-10` +0.056 (n=136) / +0.165 (n=32); `11-13` +0.313 (n=204) / +0.331 (n=48); `14-16` +0.074 (n=204) / +0.058 (n=48). No significance test is applied.

## Projected 2026 teams furthest from their seed line

- Highest `Elite SOS` relative to the 2008-2025 seed-line average: `Kansas` (4) 41.59, +11.24 vs the seed-line average; `Alabama` (4) 40.99, +10.64 vs the seed-line average; `BYU` (7) 37.36, +10.42 vs the seed-line average; `Texas Tech` (5) 38.17, +9.91 vs the seed-line average; `Kentucky` (6) 38.95, +9.69 vs the seed-line average.
- Lowest: `Miami OH` (10) 8.01, -19.85 vs the seed-line average; `Saint Louis` (10) 14.69, -13.18 vs the seed-line average; `High Point` (12) 5.82, -11.25 vs the seed-line average; `Gonzaga` (3) 24.41, -6.83 vs the seed-line average; `Utah St.` (9) 22.58, -6.16 vs the seed-line average.
- Position counts: `32` above, `17` near and `19` below the seed baseline, where above or below means at least 0.5 standard deviations from that seed line's average.

## Table

|   projected_seed | team             | conf   |   elite_sos |   full_seed_avg_elite_sos |   recent_seed_avg_elite_sos |   full_sos_delta | full_sample_position   | recent_position     |
|-----------------:|:-----------------|:-------|------------:|--------------------------:|----------------------------:|-----------------:|:-----------------------|:--------------------|
|                1 | Michigan         | B10    |      39.16  |                    30.86  |                      33.329 |            8.3   | above seed baseline    | above seed baseline |
|                1 | Duke             | ACC    |      37.817 |                    30.86  |                      33.329 |            6.957 | above seed baseline    | above seed baseline |
|                1 | Florida          | SEC    |      37.252 |                    30.86  |                      33.329 |            6.392 | above seed baseline    | above seed baseline |
|                1 | Arizona          | B12    |      36.497 |                    30.86  |                      33.329 |            5.637 | above seed baseline    | above seed baseline |
|                2 | Illinois         | B10    |      38.145 |                    31.673 |                      33.019 |            6.473 | above seed baseline    | above seed baseline |
|                2 | Houston          | B12    |      34.258 |                    31.673 |                      33.019 |            2.586 | above seed baseline    | near seed baseline  |
|                2 | Iowa St.         | B12    |      33.356 |                    31.673 |                      33.019 |            1.683 | near seed baseline     | near seed baseline  |
|                2 | Connecticut      | BE     |      32.711 |                    31.673 |                      33.019 |            1.039 | near seed baseline     | near seed baseline  |
|                3 | Purdue           | B10    |      39.046 |                    31.244 |                      33.751 |            7.803 | above seed baseline    | above seed baseline |
|                3 | Michigan St.     | B10    |      35.584 |                    31.244 |                      33.751 |            4.341 | above seed baseline    | near seed baseline  |
|                3 | Virginia         | ACC    |      28.036 |                    31.244 |                      33.751 |           -3.208 | below seed baseline    | below seed baseline |
|                3 | Gonzaga          | WCC    |      24.414 |                    31.244 |                      33.751 |           -6.83  | below seed baseline    | below seed baseline |
|                4 | Kansas           | B12    |      41.589 |                    30.35  |                      31.905 |           11.239 | above seed baseline    | above seed baseline |
|                4 | Alabama          | SEC    |      40.989 |                    30.35  |                      31.905 |           10.639 | above seed baseline    | above seed baseline |
|                4 | Nebraska         | B10    |      33.564 |                    30.35  |                      31.905 |            3.213 | above seed baseline    | near seed baseline  |
|                4 | Vanderbilt       | SEC    |      32.757 |                    30.35  |                      31.905 |            2.407 | above seed baseline    | near seed baseline  |
|                5 | Texas Tech       | B12    |      38.165 |                    28.256 |                      27.25  |            9.91  | above seed baseline    | above seed baseline |
|                5 | Tennessee        | SEC    |      37.26  |                    28.256 |                      27.25  |            9.005 | above seed baseline    | above seed baseline |
|                5 | Arkansas         | SEC    |      37.13  |                    28.256 |                      27.25  |            8.874 | above seed baseline    | above seed baseline |
|                5 | Wisconsin        | B10    |      33.687 |                    28.256 |                      27.25  |            5.432 | above seed baseline    | above seed baseline |
|                6 | Kentucky         | SEC    |      38.946 |                    29.258 |                      32.814 |            9.688 | above seed baseline    | above seed baseline |
|                6 | Louisville       | ACC    |      34.209 |                    29.258 |                      32.814 |            4.952 | above seed baseline    | near seed baseline  |
|                6 | North Carolina   | ACC    |      31.085 |                    29.258 |                      32.814 |            1.828 | near seed baseline     | near seed baseline  |
|                6 | St. John's       | BE     |      29.499 |                    29.258 |                      32.814 |            0.241 | near seed baseline     | below seed baseline |
|                7 | BYU              | B12    |      37.356 |                    26.935 |                      28.98  |           10.422 | above seed baseline    | above seed baseline |
|                7 | UCLA             | B10    |      34.705 |                    26.935 |                      28.98  |            7.77  | above seed baseline    | above seed baseline |
|                7 | Georgia          | SEC    |      32.544 |                    26.935 |                      28.98  |            5.609 | above seed baseline    | above seed baseline |
|                7 | Miami FL         | ACC    |      25.961 |                    26.935 |                      28.98  |           -0.973 | near seed baseline     | near seed baseline  |
|                8 | Ohio St.         | B10    |      35.643 |                    26.756 |                      27.964 |            8.888 | above seed baseline    | above seed baseline |
|                8 | TCU              | B12    |      32.879 |                    26.756 |                      27.964 |            6.123 | above seed baseline    | above seed baseline |
|                8 | Clemson          | ACC    |      30.108 |                    26.756 |                      27.964 |            3.353 | above seed baseline    | above seed baseline |
|                8 | Saint Mary's     | WCC    |      21.631 |                    26.756 |                      27.964 |           -5.125 | below seed baseline    | below seed baseline |
|                9 | Iowa             | B10    |      33.571 |                    28.737 |                      31.802 |            4.835 | above seed baseline    | near seed baseline  |
|                9 | Texas A&M        | SEC    |      31.371 |                    28.737 |                      31.802 |            2.634 | near seed baseline     | near seed baseline  |
|                9 | Villanova        | BE     |      30.346 |                    28.737 |                      31.802 |            1.609 | near seed baseline     | near seed baseline  |
|                9 | Utah St.         | MWC    |      22.577 |                    28.737 |                      31.802 |           -6.16  | below seed baseline    | below seed baseline |
|               10 | Oklahoma         | SEC    |      34.198 |                    27.865 |                      25.161 |            6.333 | above seed baseline    | above seed baseline |
|               10 | N.C. State       | ACC    |      32.431 |                    27.865 |                      25.161 |            4.567 | above seed baseline    | above seed baseline |
|               10 | Saint Louis      | A10    |      14.689 |                    27.865 |                      25.161 |          -13.176 | below seed baseline    | below seed baseline |
|               10 | Miami OH         | MAC    |       8.012 |                    27.865 |                      25.161 |          -19.853 | below seed baseline    | below seed baseline |
|               11 | Missouri         | SEC    |      33.051 |                    24.59  |                      26.546 |            8.461 | above seed baseline    | above seed baseline |
|               11 | UCF              | B12    |      31.224 |                    24.59  |                      26.546 |            6.633 | above seed baseline    | above seed baseline |
|               11 | SMU              | ACC    |      30.65  |                    24.59  |                      26.546 |            6.06  | above seed baseline    | above seed baseline |
|               11 | Santa Clara      | WCC    |      22.199 |                    24.59  |                      26.546 |           -2.391 | near seed baseline     | below seed baseline |
|               11 | VCU              | A10    |      19.157 |                    24.59  |                      26.546 |           -5.434 | below seed baseline    | below seed baseline |
|               11 | South Florida    | Amer   |      18.748 |                    24.59  |                      26.546 |           -5.842 | below seed baseline    | below seed baseline |
|               12 | McNeese St.      | Slnd   |      14.128 |                    17.069 |                      14.735 |           -2.941 | near seed baseline     | near seed baseline  |
|               12 | Yale             | Ivy    |      12.407 |                    17.069 |                      14.735 |           -4.662 | below seed baseline    | near seed baseline  |
|               12 | Akron            | MAC    |      11.528 |                    17.069 |                      14.735 |           -5.541 | below seed baseline    | below seed baseline |
|               12 | High Point       | BSth   |       5.818 |                    17.069 |                      14.735 |          -11.251 | below seed baseline    | below seed baseline |
|               13 | Northern Iowa    | MVC    |      14.767 |                    13.658 |                      11.283 |            1.108 | near seed baseline     | above seed baseline |
|               13 | Sam Houston St.  | CUSA   |      13.692 |                    13.658 |                      11.283 |            0.033 | near seed baseline     | above seed baseline |
|               13 | Utah Valley      | WAC    |      12.867 |                    13.658 |                      11.283 |           -0.791 | near seed baseline     | above seed baseline |
|               13 | Hofstra          | CAA    |      12.3   |                    13.658 |                      11.283 |           -1.359 | near seed baseline     | near seed baseline  |
|               14 | Troy             | SB     |      11.719 |                    13.219 |                      12.097 |           -1.5   | near seed baseline     | near seed baseline  |
|               14 | UC Irvine        | BW     |      11.259 |                    13.219 |                      12.097 |           -1.96  | below seed baseline    | near seed baseline  |
|               14 | Wright St.       | Horz   |       9.911 |                    13.219 |                      12.097 |           -3.308 | below seed baseline    | below seed baseline |
|               14 | North Dakota St. | Sum    |       8.37  |                    13.219 |                      12.097 |           -4.85  | below seed baseline    | below seed baseline |
|               15 | Idaho            | BSky   |      11.625 |                    11.733 |                      10.912 |           -0.108 | near seed baseline     | near seed baseline  |
|               15 | Tennessee St.    | OVC    |       9.437 |                    11.733 |                      10.912 |           -2.296 | below seed baseline    | below seed baseline |
|               15 | Furman           | SC     |       7.531 |                    11.733 |                      10.912 |           -4.201 | below seed baseline    | below seed baseline |
|               15 | Siena            | MAAC   |       7.056 |                    11.733 |                      10.912 |           -4.677 | below seed baseline    | below seed baseline |
|               16 | Southern         | SWAC   |      17.142 |                    11.129 |                      10.117 |            6.013 | above seed baseline    | above seed baseline |
|               16 | Queens           | ASun   |      15.161 |                    11.129 |                      10.117 |            4.032 | above seed baseline    | above seed baseline |
|               16 | LIU              | NEC    |      10.309 |                    11.129 |                      10.117 |           -0.82  | near seed baseline     | near seed baseline  |
|               16 | Lehigh           | Pat    |       9.872 |                    11.129 |                      10.117 |           -1.257 | near seed baseline     | near seed baseline  |
|               16 | Howard           | MEAC   |       9.289 |                    11.129 |                      10.117 |           -1.84  | below seed baseline    | near seed baseline  |
|               16 | UMBC             | AE     |       7.826 |                    11.129 |                      10.117 |           -3.303 | below seed baseline    | below seed baseline |