# SOS Analysis 1: Conference Strength by Year

- Input: `C:\Users\TrevorWhite\Downloads\CBBData\CBB\data\processed\sos_sample.csv`
- Figure: `C:\Users\TrevorWhite\Downloads\CBBData\CBB\reports\figures\sos_conf_strength_over_time.png`
- Rows analyzed: `581` conference-year rows from `2008-2026` (2020 excluded).
- Conference avg validation: max absolute diff between recomputed and stored `conf_avg_barthag` = `0.0000000000`.

## Key Findings

- Mean power-vs-mid-major gap across the sample: `0.388` Barthag.
- Widest gap: `2026` at `0.439`.
- Narrowest gap: `2012` at `0.335`.
- Suggested breakpoint years: `2012, 2022, 2024`.

## Suggested Era Ranges

- `2008-2011`
- `2012-2021`
- `2022-2023`
- `2024-2026`

## Latest Season Top Conferences (2026)

| Conf   |   avg_barthag |   rank |
|:-------|--------------:|-------:|
| SEC    |      0.857237 |      1 |
| B12    |      0.843363 |      2 |
| B10    |      0.835033 |      3 |
| ACC    |      0.803117 |      4 |
| BE     |      0.792082 |      5 |
| MWC    |      0.677442 |      6 |

## Breakpoint Candidates

|   break_year |   top6_churn |   gap_delta | entered_top6   | left_top6   |
|-------------:|-------------:|------------:|:---------------|:------------|
|         2012 |     0.285714 |   0.0438888 | MWC            | P12         |
|         2022 |     0        |   0.0272902 | -              | -           |
|         2024 |     0.285714 |   0.0339452 | ACC            | MWC         |

## Review Notes

- Breakpoints are based on a combined score of top-6 membership churn and year-over-year power-gap change.
- Suggested breakpoints are filtered to avoid adjacent-year cuts that create artificial one-year eras.
- The chart favors readability over completeness by plotting the conferences with the strongest and most persistent top-12 presence.
- Supporting CSV tables contain the full conference-year ranking history and gap series for a second-pass audit.