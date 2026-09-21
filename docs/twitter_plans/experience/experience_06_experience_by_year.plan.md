# EXPERIENCE 06: Experience by Year

## Purpose in Sequence
Check whether the player-side and coach-side experience signals behave like repeatable March tendencies or just isolated one-year stories.

## Depends on
- Prior brief: `docs/twitter_plans/experience/experience_05_combined_experience_archetypes.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/experience_by_year.png`
  - `reports/experience_by_year_table.csv`
  - `reports/experience_by_year_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/experience_by_year.png`
- Summary table reference: `reports/experience_by_year_table.csv`
- Supporting notes:
  - `Player Exp. was positive in 10 of 17 seasons`
  - `Coach tourney games (5y) was positive in 17 of 17 seasons`
  - `Strongest player year: 2009 at +0.267`
  - `Strongest coach year: 2021 at +0.450`
  - `2025 player Exp. fell to -0.230`

## Primary Metric Rule
- Top panel uses `exp_raw` seed-adjusted rho by season.
- Bottom panel uses `coach_tourn_games_5y` seed-adjusted rho by season.
- No era buckets; use years only.

## What to Extract
- Chosen headline stat: `Coach tourney games (5y) stayed positive in all 17 completed seasons, while player Exp. was positive in only 10 of 17.`
- Baseline comparison: `The player signal swung from +0.267 in 2009 to -0.230 in 2025, while the coach signal stayed consistently positive and peaked at +0.450 in 2021.`
- Bracket implication: `Coach experience is a stable March signal; player experience is much noisier and should be treated more cautiously.`

## Narrative Options
- Option A: Both player and coach experience matter, but one just happens to be slightly more stable.
- Option B: The year view confirms that the coach track is durable while the player track is inconsistent.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `Once the year-by-year story points this clearly toward coaching, the final step is applying the player-plus-coach experience framework to the projected 2026 field.`

## Chart Usage Note
- Keep the `2022` breakpoint visible.
- Include mean lines for both panels.
- Do not use era buckets here; the user explicitly preferred the year view.

## Blog Reuse Note
- This brief is the stability check before the current-field application.
- In longform, it should sharpen confidence in the coach-side carryover more than in the player-side carryover.
