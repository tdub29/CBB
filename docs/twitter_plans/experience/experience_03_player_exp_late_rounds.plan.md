# EXPERIENCE 03: Late-Round Player Experience Test

## Purpose in Sequence
Directly test the core player-side hypothesis: if experience helps in March, does it show up more strongly once teams reach the Elite Eight and beyond?

## Depends on
- Prior brief: `docs/twitter_plans/experience/experience_02_player_exp_vs_finish_pooled.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/player_exp_late_rounds.png`
  - `reports/player_exp_late_rounds_table.csv`
  - `reports/player_exp_late_rounds_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/player_exp_late_rounds.png`
- Summary table reference: `reports/player_exp_late_rounds_table.csv`
- Supporting notes:
  - `Seeded-field Exp. baseline: 1.84`
  - `Elite Eight+ average Exp.: 1.73`
  - `Final Four+ average Exp.: 1.74`
  - `Champions average Exp.: 1.69`
  - `Champions average season-wide Exp rank: 188.8 vs seeded-field baseline 157.0`

## Primary Metric Rule
- Raw `Exp.` is the lead late-round metric.
- `exp_rank_season` is the support lens showing whether late-round teams are also more or less experienced in the season-wide national ranking.

## What to Extract
- Chosen headline stat: `Champions averaged 1.69 Exp. versus a seeded-field baseline of 1.84, and their average season-wide Exp rank was 188.8.`
- Baseline comparison: `Every late-round gate from Elite Eight+ through Champions came in below the seeded-field average on raw Exp., and worse than the seeded-field average on Exp rank.`
- Bracket implication: `The “older teams win later in March” hypothesis does not hold on the player side in this sample.`

## Narrative Options
- Option A: Player experience concentrates in the late rounds even if the pooled sample is messy.
- Option B: The late-round test reinforces the pooled result and pushes against the classic veteran-team March story.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `If the player side of the experience story fails the late-round test, the next question is whether the coach side carries more of the March signal.`

## Chart Usage Note
- Keep the raw Exp. gate comparison on top and the season-wide rank comparison below it.
- Make it obvious that lower rank means more experience.
- This chart should read as a direct hypothesis test, not as a generic stage summary.

## Blog Reuse Note
- This brief becomes the main falsification section for the player-experience hypothesis.
- The blog should use it to pivot from roster experience into coaching.
