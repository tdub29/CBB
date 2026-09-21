# EXPERIENCE 04: Coach Experience vs March Success

## Purpose in Sequence
Test whether coach-side experience is the part of the March story that survives after the player-exp hypothesis weakens.

## Depends on
- Prior brief: `docs/twitter_plans/experience/experience_03_player_exp_late_rounds.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/coach_experience_vs_march.png`
  - `reports/coach_experience_vs_march_table.csv`
  - `reports/coach_experience_vs_march_summary.md`
  - `reports/experience_metric_selection.csv`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/coach_experience_vs_march.png`
- Summary table reference: `reports/coach_experience_vs_march_table.csv`
- Metric rule table: `reports/experience_metric_selection.csv`
- Supporting notes:
  - `Lead coach metric: coach_tourn_games_5y`
  - `coach_tourn_games_5y seed-adjusted rho: +0.240`
  - `coach_years_with_program seed-adjusted rho: +0.038`
  - `coach_tourn_win_pct_5y seed-adjusted rho: +0.472`
  - `Champions averaged 13.47 coach_tourn_games_5y vs 2.89 for R64 exits`

## Primary Metric Rule
- Lead coach metric is selected from:
  - `coach_tourn_games_5y`
  - `coach_years_with_program`
- Only switch away from `coach_tourn_games_5y` if the lead is `>= 0.05`.
- Final result for this run: `coach_tourn_games_5y`
- `coach_tourn_win_pct_5y` remains support context only, even though it is very strong descriptively.

## What to Extract
- Chosen headline stat: `coach_tourn_games_5y carries a +0.240 seed-adjusted rho, while coach_years_with_program is only +0.038.`
- Baseline comparison: `Champions averaged 13.47 recent tourney games from their coach versus 2.89 for R64 exits, and coach_tourn_win_pct_5y is even stronger at +0.472 seed-adjusted rho.`
- Bracket implication: `Coach-side March reps are much more useful than raw player Exp. when trying to separate teams in the bracket.`

## Narrative Options
- Option A: Coach experience matters, but it is mostly just another version of roster maturity.
- Option B: Coach experience is the part of the broader “experience” idea that actually survives March testing.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `Once the coach side shows up this clearly, the next step is to test how player and coach experience work together instead of one at a time.`

## Chart Usage Note
- Use the 2x2 page:
  - raw pooled views on top
  - exact-seed-adjusted views on bottom
  - lead coach metric on the left
  - coach years at program on the right
- Keep coach_tourn_win_pct_5y in text unless a second support visual is absolutely needed.

## Blog Reuse Note
- This brief is the coaching pivot in the post.
- The longform version should use it to explain why general “experience” needs to be split into player and coach components.
