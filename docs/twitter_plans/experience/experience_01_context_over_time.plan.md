# EXPERIENCE 01: Player and Coach Experience Over Time

## Purpose in Sequence
Use field-level player and coach experience trends to establish the environment before testing whether either signal actually changes March outcomes.

## Depends on
- Dedicated experience workflow:
  - `scripts/build_experience_sample.py`
  - `scripts/experience_analysis_01_context.py`
  - `scripts/experience_analysis_02_04.py`
  - `scripts/experience_analysis_05_07.py`
- Source data build: `data/processed/experience_sample.csv`
- Script outputs required before this brief is filled:
  - `reports/figures/experience_context_over_time.png`
  - `reports/experience_context_over_time_table.csv`
  - `reports/experience_context_over_time_summary.md`
  - `reports/experience_metric_selection.csv`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/experience_context_over_time.png`
- Summary table reference: `reports/experience_context_over_time_table.csv`
- Metric rule table: `reports/experience_metric_selection.csv`
- Supporting notes:
  - `Field-average Exp. rose from 1.68 in 2008 to 1.91 in 2026 (+0.23).`
  - `Field-average coach_tourn_games_5y rose from 0.31 in 2008 to 1.34 in 2026 (+1.03).`
  - `coach_tourn_games_5y beat coach_years_with_program by 0.202 on seed-adjusted rho, so it is the lead coach metric.`

## Primary Metric Rule
- Player track primary metric is fixed: `exp_raw`
- Player support metric is fixed: `exp_rank_season`
- Coach metric rule:
  - compare `coach_tourn_games_5y` vs `coach_years_with_program` on seed-adjusted Spearman rho
  - only switch if the lead is `>= 0.05`
- Record final coach choice here: `coach_tourn_games_5y`

## What to Extract
- Chosen headline stat: `Field-average Exp. rose +0.23 from 2008 to 2026, while field-average coach_tourn_games_5y rose +1.03.`
- Baseline comparison: `The modern field is older on the player side and more March-tested on the coach side than the early-sample baseline.`
- Bracket implication: `Current experience claims should be judged inside the modern field environment; this page is context, not proof that experience helps in March.`

## Narrative Options
- Option A: Experience rose across the field, so old tournament baselines understate how normal veteran teams and tested coaches are now.
- Option B: Experience got more common, which makes it more important to test whether it still separates teams in March.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `The field got more experienced, but the next question is whether player experience actually changes March outcomes once teams get to the tournament.`

## Chart Usage Note
- One two-panel over-time visual only.
- Keep the `2022` breakpoint visible.
- No conference framing.
- Use this chart as setup, not as a claim that experience itself is predictive.

## Blog Reuse Note
- This brief becomes the setup portion of the Experience post.
- Its job is to frame the modern sample before the thread tests the player and coach hypotheses separately.
