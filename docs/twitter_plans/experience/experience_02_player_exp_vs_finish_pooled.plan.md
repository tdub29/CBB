# EXPERIENCE 02: Player Experience vs Finish (Pooled)

## Purpose in Sequence
Test the base player-side question first: does raw roster experience help tournament teams make deeper runs once the whole sample is pooled together?

## Depends on
- Prior brief: `docs/twitter_plans/experience/experience_01_context_over_time.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/player_exp_vs_finish_pooled.png`
  - `reports/player_exp_vs_finish_pooled_table.csv`
  - `reports/player_exp_vs_finish_pooled_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/player_exp_vs_finish_pooled.png`
- Summary table reference: `reports/player_exp_vs_finish_pooled_table.csv`
- Supporting notes:
  - `Raw Exp. rho: -0.123`
  - `Exact-seed-controlled Exp. rho: +0.021`
  - `Champion average Exp. vs R64: 1.69 vs 1.89 (-0.20)`
  - `Champion average season-wide Exp rank vs R64: 188.8 vs 145.7 (+43.2, where lower rank means more experience)`

## Primary Metric Rule
- Use `exp_raw` as the plotting metric.
- Use `exp_rank_season` only as support context in the table, summary, and later current-field application.
- The seed-controlled panel is the lead interpretation.

## What to Extract
- Chosen headline stat: `After controlling for exact seed, the player Exp. relationship is basically zero (rho = +0.021).`
- Baseline comparison: `The raw pooled view is actually inverse (rho = -0.123), and champions were less experienced than R64 exits on both raw Exp. and season-wide Exp rank.`
- Bracket implication: `Do not treat veteran rosters as a standalone March edge just because the team has a high Exp. value.`

## Narrative Options
- Option A: Player experience still survives exact seed control, so veteran rosters deserve real bracket weight.
- Option B: The broad player-experience story is weak to inverse once you actually check tournament outcomes.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `If the pooled player-exp signal is this weak, the next check is whether experience shows up more clearly once teams reach the later rounds.`

## Chart Usage Note
- Keep both views on one page.
- Make the exact-seed-controlled panel the focal point.
- Use the rank context in text, not as a second axis.

## Blog Reuse Note
- This brief opens the player-side evidence section.
- The longform version should make it explicit that player experience fails the broad pooled test before moving to the late-round hypothesis.
