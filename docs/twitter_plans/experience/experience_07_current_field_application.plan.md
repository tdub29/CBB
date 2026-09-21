# EXPERIENCE 07: Current Field Application

## Purpose in Sequence
Apply the historical Day 3 experience findings to the projected 2026 field, using player experience as the main axis and coach experience as the support layer.

## Depends on
- Prior brief: `docs/twitter_plans/experience/experience_06_experience_by_year.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/experience_2026_field.png`
  - `reports/experience_2026_field_table.csv`
  - `reports/experience_2026_field_summary.md`
  - `reports/experience_metric_selection.csv`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/experience_2026_field.png`
- Summary table reference: `reports/experience_2026_field_table.csv`
- Metric rule table: `reports/experience_metric_selection.csv`
- Supporting notes:
  - `Strong carryover cases include UConn (1), Gonzaga (3), UCLA (8), Clemson (9), and N.C. State (9).`
  - `Weak experience cases include Arizona (1), Duke (1), Kentucky (6), BYU (7), and Central Arkansas (16).`
  - `Player-experience-only callouts are weak unless coach support is there too.`

## Primary Metric Rule
- Main chart x-axis uses `exp_raw`.
- Table and narrative must include `exp_rank_season` beside the raw value.
- Halo/outline uses `coach_tourn_games_5y` relative to the exact-seed baseline.
- `coach_tourn_win_pct_5y` stays in the table and summary as extra support context only.

## What to Extract
- Chosen headline stat: `UConn, Gonzaga, UCLA, Clemson, and N.C. State all show up as stronger carryover experience cases once player Exp. is paired with coach support.`
- Baseline comparison: `Duke and Arizona are top seeds with weak player-experience baselines, and several mid-seed teams only become true experience plays when the coach layer is positive too.`
- Bracket implication: `Use player experience as context, but only let it move picks when the coach layer agrees.`

## Narrative Options
- Option A: The 2026 field is full of veteran teams, so player Exp. alone can identify the best March bets.
- Option B: The 2026 field application only becomes useful when the player-exp axis is filtered through coach experience.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `That is the full Day 3 read: player experience alone is weak, but coach-side experience changes how much the experience story actually matters.`

## Chart Usage Note
- Logos only.
- Projected seed on y-axis and raw `Exp.` on x-axis.
- Full-sample seed markers are the main baseline; recent markers are qualifiers.
- Halo is coach support, not a second ranking axis.

## Blog Reuse Note
- This brief is the application close.
- It should end the post with named teams and clear practical use, not with another abstract historical claim.
