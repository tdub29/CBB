# TURNOVERS 02: Offensive Ball Security vs Finish (All Years Pooled)

## Purpose in Sequence
Answer the first core Day 4 question: across the full tournament sample, do teams that protect the ball actually make deeper runs once seed expectations are respected?

## Depends on
- Prior brief: `docs/twitter_plans/turnovers/turnovers_01_context_over_time.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/turnovers_vs_finish_pooled.png`
  - `reports/figures/turnovers_vs_finish_pooled_unadjusted.png`
  - `reports/turnovers_vs_finish_pooled_table.csv`
  - `reports/turnovers_metric_selection.csv`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/turnovers_vs_finish_pooled.png`
- Support figure filename: `reports/figures/turnovers_vs_finish_pooled_unadjusted.png`
- Summary table reference: `reports/turnovers_vs_finish_pooled_table.csv`
- Metric rule table: `reports/turnovers_metric_selection.csv`
- Supporting notes:
  - `Seed-controlled TOR relationship`
  - `Unadjusted TOR gradient from early exits to champions`
  - `BLKED% support only if it sharpens the offensive empty-possession story`

## Primary Metric Rule
- Compare pooled exact-seed-controlled usefulness for `TOR` versus `BLKED%`.
- Default to `TOR` unless `BLKED%` is clearly stronger by at least `0.05` on the chosen relationship metric.
- Record the final choice here: `TOR`
- Record the non-primary support stat here after run.

## What to Extract
- Chosen headline stat: `After controlling for exact seed line, lower TOR still carries a real March edge.`
- Baseline comparison: `The unadjusted pooled gradient should be reported beside the seed-controlled result so the reader sees how much of the story is structural.`
- Bracket implication: `Ball security can be a real bracket tiebreaker, but only if the seed-controlled signal survives.`

## Narrative Options
- Option A: Ball security is one of the cleanest practical March signals once seed is accounted for.
- Option B: The broad ball-security gradient looks stronger than the true seed-relative edge, so it should stay a tiebreaker.
- Lock chosen angle after results: `Option A or B after run`

## Bridge to Next Brief
- Transition sentence: `Taking care of the ball is only half the pressure story, so the next test is whether forcing mistakes and blocking shots travels the same way on defense.`

## Chart Usage Note
- Lead with the seed-controlled pooled chart.
- Keep the unadjusted pooled chart as blog or internal support unless it becomes the cleanest explainer.
- Do not let `BLKED%` overtake the main `TOR` story in the thread unless the metric rule forces it.

## Blog Reuse Note
- Pair this brief directly with `turnovers_03`.
- In longform, this should open the evidence section before the defense-side disruption check.
