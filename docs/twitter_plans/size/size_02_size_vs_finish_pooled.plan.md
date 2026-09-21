# SIZE 02: Size vs Finish (All Years Pooled)

## Purpose in Sequence
Answer the base question first: across the tournament sample, do bigger or effectively longer teams actually make deeper runs?

## Depends on
- Prior brief: `docs/twitter_plans/size/size_01_context_over_time.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/size_vs_finish_pooled.png`
  - `reports/size_vs_finish_pooled_table.csv`
  - `reports/size_vs_finish_pooled_summary.md`
  - `reports/size_metric_selection.csv`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/size_vs_finish_pooled.png`
- Summary table reference: `reports/size_vs_finish_pooled_table.csv`
- Metric rule table: `reports/size_metric_selection.csv`
- Supporting notes to fill after run:
  - `Pooled raw rho for EFFHGT: +0.235`
  - `Exact-seed-controlled relationship for EFFHGT: -0.008`

## Primary Metric Rule
- Day 2 uses `EFFHGT` only.
- The pooled page must show both the unadjusted and exact-seed-controlled view, but the seed-controlled interpretation is the lead storyline.
- Record the final choice here: `EFFHGT`

## What to Extract
- Chosen headline stat: `After controlling for exact seed, the EFFHGT relationship is essentially zero (rho = -0.008).`
- Baseline comparison: `The raw pooled EFFHGT gradient is +0.235, so almost all of the broad size signal washes out once seed is held constant.`
- Bracket implication: `Treat size as context first; only give it real bracket weight if a useful within-seed signal survives the pooled check.`

## Narrative Options
- Option A: Bigger teams clearly make deeper runs, even after controlling for seed, so size is an actionable tournament edge.
- Option B: The broad size gradient is real, but most of it is seed structure; size works better as context than as a standalone rule.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `The pooled answer matters, but the next test is whether any size signal survives consistently year by year.`

## Chart Usage Note
- Keep both views on one page.
- Make the exact-seed-controlled panel the visual focal point.
- Use the unadjusted panel as context, not as the headline claim.

## Blog Reuse Note
- This brief opens the evidence section of the blog.
- The longform version should explicitly separate descriptive size gradients from predictive usefulness.
