# SOS 06: Era Comparison

## Purpose in Sequence
Re-test the key SOS conclusions inside the eras identified earlier so the final guidance reflects how the schedule environment changed over time rather than pretending one rule fits every tournament era.

## Depends on
- Required prior brief: `docs/twitter_plans/sos/sos_01_conf_strength_by_year.plan.md`
- Supporting prior briefs:
  - `docs/twitter_plans/sos/sos_02_sos_vs_finish_pooled.plan.md`
  - `docs/twitter_plans/sos/sos_04_seed_groups_advancers.plan.md`
  - `docs/twitter_plans/sos/sos_05_overperformance_and_conferences.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/sos_era_comparison.png`
  - Printed side-by-side era comparison summary

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/sos_era_comparison.png`
- Summary table reference: `reports/sos_era_comparison_table.csv`
- Mandatory era breakpoint reference from `sos_01`: `2012 structural shift; 2022 transfer-rules era start`
- Supporting notes:
  - `Strongest pooled raw SOS-to-finish era: 2022-2025 at rho +0.508`
  - `Weakest exact-seed-adjusted SOS usefulness: 2008-2011 at rho -0.032`
  - `The 11-13 seed pocket strengthened materially after 2011, peaking at rho +0.375 in 2012-2021 and staying elevated at +0.331 in 2022-2025`

## Primary Metric Rule
- Use the same thread-wide metric chosen in `sos_02`.
- Recompute era-level strength only to describe how the relationship changes, not to replace the global metric choice.
- Record the metric used here: `Elite SOS`

## What to Extract
- Chosen headline stat: `Exact-seed-adjusted Elite SOS improved from rho -0.032 in 2008-2011 to +0.110 in 2022-2025`
- Baseline comparison: `The modern era is +0.142 rho stronger than the earliest era on the seed-adjusted measure, and the raw pooled rho also climbed from +0.415 to +0.508`
- Bracket implication: `Modern brackets justify more SOS weight than early-sample lore does, especially when evaluating the 11-13 seed upset pocket`

## Narrative Options
- Option A: SOS matters more in the modern era, so recent brackets should weight it more heavily than old tournament lore does.
- Option B: The relationship weakened or shifted, so historical bracket rules should be applied with caution.
- Lock chosen angle after results: `Option A`

## Bridge to Next Brief
- Transition sentence: `With the era-adjusted rules in place, the last step is to apply the framework to the current field and identify who stands out right now.`

## Chart Usage Note
- This chart is usually better for the blog unless the era contrast is dramatic enough to survive mobile compression.
- If used on Twitter, the tweet must state the era takeaway in plain language and avoid asking readers to decode multiple panels.

## Blog Reuse Note
- This brief is the structural pivot of the longform post.
- It should sit between the seed-adjusted decision section and the current-field application so the blog reads as "here is what changed, therefore here is how to use the framework now."
