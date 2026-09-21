# SIZE 06: Era Comparison

## Purpose in Sequence
Re-test the key size conclusions inside the eras established on Day 1 so the final guidance reflects how the modern tournament differs from early-sample brackets.

## Depends on
- Prior brief: `docs/twitter_plans/size/size_05_archetypes_vs_overperformance.plan.md`
- Reused era logic from: `docs/twitter_plans/sos/sos_01_conf_strength_by_year.plan.md`
- Era definitions to lock:
  - `2008-2011`
  - `2012-2021`
  - `2022-2025`
- Script outputs required before this brief is filled:
  - `reports/figures/size_era_comparison.png`
  - `reports/size_era_comparison_table.csv`
  - `reports/size_era_comparison_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/size_era_comparison.png`
- Summary table reference: `reports/size_era_comparison_table.csv`
- Supporting notes to fill after run:
  - `Strongest raw era: 2008-2011 at +0.254`
  - `Strongest exact-seed-adjusted era: 2008-2011 at +0.014`
  - `Weakest era: 2022-2025 at -0.042 exact-seed-adjusted`

## Primary Metric Rule
- Use the same thread-wide primary metric chosen in `size_02`.
- Compare both raw and exact-seed-adjusted usefulness by era.
- Record the metric used here: `EFFHGT`

## What to Extract
- Chosen headline stat: `Exact-seed-adjusted EFFHGT moved from +0.014 in 2008-2011 to -0.042 in 2022-2025.`
- Baseline comparison: `Raw size-vs-finish rho stayed near +0.24 in every era, but the within-seed version weakened rather than strengthening in the modern sample.`
- Bracket implication: `Modern brackets may justify more size weight than old tournament lore does, but only if the recent strengthening is supported by the full sample.`

## Narrative Options
- Option A: Size matters more in the modern era, so recent brackets deserve more size weight than early-sample history suggests.
- Option B: The modern-era bump exists, but it is still a qualifier rather than a new rule.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `With the historical size rules set, the last step is to apply them to the projected 2026 field.`

## Chart Usage Note
- Reuse the Day 1 era breaks exactly for cross-topic consistency.
- Show raw and exact-seed-adjusted relationships together only if the panel layout remains legible.
- Keep the narrative focused on what changed, not just which era ranks first.

## Blog Reuse Note
- This brief becomes the pivot section of the Day 2 blog.
- It should connect directly back to the setup from `size_01`.
