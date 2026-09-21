# SOS 03: SOS vs Finish by Year

## Purpose in Sequence
Stress-test the pooled result by showing whether the SOS-to-finish relationship is stable across tournaments or concentrated in only a subset of years.

## Depends on
- Prior brief: `docs/twitter_plans/sos/sos_02_sos_vs_finish_pooled.plan.md`
- Reused context from `docs/twitter_plans/sos/sos_01_conf_strength_by_year.plan.md` for any era references
- Script outputs required before this brief is filled:
  - `reports/figures/sos_vs_finish_by_year.png`
  - Printed year-by-year finish summary

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/sos_vs_finish_by_year.png`
- Summary table reference: `reports/sos_vs_finish_by_year_correlations.csv`
- Stability reference: `17 of 17 years positive; 13 of 17 at or above 0.40 rho`
- Supporting notes:
  - `Strongest year: 2025 at rho 0.621`
  - `Weakest year: 2010 at rho 0.329`

## Primary Metric Rule
- Use the same thread-wide metric chosen in `sos_02`.
- Only switch within this brief if the pooled rule was misapplied or later recalculation changes the margin by at least `0.05`; if that happens, update `sos_02` first.
- Record the metric used here: `Elite SOS`

## What to Extract
- Chosen headline stat: `17 of 17 tournament years showed a positive Elite SOS-to-finish relationship`
- Baseline comparison: `2025 was the strongest year at rho 0.621 versus 2010 as the weakest at 0.329`
- Bracket implication: `SOS is portable across tournaments; the strength varies, but the direction of the signal never flipped in this sample`

## Narrative Options
- Option A: The signal shows up in most years, so the pooled result is credible and portable.
- Option B: The signal is uneven, which means SOS works better as context than as a universal tournament law.
- Lock chosen angle after results: `Option A`

## Bridge to Next Brief
- Transition sentence: `If the signal is this stable overall, the next question is where it matters most by seed line rather than across the whole field.`

## Chart Usage Note
- Use the year-by-year figure only if the stability pattern is interpretable on mobile.
- If the heatmap or faceted chart is too dense for Twitter, summarize the count of "signal years" in text and reserve the image for the blog.

## Blog Reuse Note
- This brief should be merged into the same longform section as `sos_02`.
- Its role is to add nuance: confirm whether the pooled answer is robust or conditional, not to reset the narrative.
