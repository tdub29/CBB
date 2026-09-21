# SOS 01: Conference Strength by Year

## Purpose in Sequence
Use conference-level Barthag trends to establish whether the broader scheduling environment changed over time, then lock the era breakpoints that every later interpretation must respect.

## Depends on
- Source analysis plan: `c:\Users\TrevorWhite\.cursor\plans\sos_descriptive_analysis_26ef4bdd.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/sos_conf_strength_over_time.png`
  - Printed conference rankings by year
  - Printed era breakpoint summary

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/sos_conf_strength_over_time.png`
- Summary table reference: `reports/sos_conf_strength_rankings.csv`
- Era breakpoint reference: `2012 structural shift; 2022 transfer-rules era start`
- Supporting notes:
  - `Top-6 composition shifted in 2012 and again in 2024, with MWC entering the top six in 2012 and ACC reclaiming that slot in 2024`
  - `Mean power-vs-mid-major gap was 0.388 Barthag; widest gap was 2026 at 0.439 and narrowest was 2012 at 0.335`

## Primary Metric Rule
- Compare pooled Spearman correlation between finish depth and `Elite SOS` vs `conf_avg_barthag`.
- Use the stronger metric only if it leads by at least `0.05`.
- Otherwise default to `Elite SOS`.
- This brief may still foreground `conf_avg_barthag` for conference-structure context even if it is not the thread-wide primary metric, but the compiler must note that this is setup context, not the main predictive claim.
- Record the final choice here: `Elite SOS`

## What to Extract
- Chosen headline stat: `The power-vs-mid-major Barthag gap widened from 0.372 in 2022 to 0.439 in 2026, a +17.99% increase in the transfer-rules era`
- Baseline comparison: `The sample-wide gap averaged 0.388, with 2026 well above that level and 2012 marking the narrowest point at 0.335`
- Bracket implication: `Current schedules should be interpreted in a modern conference environment; older tournament seasons are a weak baseline if they ignore the 2012 structural shift and the 2022 transfer-rules break`
- Era breakpoints to reuse later:
  - Era 1: `2008-2011`
  - Era 2: `2012-2021`
  - Era 3 if needed: `2022-2025 for completed-tournament analyses; 2022-2026 for conference-context visuals`

## Narrative Options
- Option A: The conference environment materially changed, so SOS must be interpreted within eras instead of across one pooled history.
- Option B: The power structure stayed mostly intact, but a few conference swaps changed where schedule strength concentrates.
- Lock chosen angle after results: `Option A`

## Bridge to Next Brief
- Transition sentence: `If the conference environment changed this much, the next question is whether stronger schedules actually track deeper March runs at all.`

## Chart Usage Note
- Use the conference-strength chart if the visual shows a clear ranking shuffle, widening gap, or obvious breakpoint.
- If the chart is visually noisy, lead with the printed breakpoint summary in text and reserve the image for the blog.
- Twitter image title should state the insight, not the chart type.

## Blog Reuse Note
- This brief becomes the setup portion of the blog post, folded into the opening explanation of why raw SOS needs conference and era context.
- Do not let it become a standalone detour; its job in longform is to justify the later era split in `sos_06`.
