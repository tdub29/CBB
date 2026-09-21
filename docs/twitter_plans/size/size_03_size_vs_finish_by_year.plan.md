# SIZE 03: Size vs Finish by Year

## Purpose in Sequence
Stress-test the pooled size result by checking whether the relationship held across tournaments or only in isolated years.

## Depends on
- Prior brief: `docs/twitter_plans/size/size_02_size_vs_finish_pooled.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/size_vs_finish_by_year.png`
  - `reports/size_vs_finish_by_year_table.csv`
  - `reports/size_vs_finish_by_year_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/size_vs_finish_by_year.png`
- Summary table reference: `reports/size_vs_finish_by_year_table.csv`
- Supporting notes to fill after run:
  - `Positive years: 16 of 17`
  - `Strongest year: 2017 at +0.541`
  - `Weakest year: 2021 at -0.052`
  - `Recent-window note: 2022-2025 averaged +0.251 vs +0.241 for the full sample`

## Primary Metric Rule
- Use the same thread-wide primary metric chosen in `size_02`.
- Record the metric used here: `EFFHGT`

## What to Extract
- Chosen headline stat: `EFFHGT pointed in the right direction in 16 of 17 completed tournaments.`
- Baseline comparison: `The recent average (+0.251) is only modestly above the full-sample average (+0.241), so the signal is stable but not dramatically stronger now.`
- Bracket implication: `If the direction stayed stable over time, size remains a valid contextual input; if not, keep it as a weak tiebreaker only.`

## Narrative Options
- Option A: The size signal stayed directionally stable, so the pooled result is not just noise.
- Option B: The size signal is uneven, which means size is a conditional March variable rather than a tournament law.
- Lock chosen angle after results: `Option A`

## Bridge to Next Brief
- Transition sentence: `If size is not equally useful in every season, the next step is to test where it actually matters by seed band.`

## Chart Usage Note
- Include the `2022` transfer-rules breakpoint on any over-time panel.
- Keep the layout simple: one line chart plus one supporting panel at most.
- If one panel tells the story cleanly, prefer a single-chart page.

## Blog Reuse Note
- Merge this brief with `size_02` in the blog evidence section.
- Use it to qualify the pooled result, not to restart the story.
