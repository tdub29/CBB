# SOS 02: SOS vs Finish (All Years Pooled)

## Purpose in Sequence
Answer the core question of the project: across the full tournament sample, do teams with stronger schedules actually make deeper runs?

## Depends on
- Prior brief: `docs/twitter_plans/sos/sos_01_conf_strength_by_year.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/sos_vs_finish_pooled.png`
  - Printed pooled summary table by finish round
  - Pooled Spearman correlations between finish depth and both SOS metrics

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/sos_vs_finish_pooled.png`
- Support figure filename: `reports/figures/sos_vs_finish_pooled_unadjusted.png`
- Summary table reference: `reports/sos_vs_finish_pooled_table.csv`
- Correlation reference: `reports/sos_metric_selection.csv`
- Supporting notes:
  - `Champions averaged 31.95 Elite SOS vs 19.99 for R64 exits`
  - `Round medians still climb sharply from R64 through the deep-run rounds even with small-sample noise at Finals/Champ`

## Primary Metric Rule
- Compare pooled Spearman correlation between finish depth and `Elite SOS` vs `conf_avg_barthag`.
- Use the stronger metric only if it leads by at least `0.05`.
- Otherwise default to `Elite SOS`.
- Record the final choice here: `Elite SOS`
- Record the non-primary support stat here: `conf_avg_barthag rho = 0.424; Elite SOS rho = 0.452, but the 0.029 lead is below the 0.05 switch threshold`

## What to Extract
- Chosen headline stat: `After controlling for exact seed line, the within-seed Elite SOS relationship falls to rho 0.061`
- Baseline comparison: `The unadjusted pooled gradient is strong (+11.96 Elite SOS from R64 exits to champions), but most of that is seed structure`
- Bracket implication: `SOS should not be sold as a large standalone edge once seed is accounted for; it is better used as context than as the main pick driver`

## Narrative Options
- Option A: Stronger schedules clearly map to deeper runs, making SOS a real March sorting tool.
- Option B: The pooled relationship exists but is moderate, so SOS should be used as a tiebreaker rather than a standalone pick signal.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `The broad pooled gradient is real but mostly structural, so the next test is whether any useful SOS signal survives consistently year by year.`

## Chart Usage Note
- Use the seed-controlled pooled chart as the main narrative visual.
- Keep the unadjusted pooled chart as supporting material for the blog or internal review, not as the thread lead.
- The compiler may use this brief as a hook candidate if its headline stat is the strongest surprise across briefs `02`, `04`, and `07`.

## Blog Reuse Note
- This brief opens the evidence section of the blog and should be paired directly with `sos_03`.
- In longform, use this as the "headline answer" before introducing the year-to-year stability test.
