# SOS 05: Overperformance Relative to Seed and Conference Context

## Purpose in Sequence
Turn SOS into a concrete bracket lens by connecting it to overperformance versus seed expectation and by identifying whether some conferences consistently beat or miss those expectations.

## Depends on
- Prior brief: `docs/twitter_plans/sos/sos_04_seed_groups_advancers.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/sos_vs_overperformance.png`
  - `reports/figures/sos_conf_vs_overperformance.png`
  - Printed correlation stats for over/underperformance
  - Printed conference over/underperformance table

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/sos_conf_vs_overperformance.png`
- Support figure filename: `reports/figures/sos_vs_overperformance.png`
- Summary table reference: `reports/sos_vs_overperformance_table.csv`
- Conference table reference: `reports/sos_conf_vs_overperformance_table.csv`
- Supporting notes:
  - `Raw Elite SOS vs exact-seed overperformance was essentially flat at rho -0.020; seed-adjusted Elite SOS was only +0.061`
  - `Best conference relative to exact-seed expectation: Horz at +0.420`
  - `Worst conference relative to exact-seed expectation: MWC at -0.342`

## Primary Metric Rule
- Use the same thread-wide metric chosen in `sos_02`.
- If conference context tells the story better than the metric itself, keep the lead tweet centered on the overperformance relationship and use conference results as the payoff, not as a metric switch.
- Record the metric used here: `Elite SOS`

## What to Extract
- Chosen headline stat: `Raw Elite SOS barely explains exact-seed overperformance at rho -0.020, while conference-level overperformance ranges from +0.420 to -0.342`
- Baseline comparison: `Horz outperformed seed expectation by 0.762 rounds more than MWC across the sample`
- Bracket implication: `Once seed is accounted for, team SOS is mostly a context variable; conference environment is the sharper tiebreaker when similar seeds come from different leagues`

## Narrative Options
- Option A: Stronger schedules help identify teams seeded too low for their actual March readiness.
- Option B: Conference-level context matters as much as team-level SOS, especially when similar seeds come from very different league environments.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `These seed-adjusted patterns are useful, but the next question is whether they changed across eras once the conference landscape shifted.`

## Chart Usage Note
- For Twitter, prefer whichever visual produces one clean takeaway:
  - use the scatter if the relationship is strong and legible;
  - use the conference comparison if one league clearly over- or underperforms.
- Current recommendation: use `reports/figures/sos_conf_vs_overperformance.png` for Twitter and keep the team-level scatter as support.
- For the blog, both visuals can be used in the same merged decision section if they support the same argument.

## Blog Reuse Note
- This brief should be merged directly after `sos_04`.
- The longform version should move from "where SOS matters" to "how to exploit it against seeding assumptions."
