# Day 6 PRD: Rebounding

## Goal
Build the Day 6 `Rebounding` package as the second-chance and possession-control day of the series. The final story should explain where rebounding produces real March value and which 2026 teams own the glass in bracket-useful ways.

## Core Question
Is rebounding mostly a broad quality signal, or does offensive versus defensive rebounding create distinct tournament edges by team type and seed band?

## Metric Rule
- Lead metric: `DRB` as the cleaner floor-setting signal
- Support metrics: `ORB` and offensive-rebounding-overperformance splits
- Keep offensive and defensive rebounding distinct until the archetype layer

## Standard Brief Sequence
1. `rebounding_01_context_over_time`
   Purpose: establish how the field rebounding environment moved over time.
2. `rebounding_02_defensive_rebounding_vs_finish_pooled`
   Purpose: test whether teams that close possessions make deeper runs.
3. `rebounding_03_offensive_rebounding_vs_overperformance`
   Purpose: isolate the second-chance signal and see whether it changes overperformance relative to seed.
4. `rebounding_04_where_rebounding_matters_by_seed_band`
   Purpose: localize the signal by seed range and team class.
5. `rebounding_05_glass_control_archetypes`
   Purpose: compare offensive-glass hunters, defensive-cleanup teams, and balanced profiles.
6. `rebounding_06_era_comparison`
   Purpose: test whether the modern era changed how much glass dominance matters.
7. `rebounding_07_current_field_application`
   Purpose: name the 2026 teams that create or surrender rebounding leverage.

## Required Future Outputs
- `scripts/build_rebounding_sample.py`
- `scripts/rebounding_analysis_01_context.py`
- `scripts/rebounding_analysis_02_04.py`
- `scripts/rebounding_analysis_05_07.py`
- `reports/figures/rebounding_*.png`
- `reports/rebounding_*_summary.md`
- `reports/rebounding_*_table.csv`

## Compiler Layer
- Thread compiler: `rebounding_90_twitter_thread_compiler.plan.md`
- Blog compiler: `rebounding_91_blog_post_compiler.plan.md`
- Hook default: whichever of `02`, `03`, or `07` most cleanly answers how to use the glass in a bracket

## Pattern Fit
Medium-high fit.

The main risk is collapsing offensive and defensive rebounding into a vague "good on the glass" story. Keep the split clear until the combined-archetype brief.
