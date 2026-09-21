# Day 11 PRD: Seeds and Upsets

## Goal
Build the Day 11 `Seeds and Upsets` package as the bracket-chaos day of the series. The final story should quantify baseline seed expectations, identify the profiles that beat those expectations, and apply that logic to the 2026 bracket board.

## Core Question
Which seed lines are most vulnerable, which profiles actually outperform seed expectation, and how should that reshape upset picks?

## Metric Rule
- Lead lens: overperformance relative to seed expectation
- Support metrics: `WAB`, `Barthag` or model-strength proxy, `SOS`, and any validated upset-profile features
- This is a profile day, not a single-stat day

## Standard Brief Sequence
1. `seeds_01_seed_baseline_context`
   Purpose: establish the historical advancement rates by seed line and seed band.
2. `seeds_02_upset_rates_pooled`
   Purpose: show where upset rates are actually concentrated.
3. `seeds_03_upset_rates_by_year`
   Purpose: measure whether the upset environment changed over time.
4. `seeds_04_where_seed_expectation_breaks`
   Purpose: isolate the seed matchups and bands where baseline expectation is least stable.
5. `seeds_05_upset_profile_archetypes`
   Purpose: compare underseeded-strong-profile teams, overseeded-fragile-profile teams, and empty upset candidates.
6. `seeds_06_era_comparison`
   Purpose: test whether the portal and NIL era changed upset behavior.
7. `seeds_07_current_field_application`
   Purpose: name the 2026 seed lines and teams that most resemble historical overperformers and underperformers.

## Required Future Outputs
- `scripts/build_seed_profile_sample.py`
- `scripts/seeds_analysis_01_context.py`
- `scripts/seeds_analysis_02_04.py`
- `scripts/seeds_analysis_05_07.py`
- `reports/figures/seeds_*.png`
- `reports/seeds_*_summary.md`
- `reports/seeds_*_table.csv`

## Compiler Layer
- Thread compiler: `seeds_90_twitter_thread_compiler.plan.md`
- Blog compiler: `seeds_91_blog_post_compiler.plan.md`
- Hook default: the most surprising seed-line mismatch between public intuition and historical results

## Pattern Fit
Medium-low fit.

This day still works with the seven-brief package, but it should be treated as a seed-baseline plus profile-deviation study. Forcing it into a one-metric story will weaken it.
