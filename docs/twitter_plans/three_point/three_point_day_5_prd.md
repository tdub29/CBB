# Day 5 PRD: Three-Point Shooting

## Goal
Build the Day 5 `3PT Shooting` package as the variance day of the series. The final story should explain when three-point shooting is a real March edge, when it is just noise, and which 2026 teams gain or lose bracket value from their shot profile.

## Core Question
Once seed is respected, do three-point volume and three-point shooting success drive March overperformance, and does three-point defense help in the same way?

## Metric Rule
- Lead offense metric: `3P%`
- Co-headline offense support: `3PRATE`
- Defensive companion metric: `3P%D`
- Do not force a single-metric thesis if the evidence clearly points to a combined profile

## Standard Brief Sequence
1. `three_point_01_context_over_time`
   Purpose: show how tournament-team three-point environment and volume changed over time.
2. `three_point_02_three_point_metrics_vs_finish_pooled`
   Purpose: test the broad seed-controlled relationship between `3PRATE`, `3P%`, and `3P%D`.
3. `three_point_03_three_point_profile_stability_by_year`
   Purpose: measure whether volume, success, and defense are stable year to year or highly noisy.
4. `three_point_04_where_three_point_shooting_matters_by_seed_band`
   Purpose: find the seed bands where each three-point edge helps most.
5. `three_point_05_rate_success_interaction`
   Purpose: test the interaction between `3PRATE` and `3P%`, while keeping `3P%D` as support context.
6. `three_point_06_era_comparison`
   Purpose: re-run the main conclusions in the modern era and test whether the three-point era changed the use case.
7. `three_point_07_current_field_application`
   Purpose: identify 2026 teams that are live by profile, fragile by profile, or both.

## Required Future Outputs
- `scripts/build_three_point_sample.py`
- `scripts/three_point_analysis_01_context.py`
- `scripts/three_point_analysis_02_04.py`
- `scripts/three_point_analysis_05_07.py`
- `reports/figures/three_point_*.png`
- `reports/three_point_*_summary.md`
- `reports/three_point_*_table.csv`

## Compiler Layer
- Thread compiler: `three_point_90_twitter_thread_compiler.plan.md`
- Blog compiler: `three_point_91_blog_post_compiler.plan.md`
- Hook default: the strongest tension between variance and actual bracket usefulness

## Pattern Fit
High fit with one adjustment.

This day should still use the normal package shape, but it is not a clean single-metric day. If one chart forces the story into `3P%` alone, it is probably too narrow. The interaction between `3PRATE` and `3P%` should carry the synthesis step, with `3P%D` tested in parallel as the defensive companion.
