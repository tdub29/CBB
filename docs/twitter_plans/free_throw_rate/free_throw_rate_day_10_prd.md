# Day 10 PRD: Free Throw Rate

## Goal
Build the Day 10 `Free Throw Rate` package as the whistle-and-close-game day of the series. The final story should explain whether getting to the line and converting at the line create a real March edge and which 2026 teams carry that profile.

## Core Question
Is free throw rate a meaningful tournament edge by itself, or does it only become useful when paired with late-game conversion and defensive discipline?

## Metric Rule
- Lead metric: `FTR`
- Support metrics: `FT%` and defensive foul-line prevention when available
- Do not merge free throw creation and free throw conversion until the archetype brief

## Standard Brief Sequence
1. `free_throw_rate_01_context_over_time`
   Purpose: show how the field's free throw environment changed over time.
2. `free_throw_rate_02_ftr_vs_finish_pooled`
   Purpose: test the broad relationship between getting to the line and tournament finish.
3. `free_throw_rate_03_conversion_vs_finish_by_year`
   Purpose: measure whether free throw shooting itself changes the outcome story.
4. `free_throw_rate_04_where_free_throws_matter_by_seed_band`
   Purpose: identify where whistle pressure matters most by seed group.
5. `free_throw_rate_05_free_throw_archetypes`
   Purpose: compare line-generators, elite conversion teams, and two-way foul-line profiles.
6. `free_throw_rate_06_era_comparison`
   Purpose: test whether modern tournaments changed the whistle edge.
7. `free_throw_rate_07_current_field_application`
   Purpose: identify the 2026 teams that benefit most or least from the free throw layer.

## Required Future Outputs
- `scripts/build_free_throw_rate_sample.py`
- `scripts/free_throw_rate_analysis_01_context.py`
- `scripts/free_throw_rate_analysis_02_04.py`
- `scripts/free_throw_rate_analysis_05_07.py`
- `reports/figures/free_throw_rate_*.png`
- `reports/free_throw_rate_*_summary.md`
- `reports/free_throw_rate_*_table.csv`

## Compiler Layer
- Thread compiler: `free_throw_rate_90_twitter_thread_compiler.plan.md`
- Blog compiler: `free_throw_rate_91_blog_post_compiler.plan.md`
- Hook default: the strongest result tying line pressure to actual bracket utility

## Pattern Fit
Medium fit.

This is a normal package structurally, but the story should not pretend that `FTR` and `FT%` are interchangeable. Treat them as separate until Brief 5.
