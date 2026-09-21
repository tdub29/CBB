# Day 9 PRD: Defense

## Goal
Build the Day 9 `Defense` package as the "defense travels" day of the series. The final story should test the truism directly, identify which defensive layers matter most, and apply the result to the 2026 field.

## Core Question
Does elite defense travel in March, and if so, which defensive metrics create the cleanest bracket signal?

## Metric Rule
- Lead metric: `AdjDE`
- Support metrics: shot-quality suppression proxies such as `2P%D`, `BLKED%`, and related components
- Use one lead metric across the package even if supporting charts sharpen the story

## Standard Brief Sequence
1. `defense_01_context_over_time`
   Purpose: show how the tournament field's defensive environment changed over time.
2. `defense_02_adjde_vs_finish_pooled`
   Purpose: test the broad historical relationship between defense and tournament finish.
3. `defense_03_defense_signal_by_year`
   Purpose: check year-to-year stability and whether the broad signal holds up.
4. `defense_04_where_defense_matters_by_seed_band`
   Purpose: localize the defensive edge by seed band.
5. `defense_05_defensive_archetypes`
   Purpose: compare elite-defense, rim-protection, and balanced profiles and measure whether "defense-only" teams are actually safe.
6. `defense_06_era_comparison`
   Purpose: re-test the defense thesis in the modern era.
7. `defense_07_current_field_application`
   Purpose: identify the 2026 defenses that deserve bracket trust and the ones that look overrated.

## Required Future Outputs
- `scripts/build_defense_sample.py`
- `scripts/defense_analysis_01_context.py`
- `scripts/defense_analysis_02_04.py`
- `scripts/defense_analysis_05_07.py`
- `reports/figures/defense_*.png`
- `reports/defense_*_summary.md`
- `reports/defense_*_table.csv`

## Compiler Layer
- Thread compiler: `defense_90_twitter_thread_compiler.plan.md`
- Blog compiler: `defense_91_blog_post_compiler.plan.md`
- Hook default: whichever of `02`, `05`, or `07` most directly answers whether defense actually travels

## Pattern Fit
Medium-high fit.

This fits the normal package well, but the lead-metric decision matters more than usual. Keep `AdjDE` as the anchor so the day does not fragment into too many component stats.
