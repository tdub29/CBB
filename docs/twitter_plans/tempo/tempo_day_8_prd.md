# Day 8 PRD: Tempo

## Goal
Build the Day 8 `Tempo` package as the style-control day of the series. The final story should explain whether pace itself matters, whether certain tempo archetypes matter more, and which 2026 teams can impose the game they want.

## Core Question
Does tempo change tournament outcomes on its own, or is pace mostly a descriptive style layer that only matters when tied to other strengths?

## Metric Rule
- Lead metric: `Adj T.`
- Support framing: possession control, style clashes, and pace-archetype overlays
- Avoid treating raw fast or slow labels as automatic quality signals

## Standard Brief Sequence
1. `tempo_01_context_over_time`
   Purpose: establish how the tempo environment changed across the tournament field.
2. `tempo_02_tempo_vs_finish_pooled`
   Purpose: test whether faster or slower teams actually advance more.
3. `tempo_03_tempo_signal_by_year`
   Purpose: measure whether the tempo effect is stable or mostly noise.
4. `tempo_04_where_tempo_matters_by_seed_band`
   Purpose: locate the seed bands where pace helps or hurts the most.
5. `tempo_05_style_control_archetypes`
   Purpose: compare slow-control teams, fast-pressure teams, and empty-tempo profiles.
6. `tempo_06_era_comparison`
   Purpose: test whether the modern era changed the pace story.
7. `tempo_07_current_field_application`
   Purpose: identify the 2026 teams most likely to create style mismatches.

## Required Future Outputs
- `scripts/build_tempo_sample.py`
- `scripts/tempo_analysis_01_context.py`
- `scripts/tempo_analysis_02_04.py`
- `scripts/tempo_analysis_05_07.py`
- `reports/figures/tempo_*.png`
- `reports/tempo_*_summary.md`
- `reports/tempo_*_table.csv`

## Compiler Layer
- Thread compiler: `tempo_90_twitter_thread_compiler.plan.md`
- Blog compiler: `tempo_91_blog_post_compiler.plan.md`
- Hook default: the clearest mismatch between common pace wisdom and actual bracket signal

## Pattern Fit
High fit.

The caution is interpretive, not structural. Pace has to be tested before it is narrated as an edge.
