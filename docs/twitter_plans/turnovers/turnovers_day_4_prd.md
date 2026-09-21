# Day 4 PRD: Turnovers

## Goal
Build the Day 4 `Turnovers` package as the pressure-and-possession day of the series. The final story should answer whether ball security and turnover creation actually matter in March, where they matter most, how blocks sharpen the disruption story, and which 2026 teams stand out.

## Core Question
Do offensive turnover avoidance and defensive turnover creation translate into deeper tournament runs once seed expectations are respected?

## Metric Rule
- Lead metric: offensive turnover rate proxy from Torvik `TOR`
- Support metric: defensive turnover force proxy `TORD`
- Secondary disruption support: `BLK%` and `BLKED%`
- Interaction support: `Adj T.` paired with turnover profile in the archetype stage
- Keep offense and defense separate until the archetype brief

## Standard Brief Sequence
1. `turnovers_01_context_over_time`
   Purpose: show how the field-level turnover environment changed over time and whether the modern game is cleaner or sloppier.
2. `turnovers_02_offensive_turnovers_vs_finish_pooled`
   Purpose: test whether teams that protect the ball make deeper runs in the pooled historical sample.
3. `turnovers_03_defensive_turnovers_vs_finish_by_year`
   Purpose: measure whether turnover-forcing translates consistently or is more matchup-dependent year to year, then check whether blocks sharpen the defensive disruption read.
4. `turnovers_04_where_turnovers_matter_by_seed_band`
   Purpose: localize the signal by seed group and identify where it is actually bracket-useful.
5. `turnovers_05_offense_defense_turnover_archetypes`
   Purpose: compare clean-offense teams, havoc defenses, balanced teams, and empty-calorie profiles while also testing the `Adj T.` interaction and the value of block-driven disruption.
6. `turnovers_06_era_comparison`
   Purpose: re-test the turnover story inside the modern era and compare it with the full sample.
7. `turnovers_07_current_field_application`
   Purpose: apply the historical rules to the projected 2026 field and name the best and worst turnover profiles.

## Required Future Outputs
- `scripts/build_turnovers_sample.py`
- `scripts/turnovers_analysis_01_context.py`
- `scripts/turnovers_analysis_02_04.py`
- `scripts/turnovers_analysis_05_07.py`
- `reports/figures/turnovers_*.png`
- `reports/turnovers_*_summary.md`
- `reports/turnovers_*_table.csv`

## Compiler Layer
- Thread compiler: `turnovers_90_twitter_thread_compiler.plan.md`
- Blog compiler: `turnovers_91_blog_post_compiler.plan.md`
- Hook default: whichever of `02`, `04`, or `07` gives the clearest bracket consequence

## Pattern Fit
High fit.

The only adjustment from the Day 1 to Day 3 pattern is that the offense-defense split has to stay explicit until Brief 5, and blocks plus tempo should stay as support layers instead of replacing the turnover-led thesis.
