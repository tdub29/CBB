# Day 7 PRD: Coaching Experience

## Goal
Build the Day 7 `Coaching Experience` package as the sideline-resume day of the series. The final story should separate raw tenure from actual March experience, then apply the resulting rules to the 2026 field.

## Core Question
Which coach-side signals actually matter in March: tenure, recent tournament reps, recent tournament wins, or the combination of those into stable program-level experience?

## Metric Rule
- Lead metric: `coach_tourn_games_5y`
- Support metrics: `coach_tourn_win_pct_5y`, years at school, and overall NCAA tournament history where available
- Keep this day distinct from Day 3 by focusing on the coach side only

## Standard Brief Sequence
1. `coaching_01_context_over_time`
   Purpose: show how experienced the tournament field has been on the sideline over time.
2. `coaching_02_tenure_vs_finish_pooled`
   Purpose: test whether simple time at school matters on its own.
3. `coaching_03_recent_march_reps_vs_finish`
   Purpose: isolate recent tournament exposure as a cleaner experience measure.
4. `coaching_04_where_coaching_experience_matters_by_seed_band`
   Purpose: find where coach-side edges are most actionable.
5. `coaching_05_coach_resume_archetypes`
   Purpose: compare veteran winners, veteran non-winners, rising coaches, and true first-timers.
6. `coaching_06_era_comparison`
   Purpose: test whether portal-era roster churn made coach continuity more or less useful.
7. `coaching_07_current_field_application`
   Purpose: map the 2026 field into coach-resume buckets and highlight the biggest edges.

## Required Future Outputs
- `scripts/build_coaching_sample.py`
- `scripts/coaching_analysis_01_context.py`
- `scripts/coaching_analysis_02_04.py`
- `scripts/coaching_analysis_05_07.py`
- `reports/figures/coaching_*.png`
- `reports/coaching_*_summary.md`
- `reports/coaching_*_table.csv`

## Data Notes
- This day depends on external coach-history data rather than Torvik alone.
- Reuse the existing team-history and coach-experience work where possible instead of starting from scratch.
- Make sure the Day 3 experience docs remain the player-plus-coach interaction day, not the replacement for this topic.

## Compiler Layer
- Thread compiler: `coaching_90_twitter_thread_compiler.plan.md`
- Blog compiler: `coaching_91_blog_post_compiler.plan.md`
- Hook default: recent March reps or the cleanest current-field coach mismatch

## Pattern Fit
Medium fit.

This is a standard package structurally, but the data source is less native than the other days and the overlap with Day 3 needs to stay controlled.
