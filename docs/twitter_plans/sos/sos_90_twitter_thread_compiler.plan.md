# SOS 90: Twitter Thread Compiler

## Summary
Compile the filled SOS analysis briefs into one deterministic Day 1 Twitter thread for the "12 Days of Madness" series. This file assumes the seven source briefs are already completed and that each one contains a final headline stat, baseline comparison, bracket implication, transition sentence, and chart recommendation.

## Source Briefs
- `docs/twitter_plans/sos/sos_01_conf_strength_by_year.plan.md`
- `docs/twitter_plans/sos/sos_02_sos_vs_finish_pooled.plan.md`
- `docs/twitter_plans/sos/sos_03_sos_vs_finish_by_year.plan.md`
- `docs/twitter_plans/sos/sos_04_seed_groups_advancers.plan.md`
- `docs/twitter_plans/sos/sos_05_overperformance_and_conferences.plan.md`
- `docs/twitter_plans/sos/sos_06_era_comparison.plan.md`
- `docs/twitter_plans/sos/sos_07_current_field_application.plan.md`

## Inputs to Read from Each Brief
- `Chosen headline stat`
- `Baseline comparison`
- `Bracket implication`
- `Transition sentence`
- `Primary figure filename`
- `Chart usage note`
- `primary_metric_for_story`

## Thread Rules
- Thread length: `10` tweets total.
- Number every tweet as `1/10` through `10/10`.
- One idea per tweet.
- One stat or one comparison per tweet.
- Use line breaks for pacing.
- Use at most one chart per tweet.
- Cite source in-thread as `per Torvik` when referencing the metrics or charts.
- Keep the thread aligned to Day 1 SOS positioning from `docs/12_days_of_madness_plan.md`.

## Deterministic Tweet Map
### Tweet 1: Hook
- Pull candidate hook stats from:
  - `sos_02` chosen headline stat
  - `sos_04` chosen headline stat
  - `sos_07` chosen headline stat
- Tell readers in Tweet 1 to stick around for what the analysis says about this year's field.
- Rank candidates with this order:
  1. largest surprise gap or percentile extreme
  2. clearest bracket consequence
  3. shortest wording path to a standalone hook
- Choose the top-ranked candidate and record it here: `[fill_after_compile: hook_source_brief]`
- Attach a chart only if the selected source brief marked its visual as Twitter-appropriate.

### Tweet 2: Context
- Include:
  - historical tournament sample: `2008-2025`, excluding `2020`
  - tournament-team filter: seeded teams, drop `R68`
  - note that the current-field application is a `2026` watch-list, not a completed-tournament sample
  - two SOS definitions in one short line
  - why bracket players should care
- Do not add a new stat here beyond the framing needed to define the sample and the metric choice.

### Tweets 3-9: One tweet per brief in order
- Tweet 3 uses `sos_01`
- Tweet 4 uses `sos_02`
- Tweet 5 uses `sos_03`
- Tweet 6 uses `sos_04`
- Tweet 7 uses `sos_05`
- Tweet 8 uses `sos_06`
- Tweet 9 uses `sos_07`
- Each of these tweets must include:
  - the brief's chosen headline stat
  - one baseline comparison or implication
  - the brief's transition sentence as the final line

### Tweet 10: Close
- Summarize the full story in three short clauses:
  - what SOS says overall
  - where it matters most
  - who stands out this year
- End with one CTA only:
  - default CTA: `Bookmark this before you fill out your bracket.`
  - alternate CTA if the current-field angle is stronger: `Which team surprised you most?`

## Chart Selection Logic
- Attach charts only for:
  - rankings over time
  - clear round-by-round gradients
  - legible seed-group splits
  - readable current-field leaderboards
- Do not attach images to tweets where the insight can be stated more clearly in text.
- If more than four briefs request charts, keep only the four clearest visuals and convert the rest to text-only tweets.

## Assembly Checklist
- Confirm the same `primary_metric_for_story` is used consistently from `sos_02` through `sos_07`.
- Confirm `sos_06` uses the era breakpoints defined in `sos_01`.
- Confirm every tweet ends with a forward transition except Tweet 10.
- Confirm the thread reads as one argument instead of seven disconnected factoids.
