# TURNOVERS 90: Twitter Thread Compiler

## Summary
Compile the filled Turnovers analysis briefs into one deterministic Day 4 Twitter thread for the `12 Days of Madness` series. This compiler assumes the seven source briefs are already completed and that each one contains a final headline stat, baseline comparison, bracket implication, transition sentence, and chart recommendation.

## Source Briefs
- `docs/twitter_plans/turnovers/turnovers_01_context_over_time.plan.md`
- `docs/twitter_plans/turnovers/turnovers_02_offensive_turnovers_vs_finish_pooled.plan.md`
- `docs/twitter_plans/turnovers/turnovers_03_defensive_disruption_by_year.plan.md`
- `docs/twitter_plans/turnovers/turnovers_04_where_turnovers_matter_by_seed_band.plan.md`
- `docs/twitter_plans/turnovers/turnovers_05_disruption_archetypes_and_tempo.plan.md`
- `docs/twitter_plans/turnovers/turnovers_06_era_comparison.plan.md`
- `docs/twitter_plans/turnovers/turnovers_07_current_field_application.plan.md`

## Inputs to Read from Each Brief
- `Chosen headline stat`
- `Baseline comparison`
- `Bracket implication`
- `Transition sentence`
- `Primary figure filename`
- `Secondary support figure` if present
- `Chart usage note`
- `Primary metric rule`

## Locked After Run
- Thread-wide primary metric: `TOR`
- Best hook source based on the completed briefs: `turnovers_02` or `turnovers_05`
- Blocks and `Adj T.` are support layers unless the completed briefs prove they carry the strongest standalone surprise

## Thread Rules
- Thread length: `10` tweets total.
- Number every tweet as `1/10` through `10/10`.
- One idea per tweet.
- One stat or one comparison per tweet.
- Use line breaks for pacing.
- Use at most one chart per tweet.
- Cite source in-thread as `per Torvik` when referencing the metrics or charts.
- Keep the thread aligned to Day 4 `Turnovers` positioning from `docs/12_days_of_madness_plan.md`.

## Deterministic Tweet Map
### Tweet 1: Hook
- Pull candidate hook stats from:
  - `turnovers_02` chosen headline stat
  - `turnovers_04` chosen headline stat
  - `turnovers_05` chosen headline stat
  - `turnovers_07` chosen headline stat
- Tell readers in Tweet 1 to stick around for what the analysis says about the `2026` field.
- Rank candidates with this order:
  1. clearest bracket consequence
  2. biggest surprise relative to common turnover wisdom
  3. shortest wording path to a standalone hook
- Choose the top-ranked candidate and record it here after run.

### Tweet 2: Context
- Include:
  - historical tournament sample: `2008-2025`, excluding `2020`
  - tournament-team filter: seeded teams, drop `R68`
  - note that the current-field application uses a live `2026` projected field rather than completed tournament outcomes
  - one short turnover definition in plain language
  - why bracket players should care

### Tweets 3-9: One tweet per brief in order
- Tweet 3 uses `turnovers_01`
- Tweet 4 uses `turnovers_02`
- Tweet 5 uses `turnovers_03`
- Tweet 6 uses `turnovers_04`
- Tweet 7 uses `turnovers_05`
- Tweet 8 uses `turnovers_06`
- Tweet 9 uses `turnovers_07`
- Each of these tweets must include:
  - the brief's chosen headline stat
  - one baseline comparison or implication
  - the brief's transition sentence as the final line

### Tweet 10: Close
- Summarize the full story in three short clauses:
  - what turnovers say overall
  - where the signal matters most
  - who stands out this year
- End with one CTA only.

## Chart Selection Logic
- Attach charts only for:
  - clear pooled gradients
  - legible seed-band splits
  - readable archetype pages
  - readable current-field scatter pages
- Use the tempo-interaction chart only if the interaction is one of the four clearest visuals.
- If more than four briefs request charts, keep only the four clearest visuals and convert the rest to text-only tweets.

## Assembly Checklist
- Confirm `TOR` stays the primary language of the thread.
- Confirm `TORD`, `BLK%`, `BLKED%`, and `Adj T.` sharpen the story instead of replacing it.
- Confirm every chart uses intuitive labels and a visible title.
- Confirm every tweet ends with a forward transition except Tweet 10.
- Confirm the thread reads as one argument instead of seven disconnected factoids.
