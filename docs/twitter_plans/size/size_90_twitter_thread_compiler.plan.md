# SIZE 90: Twitter Thread Compiler

## Summary
Compile the filled Size analysis briefs into one deterministic Day 2 Twitter thread for the `12 Days of Madness` series. This file assumes the seven source briefs are already completed and that each one contains a final headline stat, baseline comparison, bracket implication, transition sentence, and chart recommendation.

## Source Briefs
- `docs/twitter_plans/size/size_01_context_over_time.plan.md`
- `docs/twitter_plans/size/size_02_size_vs_finish_pooled.plan.md`
- `docs/twitter_plans/size/size_03_size_vs_finish_by_year.plan.md`
- `docs/twitter_plans/size/size_04_where_size_matters_by_seed_band.plan.md`
- `docs/twitter_plans/size/size_05_archetypes_vs_overperformance.plan.md`
- `docs/twitter_plans/size/size_06_era_comparison.plan.md`
- `docs/twitter_plans/size/size_07_current_field_application.plan.md`

## Inputs to Read from Each Brief
- `Chosen headline stat`
- `Baseline comparison`
- `Bracket implication`
- `Transition sentence`
- `Primary figure filename`
- `Secondary support figure` if present
- `Chart usage note`
- `primary_metric_for_story`

## Locked After Run
- Thread-wide primary metric: `EFFHGT`
- Best hook source based on the completed briefs: `size_02`
- Reason: the strongest surprise is that the raw size gradient looks real, but the exact-seed-controlled relationship falls to roughly zero (`rho = -0.008`).

## Thread Rules
- Thread length: `10` tweets total.
- Number every tweet as `1/10` through `10/10`.
- One idea per tweet.
- One stat or one comparison per tweet.
- Use line breaks for pacing.
- Use at most one chart per tweet.
- Cite source in-thread as `per Torvik` when referencing the metrics or charts.
- Keep the thread aligned to Day 2 `Size` positioning from `docs/12_days_of_madness_plan.md`.

## Deterministic Tweet Map
### Tweet 1: Hook
- Pull candidate hook stats from:
  - `size_02` chosen headline stat
  - `size_04` chosen headline stat
  - `size_05` chosen headline stat
  - `size_07` chosen headline stat
- Tell readers in Tweet 1 to stick around for what the analysis says about the `2026` field.
- Rank candidates with this order:
  1. clearest bracket consequence
  2. biggest surprise relative to common size assumptions
  3. shortest wording path to a standalone hook
- Choose the top-ranked candidate and record it here: `size_02`
- Attach a chart only if the selected source brief marked its visual as Twitter-appropriate.

### Tweet 2: Context
- Include:
  - historical tournament sample: `2008-2025`, excluding `2020`
  - tournament-team filter: seeded teams, drop `R68`
  - note that the current-field application uses a live `2026` projected field rather than completed tournament outcomes
  - one size definition in one short line (`EFFHGT`)
  - why bracket players should care
- Do not add a new stat here beyond the framing needed to define the sample and the metric choice.

### Tweets 3-9: One tweet per brief in order
- Tweet 3 uses `size_01`
- Tweet 4 uses `size_02`
- Tweet 5 uses `size_03`
- Tweet 6 uses `size_04`
- Tweet 7 uses `size_05`
- Tweet 8 uses `size_06`
- Tweet 9 uses `size_07`
- Each of these tweets must include:
  - the brief's chosen headline stat
  - one baseline comparison or implication
  - the brief's transition sentence as the final line
- For `size_05`, fold the dominant-big-man threshold and the archetype result into one decision tweet rather than splitting them apart.
- For `size_07`, use the main size chart first and only reference the dominant-big-man support chart if it sharpens the named-team callouts.

### Tweet 10: Close
- Summarize the full story in three short clauses:
  - what size says overall
  - where it matters most
  - who stands out this year
- End with one CTA only:
  - default CTA: `Bookmark this before you fill out your bracket.`
  - alternate CTA if the current-field angle is stronger: `Which team surprised you most?`

## Chart Selection Logic
- Attach charts only for:
  - clear over-time shifts
  - strong pooled gradient pages
  - legible seed-band splits
  - clean threshold support pages
  - readable archetype comparisons
  - readable current-field scatter pages
- Do not attach images to tweets where the insight is clearer in text.
- If more than four briefs request charts, keep only the four clearest visuals and convert the rest to text-only tweets.
- Default priority after the core four:
  1. `size_big_man_signal.png`
  2. `size_2026_big_man.png`

## Assembly Checklist
- Confirm the same `primary_metric_for_story` is used consistently from `size_02` through `size_07`.
- Confirm `size_06` uses the Day 1 era breaks.
- Confirm every over-time size visual referenced in-thread includes the `2022` breakpoint.
- Confirm the dominant-big-man support signal stays in `size_05`/`size_07` and does not overwrite the main Day 2 size thesis.
- Confirm every tweet ends with a forward transition except Tweet 10.
- Confirm the thread reads as one argument instead of seven disconnected factoids.
