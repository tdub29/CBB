# EXPERIENCE 90: Twitter Thread Compiler

## Summary
Compile the filled Experience briefs into one deterministic Day 3 thread for the `12 Days of Madness` series. This compiler assumes the seven source briefs are already completed and that each one contains a final headline stat, baseline comparison, bracket implication, transition sentence, and chart recommendation.

## Source Briefs
- `docs/twitter_plans/experience/experience_01_context_over_time.plan.md`
- `docs/twitter_plans/experience/experience_02_player_exp_vs_finish_pooled.plan.md`
- `docs/twitter_plans/experience/experience_03_player_exp_late_rounds.plan.md`
- `docs/twitter_plans/experience/experience_04_coach_experience_vs_march.plan.md`
- `docs/twitter_plans/experience/experience_05_combined_experience_archetypes.plan.md`
- `docs/twitter_plans/experience/experience_06_experience_by_year.plan.md`
- `docs/twitter_plans/experience/experience_07_current_field_application.plan.md`

## Inputs to Read from Each Brief
- `Chosen headline stat`
- `Baseline comparison`
- `Bracket implication`
- `Transition sentence`
- `Primary figure filename`
- `Chart usage note`

## Locked After Run
- Player metric: `exp_raw`
- Player support metric: `exp_rank_season`
- Lead coach metric: `coach_tourn_games_5y`
- Best hook source based on the completed briefs: `experience_05`
- Reason: the cleanest surprise is that player experience alone is weak, while coach-side experience and the combined archetypes are strongly positive.

## Thread Rules
- Thread length: `10` tweets total.
- Number every tweet as `1/10` through `10/10`.
- One idea per tweet.
- One stat or one comparison per tweet.
- Use line breaks for pacing.
- Use at most one chart per tweet.
- Cite source in-thread as `per Torvik` when referencing the metrics or charts.
- Keep the thread aligned to Day 3 `Experience` positioning from `docs/12_days_of_madness_plan.md`.

## Deterministic Tweet Map
### Tweet 1: Hook
- Pull candidate hook stats from:
  - `experience_04` chosen headline stat
  - `experience_05` chosen headline stat
  - `experience_07` chosen headline stat
- Choose the top-ranked candidate in this order:
  1. clearest bracket consequence
  2. biggest surprise relative to the “veteran teams win in March” assumption
  3. shortest wording path to a standalone opener
- Final hook source: `experience_05`

### Tweet 2: Context
- Include:
  - historical tournament sample: `2008-2025`, excluding `2020`
  - seeded teams only, dropping `R68`
  - one short metric-definition line for `Exp.`
  - one short line explaining that coaching is a co-headline, not a separate add-on

### Tweets 3-9: One tweet per brief in order
- Tweet 3 uses `experience_01`
- Tweet 4 uses `experience_02`
- Tweet 5 uses `experience_03`
- Tweet 6 uses `experience_04`
- Tweet 7 uses `experience_05`
- Tweet 8 uses `experience_06`
- Tweet 9 uses `experience_07`
- Each tweet must include:
  - the brief’s headline stat
  - one baseline comparison or implication
  - the brief’s transition sentence as the final line

### Tweet 10: Close
- Summarize the full story in three short clauses:
  - player experience alone is weak
  - coach-side experience carries the March signal
  - the 2026 field only becomes useful once both layers are read together
- End with one CTA only:
  - default CTA: `Bookmark this before you fill out your bracket.`

## Chart Selection Logic
- Default chart priority:
  1. `combined_experience_archetypes.png`
  2. `coach_experience_vs_march.png`
  3. `experience_2026_field.png`
  4. `player_exp_vs_finish_pooled.png`
- Use `experience_context_over_time.png` and `experience_by_year.png` only if the text needs extra support.
- Keep `player_exp_late_rounds.png` if the thread needs the direct falsification of the player-experience hypothesis.

## Assembly Checklist
- Keep the player vs coach split clear from Tweet 2 onward.
- Do not let `Exp rank` become the main plot axis in the thread; it is support context.
- Make the coach-side March reps the main coaching frame, with win pct used as support.
- Ensure the thread reads as one argument: player experience weak -> coach experience stronger -> combined signal most useful -> 2026 application.
