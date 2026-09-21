# TURNOVERS 91: Blog Post Compiler

## Summary
Compile the filled Turnovers analysis briefs into one longform Day 4 blog post built on the same evidence base as the Twitter thread. The blog should flow through the findings rather than reading like seven isolated mini-sections, while still preserving the exact claims and evidence selected in the source briefs.

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
- `Chart usage note`
- `Blog reuse note`
- `Primary metric rule`

## Post Rules
- Output format: Markdown.
- Tone: analytical, bracket-facing, and explanatory rather than tweet-sized.
- Use the same lead claim as Twitter Tweet 1, but expand it enough to stand alone as a blog opening.
- Keep the body flowing; do not force seven hard section headers unless clarity requires them.
- Use `3-5` charts max.
- Every embedded chart should get:
  - one sentence caption
  - one sentence on why it matters
  - source attribution `per Torvik`

## Deterministic Narrative Flow
### Opening Hook
- Start with the same core claim selected for Twitter Tweet 1.
- Add one short paragraph explaining the sample:
  - completed tournament sample: `2008-2025`
  - exclude `2020`
  - seeded teams only
  - drop `R68`
- State separately that the current-field application uses a live `2026` snapshot rather than completed tournament outcomes.
- Introduce `TOR` immediately as the lead metric, then note that disruption and pace are support layers.

### Setup: Possession Environment
- Pull from `turnovers_01`.
- Explain how the turnover environment changed across eras.
- Keep this section short; it exists to frame the later modern-era interpretation.

### Evidence: Offense First, Defense Second
- Merge `turnovers_02` and `turnovers_03`.
- First answer whether ball security itself maps to deeper runs.
- Then qualify the story by testing whether defensive disruption and blocks reinforce it.
- End this section with the practical takeaway on what is stable versus noisy.

### Decision Section: Seed Bands and Archetypes
- Merge `turnovers_04` and `turnovers_05`.
- Explain where turnover edges matter most by seed band.
- Then show how archetypes, blocks, and tempo refine the read.
- Keep `Adj T.` as a support lens unless it clearly becomes the most useful surprise.

### Pivot: Era Comparison
- Pull from `turnovers_06`.
- Use the breakpoint-defined era comparison to explain what changed and whether modern tournaments reward turnover control differently.

### Application: Current Field
- Pull from `turnovers_07`.
- Identify `3-5` teams or team types that stand out through the completed Day 4 framework.
- Use historical percentile or seed-relative context rather than loose adjectives.

### Closing
- End with exactly three bracket-use bullets:
  - one rule about the overall value of ball security
  - one rule about where disruption matters most
  - one rule about how to read the current field
- Close with a one-line teaser for Day 5.

## Chart Selection Logic
- Prefer these visuals, in this order:
  1. `turnovers_vs_finish_pooled.png`
  2. `turnovers_vs_finish_by_seed_group.png`
  3. `turnovers_archetypes_vs_overperformance.png`
  4. `turnovers_2026_field.png`
  5. `turnovers_era_comparison.png`
- Use `turnovers_tempo_interaction.png` only if the interaction is clearer visually than in prose.
- Use the context-over-time chart only as setup if the opening needs it.

## Assembly Checklist
- Confirm `TOR` remains the lead metric throughout the post.
- Confirm `TORD`, blocks, and tempo stay support layers unless the completed briefs force a different story.
- Confirm all visual labels and chart titles are intuitive and legible.
- Confirm the current-field calls in the close are supported by the thresholds established earlier in the post.
- Confirm the blog can be written directly from the briefs without inventing new structure, new metrics, or new storyline decisions.
