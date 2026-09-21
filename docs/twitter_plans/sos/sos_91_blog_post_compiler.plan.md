# SOS 91: Blog Post Compiler

## Summary
Compile the filled SOS analysis briefs into one longform blog post built on the same evidence base as the Twitter thread. The blog should flow through the findings rather than reading like seven isolated mini-sections, while still preserving the exact claims and evidence selected in the source briefs.

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
- `Blog reuse note`
- `primary_metric_for_story`

## Post Rules
- Output format: Markdown.
- Tone: analytical, bracket-facing, and explanatory rather than tweet-sized.
- Use the same lead claim as Twitter Tweet 1, but expand it with enough context to stand alone as a blog opening.
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
- Introduce the two SOS measures and immediately state which one won the metric rule.

### Setup: Conference Environment and Era Context
- Pull from `sos_01`.
- Use the chosen headline stat and era breakpoint reference to explain why the schedule environment is not static across the sample.
- Keep this section short; its role is to frame the later era-adjusted interpretation.

### Evidence: Overall Signal and Year-to-Year Stability
- Merge `sos_02` and `sos_03`.
- First answer the pooled question: does stronger SOS map to deeper runs?
- Then qualify it with the year-by-year stability result.
- End this section with the practical takeaway: whether the signal is strong, moderate, or conditional.

### Decision Section: Seed Groups and Overperformance
- Merge `sos_04` and `sos_05`.
- Explain where SOS helps most by seed band.
- Then show how that interacts with overperformance versus seed expectation and conference context.
- Use this merged section to produce the main "how to use this in your bracket" logic.

### Pivot: Era Comparison
- Pull from `sos_06`.
- Use the breakpoint-defined era comparison to explain what changed and whether modern tournaments reward SOS differently.
- This section should explicitly connect back to the setup, so the blog reads as a developed argument rather than a late add-on.

### Application: Current Field
- Pull from `sos_07`.
- Identify `3-5` teams or team types that stand out through the completed SOS framework.
- Use historical percentile or rank context rather than unsupported adjectives.

### Closing
- End with exactly three bracket-use bullets:
  - one rule about the overall value of SOS
  - one rule about where to weight it most by seed or team type
  - one rule about how to read the current field
- Close with a one-line teaser for the next content day.

## Chart Selection Logic
- Prefer these visuals, in this order:
  1. `sos_vs_finish_pooled.png`
  2. `sos_vs_finish_by_seed_group.png`
  3. `sos_2026_field.png`
  4. `sos_conf_strength_over_time.png`
  5. `sos_era_comparison.png`
- Use `sos_vs_finish_by_year.png` only if the stability story is clearer visually than in prose.
- Use `sos_conf_vs_overperformance.png` only if one conference result is central to the argument.

## Assembly Checklist
- Confirm the same `primary_metric_for_story` appears throughout the post.
- Confirm all modern-era claims in the close are supported by the era comparison in `sos_06`.
- Confirm the current-field calls in the close are supported by the thresholds established earlier in the post.
- Confirm the blog can be written directly from the briefs without inventing new structure, new metrics, or new storyline decisions.
