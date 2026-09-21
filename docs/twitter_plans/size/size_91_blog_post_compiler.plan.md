# SIZE 91: Blog Post Compiler

## Summary
Compile the filled Size analysis briefs into one longform blog post built on the same evidence base as the Twitter thread. The blog should flow through the findings rather than reading like seven isolated mini-sections, while still preserving the exact claims and evidence selected in the source briefs.

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
- `Blog reuse note`
- `primary_metric_for_story`

## Locked After Run
- Lead claim to carry from the thread: size looks meaningful in the pooled sample, but very little survives after exact-seed control.
- Locked primary metric: `EFFHGT`
- Use the current field section to name specific seed-line outliers rather than to claim that size is a universal 2026 edge.

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
- State separately that the current-field application uses a live `2026` projected field rather than completed tournament outcomes.
- Introduce the two size measures and immediately state which one won the metric rule.

### Setup: Field and Conference Size Context
- Pull from `size_01`.
- Use the chosen headline stat and the `2022` breakpoint to explain why the size environment is not static across the sample.
- Keep this section short; its role is to frame the later era-adjusted interpretation.

### Evidence: Overall Signal and Year-to-Year Stability
- Merge `size_02` and `size_03`.
- First answer the pooled question: does size map to deeper runs?
- Then qualify it with the year-by-year stability result.
- End this section with the practical takeaway: whether size is strong, moderate, or conditional as a bracket input.

### Decision Section: Seed Bands and Archetypes
- Merge `size_04` and `size_05`.
- Explain where size helps most by seed band.
- Then add one short paragraph on the dominant-big-man threshold as a conditional support signal.
- Then show how that interacts with size archetypes relative to seed expectation.
- Use this merged section to produce the main `how to use this in your bracket` logic.

### Pivot: Era Comparison
- Pull from `size_06`.
- Use the Day 1 breakpoint-defined era comparison to explain what changed and whether modern tournaments reward size differently.
- This section should explicitly connect back to the setup, so the blog reads as a developed argument rather than a late add-on.

### Application: Current Field
- Pull from `size_07`.
- Identify `3-5` teams or team types that stand out through the completed size framework.
- Use the dominant-big-man support page only for a short follow-on paragraph on teams that clear the `3.0` threshold in the actionable seed bands.
- Use seed-line-relative size context rather than unsupported adjectives.

### Closing
- End with exactly three bracket-use bullets:
  - one rule about the overall value of size
  - one rule about where to weight it most by seed band or archetype
  - one rule about how to read the `2026` field
- Close with a one-line teaser for the next content day.

## Chart Selection Logic
- Prefer these visuals, in this order:
  1. `size_vs_finish_pooled.png`
  2. `size_vs_finish_by_seed_group.png`
  3. `size_big_man_signal.png`
  4. `size_archetypes_vs_overperformance.png`
  5. `size_2026_field.png`
- Use `size_vs_finish_by_year.png` only if the stability story is clearer visually than in prose.
- Use `size_2026_big_man.png` only if the named-team application section materially improves with the extra support page.
- Use `size_era_comparison.png` only if the era comparison materially changes the final bracket guidance.

## Assembly Checklist
- Confirm the same `primary_metric_for_story` appears throughout the post.
- Confirm all modern-era claims in the close are supported by the era comparison in `size_06`.
- Confirm the dominant-big-man paragraph stays subordinate to the main size argument.
- Confirm the current-field calls in the close are supported by the thresholds established earlier in the post.
- Confirm the blog can be written directly from the briefs without inventing new structure, new metrics, or new storyline decisions.
