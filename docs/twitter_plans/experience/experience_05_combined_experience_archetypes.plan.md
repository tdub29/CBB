# EXPERIENCE 05: Combined Player + Coach Experience Archetypes

## Purpose in Sequence
Test the full Day 3 thesis: if March experience matters, is it strongest when player and coach experience line up together?

## Depends on
- Prior brief: `docs/twitter_plans/experience/experience_04_coach_experience_vs_march.plan.md`
- Script outputs required before this brief is filled:
  - `reports/figures/combined_experience_archetypes.png`
  - `reports/combined_experience_archetypes_table.csv`
  - `reports/combined_experience_archetypes_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/combined_experience_archetypes.png`
- Summary table reference: `reports/combined_experience_archetypes_table.csv`
- Supporting notes:
  - `Above both seed baselines: +0.319`
  - `Above coach experience only: +0.305`
  - `Above player experience only: -0.162`
  - `Below both seed baselines: -0.232`
  - `Best archetype average coach_tourn_win_pct_5y: 0.479`

## Primary Metric Rule
- Archetype assignment uses:
  - `exp_raw` vs its exact-seed baseline
  - `coach_tourn_games_5y` vs its exact-seed baseline
- `coach_tourn_win_pct_5y` does not assign archetypes; it only sharpens interpretation after the archetypes are defined.

## What to Extract
- Chosen headline stat: `Teams above both player and coach experience baselines averaged +0.319 overperformance, while teams above player experience only averaged -0.162.`
- Baseline comparison: `The “above coach only” group was also strong at +0.305, while the “below both” group fell to -0.232.`
- Bracket implication: `Player experience is only useful when the coach side is there too; coach-side experience is doing far more of the real work.`

## Narrative Options
- Option A: Experience works only when player and coach experience line up together.
- Option B: The coach side is doing most of the heavy lifting, and player experience alone is weak.
- Lock chosen angle after results: `Option B`

## Bridge to Next Brief
- Transition sentence: `The combined archetypes point strongly toward coaching, so the next step is checking whether that story stays stable year to year.`

## Chart Usage Note
- Use a single horizontal lollipop comparison.
- Keep the zero line prominent.
- This is the best hook candidate because it compresses the full Day 3 story into one page.

## Blog Reuse Note
- This brief is the synthesis section of the blog.
- It should serve as the clearest answer to the original Day 3 question.
