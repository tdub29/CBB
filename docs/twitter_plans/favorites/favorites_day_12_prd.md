# Day 12 PRD: Favorite Teams

## Goal
Build the Day 12 `Favorite Teams` package as the final synthesis day of the series. This should convert the prior eleven days of evidence into actual bracket opinions, not pretend to be another isolated historical-analysis topic.

## Core Question
Given the evidence from Days 1 through 11, which teams are the best title picks, Final Four bets, sleeper plays, and avoid candidates in the 2026 field?

## Source Rule
- Primary inputs are the completed Day 1 through Day 11 packages plus the current predictive model outputs.
- This day should not introduce a new standalone historical metric study unless a gap is obvious.
- Every opinion should trace back to at least one prior day signal or one model output.

## Synthesis Sequence
1. `favorites_01_field_filter`
   Purpose: narrow the field to credible title and Final Four candidates using the prior day rules.
2. `favorites_02_title_pick_case`
   Purpose: make the strongest fully-supported title case.
3. `favorites_03_final_four_shortlist`
   Purpose: separate strong semifinal paths from true title picks.
4. `favorites_04_sleeper_and_dark_horse_case`
   Purpose: identify teams with credible upside but lower public expectation.
5. `favorites_05_upset_dart_throws`
   Purpose: name bracket picks that are aggressive but still evidence-backed.
6. `favorites_06_avoid_and_fade_case`
   Purpose: identify popular teams that conflict with the prior day rules.
7. `favorites_07_bracket_card`
   Purpose: lock the final public card with the top picks, sleeper, and one or two clear fades.

## Required Future Outputs
- `reports/favorites_day_12_shortlist.md`
- `reports/favorites_day_12_bracket_card.md`
- `reports/favorites_day_12_support_table.csv`
- optional viewer or share image if you want a visual recap card

## Compiler Layer
- Thread compiler: `favorites_90_twitter_thread_compiler.plan.md`
- Blog compiler: `favorites_91_blog_post_compiler.plan.md`
- Hook default: the sharpest title-or-fade opinion that is fully supported by the prior days

## Pattern Fit
Low fit.

This should be treated as a synthesis package, not a normal seven-brief historical workflow. The sequence above mirrors the project pattern so execution stays organized, but the source material is the prior eleven days rather than a fresh one-topic sample build.
