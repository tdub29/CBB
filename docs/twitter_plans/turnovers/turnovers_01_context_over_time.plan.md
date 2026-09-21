# TURNOVERS 01: Possession Pressure Context Over Time

## Purpose in Sequence
Establish how the tournament field's possession-pressure environment changed over time before asking whether turnover edges actually translate into March wins.

## Depends on
- Dedicated turnovers workflow to implement:
  - `scripts/build_turnovers_sample.py`
  - `scripts/turnovers_analysis_01_context.py`
  - `scripts/turnovers_analysis_02_04.py`
  - `scripts/turnovers_analysis_05_07.py`
- Source data build: `data/processed/turnovers_sample.csv`
- Script outputs required before this brief is filled:
  - `reports/figures/turnovers_context_over_time.png`
  - `reports/turnovers_context_over_time_table.csv`
  - `reports/turnovers_context_over_time_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/turnovers_context_over_time.png`
- Summary table reference: `reports/turnovers_context_over_time_table.csv`
- Breakpoint reference to lock after results: `2022 transfer-rules era start`
- Supporting notes to fill after run:
  - `Field-average TOR trend`
  - `Field-average TORD trend`
  - `Block-pressure context using BLK% and BLKED%`

## Primary Metric Rule
- Day 4 leads with `TOR`.
- `TORD`, `BLK%`, and `BLKED%` are context metrics here, not replacement headline metrics.
- Record the final choice here: `TOR`

## What to Extract
- Chosen headline stat: `Field-level possession pressure changed enough over time that old turnover baselines should not be treated as static March truths.`
- Baseline comparison: `Modern tournament teams should be compared against a modern turnover environment, not the early-sample baseline.`
- Bracket implication: `Any turnover or disruption edge later in the day has to be interpreted against the current field environment.`

## Narrative Options
- Option A: The field has become cleaner with the ball, so true turnover edges are harder to find.
- Option B: The field environment moved, but disruption still exists in different forms through pressure and rim denial.
- Lock chosen angle after results: `Option A or B after run`

## Bridge to Next Brief
- Transition sentence: `Once the tournament possession environment is set, the next question is whether simply taking care of the ball actually buys deeper March runs.`

## Chart Usage Note
- Use one over-time context visual only.
- If multiple metrics appear, keep `TOR` visually primary and treat `TORD` plus block metrics as supporting context.
- Titles and metric labels must be plain English on first glance.

## Blog Reuse Note
- This brief is setup only.
- Its job is to frame the later offense-versus-defense turnover story, not to become its own detour.
