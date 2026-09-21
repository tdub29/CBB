# SIZE 01: Size Context Over Time

## Purpose in Sequence
Use field-level size trends to establish whether the tournament environment itself changed over time, then lock the `2022` transfer-rules breakpoint for later interpretation.

## Depends on
- Dedicated size workflow to implement:
  - `scripts/build_size_sample.py`
  - `scripts/size_analysis_01_context.py`
  - `scripts/size_analysis_02_04.py`
  - `scripts/size_analysis_05_07.py`
- Source data build: `data/processed/size_sample.csv`
- Script outputs required before this brief is filled:
  - `reports/figures/size_context_over_time.png`
  - `reports/size_context_over_time_table.csv`
  - `reports/size_context_over_time_summary.md`

## Inputs from Analysis Outputs
- Primary figure filename: `reports/figures/size_context_over_time.png`
- Summary table reference: `reports/size_context_over_time_table.csv`
- Breakpoint reference: `2022 transfer-rules era start`
- Supporting notes to fill after run:
  - `Full-field EFFHGT rose from 79.71 in 2008 to 80.14 in 2025 (+0.43).`

## Primary Metric Rule
- Day 2 uses `EFFHGT` only.
- This brief should stay on the same single metric as the rest of the Size post.
- Record the final choice here: `EFFHGT`

## What to Extract
- Chosen headline stat: `Field-level EFFHGT climbed +0.43 from 2008 to 2025.`
- Baseline comparison: `2025 sat +0.43 above the 2008 starting point in field-average EFFHGT.`
- Bracket implication: `Size should be interpreted inside the modern field environment rather than treated as a static tournament truth across the full sample.`

## Narrative Options
- Option A: The field got effectively bigger/longer, so old size baselines understate what counts as a real physical edge now.
- Option B: Raw team size moved gradually upward over time, which matters as context before asking whether size actually changes tournament outcomes.
- Lock chosen angle after results: `Option A`

## Bridge to Next Brief
- Transition sentence: `Once the size environment is set, the next question is whether bigger teams actually make deeper March runs at all.`

## Chart Usage Note
- Use one over-time visual only.
- Include the `2022` transfer-rules breakpoint.
- Remove gridlines unless the chart becomes unreadable without them.
- Keep the chart field-level only; do not add conference or power-conference framing to Size Day.

## Blog Reuse Note
- This brief becomes the setup portion of the Day 2 post.
- Its job is to justify modern qualifiers later, not to become a standalone detour.
