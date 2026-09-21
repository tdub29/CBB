# Ralph Wiggum Loop Workflow

## Purpose
This repo now has a Codex-compatible Ralph Wiggum workflow for iterative loops on planning, analysis, and writing tasks.

It is an adaptation of the Claude Code `ralph-wiggum` plugin, not a verbatim install. The original plugin depends on Claude-specific slash commands and stop hooks that do not exist in this environment.

## Vetting Summary
SKILL VETTING REPORT
=======================================
Skill: ralph-wiggum plugin
Source: GitHub
Author: Daisy Hollman / anthropics
Version: 1.0.0
---------------------------------------
METRICS:
- Last reviewed: 2026-03-09
- Files reviewed: 6
---------------------------------------
RED FLAGS: None found

PERMISSIONS NEEDED:
- Files: `.claude/ralph-loop.local.md`
- Network: None at runtime
- Commands: local shell only, plus `jq`, `sed`, `awk`, `perl` in the Claude variant
---------------------------------------
RISK LEVEL: LOW

VERDICT: SAFE TO ADAPT
=======================================

## What The Original Plugin Does
- Starts a loop with the same prompt every iteration.
- Stores loop state in `.claude/ralph-loop.local.md`.
- Uses a stop hook to block exit and re-feed the same prompt.
- Stops only when a completion promise is emitted or a max-iteration cap is hit.

## What This Repo Version Supports
- Local loop state in `.claude/ralph-loop.local.md`
- PowerShell helpers to start, inspect, advance, and cancel the loop
- A documented manual protocol for future Codex turns

What it does not support:
- automatic stop-hook interception
- Claude slash commands
- transparent replay of prompts at runtime

## Local Scripts
- `scripts/start_ralph_loop.ps1`
- `scripts/show_ralph_loop.ps1`
- `scripts/advance_ralph_loop.ps1`
- `scripts/cancel_ralph_loop.ps1`

## Start A Loop
```powershell
powershell -ExecutionPolicy Bypass -File scripts/start_ralph_loop.ps1 `
  -Prompt "Create the Day 4 turnovers planning package" `
  -MaxIterations 6 `
  -CompletionPromise "DAY 4 PACKAGE COMPLETE"
```

## Check State
```powershell
powershell -ExecutionPolicy Bypass -File scripts/show_ralph_loop.ps1
```

## Advance State
```powershell
powershell -ExecutionPolicy Bypass -File scripts/advance_ralph_loop.ps1
```

Mark the loop complete:
```powershell
powershell -ExecutionPolicy Bypass -File scripts/advance_ralph_loop.ps1 -Complete
```

## Cancel A Loop
```powershell
powershell -ExecutionPolicy Bypass -File scripts/cancel_ralph_loop.ps1
```

## Operating Protocol For Future Turns
When you want a Ralph-style pass in this repo, use this pattern:

1. Start the loop with a single well-scoped prompt.
2. Tell Codex the max iterations and completion promise if you want hard stops.
3. In each turn, Codex should:
   - read `.claude/ralph-loop.local.md`
   - treat the stored prompt as the fixed task
   - make one substantive pass
   - update state with `scripts/advance_ralph_loop.ps1`
   - stop only when the promise is genuinely true or the cap is reached
4. Use `scripts/cancel_ralph_loop.ps1` if you want to abandon the loop.

## Human Validation Gates
Before locking a day package, Codex should use its own judgment first, then stop and ask for user validation in these cases:

1. Metric-set lock
   - Codex proposes the lead metric and support metrics for the day.
   - User validates whether the chosen metric set is the right one.

2. Research-question lock
   - Codex proposes the exact research question and story thesis for the day.
   - User validates whether the framing and general idea are correct.

3. Pattern-fit lock
   - Codex decides whether the day should follow the normal pattern:
     - context over time
     - pooled relationship
     - by-year or stability check
     - seed-band split
     - archetype or interaction layer
     - era comparison
     - current-field application
   - User validates whether that structure actually makes sense for the topic.

Operational rule:
- Codex should not ask first by default.
- Codex should make the best initial call, explain it briefly, and then request validation before the day is treated as locked.
- If a day is clearly low-fit for the standard pattern, Codex should say so explicitly and propose the alternative structure before asking for confirmation.

## Visual And Readability Enforcement
Every Ralph-style loop in this repo should enforce the user's standing critiques on:
- metric labels
- chart title visibility
- intuitive visualization
- readability on first glance
- clean basketball-first interpretation

Required rule before a chart, viewer, or thread is considered ready:
1. First pass: build the analysis artifact.
2. Second pass: critique it specifically for labels, titles, and intuitive readability.
3. Third pass if needed: revise any chart that still needs explanation to be understood.

No artifact should be treated as complete if:
- axis labels use shorthand without plain-English support
- the chart title is too small, vague, or visually buried
- the reader cannot tell what the chart is saying within a few seconds
- the chart is statistically correct but visually unintuitive
- the tweet or prose has to rescue a confusing chart

Working standard:
- do not pass work back after the first acceptable version
- pass it back only after targeted self-critique and revision
- if there is still a tradeoff or ambiguity, surface it explicitly instead of pretending it is ideal

## Good Ralph Candidates
- expanding one topic into a full planning package
- tightening a thread draft against a locked metric rule
- repeated revision on a viewer, chart set, or post draft
- deterministic cleanup after analysis outputs land

## Bad Ralph Candidates
- open-ended brainstorming
- topics that require real-time human judgment before each pass
- tasks with moving requirements
- anything where the completion condition is vague
