# Candidate verdict (coordinator, not yet reviewed)

Status: CANDIDATE. The coordinator did the lanes, so this is not certified until `review.md` agrees.

## Is the project done?

**Research: yes. Project: no.** The question the charter asked has a final, reproducible answer, and no
more analysis on 2008-2026 can sharpen it. What is missing is engineering: the result is not committed,
the documented command destroys its input, and the frozen model cannot be run before a tournament.

## Decision matrix

| Axis | Verdict | Evidence |
|---|---|---|
| Research | **COMPLETE** | Pre-registered charter (locked 16:11Z, frozen 16:22Z on 2026-09-29); selection reproduced to 6 dp (lane1 L4); test reproduced to 1e-16 from raw inputs with independent code (lane3 U1); no leakage (lane2 V1-V8). More power only comes from future tournaments. |
| Pipeline | **NOT READY** | Nothing committed (L1-L2); `run_pipeline.py` default overwrites the frozen training CSV (L3); no pre-tournament run path for log5 (L7); README/AGENTS describe the old model (L8); `run_bracket_evaluation.py` CLI raises NameError (L5). |
| Improvement over seed-pair | **NOT ESTABLISHED** | 2021-2026 Brier -0.0059 [-0.0123, +0.0004], DM p 0.16, power 15% at the 0.005 edge, MDE 0.0126. Direction stable under drop-one-year; log5 better calibrated (slope 0.96 vs 0.73 over 16 years). The pooled 2010-2026 significant row includes the selection window and must not be cited. |
| Bracket use | **LIMITED** | Calibrated probability sheet, as good as seeds and level with T-Rank's game-day line on the test games. Committed-bracket points: +1.7/yr vs seed-pair [-110, +127]; 1 champion in 6. No pool-win evidence. Not for betting (model card). |

## Must-fix to call the project done (ranked)

1. **Commit the powered stack and its frozen inputs** (Trevor's git action; the audit may not stage): `scripts/run_powered_comparison.py`, `sensitivity_2026_inputs.py`, `src/models/tournament.py`, the three new tests, `data/processed/cbb_training_clean.csv` (a35137c1), `data/raw/tournament_games.csv`, `seed_history_games.csv` and manifests, `reports/model_selection_frozen.json`, `reports/model_vs_seeds_powered.md`, both powered prediction CSVs, `data/external/torvik_2026_selection_sunday/`.
2. **Stop the default pipeline from overwriting the frozen input** (`src/pipelines/madness.py:2394-2398`): write elsewhere, or refuse when the target's hash equals the frozen one.
3. **Add a Selection Sunday command for log5**: bracket CSV (Team, Seed, Region) plus a Torvik table in, game matrix and path-aware round probabilities out. Acceptance: run on the 2026 bracket it reproduces the saved 2026 `p_frozen` and round probabilities.
4. **Rewrite README and AGENTS** to the charter, frozen log5, the null result, and the score command.
5. **Fix or remove the `run_bracket_evaluation.py` CLI** (undefined `hist`, line 131) and label `model_optimization_20260929.md` / `bracket_simulation_2025.md` as the pre-2026 data vintage.

## Optional

- One sentence in the powered report on algorithm hindsight (historical rows are Torvik's current algorithm).
- Score 2027 prospectively with the frozen model before tip; that is the only route to more power.
- Pool-strategy evaluation (contrarian picks vs field) if bracket pools are a goal.
- Retire or re-back the 7 unbacked published claims (posts 36-38, 41, 44-46).

## Evidence that would permit "done"

A commit hash containing items 1-5; from a clean clone of it, `python scripts/run_powered_comparison.py score`
passes the hash guard and reproduces the 2021-2026 row; the new Selection Sunday command reproduces 2026;
pytest green including a test that exercises the fixed CLI.

## What Trevor can be proud of

- A real pre-registration: charter, frozen choice and timestamps before the test was scored, and a selection that reproduces to 6 dp.
- An honest null with power and MDE stated, instead of a cherry-picked win.
- Leakage defended by a poisoning test and checked here eight ways with nothing found.
- 1985-2007 seed history sourced and cross-validated against Torvik to keep the baseline fair.
- Finding that a parameter-free Selection Sunday formula matches the public game-day line and is better calibrated than seed history.
