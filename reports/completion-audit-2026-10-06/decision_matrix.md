# Limits of the log5 test (2026-10-06)

The pre-registered question has a final, reproducible answer that 2008-2026 data cannot sharpen: log5's edge
over seed-pair rates is not established. This page sets out what the test supports and where it stops. Full
write-up: [model_vs_seeds_powered.md](../model_vs_seeds_powered.md).

## Summary by axis

| Axis | Verdict | Evidence and limits |
|---|---|---|
| Research | **COMPLETE** | Charter locked 2026-09-29T16:11Z, log5 frozen 16:22Z. Test reproduced from raw inputs with two independent codebases to 2.2e-16. Selection: log5 and seed-pair re-derived from raw inputs (-0.01199); ridge_bt and madness_bt selection scores rest on saved predictions, since `select` may not be rerun. No leakage into the game-level test. Limits: 2021-2026 is not blind. A Barthag-dominated ridge model was scored on 2021-2025 before the charter (`bracket_simulation_2025.md`), and the topic metrics were chosen on outcomes that include the test years. The selection rule itself is mechanical. Score was run twice (09-29, 10-01) with identical numbers; log5 has no parameters. |
| Pipeline | **READY** | Committed in 32f6050 (everything `score` imports or reads, the three hashed inputs and their manifests, the frozen-input guard in `src/frozen_inputs.py` and the Selection Sunday command), 7e7bb3a (hashed evidence stored byte for byte, so a clone passes the hash guard) and 88b9fc4 (`select` refuses at the guard). `run_pipeline.py` writes run-tagged `cbb_training_clean_<YYYYMMDD>.csv` and never the frozen file. `scripts/predict_selection_sunday.py` is the Selection Sunday run path for log5: `--check` reproduces the saved 2026 `p_frozen` game rows and round probabilities to 1e-12, and a 68-team bracket weights each First Four slot by its log5 play-in probability. The `run_bracket_evaluation.py` CLI is fixed and tested. Limits: the frozen CSV cannot be rebuilt from live Torvik; `score`'s round probabilities are post-First-Four. |
| Improvement over seed-pair | **NOT ESTABLISHED** | 2021-2026 Brier -0.0059, bootstrap [-0.0123, +0.0004], clustered t(5) interval [-0.0152, +0.0033] (wider), DM p 0.16, power 15% at the 0.005 edge, MDE 0.0126. Direction holds under drop-one-year. Better calibrated over 2010-2026 (slope 0.96 vs 0.73), a span that includes the selection years; on the six test tournaments the calibration difference is not resolved. Not citable: the pooled 2010-2026 row (contains selection years) and the existing-model 2021-2026 row (-0.0044 [-0.0073, -0.0011]: not the frozen candidate, predecessor seen on the test years, iid p 0.41, one of many secondary rows). |
| Bracket use | **LIMITED** | A calibrated probability sheet as good as seeds. Committed-bracket points vs seed-pair +1.7/yr [-110, +127]; 1 champion in 6. No edge in the Round of 64 (+0.0008 Brier); any gain sits in 186 later-round games. Round probabilities are post-First-Four, not Selection Sunday. No pool-win evidence. Not for betting. |

## Optional

- Fix the powered report's wording on three points:
  - "scored once" becomes "single frozen candidate, deterministic reruns identical".
  - Add algorithm hindsight (historical rows use Torvik's current algorithm).
  - Add the pre-charter 2021-2025 look.
- Score 2027 prospectively before tip, which is the only route to more power.
- Evaluate pool strategy if bracket pools are a goal.
