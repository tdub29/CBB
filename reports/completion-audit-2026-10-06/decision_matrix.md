# CBB completion audit: reviewed decision matrix (2026-10-06)

Certified by the independent reviewer (`review.md`, AGREE WITH CHANGES; all 12 changes applied here).
Supporting lanes: `lane1_lineage.md`, `lane2_validation.md`, `lane3_utility.md`.

## Is the project done?

**No. The research is done; the project is not.** The pre-registered question has a final, reproducible
answer that 2008-2026 data cannot sharpen. The work that produced it is not committed, cannot be run from
a clean clone, the documented command destroys its frozen input, and the frozen model has no
Selection Sunday run path.

## Decision matrix

| Axis | Verdict | Evidence and limits |
|---|---|---|
| Research | **COMPLETE** | Charter locked 2026-09-29T16:11Z, log5 frozen 16:22Z. Test reproduced from raw inputs with two independent codebases to 2.2e-16. Selection: log5 and seed-pair re-derived from raw inputs (-0.01199); ridge_bt and madness_bt selection scores rest on saved predictions, since `select` may not be rerun. No leakage into the game-level test. Limits: 2021-2026 is not blind. A Barthag-dominated ridge model was scored on 2021-2025 before the charter (`bracket_simulation_2025.md`), and the topic metrics were chosen on outcomes that include the test years. The selection rule itself is mechanical. Score was run twice (09-29, 10-01) with identical numbers; log5 has no parameters. |
| Pipeline | **NOT READY** | Nothing committed. A commit of only the obvious files would not import (`run_analysis.py`, `win_probability.py`, `run_bracket_evaluation.py`, `madness.py` all differ from HEAD). Default `run_pipeline.py` (year range 2008-2025) overwrites the frozen CSV and drops every 2026 row; the CSV cannot be rebuilt from live Torvik. No pre-tournament run path for log5. README and contributor notes describe the old model. `run_bracket_evaluation.py` CLI raises NameError. |
| Improvement over seed-pair | **NOT ESTABLISHED** | 2021-2026 Brier -0.0059, bootstrap [-0.0123, +0.0004], clustered t(5) interval [-0.0152, +0.0033] (wider), DM p 0.16, power 15% at the 0.005 edge, MDE 0.0126. Direction holds under drop-one-year. Better calibrated over 2010-2026 (slope 0.96 vs 0.73), a span that includes the selection years; on the six test tournaments the calibration difference is not resolved. Not citable: the pooled 2010-2026 row (contains selection years) and the existing-model 2021-2026 row (-0.0044 [-0.0073, -0.0011]: not the frozen candidate, predecessor seen on the test years, iid p 0.41, one of many secondary rows). |
| Bracket use | **LIMITED** | A calibrated probability sheet as good as seeds. Committed-bracket points vs seed-pair +1.7/yr [-110, +127]; 1 champion in 6. No edge in the Round of 64 (+0.0008 Brier); any gain sits in 186 later-round games. Round probabilities are post-First-Four, not Selection Sunday. No pool-win evidence. Not for betting. |

## Must-fix to call the project done (ranked, minimal)

1. **Commit everything `score` imports or reads, plus what made its three hashed inputs.** That covers:
   - `scripts/run_powered_comparison.py`, `run_analysis.py`, `run_bracket_evaluation.py` and `sensitivity_2026_inputs.py`
   - `src/models/tournament.py`, `win_probability.py` and `simulate.py`, plus `src/pipelines/madness.py` and `src/visualization/palette.py`
   - the three new tests
   - `data/processed/cbb_training_clean.csv` (a35137c1), `data/raw/tournament_games.csv` and `seed_history_games.csv` with their manifests
   - `data/raw/torvik_2026/`, `data/external/torvik_2026_selection_sunday/` and `data/external/trevor_202{4,5}_*`
   - `scripts/ingest_2026.py`, `qa_2026_data.py`, `fetch_seed_history.py` and `fetch_tournament_games.py`
   - `reports/model_selection_frozen.json`, `model_vs_seeds_powered.md` and both powered prediction CSVs

   Trevor does the git step; the audit may not stage.
2. **Default pipeline never writes `cbb_training_clean.csv`.** Point it at a run-tagged path (`src/pipelines/madness.py:2394-2398`).
3. **Selection Sunday command for log5.** Acceptance has two parts:
   - (a) From the frozen CSV's 2026 rows and the 64-team bracket, it reproduces saved `p_frozen` and the round probabilities to 1e-12.
   - (b) A 68-team mode weights each First Four slot by log5 P(win).
4. **README and contributor notes rewritten.** Cover the charter, frozen log5 and the null result. Give the score command and the MANIFEST rebuild recipe, and state that the frozen CSV is not regenerable from live Torvik.
5. **Clean up two loose ends.** Delete or fix the `run_bracket_evaluation.py` CLI (`walk_forward_year`, `main`). Label `model_optimization_20260929.md` and `bracket_simulation_2025.md` as the pre-2026 data vintage.

## Optional

- Fix the powered report's wording on three points:
  - "scored once" becomes "single frozen candidate, deterministic reruns identical".
  - Add algorithm hindsight (historical rows use Torvik's current algorithm).
  - Add the pre-charter 2021-2025 look.
- Score 2027 prospectively before tip, which is the only route to more power.
- Evaluate pool strategy if bracket pools are a goal.
- Retire or re-back the 7 unbacked published claims (posts 36-38, 41, 44-46).

## Evidence that permits "done"

- A commit hash containing items 1-5.
- From a clean clone of it, `python scripts/run_powered_comparison.py score` passes the hash guard and matches `model_vs_seeds_powered.md` except timestamps.
- The Selection Sunday command meets acceptance (a).
- pytest is green, including a test that exercises the fixed or removed CLI path.

## What Trevor can be proud of

- A real pre-registration: charter and frozen choice timestamped before the test was scored.
- Independent code reproduces the test to machine precision.
- An honest null with power and MDE stated, not a cherry-picked win.
- Leakage into the game-level test was defended by a poisoning test and checked eight ways here with nothing found.
- 1985-2007 seed history was sourced and cross-checked against Torvik, so the baseline is fair.
- A parameter-free Selection Sunday formula came out no worse than T-Rank's game-day line on six test tournaments. That line sees earlier tournament games, so the comparison is not like-for-like.
