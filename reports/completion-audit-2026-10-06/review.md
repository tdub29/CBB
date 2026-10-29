# Independent review of the cbb completion audit (2026-10-06)

Reviewer: independent, read-only. The review is reproduced verbatim below.

---

# Independent review of the cbb completion audit (2026-10-06)

**Overall verdict: AGREE WITH CHANGES.** I agree with all four matrix verdicts. Must-fix 1 has to change before the "done" bar can be met: a commit of only the listed files cannot run `score`. Must-fix items 2-4 and two evidence cells need amending.

## Matrix

| Axis | Candidate | Review | Why |
|---|---|---|---|
| Research | COMPLETE | AGREE, add limits | Charter answered; test reproduced from raw inputs to 2.2e-16. The Research cell must disclose two things. First, a Barthag-dominated model was scored on 2021-2025 before the charter (finding 6). Second, the "6 dp" selection reproduction rests on saved predictions (finding 10). |
| Pipeline | NOT READY | AGREE, worse than stated | The committed stack would not import (finding 1). The default run drops every 2026 row from the frozen CSV (finding 2). |
| Improvement over seed-pair | NOT ESTABLISHED | AGREE, fix evidence cell | The cell's calibration evidence uses the selection window, the same reason the cell bans the pooled row (finding 4). It also leaves out the existing-model 2021-2026 row, whose CI excludes 0 (finding 5). |
| Bracket use | LIMITED | AGREE, add limits | log5 has no edge in the Round of 64 (finding 8). Round probabilities are post-First-Four (finding 3). |

Must-fix ranking 1-5 is right and minimal. Items 1-4 need the amendments below. Nothing is missing beyond those amendments.

## Findings

**1. [blocking] Must-fix 1's file list cannot reproduce `score` from a clean clone.**
- Evidence:
  - `scripts/run_powered_comparison.py:46-57` imports `assert_prediction_time_contract` and `run_provenance` from `scripts/run_analysis.py`. Neither name exists in that file at HEAD (grep count 0).
  - It imports `fit_bradley_terry_k` from `src/models/win_probability.py`, which is also absent at HEAD.
  - It imports from `scripts/run_bracket_evaluation.py`, which is untracked.
  - `src/pipelines/madness.py` (intercept fix used by madness_bt), `src/models/simulate.py` and `src/visualization/palette.py` are modified, not committed.
  - `score` reads `data/external/trevor_2024_*` and `trevor_2025_*` behind `exists()` guards (`:771-801`). A clone without them silently drops report sections instead of failing.
  - The provenance of all three hashed inputs is untracked: `data/raw/torvik_2026/` (MANIFEST, 03-05 export, super_sked, Wikipedia bracket), `scripts/ingest_2026.py`, `qa_2026_data.py`, `fetch_seed_history.py` and `fetch_tournament_games.py`.
- Change: replace the hand list with "every file `run_powered_comparison.py` imports or reads, plus the scripts and raw sources that produced the three hashed inputs". Name the files above explicitly. Add to the done evidence: the clean-clone `score` output matches `model_vs_seeds_powered.md` except for timestamps.

**2. [should-change] L3 is worse than stated: the default run deletes 2026.**
- Evidence:
  - `configs/default.yaml:5`: `year_range: [2008, 2025]`. `run_pipeline()` uses that range unless `--year` is passed (`madness.py` around 2322-2334). It then writes `data/processed/cbb_training_clean.csv` unconditionally (`:2394-2398`).
  - A default `python scripts/run_pipeline.py`, documented at `README.md:53`, therefore replaces the frozen 6,336-row CSV with one that has no 2026 rows. `--year Y` replaces it with a single season. The `score` hash guard (`:629-630`) would then fail loudly, but the file is already gone, and the committed version is the old vintage.
  - The only rebuild recipe is in `data/raw/torvik_2026/MANIFEST.md` (`data_only=True`, 2008-2026). It depends on Torvik `.php` pages that were blocked as of 2026-09-29.
- Change:
  - Must-fix 2 should read: "the default run never writes `cbb_training_clean.csv`; it writes a run-tagged path."
  - Must-fix 4 should add: "README cites the MANIFEST recipe and states the frozen CSV is not regenerable from live Torvik."

**3. [should-change] Must-fix 3's acceptance test conflicts with the input vintage and the First Four. Lane 2 V8 overstates the timing.**
- Evidence:
  - The saved 2026 `p_frozen` uses the 03-05 regular-season export (MANIFEST). A command fed the Selection Sunday table (`data/external/torvik_2026_selection_sunday/trank_2026_through_20260315.csv`) reproduces the sensitivity run (-0.0054), not `p_frozen`.
  - Round probabilities use `bracket_from_games` (`src/models/tournament.py:24`), which rebuilds 64 teams from played games. That means the First Four winners are known.
  - The charter's decision time is "the published bracket on Selection Sunday, before any tournament game". Game-level Brier is unaffected, because P(a beats b) is a valid conditional forecast. The path-aware probabilities are post-First-Four.
- Change:
  - Acceptance (a): given the frozen CSV's 2026 rows and the post-First-Four 64-team bracket, the command reproduces `p_frozen` and the round probabilities to 1e-12.
  - Acceptance (b): a 68-team mode weights each First Four slot by log5 P(win), documented as the actual Selection Sunday product.
  - V8 should read "post-First-Four, pre-Round-of-64 forecast".

**4. [should-change] The calibration claim uses the selection window.**
- Evidence (all recomputed):
  - Over 2010-2026, the slope is log5 0.956 vs seed-pair 0.726. The year-block CI on the difference is [+0.04, +0.38], but this pool includes the 2010-2019 selection years.
  - On 2021-2026 alone, the slope is 0.908 vs 0.796, with a difference CI of [-0.13, +0.26].
  - The report's test-window ECE slightly favours seed-pair (0.0397 vs 0.0411). Murphy reliability favours log5 (0.0032 vs 0.0048).
- Change: in the Improvement cell and proud-of line 47, write "better calibrated over 2010-2026, a span that includes the selection years; on the six test tournaments the difference is not resolved."

**5. [should-change] The verdict is silent on a row whose CI excludes 0.**
- Evidence: existing model minus seed-pair on 2021-2026 is Brier -0.00436 [-0.00730, -0.00113], DM p 0.048, 5 of 6 years better, iid p 0.41. I recomputed this from saved `p_ridge`.
- Change: add one sentence to the Improvement cell saying the row is not citable, for four reasons:
  - It is not the frozen candidate.
  - Its predecessor was scored on 2021-2025 before the charter (finding 6).
  - Its iid p is 0.41.
  - It is one of many secondary rows, and a 6-block percentile bootstrap undercovers.

**6. [should-change] Lane 2 H3 "fine" understates a pre-charter look at the test years.**
- Evidence: `reports/bracket_simulation_2025.md:4-5` scored a walk-forward ridge model against seed-pair on 2021-2025: -0.0049 [-0.0174, +0.0076]. The file mtime is 2026-09-29 01:59 -0700, before the 16:11Z charter lock. In `model_optimization_20260929.md`, Barthag is that model's top ridge coefficient (0.594).
  - The selection rule (2010-2019 Brier) is mechanical, so there is no direct bias.
  - However, the candidate menu (H2) was written by someone who had seen 5 of the 6 test years for a Barthag-heavy model.
- Change:
  - Add this as a fourth hindsight channel in the powered report and in the Research cell's limitations.
  - Never describe 2021-2026 as untouched or blind.
  - I could not establish whether log5 was on the menu before that file existed.

**7. [minor] T-Rank: keep "level with" and do not upgrade it.**
- Evidence (recomputed):
  - On the test years, log5 minus T-Rank is -0.0025 [-0.0051, +0.0002].
  - Over all 11 T-Rank years (692 games), it is -0.0023 [-0.0042, -0.0004], DM p 0.048, 8 of 11 years. On the Round of 64 only (equal information), it is -0.0026 [-0.0041, -0.0010].
  - These comparisons are post hoc, overlap the selection window, and are biased toward log5 by H1 in older years (current algorithm vs archived lines).
- Change: proud-of line 47 should read "no worse than T-Rank's game-day line on six test tournaments, a line that sees earlier tournament games (not like-for-like)".

**8. [minor] The Bracket cell should state that log5 has no edge in the Round of 64.**
- Evidence: on the test years, log5 minus seed-pair is +0.0008 in the Round of 64 (n 191). By later round it is -0.0107 (R32), -0.0086, -0.0424, -0.0121, +0.0358 (final, n 6). The whole gain sits in 186 later-round games.
- Change: add one clause to the limitations.

**9. [minor] "Scored once" (charter; report lines 5 and 25) is inaccurate.**
- Evidence: `CHECKPOINT-model-vs-seeds.md` items 8 and 10 show the score ran about 18:35Z on 09-29. It was rerun on 10-01 02:50Z after script edits at 09-30 19:22 local. The numbers were identical. log5 is parameter-free, so a rerun cannot move the test.
- Change: "scored for a single frozen candidate; reruns are deterministic and returned identical numbers."

**10. [minor] "Selection reproduced to 6 dp" comes from saved predictions, not a rerun.**
- Evidence: the script was edited after the freeze, and `select` may not be rerun. I re-derived the log5 and seed-pair part from raw inputs: difference -0.01199, which matches 0.184114 - 0.196108. The ridge_bt and madness_bt selection Brier scores rest only on saved CSVs.
- Change: reword the Research cell to say exactly that.

**11. [minor] With 6 blocks the CI is, if anything, too narrow.**
- Evidence: the clustered t(5) interval is -0.0059 +/- 2.571 x 0.0036 = [-0.0152, +0.0033], wider than the bootstrap [-0.0123, +0.0004].
- Change: one sentence, so no reader takes the +0.0004 upper bound as "nearly significant". This strengthens NOT ESTABLISHED.

**12. [minor] Must-fix 5 is confirmed and is off the critical path.**
- Evidence: `hist` is assigned at `run_bracket_evaluation.py:112` inside `fit_year_model` (def at :90). It is used at `:131` inside `walk_forward_year` (def at :123), which `main` calls at `:293`. There is no module-level `hist`, so the CLI raises NameError. The powered stack imports only helpers from this file.
- Change: the minimal fix is to delete `walk_forward_year` and `main`, or pass `hist` in.

The "proud of" line "checked here eight ways" is accurate for leakage into the game-level test. The overclaims are findings 4 and 7.

## Numbers I independently reproduced (fresh code, raw CSVs)

| Item | Result |
|---|---|
| Inputs | 1,134 games = 18 x 63, Round of 64 on, no First Four games. 1 no-contest (2021 Oregon-VCU). History 1985-2007, 1,449 games, ends before the games start. 0 teams missing from the training CSV. 0 seed mismatches between the training CSV and the games. |
| Agreement with saved predictions (1,007 rows) | log5 vs saved `p_frozen`: max diff 2.2e-16. Seed-pair vs `p_seed`: 1.1e-16. T-Rank vs `p_trank`: 1.1e-16. |
| Test Brier, 2021-2026 (377 games) | log5 0.18135, seed-pair 0.18729, T-Rank 0.18384. Difference -0.00594 [-0.01225, +0.00037], DM t -1.651, p 0.1597, MDE 0.01256, 4 of 6 years better. Power at 0.005 is about 0.145 (hand calculation). |
| Per-year differences | 2021 -0.0012, 2022 +0.0037, 2023 -0.0163, 2024 +0.0021, 2025 -0.0082, 2026 -0.0157. Drop-one-year range -0.00386 to -0.00787. |
| Test log loss | -0.01901 [-0.03924, +0.00122], p 0.1574. |
| 2010-2019 / 2010-2026 | -0.01199 (p 0.053) / -0.00973 [-0.01598, -0.00247] (p 0.017). |
| Existing model minus seed-pair | 2010-2026: -0.00202 [-0.00871, +0.00587], p 0.61 (the brief's figure). 2021-2026: -0.00436 [-0.00730, -0.00113]. |
| Committed log5 bracket | Mean points 923.3 (2021-2026), 884.4 (2010-2026). 1 champion in 6. Champion picks match lane 3. |

## What I could not check

- Committed-bracket points for seed-pair and chalk, and the +1.7 [-110, +127] difference. I verified only the log5 side.
- A refit of ridge_bt and madness_bt, or a rerun of `select` or `score`. Both are forbidden, so those rows rest on saved CSVs.
- Whether log5 was on the candidate menu before `bracket_simulation_2025.md` was written. The work is uncommitted, so there are no git timestamps, and file mtimes can be altered.
- The size of H1 in years other than 2025.
- pytest (not run). ruff is not installed in the venv; the NameError was confirmed by reading scopes.
