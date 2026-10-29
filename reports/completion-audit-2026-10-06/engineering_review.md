# Engineering review: commits 99eb50c, 284d1ff and 687d3ca (parent 2ce8199)

Reviewer: independent single reviewer, 2026-10-07. I did not write any of the reviewed code.
In the shared checkout I ran read-only git commands only. Everything I ran that writes ran in my own clone at
a separate local review clone (now HEAD 687d3ca).
Python: `cbb\.venv\Scripts\python.exe` (3.12). I never ran `run_powered_comparison.py select`.

VERDICT: CERTIFIED

Must-fix: 0. Should-fix: 0 open (687d3ca closes F1, F2 and F3). Nits: 7 open (N1, N3 to N8); 687d3ca closes N2.

## PASS/FAIL per brief requirement

| # | Requirement | Result |
|---|---|---|
| 1 | Frozen-input protection | PASS. 687d3ca closes F1; nit N7 remains |
| 2 | predict_selection_sunday.py math, joins, timing labels | PASS |
| 3 | run_bracket_evaluation NameError fix, --out-tag, test | PASS |
| 4 | README and contributor-notes claims, null result, vintage notes | PASS. 687d3ca closes F2 and F3; nit N8 remains |
| 5 | Commit closure, 4 staged deletions untouched | PASS |
| R | Reproduction | PASS. Rests on the coordinator's clean-clone score, which I verified read-only (section 6) |

### 1. Frozen-input protection

- `src/frozen_inputs.py` pins the full sha256 of 6 paths:
  - training CSV a35137c1
  - tournament_games 170ba153
  - seed_history a210e084
  - two manifests, 2d65f2a9 and 61360bf2
  - `reports/model_selection_frozen.json`, 624bcd73

  `assert_writable` resolves and normcases the path, then raises `FrozenInputError`. `write_csv` calls it before mkdir and `to_csv`.
- Writer audit: I grepped for `to_csv`, `write_text` and `open(..., "w")` across scripts/, src/, the R scripts and the notebooks.
  - build_clean_training_data, rebuild_from_raw, madness.py and the fetch_* scripts all go through `write_csv` or `assert_writable`.
  - add_center_porpag_feature calls `assert_writable`.
  - ingest_2026, qa_2026_data, sensitivity_2026_inputs and fetch_coach_experience write only paths that are not frozen.
  - No R script or notebook names a frozen path.
- Score guard: `run_score` (`scripts/run_powered_comparison.py:623`) refuses to score when the three input hash prefixes differ from the frozen JSON.
- At 284d1ff, `select --force` wrote the frozen JSON without `assert_writable` (F1).
  - 687d3ca closes this: `run_select` now opens with `assert_writable(FROZEN_PATH)` (`scripts/run_powered_comparison.py:530`), and `--force` is gone.
  - `git grep -E -- "--force|args\.force"` over scripts, src, tests, README.md and the contributor notes finds nothing.
- New test `test_select_refuses_before_loading_any_data` (`tests/test_frozen_inputs.py:46`) stubs `load_inputs` to raise and expects `FrozenInputError`. The test catches real regressions:
  - Against the 284d1ff code, `run_select(argparse.Namespace())` raises AttributeError on `args.force`, so the test fails.
  - If the guard moves below `load_inputs`, or the JSON drops out of `FROZEN_INPUTS`, the stub raises AssertionError and the test fails.
  - I worked this out by reading the code. I did not run the mutation, because that would call the old select function.
- The score path is unchanged. `git diff -U0 284d1ff 687d3ca -- scripts/run_powered_comparison.py` has three hunks only:
  - the import at :54
  - the select guard at :530
  - removal of the `--force` argparse line at :1154

  `run_score` and everything it calls are byte-identical.
- Tests:
  - Full `pytest` at 284d1ff: `39 passed`.
  - At 687d3ca, `pytest tests/test_frozen_inputs.py tests/test_bracket_evaluation_cli.py`: `7 passed in 243.03s`, exit 0.
  - At 687d3ca, `check_frozen_inputs()` returns no problems over the 6 pinned paths.

### 2. scripts/predict_selection_sunday.py

- I ran the README `--check` command (`--year 2026 --check --bracket data/raw/torvik_2026/selection_sunday_bracket_2026.csv --semis East-South,West-Midwest`). It printed `CHECK PASS` and exited 0, at both 284d1ff and 687d3ca.
- Independent check: I wrote my own exact dynamic program and compared it with the tool's 68-team output.
  - Setup: seed order 1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15; First Four slots as two-team leaves; semis East-South and West-Midwest.
  - Max absolute difference over 68 teams: 2.22e-16. Round sums: exactly [64, 32, 16, 8, 4, 2, 1].
  - A 200,000-run vectorised Monte Carlo agreed on champion probability to 0.0011 (3 standard errors = 0.0026).
- The 2026 semifinal pairing, East-South and West-Midwest, matches the official bracket record.
- Joins: `run_check` requires all 384 joined rows and exactly 64 teams, so a missing or duplicated team fails loudly.
- No outcome dependence: the selection_sunday path reads only ratings and the bracket.
  - Game results enter only through `--from-games` (post_first_four).
  - The output file name carries the timing label.
- Timing labels: README lines 51-54 say the powered round probabilities are post-First-Four, and that the first prospective test is a 2027 selection_sunday run. I found no retrospective output labelled prospective.

### 3. run_bracket_evaluation

- `ruff check --select F` over scripts/ and src/ finds no undefined names, so the NameError class is gone. One F541 remains (N6).
- `--out-tag` exists (`scripts/run_bracket_evaluation.py:85`) and is applied to every output path (lines 305, 324, 500).
- `tests/test_bracket_evaluation_cli.py` checks two things, and it passes in my clone at 284d1ff and at 687d3ca:
  - 2021 p_model equals the powered p_ridge to 1e-12 on 62 games.
  - p_seed_pair is correct.

### 4. Documentation

- Every flag named in README and the contributor notes exists in argparse: --bracket, --check, --from-games, --out-tag, --prep-raw, --semis and --year. 687d3ca removes `--force` from the code and the docs together.
- Null result: README lines 12-14 and contributor notes line 7 match `reports/model_vs_seeds_powered.md`.
  - They give Brier -0.0059, CI [-0.0123, +0.0004], power 15% and MDE 0.0126.
  - They say "No detectable edge" and "not established".
  - Contributor notes line 7 forbids describing the result as beating seeds.
- F3 closed. `README.md:15-16` now reads "Calibration: no detectable miscalibration at this sample size, which is not proof of calibration", which matches the report.
- F2 closed. `README.md:80-81` now reads "The 2021 walk-forward matches the powered comparison's saved `p_ridge` on the frozen CSV".
- `README.md:40`, "It refuses to run: the frozen JSON is a protected path in `src/frozen_inputs.py`", matches the code at 687d3ca.
- The vintage notes on `reports/model_optimization_20260929.md` and `reports/bracket_simulation_2025.md` are prepended text only.
  - I compared them with the pre-note text recovered from earlier captured output.
  - model_optimization: 128 of 129 original lines are present verbatim. The 129th is an ellipsis in the capture.
  - bracket_simulation: all 10 headline lines are present verbatim (`grep -F`).
  - The hash prefixes in the notes (8565be043d11155d, 504f855b2dac0ee1) match each report's own provenance lines.

### 5. Commit closure

- 99eb50c: 83 files.
  - All of them fall within the decision-matrix item-1 closure plus audit artifacts: staff docs, completion-audit files, round_predictions_2021-2025.csv, 2 figures, and requirements.txt adding tabulate and scipy.
  - Nothing from the unrelated experience-analysis work is committed.
- 284d1ff: `git diff --ignore-cr-at-eol --stat 99eb50c 284d1ff -- . ':!.gitattributes'` is empty, so the commit changes only line endings and `.gitattributes`.
- 687d3ca: 4 files, +20/-8, all of it the F1, F2, F3 and N2 fixes.
  - Files: scripts/run_powered_comparison.py, README.md, tests/test_frozen_inputs.py, tests/test_bracket_evaluation_cli.py.
  - Author: Trevor White.
- None of the three messages has an AI co-author trailer.
- Shared checkout, rechecked when I finalized: HEAD 687d3ca. `git diff --cached --name-status` still shows exactly the 4 staged deletions, uncommitted:
  - models/analysis_best_model_20260304.joblib
  - models/analysis_best_model_20260306.joblib
  - reports/predictive_analysis_20260304.md
  - reports/predictive_analysis_20260306.md

### 6. Reproduction (R)

- My clone at 284d1ff:
  - all 9 hashes match the baseline (a35137c1, 170ba153, a210e084, 2d65f2a9, 61360bf2, 624bcd73, d42ea433, ddf3d42f, 74ffa06e)
  - `--check`: CHECK PASS
  - full `pytest`: 39 passed
- My clone at 687d3ca:
  - `sha256sum -c ../frozen_baseline.sha256`: 7 of 7 OK
  - the two changed test files: 7 passed
  - README `--check`: CHECK PASS, exit 0. My first attempt left out `--year`, and argparse exited 2. That was my command, not the code.
  - `check_frozen_inputs()`: no problems
  - `git status`: clean apart from `__pycache__`
- My own score run did not finish. Other processes held the CPU (processor queue 118). After about 88 minutes it had only reached `[strength fits] 2012`, so I stopped it; `review_score.txt` ends `exit 1`. It is not evidence either way.
- Score reproduction comes from the coordinator's direct clean-clone run (record `tmp/clean_clone_score_284d1ff.txt`). I did not run it. I checked what it left on disk, read-only, with `GIT_OPTIONAL_LOCKS=0`:
  - `tmp/clean_clone` is a clone of the shared repo at HEAD 284d1ff. Apart from caches, it has no untracked or ignored files.
  - `git status` shows only `reports/model_vs_seeds_powered.md` modified.
  - `git diff -U0` on that report shows lines 307-308 only:
    - the score run timestamp: 2026-10-07T14:06:40 vs 2026-10-01T02:50:18
    - code at scoring: `284d1ff` vs `2ce8199 + uncommitted changes`
  - `sha256sum -c ../frozen_baseline.sha256`: 6 of 7 OK. The one FAILED entry is that report, and those two lines account for it.
  - Score rewrote both powered CSVs (ddf3d42f, 74ffa06e) and the calibration figure (cdba1286): their mtimes are 2026-10-07 07:05 to 07:06. All three are byte-identical to the committed blobs.
  - The logged headline, frozen-seed 2021-2026 Brier -0.0059 [-0.0123, +0.0004], matches the report.
- Supporting evidence that does not depend on that run:
  - The implementer's `wt_copy` score outputs, which I hashed on 2026-10-06, equal the committed blobs apart from report lines 307-308.
  - All 22 files in score's read set are equal, modulo CR, to my clone at 284d1ff.
- The reproduction carries to 687d3ca because the score path is unchanged (section 1).
- Does the verdict depend on the clean-clone score? Yes, through R.
  - That run is the direct evidence for R.
  - Without it, R would rest only on the indirect evidence above, and the verdict would have stayed pending, as it was before the run landed.

## Findings

### Must-fix

None.

### Should-fix

None open. 687d3ca closes all three; I verified each closure at that commit:

- **F1.** Was at `scripts/run_powered_comparison.py:529` and `:597` at 284d1ff: `select --force` overwrote the frozen JSON and bypassed `assert_writable`.
  - Closed by the guard at the top of `run_select`, the removal of `--force`, and the new test.
  - The other half of F1, that score does not hash the frozen JSON itself, is downgraded to N7.
- **F2.** Was at `README.md:79-80`: "reproduces its saved probabilities" was ambiguous. Closed by the new wording at `README.md:80-81`.
- **F3.** Was at `README.md:15`: "A calibrated probability sheet" overstated the report. Closed by the new wording at `README.md:15-16`.

### Nit

- **N1. `tests/test_bracket_evaluation_cli.py:38`.** `test_outputs_are_tagged` checks only `parse_args`, not the paths actually written.
  - Failure: a future writer that skips the tag would still pass.
  - Fix: run main on one year into tmp paths and assert that no untagged file appears.
- **N2. Closed by 687d3ca.** The docstring at `tests/test_bracket_evaluation_cli.py:5` now says "2026-09-29 evidence".
- **N3. `scripts/run_bracket_evaluation.py:85`.** `--out-tag` defaults to today's date, so two runs on the same day overwrite each other.
- **N4. `scripts/predict_selection_sunday.py`.** The output CSV has no made-at or data-vintage column.
  - Failure: a 2026 selection_sunday file made in October 2026 has the same name and shape as a genuine pre-tournament one.
  - README lines 53-54 address this, but in prose only.
- **N5. Commit 99eb50c** is broad: audit scratch, staff docs, round_predictions CSVs. All of it relates to the audit; none of it is unrelated work.
- **N6. `scripts/ingest_2026.py:162`.** ruff F541: an f-string with no placeholders.
- **N7. `scripts/run_powered_comparison.py:623` (`run_score`).** Score does not call `check_frozen_inputs()`.
  - Failure: if the frozen JSON is changed outside Python, for example by an editor or a git checkout of another version, score accepts it silently, and a different selection gets scored under the 2026-09-29 registration.
  - Mitigations: the report prints the live sha prefix on line 308 (computed at :627), and `tests/test_frozen_inputs.py` hashes the JSON.
  - Fix: call `check_frozen_inputs()` at the top of `run_score` and exit on any problem.
- **N8. `README.md:35-36` and contributor notes line 14.** Both say that after `score`, "only the run timestamps" should differ. The clean-clone run also changes the code-revision text on report line 308.
  - Failure: a reproducer sees an unexpected change on line 308 and doubts a good reproduction, or learns to wave through any 2-line diff.
  - Fix: "Only the run timestamp and code-revision lines (307-308) should differ".

## What I could not verify

- The exact pre-note bytes of the two vintage reports. I had only earlier captured output, so "no numbers changed" rests on a line-by-line comparison of those captures.
- A full rerun of the bracket evaluation for any year other than 2021.
- A score run executed by me. Mine was CPU-starved and stopped. R rests on the coordinator's clean-clone run: I checked its outputs on disk, but I did not watch it execute.
- That the new select test fails on the old code. I established this by reading, not by running the mutation, because the hard rules forbid calling select.
- The full pytest suite at 687d3ca. I ran only the two changed test files (7 passed). The rest passed at 284d1ff (39), and none of them references the removed flag.
