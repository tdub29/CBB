# Engineering completion checkpoint (2026-10-06)

Session aafe185e-2380-4a46-a6c8-ea91dbf24f08 (Opus coordinator, sole implementation owner).
Brief: docs/engineering-completion-brief-2026-10-06.md. Started 07:51 PDT.
Scientific verdict unchanged: no established edge over seed-pair.

## Frozen baseline (sha256, must stay identical)

```
a35137c1619032e600f42b45eaff1e9c2b4db12348fdf5e1408be15436b77980  data/processed/cbb_training_clean.csv
170ba15342239c73cf89aaf02c9b2dcaebf6ac94ebd337beef3d1fad8b5cf369  data/raw/tournament_games.csv
a210e08449f488751915094efeb67dd3ff51a991e9bcbe7be9f4ab05d67a2488  data/raw/seed_history_games.csv
624bcd737db6f3d088fdf05b52958f56c6f6034f6700076d7fae5719243b9d85  reports/model_selection_frozen.json
d42ea4330f5ad35a7c6f5374de018b72260de842717584a3dd31814a3f7460b2  reports/model_vs_seeds_powered.md
ddf3d42ff0b0da4dc60069804d4c7ea645aacd48bd9ea8c41e0074be2d2df5aa  data/processed/powered_game_predictions_2010_2026.csv
74ffa06edf89441e272ccf9ea42810765a9bb490bc386f8ca3fd20fef53c746d  data/processed/powered_round_probabilities_2010_2026.csv
```

Git at start: HEAD 2ce8199, 365 dirty paths, 4 pre-existing STAGED deletions
(models/analysis_best_model_2026030{4,6}.joblib, reports/predictive_analysis_2026030{4,6}.md).
Those stay staged and out of any commit (commit with explicit pathspec only).

## Phases

| Phase | Output | Status |
|---|---|---|
| E0 baseline hashes + git state | this file | done |
| E1 frozen-input protection + regression test | src/frozen_inputs.py; 7 writers guarded; tests/test_frozen_inputs.py (10 pass) | done |
| E2 Selection Sunday log5 command (64 + 68 modes) + tests | scripts/predict_selection_sunday.py; data/raw/torvik_2026/selection_sunday_bracket_2026.csv; tests/test_selection_sunday.py (6 pass). --check: saved 2026 p_frozen games and rounds max diff 1.1e-16; 68-team with First Four fixed at real winners = post-First-Four (5.6e-17) | done |
| E3 bracket-eval CLI fix + test; README/AGENTS/recipe; vintage notes | run_bracket_evaluation.py: NameError fixed, --out-tag on all outputs; tests/test_bracket_evaluation_cli.py (2 pass, 2021 p_ridge to 1.1e-16); README + AGENTS rewritten; vintage notes on model_optimization_20260929.md and bracket_simulation_2025.md (numbers unchanged). Full pytest 39 pass; frozen hashes OK | done |
| E4 narrow local commit of pipeline closure | 99eb50c (83 paths, explicit pathspec; closure from traced reads of pytest, --check and score) + 284d1ff (.gitattributes -text on hashed evidence: core.autocrlf=input had stored LF, so a clone failed the hash guard). 4 pre-existing staged deletions still staged, not committed. Stale 0-byte .git/index.lock from 03:29 moved to job tmp (no writer running). No push | done |
| E5 clean-clone reproduction (score + acceptance + pytest) | scratch/clean_clone_*.txt | done: fresh clone at 284d1ff passes --check (CHECK PASS) and pytest (39 pass); the reviewer got the same in its own clone. Direct clean-clone `score` (task boassf2ta, 01:04 to 07:06 PDT 2026-10-07): exit 0; frozen JSON, both powered CSVs and the calibration PNG byte-identical; the report differs only in lines 307-308, run timestamp and code revision (scratch/clean_clone_score_284d1ff.txt). Same result as the 10-06 working-tree copy run and the reviewer's 22-file read-set check. 687d3ca touches only select and argparse on that path; its fresh clone passes pytest 40, --check and 7 hashes (E6a). Six hours against 23 min is the machine, not code or inputs (same read set, identical outputs): laptop on battery at 08:38, Balanced plan, Core Ultra 7 255U with 14 logical CPUs at 42.6% to 82.7% of nominal performance across samples, run queue 57 to 118, Cursor near 4 cores. In a 20 s sample the score's one compute thread sat Ready (runnable, no core) and got 4.5 CPU-s; 2897 CPU-s in 4.6 h. Its OpenBLAS pool (14 threads, no env cap) was parked at 0 CPU, so not spin oversubscription. From 01:12 to 02:44 it also shared the machine with the reviewer's two score runs |
| E6 independent high reviewer (1 agent, budget checked first) | engineering_review.md | done: VERDICT CERTIFIED, finalized 08:52 PDT 2026-10-07 by one Opus reviewer (agent a15517a5df34961fb), the same agent throughout, no second reviewer. Session-limit 429s at 02:51 PDT (req_011CfnfoG8q76gDUQb6WZ9r4) and 05:47 PDT (req_011CfnuFJQVvzYfsrugybg6B); resumed 05:37 on Trevor's instruction (cron 42fdcba1 cancelled after that resume succeeded) and 08:38 after the 08:20 reset. Must-fix 0; should-fix F1-F3 closed by 687d3ca; 7 nits open: N1, N3-N6, N7 (score does not call check_frozen_inputs), N8 (README:35-36 and AGENTS:14 say only timestamps change on a rerun; report line 308, code revision, changes too). R PASS rests on the E5 clean-clone score, which the reviewer verified read-only; its own score run was CPU-starved and stopped |
| E6a should-fix F1-F3 + N2 | scripts/run_powered_comparison.py, README.md, tests/test_frozen_inputs.py, tests/test_bracket_evaluation_cli.py | done: commit 687d3ca (4 files, explicit pathspec, staged deletions untouched). select refuses at assert_writable before loading data, --force dropped; the new test fails on the pre-fix code (old select --force reached load_inputs). Fresh clone at 687d3ca: pytest 40 pass, CHECK PASS (5.6e-17), 7 frozen hashes OK, status clean (scratch/clean_clone_687d3ca_verify.txt) |
| E7 final report | chat | done 2026-10-07: engineering DONE (independently certified, clean-clone reproduced, 7 frozen hashes intact, select never rerun). Scientific null result unchanged |
