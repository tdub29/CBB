# March Madness (Men's) Analysis and Prediction

Pre-tournament win probabilities for the NCAA men's tournament, built on Bart Torvik team stats and
tested against historical seed-pair win rates under a pre-registered charter.

## Result: log5 is frozen, and its edge over seeds is not established

- **Model.** `log5` on Torvik's Barthag: P(a beats b) = expit(logit(Barthag_a) - logit(Barthag_b)). It has
  no parameters. It was chosen on 2010-2019 only and frozen at 2026-09-29T16:22:27Z
  ([reports/model_selection_frozen.json](reports/model_selection_frozen.json)), after the charter was
  locked at 16:11:27Z.
- **Test, 2021-2026 (6 tournaments, 377 games).** Brier minus seed-pair -0.0059, year-block 95% CI
  [-0.0123, +0.0004]. Power against the pre-set 0.005 edge was 15%; the minimum detectable edge is 0.0126.
  **No detectable edge.** 2008-2026 data cannot sharpen this; only future tournaments can.
- **Use.** A probability sheet about as good as seeds. Calibration: no detectable miscalibration at this
  sample size, which is not proof of calibration. Not a betting edge, no pool-win evidence.
- Full write-up: [reports/model_vs_seeds_powered.md](reports/model_vs_seeds_powered.md). Limits of the
  test (2021-2026 is not blind: a Barthag-heavy ridge model was scored on 2021-2025 before the charter):
  [reports/completion-audit-2026-10-06/decision_matrix.md](reports/completion-audit-2026-10-06/decision_matrix.md).

## Reproduce from a clean clone

Python 3.12, from the project root:

```bash
python -m venv .venv && .venv/Scripts/python -m pip install -r requirements.txt   # .venv/bin/python on macOS/Linux
.venv/Scripts/python -m pytest
.venv/Scripts/python scripts/run_powered_comparison.py score
.venv/Scripts/python scripts/predict_selection_sunday.py --year 2026 --check \
    --bracket data/raw/torvik_2026/selection_sunday_bracket_2026.csv --semis East-South,West-Midwest
```

- `score` first checks that the three hashed inputs match the `inputs` block of the frozen JSON and exits
  if any changed. It then rewrites `reports/model_vs_seeds_powered.md`, the two
  `data/processed/powered_*_2010_2026.csv` files and the calibration figure. Only the run timestamps
  should differ from the committed copies.
- `--check` must print `CHECK PASS`: the outcome-free bracket math reproduces the saved 2026 `p_frozen`
  game rows and round probabilities to 1e-12.
- **Never run `run_powered_comparison.py select`.** The selection is frozen; rerunning it would be a
  second look at the data. It refuses to run: the frozen JSON is a protected path in `src/frozen_inputs.py`.

## Supported run paths

| Command | What it does | Timing label |
|---|---|---|
| `scripts/predict_selection_sunday.py --year Y --bracket CSV --semis R1-R2,R3-R4` | log5 round probabilities for a 68-team bracket; each First Four slot is weighted by its log5 play-in probability | `selection_sunday` |
| same, 64-team CSV | a field with no play-in left | `64_team_field` |
| `scripts/predict_selection_sunday.py --year Y --from-games` | the field the powered report scored, rebuilt from the games, so it knows the First Four winners | `post_first_four` |
| `scripts/run_powered_comparison.py score` | the pre-registered test, deterministic | retrospective |
| `scripts/run_bracket_evaluation.py` | the older ridge model walked forward 2021+; superseded by `score`; writes `_<out-tag>` files | retrospective |
| `scripts/run_pipeline.py` | legacy madness.py data build and seed-stratified models; writes `cbb_training_clean_<YYYYMMDD>.csv`, never the frozen file | none |

The round probabilities in `powered_round_probabilities_2010_2026.csv` are **post-First-Four**: they use
the 64-team field after the play-in games. Retrospective scores are not prospective evidence. The first
prospective test is a `selection_sunday` run made before the 2027 tournament tips off.

The bracket CSV needs `Team,Seed,Region` (Torvik team names; any other column is ignored) and `--semis`
names the national semifinal pairing. 2026: East-South and West-Midwest.

## Inputs: frozen, manifested, not regenerable

[src/frozen_inputs.py](src/frozen_inputs.py) holds the full sha256 of the three CSVs, their two game manifests
and the frozen JSON. Every writer of
a training CSV goes through `write_csv`, which refuses those paths, and `tests/test_frozen_inputs.py`
fails if any of them changes.

| Input | Provenance | Regenerable? |
|---|---|---|
| `data/processed/cbb_training_clean.csv` | 2008-2025 rows from the March 2026 build, 2026 layered in by `scripts/ingest_2026.py`; [data/raw/torvik_2026/MANIFEST.md](data/raw/torvik_2026/MANIFEST.md) | **No.** In 2026-09 Torvik's `.php` pages returned a browser check or HTTP 403 to scripts, and a later pull serves today's tables, not the March ones |
| `data/raw/tournament_games.csv` | Torvik `YYYY_super_sked.json`, 2008-2026; `tournament_games_manifest.json` | Re-fetch: `scripts/fetch_tournament_games.py` writes to `data/raw/refetch/<date>/` for a diff |
| `data/raw/seed_history_games.csv` | Sports Reference 1985-2007; `seed_history_games_manifest.json` | Re-fetch: `scripts/fetch_seed_history.py`, same `refetch/` rule |
| `data/raw/torvik_2026/selection_sunday_bracket_2026.csv` | `Team,Seed,Region` of `ncaa_2026_results.csv` (the official bracket from the Wikipedia wikitext) | Yes, from the files in that folder |
| `data/external/torvik_2026_selection_sunday/` | Torvik tables through Selection Sunday 2025 and 2026, for `scripts/sensitivity_2026_inputs.py` | Archived copy |

**Source vintage limits.** The 2026 regular-season export is dated 2026-03-05, before the conference
tournaments, so 2026 teams sit a few games short of other years (sensitivity in the powered report).
Historical rows come from Torvik's tables as served when they were pulled, years after each season, so
they may carry rating revisions a forecaster in that March did not have. `reports/model_optimization_20260929.md` and
`reports/bracket_simulation_2025.md` were run on 2026-09-29 against an earlier training CSV (sha256 prefix
`8565be043d11155d`, not kept); their numbers stand as recorded. The 2021 walk-forward matches the powered
comparison's saved `p_ridge` on the frozen CSV (`tests/test_bracket_evaluation_cli.py`); other years are untested.

## Legacy pipeline

The original project predicted round reached (0-6) with one general linear model plus 14 seed-stratified
models, chosen by jackknife leave-one-out ([src/pipelines/madness.py](src/pipelines/madness.py), R twin
`madness.R`). It is kept for the topic analyses and the raw-data build; the powered report scores it
(`madness`) and it does not beat seed-pair rates.

```bash
python scripts/run_pipeline.py              # data build + models; run-tagged outputs only
python scripts/run_pipeline.py --prep-raw   # rebuild data/raw/assembled/ from per-year CSVs
Rscript scripts/install_deps.R && Rscript scripts/run_pipeline.R
```

Config: [configs/default.yaml](configs/default.yaml), local overrides in `configs/local.yaml` (gitignored).
Data sources and rate limits: [docs/data_sources.md](docs/data_sources.md). Naming:
[docs/naming_conventions.md](docs/naming_conventions.md).

## Project structure

```
configs/      year range, paths
data/raw/     source tables, manifests, torvik_2026/ (2026 layer)
data/processed/  frozen training CSV, powered predictions, run-tagged builds
reports/      powered report, frozen JSON, topic analyses, completion audit
scripts/      run_powered_comparison.py, predict_selection_sunday.py, fetch and ingest scripts
src/          frozen_inputs.py, models/ (tournament, win_probability, bracket), pipelines/
tests/        pytest suite
```
