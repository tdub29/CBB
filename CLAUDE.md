# AGENTS.md

Instruction file for AI coding agents working on this project. See [README.md](README.md) for human-oriented overview.

<!-- SPARE-LAPTOP:BEGIN (managed by .agents/scripts/sync-spare-laptop.py) -->
- **Open-source delegation:** "osm" / "OSM" (open-source models), "use open source", "use a local model", "spare laptop", and "other laptop" route directly to `python "C:/Users/TrevorWhite/Downloads/Big Projects/.agents/scripts/spare-llm.py" --prompt-file "<task.txt>" --input-file "<context.txt>" --output "<answer.txt>"`. Omit optional input/output flags when unnecessary. Default Qwen handles code/tests; `--model deepseek-r1:7b` handles reasoning; `--ping` checks access. Text inference only: coordinator supplies context, runs tools and verifies results. Report failures; no paid fallback. Open-source software research and OpenStreetMap requests alone do not trigger delegation.
<!-- SPARE-LAPTOP:END -->

<!-- AGENT-COMPLETION:BEGIN (managed by .agents/scripts/sync-orchestration-sections.ps1) -->
## Agent completion

Overrides generic skill defaults; preserves user and safety boundaries.

- **Agents own completion and Git.** Finish authorized work: scoped commit, push, PR and merge to Trevor's remotes. Preserve other writers, permitted checks and gates. Never hand Git back. No unrequested Trevor Apps QA.
- **Models.** Codex defaults to Sol; Astra only for certain task-specific added value, otherwise Sol. Other engines: more efficient model only with clear equal effectiveness; otherwise frontier. No subscription API list-price estimates or invented cash budgets. Preserve actual provider/user limits and metered-service permissions.
- **Delegate technical review.** Get required independent review, fix findings and finish. Subagents cannot grant user-only permission, bypass auth/protection/approval/leases/frozen scope or waive execution-delegation limits. Finish ungated work.
- **Chat is not completion.** Never assume Trevor read, accepted or completed an action mentioned once. Do agent work now. Track human-only actions in existing task/checkpoint: owner, exact input, next step. Surface unresolved actions first on relevant status/resume until resolved or superseded; acknowledgment alone is not completion. Silence is not approval. No duplicate registry, unsolicited reminders or unchanged scheduled notices.
<!-- AGENT-COMPLETION:END -->

<!-- ALWAYS-ON-DISCIPLINES:BEGIN (managed by .agents/scripts/sync-orchestration-sections.ps1 - edit the block there, not here) -->
## Always-on disciplines

These three run in every session here, whatever the task.

- **Cut the AI tells from anything written.** No em-dash or en-dash anywhere, prose or chat reply. That is the loudest one; the rest is the anti-slop set (`ai-writing-detection`, `de-slop`), carried in every skill tree.
- **Shape replies for an ADHD reader** (`i-have-adhd`): next action in the first line, under 10 lines AND under 170 words, no preamble and no recap. Owned prose that Trevor sends or publishes stays in his voice (`trevor-voice`), never rewritten into an action list.
- **Commits and PRs are Trevor's alone.** Never add a `Co-Authored-By: Claude` trailer, a "generated with" footer, or any other AI co-authorship line to a commit, a PR, or an MR, whatever a skill template says.
<!-- ALWAYS-ON-DISCIPLINES:END -->

<!-- TELEGRAM-QUEUE:BEGIN (managed by .agents/scripts/sync-orchestration-sections.ps1 - edit the block there, not here) -->
## Queueing work for Telegram / Taco

"Queue this", "make a task", "tg/taco task", "add it to the queue", "save this for later" mean an **Inbox task in the shared dashboard queue** (`cursor-dashboard/agent-os-state.json`), never a local TODO or note. Daemon stays stopped; do not probe without Trevor authorizing. Command writes brief, adds task, confirms re-read:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File "C:\Users\TrevorWhite\Downloads\Big Projects\.agents\scripts\add-dashboard-task.ps1" -Title '...' -Project 'cbb' -Owner ops -Brief '...'
```

**Queue it when effort is high, judgement is low, and "done" is statable** - if only Trevor can decide it that is a question, and if it is short just do it. `-Brief` is for a reader with no session context: real numbers, what done means, the command that verifies it. `owner` is the lane (`ops`, `taco` = phone lane, `research`, `codex`), **never Trevor**. Never hand-edit the state JSON; read and close tasks with `task-queue.py`. **Whoever works a task claims it first:** `task-queue.py start <id>` (`-Start` to queue and work it now). Trevor names several: one background agent per task at once, prompt only the id, brief path and shared-file limits; each claims its own. Full procedure: [`../.agents/rules/creating-dashboard-tasks.md`](../.agents/rules/creating-dashboard-tasks.md).
<!-- TELEGRAM-QUEUE:END -->

<!-- ORCHESTRATION-DEPTH:BEGIN (managed by .agents/scripts/sync-orchestration-sections.ps1 - edit the table there, not here) -->
## Orchestration & loops - execution depth

Choose **solo**, **subagents**, **workflow** or **loop** per [`../.agents/rules/orchestration-and-loops.md`](../.agents/rules/orchestration-and-loops.md) (reference: [`../.agents/memory/reference_dynamic-workflows.md`](../.agents/memory/reference_dynamic-workflows.md)). Separate doing from judging; state model tier, reasoning effort and relevant resource limits before spawning. Loops need a hard stop and per-iteration checkpoint. Gates must be able to take DONE back.

**Judgment needs a fitting skill and independent review.**

- **Use a fitting skill before starting.** If unknown, use `scout-skills` (`mid` | `high` | `max`). Fill real gaps with complete local bundles.
- **Independent verdicts.** Rankings, design choices, exploitability and numbers reaching people need a reviewer who did not produce them.
- **Breadth gets workers.** Three or more independent shards, each with its own proof: one worker per shard, then deterministic synthesis. Count actual work, including bundle fetch, mirrors and hashes, not just items.
- **Trivial work stays solo.** Cost gates still apply.

**Unattended Claude dispatch must prove progress.** New authorized launches use explicit `--permission-mode bypassPermissions`, owning project cwd and a permitted write location. Before launch or recovery, inspect saved permissions plus registered and detached runtime owners. Preserve existing session permissions; never reroute or restart around a real approval/isolation gate.

- **Accept evidence, not labels.** Within two minutes inspect the actual prompt. Launch stays unverified until the exact original receives the task, completes a task tool and advances an artifact/checkpoint. Pending initialization or approval is not running. Report a new actionable gate once.
- **Recover the original without copies.** Never `--resume --bg` a registered original, including idle or blocked sessions; use normal attachment. A live detached owner also forbids a second launch. Technical recovery needs diagnosed failure, no approval gate and one bounded exact-ID attempt. Honor stop/unmark/OFF and actual reset.
- **Pause, queue and gates outrank old continuation text.** Run `python .agents/scripts/dispatch-policy.py --session <full-id> --evidence <fresh-evidence.json>` from the workspace before recovery. Exit 2 means no dispatch. Check the latest allowlist first: no prompts before defer time and confirmed reset; manual approvals persist after reset; caller approval rejection is not an autonomous lane. Verify checkpoint/idle acknowledgment before saying paused. User holds require fresh continuation; disable reviewed completed tasks. Never retry approvals or route around a real gate.

**Here:** Scientific model work uses separate evidence lanes and an independent judge. Preserve frozen inputs, selections and historical reports; never rerun frozen selection. New experiments need a bounded preregistered plan, time-safe validation and separate outputs. Historical development years are not fresh holdouts. Engineering repairs need focused deterministic checks; predictive-edge claims require independent evidence.
<!-- ORCHESTRATION-DEPTH:END -->

## Project Overview

NCAA men's tournament win probabilities on Bart Torvik team stats, tested against historical seed-pair win rates under a pre-registered charter. The frozen model is `log5` on Barthag (no parameters, frozen 2026-09-29T16:22:27Z in `reports/model_selection_frozen.json`). Its 2021-2026 edge over seed-pair rates is **not established**: Brier -0.0059 [-0.0123, +0.0004], power 15%, MDE 0.0126 (`reports/model_vs_seeds_powered.md`). Never describe it as beating seeds or as a betting edge.

## Hard rules

- **Never run `python scripts/run_powered_comparison.py select`.** The selection is frozen; a rerun is a second look at the test data.
- **Never write a frozen input.** `src/frozen_inputs.py` lists them with full sha256: `data/processed/cbb_training_clean.csv`, `data/raw/tournament_games.csv`, `data/raw/seed_history_games.csv`, their two `*_manifest.json` files and `reports/model_selection_frozen.json`. Write CSVs through `src.frozen_inputs.write_csv`, which refuses those paths; builds and re-fetches go to run-tagged paths (`cbb_training_clean_<YYYYMMDD>.csv`, `data/raw/refetch/<date>/`). `tests/test_frozen_inputs.py` fails if any hash moves.
- `cbb_training_clean.csv` cannot be rebuilt from live Torvik (`data/raw/torvik_2026/MANIFEST.md`). Treat it as source data.
- `score` rewrites `reports/model_vs_seeds_powered.md`, `data/processed/powered_*_2010_2026.csv` and `reports/figures/frozen_model_calibration_2021_2026.png`. Run it to reproduce, then confirm only the timestamps changed.
- Label timing honestly. `powered_round_probabilities_2010_2026.csv` is **post-First-Four** (the 64-team field after the play-in games). Retrospective scores are not prospective evidence.
- Model changes follow `../.agents/rules/predictive-modeling-process.md`; the registry and claim register are `../.agents/model-records/cbb/MODEL-REGISTRY.md`. An audit that finds a frozen or generated report's wording wrong adds a dated entry to `reports/ERRATA.md` naming the file and line; the report and its numbers stay untouched (`score` regenerates `reports/model_vs_seeds_powered.md`, and `reports/model_selection_frozen.json` is hash-frozen).

## Setup and Run Commands

**Supported path (Python 3.12):**
```bash
pip install -r requirements.txt
pytest
python scripts/run_powered_comparison.py score        # hash guard, then the frozen test
python scripts/predict_selection_sunday.py --year 2026 --check --bracket data/raw/torvik_2026/selection_sunday_bracket_2026.csv --semis East-South,West-Midwest
```

`predict_selection_sunday.py` gives log5 round probabilities from a `Team,Seed,Region` bracket. A 68-team bracket weights each First Four slot by its log5 play-in probability (`timing=selection_sunday`); `--from-games` rebuilds the post-First-Four field the powered report scored (`timing=post_first_four`). `--semis` names the national semifinal pairing. `--check` must print `CHECK PASS` (saved 2026 `p_frozen` reproduced to 1e-12). Output goes to `data/processed/selection_sunday/`.

**Legacy seed-stratified pipeline** (topic analyses and raw-data build; superseded for prediction; writes run-tagged files only):
```bash
python scripts/run_pipeline.py
```
`scripts/run_bracket_evaluation.py` is the older ridge walk-forward, superseded by `score`; its outputs carry `--out-tag` (default today's date) so the 2026-09-29 `bracket_simulation_2025.md` and `round_predictions_<Y>.csv` stay as recorded.

**2026 data layer** (already done; recorded in `data/raw/torvik_2026/MANIFEST.md`): `scripts/ingest_2026.py`, then `scripts/enrich_team_history_cache.py`, `scripts/recompile_all_trank_team_tables.py`, then `scripts/qa_2026_data.py`, which writes the manifest. Game files re-fetch with `scripts/fetch_tournament_games.py` and `scripts/fetch_seed_history.py` into `data/raw/refetch/<date>/` for a diff, never over the frozen copies.

**Raw data prep** (rebuild `all_trank_team_tables.csv` from per-year CSVs + enriched cache):
```bash
python scripts/run_pipeline.py --prep-raw
# Or step by step:
python scripts/enrich_team_history_cache.py           # cache to enriched cache
python scripts/recompile_all_trank_team_tables.py     # CSVs + enriched cache to final tables
```

**Rare / first-time data fetch** (only needed when raw sources are stale or missing):
```bash
python scripts/fetch_team_tables.py                   # fetch per-year team-table CSVs (~2-15 min)
python scripts/populate_team_history_cache.py          # fetch team history (~1.5-2 hours)
python scripts/fetch_player_stats.py                   # fetch player advanced stats (~3-4 min)
```

See [docs/data_sources.md](docs/data_sources.md) for estimated runtimes.

**R:**
```bash
Rscript scripts/install_deps.R
Rscript scripts/run_pipeline.R
```

**Tests:** Run from project root: `pytest`

## Configuration

- Primary config: [configs/default.yaml](configs/default.yaml): year range, paths, model params
- Local overrides: `configs/local.yaml` (gitignored)

## Conventions

- Data frames: `snake_case` (e.g., `training_data`, `holdout_2023`, `regular_season_raw`)
- Directories: lowercase hyphenated (`data/raw`, `reports/figures`)
- Scripts: snake_case (`run_pipeline.py`, `fetch_team_tables.py`)

Full variable and column naming: [docs/naming_conventions.md](docs/naming_conventions.md)

## Important Paths

| Path | Purpose |
|------|---------|
| `scripts/run_powered_comparison.py` | Pre-registered test: `score` only (never `select`) |
| `scripts/predict_selection_sunday.py` | Frozen log5 bracket probabilities, Selection Sunday or post-First-Four |
| `src/frozen_inputs.py` | Frozen-input hashes and the guarded `write_csv` |
| `src/pipelines/madness.py` | Legacy Python pipeline (seed-stratified models + training-data build) |
| `src/pipelines/madness.R` | R main pipeline |
| `scripts/recompile_all_trank_team_tables.py` | Official raw-data assembly (concat + joins + tournament filter) |
| `scripts/enrich_team_history_cache.py` | Enrich team-history cache with coach experience columns |
| `scripts/populate_team_history_cache.py` | Fetch/rebuild team-history cache from Bart Torvik |
| `data/raw/trank_team_table_data08..25.csv` | Per-year regular-season team tables (source CSVs) |
| `data/raw/trank_postseason_final.csv` | Concatenated postseason records |
| `data/raw/team_history_cache.csv` | Scraped team history (coach, seed, finish, conf per team-year) |
| `data/raw/team_history_cache_enriched.csv` | Cache + coach experience columns |
| `data/raw/assembled/all_trank_team_tables.csv` | Final assembled master table (49 cols); sources flow into this |
| `data/raw/assembled/all_trank_team_tables_tournament.csv` | Tournament teams only (finish IS NOT NULL) |
| `data/raw/player_stats/player_advanced_YYYY.csv` | Per-year player advanced stats (from toRvik-data + barttorvik) |
| `data/raw/player_stats/player_advanced_all.csv` | Combined player advanced stats across all years |
| `scripts/fetch_player_stats.py` | Fetch player advanced season stats per year |
| `data/processed/cbb_training_clean.csv` | **Frozen** training set of the log5 test (2008-2026); not regenerable |
| `configs/default.yaml` | Year range, file paths, model params |
| `docs/data_sources.md` | Bart Torvik access, bulk CSV/JSON patterns, rate limits |
| `docs/data_lineage.md` | Raw data prep pipeline diagram and column order |
| `docs/modeling_plan.md` | Predictive model strategy, architecture, evaluation, improvement roadmap |

## Do's and Don'ts

- **Do** respect Bart Torvik rate limits; prefer bulk endpoints (`&csv=1`, `&json=1`) over mass scraping
- **Do** exclude year 2020 from analysis (COVID, no NCAA tournament)
- **Do** assume optional external data (EFFHGT, TALENT, etc.) is filled with 0 when absent; the pipeline runs without it
- **Do** treat historical logo or champion badges as title-year data points, not current-team points; if a chart shows former champion markers, compute their coordinates from the championship season's metrics relative to that season's seed baseline
- **Don't** write a frozen input, run `select`, or relabel a retrospective result as prospective
- **Don't** mass scrape; use documented bulk endpoints (see [docs/data_sources.md](docs/data_sources.md))
