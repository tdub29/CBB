# portfolio-fixes reconciliation, 2026-10-27

`portfolio-fixes` (archived as `archive/portfolio-fixes`; 035e94e, 28 commits) branched from main at 2a07385, before the frozen log5 test
harness reached the published main. This integration brings that harness onto main and takes from the branch
only what is safe on top of it. The branch is left on the remote unchanged.

## Disposition of the 28 commits

| Commits | Disposition |
|---|---|
| e2f5b9f coach tenure across 2020 | **Ported.** Legacy builds now count 2020 toward `coach_years_with_program`. Run-tagged builds only; the frozen CSV keeps its values. |
| 1da8ed7 stale coach-stats test | **Already on main** (same expectation fix); its two extra tests came over with e2f5b9f. |
| 16f9946 regenerate `cbb_training_clean.csv` | **Rejected.** It rewrites a frozen input (`src/frozen_inputs.py`). |
| 8ae9537, 23eb659, e911d31 season-grouped validation, seed baselines in `run_analysis.py` | **Not ported.** Legacy ridge validation; conflicts with main's `run_analysis.py`. The validation of record is `run_powered_comparison.py score`. |
| 1b5aef3, 035e94e split `madness.py` into packages, shared topic helpers | **Not ported.** Conflicts with main's `madness.py` (guarded `write_csv`, frozen-path refusal). Needs a rebase onto main and a fresh before/after equality check. |
| 06d992e, dd6864a experience `.corr()` alignment | **Not ported this pass.** Touches 12 files including stored topic reports, which would need regenerating. |
| b3b9d9a, ebb85cd, 3deea04, b5a7310 descriptive robustness checks | **Not ported this pass.** New script and reports; reports would need rerunning on main's inputs. |
| 736eb05, 93a4748, 80d2a44, 60f79fe, 33c23f9 report and data-doc wording | **Not ported this pass.** 65+ files overlapping main's 2a07385 cleanup. |
| 5c70f3e, 3ffd855, fc37b96 removals and export cleanup | **Not ported.** Overlaps main's 2a07385 public-source cleanup, which stands. |
| 197dd4c, 0c8ef21, ab87ba8, fddca4b, e7520de, 61f6e12 | Merge commits, no content of their own. |

### Follow-up, 2026-10-30 (`fix/analysis-followups`)

| Archived commits | Disposition on `fix/analysis-followups` |
|---|---|
| 06d992e, dd6864a experience `.corr()` alignment | **Ported** in 295bec4: index alignment, the sign convention and the aligned-array tests. Outputs regenerated in 01e793f. |
| b3b9d9a, ebb85cd, 3deea04 descriptive robustness checks | **Ported** in 98e61bb (`scripts/robustness_checks.py` and its test as on the branch, rerun on main's inputs); README and coverage notes in 97061be and 3a9a304. b5a7310 (scipy) was already on main. |
| 736eb05, 93a4748, 80d2a44 report wording | **Ported in part**: post-series wording (f78a9c2), 2026-field position labels (2f4d1dd, 0a819e0, 0f4411b), field score names (b3dcb32), repo-relative paths (fd45028), the archived March 2026 projection (584af7c), the 2026 dominant-big-man count (01e793f) and the simulation summary labels (bf54c4a). Not ported: "Headline finding" lines in five summaries, All-D1 labels and 2020 line breaks on the context charts, removal of the `reports/figures/*_thread/` copies, and the five turnovers figures the scripts write but the repo does not track. |
| 60f79fe, 33c23f9 data-doc wording | **Not ported.** `docs/data_sources.md` gained only the tenure-cap (0a43960) and tournament-games (0239eae) notes. |
| 5c70f3e removals | **In part:** b3973fa removes 83 of its 166 paths (social drafts, thread viewers, loop scripts). The postseason file renames are not ported. |

## Coach features in the legacy default

The branch's open question was whether to drop the six coach variables from `GEN_FEATURES` in
`src/pipelines/madness.py`. They feed only the legacy ridge family (`run_pipeline.py`, `run_analysis.py`). The frozen model is
log5 on Barthag and reads no coach column. `run_powered_comparison.py score` fits that legacy family as the
`madness_bt` candidate straight from `mad.GEN_FEATURES` on the frozen CSV, so dropping coach variables would
change a recorded candidate and break the `score` reproduction. **No change.** The default stays as recorded,
and the branch's request is obsolete under the frozen test.

## Claims this integration does not settle

- **Source vintage and decision time.** Historical Torvik rows were pulled years after each season and may
  carry rating revisions a March forecaster did not have. No as-of publication inventory exists, so the
  pre-tournament timing of historical features is **unverified**. README "Source vintage limits" says so.
- **2021-2026 and 2025 are not blind.** A Barthag-heavy ridge model was scored on 2021-2025 before the
  charter, and the branch's `reports/predictive_analysis_20261025.md`
  records that coach features were added after the 2025 tournament. No result here is prospective.
- The log5 edge over seed-pair rates remains **not established** (Brier -0.0059, CI [-0.0123, +0.0004]).
