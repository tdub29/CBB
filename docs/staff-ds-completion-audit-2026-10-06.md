# Staff data science completion audit

Trevor asks where the CBB prediction work last landed, whether it can honestly be called done, and whether the result merits pride. The audit should be done at very experienced staff data scientist depth. This is an evidence audit and a completion decision, not a request to invent stronger results or fit endless models.

## Current evidence and launch gate

Read README.md and the applicable project rules. Other projects stay separate and untouched.

Last checkpoint CHECKPOINT-model-vs-seeds.md says powered score stage and final 26 tests finished. Main evidence is reports/model_vs_seeds_powered.md, model_selection_frozen.json, model_optimization_20260929.md, verification_2026-09-29.md, bracket_simulation_2025.md, statistical_methods.md, published_claims_provenance.md, docs/modeling_plan.md and execution_plan.md. Later reproduction timestamp in the powered report is October 1; do not infer chronology from the checkpoint alone.

## Scope and scientific contract

Reconstruct the actual intended use and definition of done before scoring. Distinguish completed research, a reproducible prediction pipeline, validated advantage over baselines, and practical bracket utility. Define target, grain, prediction timestamp, available source vintage, forecast horizon, population, primary metric, baseline, acceptance rule and stop conditions from the existing frozen contract. Do not move criteria after seeing results.

The report's existing model over 2010-2026 gives Brier difference versus seed-pair -0.0020, interval [-0.0087,+0.0059]. Frozen log5 on 2021-2026 gives -0.0059, interval [-0.0123,+0.0004]. Both intervals include zero. These are report claims to independently reproduce, not new certification. The claim is no detectable edge, not equality, proof of no effect, or established predictive superiority. Inspect reported low power, minimum detectable effect and six tournament blocks. T-Rank pregame lines include earlier tournament information after round one and are not a like-for-like Selection Sunday benchmark.

## Evidence lanes

1. Last state and artifact lineage: reconstruct original R/Python/notebook forecasts, selection/freeze/scoring chronology, hashes and experiment history. Inventory actual delivered outputs, reproducibility, tests, intended future-season procedure and operational failure behavior. Preserve historical claims separately from newer reruns. There are hundreds of dirty/untracked files; never commit or reset them wholesale.
2. Adversarial data/validation audit: apply the standard leakage, feature-availability, validation-design and calibration checks as applicable. Trace every feature to availability at Selection Sunday, including source revisions, coach history, regular-season rating recalculation, opponent joins, PASE/PAKE aliases, preprocessing, tuning, grouped versus forward splits and topic analyses performed after outcomes were known. Correlation below a leakage threshold is not proof of cleanliness. Investigate whether apparent historical holdouts are truly untouched given prior descriptive and selection work. Check 2020 exclusion, First Four, no-contests, paired teams and 2026 source-vintage sensitivity.
3. Predictive and practical utility: independently recompute primary losses from saved per-game rows, frozen candidate and historical seed baselines; verify calibration/uncertainty, year/round/seed segments, cluster/bootstrap assumptions, selection multiplicity, power and target/path coherence. Compare a strong fair benchmark at identical decision time where data permits. Separate game probability quality, advancement probabilities, expected wins and bracket scoring. Inspect negative/out-of-range regression outputs and how production handles them. No financial edge claim from retrospective results.

Audit lanes write only new outputs under reports/completion-audit-2026-10-06, with file/line/data evidence and runnable checks. Read-only scripts may use scratch outputs there. Do not overwrite frozen reports, data vintages, saved model artifacts or selection file. NEVER rerun run_powered_comparison.py select. Any legitimate model revision is a new version with a new contract and fresh future validation, not a rewrite of the frozen result.

## Completion and reviewer gate

Combine lane evidence into a candidate verdict. A fresh independent reviewer challenges it and checks whether identified issues invalidate results or merely limit claims. The coordinator cannot certify its own assessment. Run smallest meaningful tests/checks needed to reproduce claims; avoid rerunning expensive model searches. Routine fixes necessary for reproducibility may be proposed concretely or implemented on isolated copies with review; no broad rebuild, refetch or redesign without a stated evidence gap.

Deliver a clear decision matrix: research COMPLETE/INCOMPLETE, prediction pipeline READY/NOT READY, demonstrated improvement SUPPORTED/NOT ESTABLISHED, practical bracket use SUPPORTED/LIMITED/UNSUPPORTED. Each cell needs evidence and limitations. Give the shortest ranked list of remaining must-fix items, optional improvements separately, and what observable evidence would permit calling the project done. Explain what Trevor can reasonably be proud of without praise substituting for evidence: honest rejection, leakage repairs, reproducible science and useful delivery may be substantial achievements even without a proven edge.

Final answer: lead with whether we can call it done; last accepted result; what has been independently checked; concrete unfinished items; a bounded finish plan. No 'genius' or 'best ever' certification, no invented full data or statistical significance. Keep a resumable per-phase checkpoint. Stop at a completed independently reviewed verdict or a real blocker, not an endless optimization loop. No external messages, account changes, trades, production deployment or pushes are authorized by this audit.
