# Experiment Log Schema

```text
experiment_id: YYYYMMDD-<slug>-<nn>
created_at_utc:
operator:
hypothesis:
expected_direction:
sport:
competition:
grain:
target:
eligible_population:
prediction_timestamp_rule:
data_sources:
immutable_snapshot:
data_window:
feature_set_ref:
baseline_refs:
validation_charter_ref:
primary_metric:
success_rule:
window:
prior_looks:
family_index:
look_budget:
model_family:
config_ref:
code_version:
environment_ref:
random_seeds:
commands:
status: planned | running | completed | failed | invalidated
fold_metrics:
metrics_primary:
metrics_secondary:
calibration_results:
slice_results:
stability_results:
leakage_audit_status:
failures:
deviations:
results_summary:
decision: keep | discard | follow-up | invalid
decision_reason:
next_actions:
artifacts:
checksums:
notes:
```

Rules:

- Create ID before the run
- Create files exclusively; never overwrite or reuse an existing ID
- Hypothesis must be falsifiable
- Freeze primary metric, baseline, and success rule before execution
- Log losers as well as winners
- Post-hoc metrics must be labeled post-hoc
- Append timestamped corrections and deviations; do not rewrite frozen fields

Look fields (fill before execution; they are frozen with the hypothesis):

- `window`: the evaluation window this run scores (seasons, classes, dates).
- `prior_looks`: every earlier look at that window from the window ledger
  (what scored it, who, when), or `none`. Any entry makes the window
  development: never call it pre-registered, untouched, blind or scored once.
- `family_index`: n, this family's position in the target's family registry.
- `look_budget`: families and promotions allowed on this target, n so far, and
  the null-simulated bar at this index with how it was computed
  ([look_budget.md](look_budget.md)).
