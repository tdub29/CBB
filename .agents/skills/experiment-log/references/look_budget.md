# Look Budget

A look is any time a model, analysis, diagnostic or report scores an evaluation
window. The look budget counts every look and every candidate family along the
whole search path, not only inside the final study, so the bar a candidate must
clear grows with the search that produced it.

Tags: [V] verified against the primary paper, abstract or official docs; [S]
secondary summary only; [K] background knowledge, a lead to verify before citing.

## 1. The family registry (append-only)

Append one row per family before the family's first score: every candidate
family, every served model, every in-browser or ad hoc fit that renders a
number, and every late variant. A family is a model class or rule with its own
tuning grid; its configurations are its trials.

```text
family_index | family_id | added_at_utc | mechanism | trials (configurations) | windows scored | scored_by | first_score_at_utc | gain (metric, unit) | bar at this index | status
```

- `family_index` is n for the n-th family registered on this target. It never
  resets after a null, a rename or a new session.
- Rows are never deleted. Failed and abandoned families stay: they are the
  evidence that selection happened (`quant-strategy-tear-sheet` section 4).
- A family whose number is shown or acted on is registered before it renders.

## 2. The bar for the n-th family comes from a null over all n

Set the promotion bar from a null simulated over every registered family, not
by judgment:

1. Permute the labels within each replication unit (within season, class or
   tournament): unit-level structure survives and only the link between inputs
   and outcomes breaks.
2. Refit and rescore every registered family on the permuted labels, its own
   tuning included, exactly as it ran on the real labels.
3. Record the maximum gain over the baseline across all n families.
4. Repeat 200 to 1,000 times. The bar for the n-th family is the 95th
   percentile of those maxima.

A family whose real gain falls below the bar is research-only, whatever its own
p-value says. Every pass reports n.

```python
import numpy as np

def permutation_bar(families, frame, label, unit, score_gain, perms=200, q=0.95, seed=7):
    """q-quantile of the best family's gain after refitting every family on labels permuted within units.

    score_gain(family, frame) refits the family as it ran (own tuning included) and returns its
    out-of-sample gain over the baseline.
    """
    rng = np.random.default_rng(seed)
    maxima = []
    for _ in range(perms):
        permuted = frame.copy()
        permuted[label] = frame.groupby(unit)[label].transform(lambda s: rng.permutation(s.to_numpy()))
        maxima.append(max(score_gain(family, permuted) for family in families))
    return float(np.quantile(maxima, q))
```

**Cheaper version, when refitting every family is too expensive** and each
family already has honest out-of-sample predictions on the same rows: flip the
sign of each unit's gain (candidate loss reduction over the baseline), apply the
same flips to every family in a draw so their correlation survives, and take the
best family's mean gain per draw. It accounts for picking the best of n
families, not for selection inside a family, so say which version ran.

```python
def max_gain_bar(unit_gains, draws=2000, q=0.95, seed=7):
    """q-quantile of the best family's mean gain under shared within-unit sign flips.

    unit_gains has shape (n_families, G): entry [f, u] is family f's loss reduction over the
    baseline in unit u (positive is better), from honest out-of-sample predictions.
    """
    gains = np.asarray(unit_gains, dtype=float)
    rng = np.random.default_rng(seed)
    signs = rng.choice((-1.0, 1.0), size=(draws, gains.shape[1]))
    maxima = (signs @ gains.T / gains.shape[1]).max(axis=1)
    return float(np.quantile(maxima, q))
```

For a claim that a feature group adds to a baseline, the matched-noise variant
permutes that group within units and keeps the baseline's signal intact; report
the candidate's percentile among the noise draws.

## 3. Corrections when the bar cannot be simulated

| Situation | Correction | Source |
|---|---|---|
| Best of m configurations, one test | p_adj = 1 - (1 - p)^m, with m every configuration evaluated, not the survivors | `katz-system-significance-test` |
| Several declared comparisons inside one family | Holm step-down | `statistical-modeling`, test selection guide |
| A rule picked from a universe of rules | White's Reality Check or a Monte Carlo permutation over the whole universe; Hansen's SPA test is a studentized refinement [K, not in the research notes behind this skill] | `evidence-based-technical-analysis` |
| A Sharpe ratio from a research program | deflate at the program's raw trial count; effective N beside raw N as description only | Bailey and Lopez de Prado, J Portfolio Management 2014 [V]; `quant-strategy-tear-sheet` section 3.2 |
| A new factor or signal | multiple-testing hurdle near t of 3 | Harvey, Liu and Zhu, Review of Financial Studies 2016 [K] |

Holm step-down: sort the m p-values ascending and reject the i-th smallest
while p(i) <= alpha / (m - i + 1); stop at the first failure.

```python
def holm(p_values, alpha=0.05):
    """True for each hypothesis Holm rejects at family-wise level alpha."""
    order = np.argsort(p_values)
    m = len(p_values)
    reject = np.zeros(m, dtype=bool)
    for rank, i in enumerate(order):
        if p_values[i] > alpha / (m - rank):
            break
        reject[i] = True
    return reject
```

State the economic hypothesis first, count and report every trial, and keep a
true holdout (Arnott, Harvey and Markowitz, J Financial Data Science 1(1), 2019
[V]). The best-of-many winner's in-sample number is a selection criterion, not
an estimate of its future performance (`evidence-based-technical-analysis`).

## 4. The window ledger and the candidates-at-first-score stamp

Keep an append-only ledger per evaluation window, one row per look:

```text
window | what scored it (model, analysis, diagnostic, published report) | by whom (person, agent, session) | when (UTC) | commit or artifact
```

- Build it from reports, results, notebooks, git history and published pages,
  not from memory.
- Any row means the window is seen: never call it pre-registered, untouched,
  blind or scored once. The confirmation is the next window nobody has scored,
  named in advance.
- A reviewer's diagnostic on the same data is a look.
- Candidates at first score: the candidate list at the moment the first test
  table printed, stamped with its time in the results. A candidate added later
  is post hoc and gets the next unseen window as its test.

## 5. Reusable holdout budget

When the sample is too small to keep a final holdout untouched, consult it the
Thresholdout way with a fixed budget of revealed answers (Dwork, Feldman, Hardt,
Pitassi, Reingold and Roth 2015, as `validation-design` anti-pattern 9
describes). When the budget is spent, the holdout is development data.

## 6. Reading a forward ledger

A forward (prospective) ledger is read only at the dates its power card names
(`statistical-modeling`, power card reference). An interim mark is neither a
win nor a loss; an unplanned early read counts as a look in this budget, and the
stopping rule is reported with the result. Before the date the status is "open;
resolves on <date>".

## 7. Log fields

Every experiment record carries `window`, `prior_looks`, `family_index` and
`look_budget` ([log_schema.md](log_schema.md)).
