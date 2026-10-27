"""Target-leakage guard, run on every feature set before it is fitted."""

from __future__ import annotations

import numpy as np
import pandas as pd


def assert_no_target_leakage(df: pd.DataFrame, features: list[str], target: str,
                             condition: str = "seed", threshold: float = 0.90,
                             label: str = "") -> None:
    """Fail loudly if a feature reconstructs the target once seed is accounted for.

    A plain corr(feature, target) does NOT catch this: PASE IS wins minus seed
    expectation, so the identity needs two variables and its plain correlation with round
    is unremarkable. Residualising the target against seed first separates leaks from
    real predictors cleanly; the measured correlations are in docs/modeling_plan.md
    (Training Data, "Outcome-derived").
    """
    y = pd.to_numeric(df[target], errors="coerce")
    cond = pd.to_numeric(df[condition], errors="coerce") if condition in df.columns else None

    bad = []
    for col in features:
        if col not in df.columns or col == condition:
            continue
        x = pd.to_numeric(df[col], errors="coerce")
        ok = x.notna() & y.notna()
        if cond is not None:
            ok &= cond.notna()
        if ok.sum() < 30 or x[ok].nunique() < 3:
            continue
        resid = y[ok] - y[ok].groupby(cond[ok]).transform("mean") if cond is not None else y[ok]
        if resid.std() == 0:
            continue
        r = abs(float(np.corrcoef(resid, x[ok])[0, 1]))
        if r >= threshold:
            bad.append((col, r))

    if bad:
        detail = ", ".join(f"{c} r={r:.4f}" for c, r in sorted(bad, key=lambda t: -t[1]))
        raise ValueError(
            f"target leakage in feature set {label or '(unnamed)'}: {detail}. "
            f"Against '{target}' residualised on '{condition}', r>={threshold} means the "
            f"feature is derived from the outcome, not a predictor of it."
        )
