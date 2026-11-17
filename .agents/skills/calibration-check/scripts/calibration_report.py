#!/usr/bin/env python3
"""Measure probability calibration from observed outcomes and predictions.

Reports proper scores (Brier, log loss) with skill against the base rate and an
optional reference forecaster, the calibration intercept (slope fixed at 1) and
slope from a logistic fit of the outcome on logit(p) with a cluster-bootstrap
interval at the replication unit, CORP reliability points (isotonic
pool-adjacent-violators) with counts, the CORP Brier decomposition, and binned
ECE as a secondary diagnostic. --selftest checks the math on synthetic data and
reads no files.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path

LOGIT_CLIP = 1e-12


def parse_args(argv=None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", help="CSV, Parquet, or JSON records file (required unless --selftest)")
    parser.add_argument("--target", help="Binary 0/1 outcome column (required unless --selftest)")
    parser.add_argument("--probability", help="Predicted probability column (required unless --selftest)")
    parser.add_argument("--bins", type=int, default=10)
    parser.add_argument(
        "--bins-strategy",
        choices=("uniform", "quantile"),
        default="uniform",
        help="uniform: equal-width bins on [0, 1] (default); quantile: equal-mass bins, preferred for ECE",
    )
    parser.add_argument(
        "--min-bin-n",
        type=int,
        default=20,
        help="Minimum populated-bin count used only to flag sparse summaries",
    )
    parser.add_argument("--group-col", help="Optional fold or season column for group metrics")
    parser.add_argument(
        "--cluster-col",
        help="Replication unit (season, class, tournament) for the cluster-bootstrap intervals "
        "of intercept, slope and Brier skill; pass a row id when rows are independent",
    )
    parser.add_argument(
        "--reference-col",
        help="Reference forecaster probability column (training-window base rate, strongest "
        "baseline or market) scored on the same rows for skill",
    )
    parser.add_argument("--boot", type=int, default=1000, help="Cluster-bootstrap resamples (default 1000)")
    parser.add_argument("--seed", type=int, default=7, help="Bootstrap seed (default 7)")
    parser.add_argument("--filter-col", help="Optional column used to select one evaluation perspective")
    parser.add_argument("--filter-value", help="String value required in --filter-col")
    parser.add_argument("--out", help="Optional JSON output path")
    parser.add_argument(
        "--selftest",
        action="store_true",
        help="Run the synthetic self-checks (numpy only, reads no files) and exit",
    )
    args = parser.parse_args(argv)
    if not args.selftest:
        missing = [
            flag
            for flag, value in (("--input", args.input), ("--target", args.target), ("--probability", args.probability))
            if not value
        ]
        if missing:
            parser.error("the following arguments are required: " + ", ".join(missing))
    return args


def load_frame(path: str):
    try:
        import pandas as pd
    except ImportError as exc:
        raise SystemExit("pandas is required; install it with: python -m pip install pandas") from exc
    suffix = Path(path).suffix.lower()
    if suffix == ".csv": return pd.read_csv(path)
    if suffix in {".parquet", ".pq"}: return pd.read_parquet(path)
    if suffix in {".json", ".jsonl", ".ndjson"}: return pd.read_json(path, lines=suffix != ".json")
    raise SystemExit("--input must be CSV, Parquet, JSON, JSONL, or NDJSON")


def _np():
    try:
        import numpy as np
    except ImportError as exc:
        raise SystemExit("numpy is required (it installs with pandas): python -m pip install numpy") from exc
    return np


def metrics(rows, target: str, probability: str, bins: int, strategy: str = "uniform") -> dict:
    y = [float(v) for v in rows[target]]
    p = [float(v) for v in rows[probability]]
    n = len(y)
    brier = sum((yi - pi) ** 2 for yi, pi in zip(y, p)) / n
    eps = 1e-15
    log_loss = -sum(
        yi * math.log(min(max(pi, eps), 1 - eps))
        + (1 - yi) * math.log(1 - min(max(pi, eps), 1 - eps))
        for yi, pi in zip(y, p)
    ) / n
    table = []
    ece = 0.0
    if strategy == "quantile":
        np = _np()
        parr, yarr = np.asarray(p), np.asarray(y)
        edges = np.unique(np.quantile(parr, np.linspace(0.0, 1.0, bins + 1)))
        if edges.size < 2:
            edges = np.array([parr.min(), parr.max()])
        which = np.searchsorted(edges[1:-1], parr, side="right")
        for index in range(edges.size - 1):
            mask = which == index
            count = int(mask.sum())
            if count == 0:
                continue
            mean_p = float(parr[mask].mean())
            rate = float(yarr[mask].mean())
            ece += count / n * abs(rate - mean_p)
            table.append({"bin": index + 1, "lower": float(edges[index]), "upper": float(edges[index + 1]), "n": count, "mean_probability": mean_p, "observed_rate": rate})
        return {"n": n, "brier": brier, "log_loss": log_loss, "ece": ece, "calibration_table": table}
    for index in range(bins):
        lower, upper = index / bins, (index + 1) / bins
        positions = [i for i, value in enumerate(p) if lower <= value <= upper and (index == bins - 1 or value < upper)]
        if not positions:
            continue
        mean_p = sum(p[i] for i in positions) / len(positions)
        rate = sum(y[i] for i in positions) / len(positions)
        ece += len(positions) / n * abs(rate - mean_p)
        table.append({"bin": index + 1, "lower": lower, "upper": upper, "n": len(positions), "mean_probability": mean_p, "observed_rate": rate})
    return {"n": n, "brier": brier, "log_loss": log_loss, "ece": ece, "calibration_table": table}


def logit(p):
    np = _np()
    q = np.clip(np.asarray(p, dtype=float), LOGIT_CLIP, 1 - LOGIT_CLIP)
    return np.log(q / (1 - q))


def sigmoid(eta):
    np = _np()
    return np.exp(-np.logaddexp(0.0, -np.asarray(eta, dtype=float)))


def fit_logistic(y, x=None, offset=None, max_iter=100, tol=1e-10):
    """Newton-Raphson (IRLS) logistic regression of y on [1, x] plus an optional offset.

    Returns the coefficients (intercept, slope), or (intercept,) when x is None, or None
    when the fit does not converge (separation or a singular information matrix).
    """
    np = _np()
    y = np.asarray(y, dtype=float)
    n = y.size
    design = np.ones((n, 1)) if x is None else np.column_stack([np.ones(n), np.asarray(x, dtype=float)])
    off = np.zeros(n) if offset is None else np.asarray(offset, dtype=float)

    def loglik(beta):
        eta = design @ beta + off
        return float(np.sum(y * eta - np.logaddexp(0.0, eta)))

    beta = np.zeros(design.shape[1])
    current = loglik(beta)
    for _ in range(max_iter):
        mu = sigmoid(design @ beta + off)
        weight = mu * (1.0 - mu)
        info = design.T @ (design * weight[:, None])
        try:
            step = np.linalg.solve(info, design.T @ (y - mu))
        except np.linalg.LinAlgError:
            return None
        if not np.all(np.isfinite(step)):
            return None
        scale = 1.0
        for _ in range(40):
            candidate = beta + scale * step
            value = loglik(candidate)
            if value >= current - 1e-12:
                break
            scale /= 2.0
        else:
            return None
        beta, current = candidate, value
        if np.max(np.abs(scale * step)) < tol:
            return beta
    return None


def intercept_slope(y, p):
    """Calibration intercept (slope fixed at 1) and slope (joint fit) of y on logit(p)."""
    x = logit(p)
    joint = fit_logistic(y, x)
    offset_only = fit_logistic(y, None, offset=x)
    if joint is None or offset_only is None:
        return None
    return {
        "calibration_intercept": float(offset_only[0]),
        "calibration_slope": float(joint[1]),
        "joint_intercept": float(joint[0]),
    }


def brier_score(y, p):
    np = _np()
    return float(np.mean((np.asarray(p, dtype=float) - np.asarray(y, dtype=float)) ** 2))


def log_score(y, p):
    np = _np()
    q = np.clip(np.asarray(p, dtype=float), 1e-15, 1 - 1e-15)
    y = np.asarray(y, dtype=float)
    return float(-np.mean(y * np.log(q) + (1 - y) * np.log(1 - q)))


def skill(score, reference_score):
    if reference_score <= 0:
        return None
    return 1.0 - score / reference_score


def brier_skills(y, p, ref=None):
    """Brier skill against the evaluated rows' base rate and, if given, a reference forecaster."""
    np = _np()
    base_rate = float(np.mean(y))
    out = {"vs_base_rate": skill(brier_score(y, p), base_rate * (1 - base_rate))}
    if ref is not None:
        out["vs_reference"] = skill(brier_score(y, p), brier_score(y, ref))
    return out


def proper_scores(y, p, ref=None) -> dict:
    np = _np()
    base_rate = float(np.mean(y))
    brier = brier_score(y, p)
    log_loss = log_score(y, p)
    base_brier = base_rate * (1 - base_rate)
    base_log = log_score(y, np.full(len(y), base_rate))
    out = {
        "base_rate": base_rate,
        "brier": brier,
        "brier_base_rate": base_brier,
        "brier_skill_vs_base_rate": skill(brier, base_brier),
        "log_loss": log_loss,
        "log_loss_base_rate": base_log,
        "log_loss_skill_vs_base_rate": skill(log_loss, base_log),
    }
    if ref is not None:
        ref_brier, ref_log = brier_score(y, ref), log_score(y, ref)
        out.update({
            "brier_reference": ref_brier,
            "brier_skill_vs_reference": skill(brier, ref_brier),
            "log_loss_reference": ref_log,
            "log_loss_skill_vs_reference": skill(log_loss, ref_log),
        })
    return out


def pav(values, weights):
    """Weighted pool-adjacent-violators: the non-decreasing fit to values sorted by forecast."""
    blocks = []
    for value, weight in zip(values, weights):
        blocks.append([float(weight), float(weight) * float(value), 1])
        while len(blocks) > 1 and blocks[-2][1] / blocks[-2][0] > blocks[-1][1] / blocks[-1][0]:
            weight2, total2, count2 = blocks.pop()
            blocks[-1][0] += weight2
            blocks[-1][1] += total2
            blocks[-1][2] += count2
    fitted = []
    for weight, total, count in blocks:
        fitted.extend([total / weight] * count)
    return fitted


def corp(y, p):
    """CORP recalibration by PAV, its reliability points and the Brier decomposition.

    BS = MCB - DSC + UNC with MCB = BS(p) - BS(recalibrated), DSC = UNC - BS(recalibrated)
    and UNC = base_rate * (1 - base_rate) (Dimitriadis, Gneiting and Jordan 2021).
    """
    np = _np()
    y = np.asarray(y, dtype=float)
    p = np.asarray(p, dtype=float)
    order = np.argsort(p, kind="mergesort")
    sorted_p, sorted_y = p[order], y[order]
    unique_p, start, counts = np.unique(sorted_p, return_index=True, return_counts=True)
    rates = np.add.reduceat(sorted_y, start) / counts
    fitted = np.asarray(pav(rates, counts))
    recalibrated = np.empty_like(p)
    recalibrated[order] = np.repeat(fitted, counts)
    brier = brier_score(y, p)
    base_rate = float(y.mean())
    uncertainty = base_rate * (1 - base_rate)
    brier_recal = brier_score(y, recalibrated)
    decomposition = {
        "brier": brier,
        "miscalibration": brier - brier_recal,
        "discrimination": uncertainty - brier_recal,
        "uncertainty": uncertainty,
    }
    points = []
    for forecast, value, count in zip(unique_p, fitted, counts):
        if points and abs(points[-1]["recalibrated"] - float(value)) <= 1e-15:
            points[-1]["forecast_max"] = float(forecast)
            points[-1]["n"] += int(count)
        else:
            points.append({"forecast_min": float(forecast), "forecast_max": float(forecast), "recalibrated": float(value), "n": int(count)})
    return decomposition, {"method": "isotonic pool-adjacent-violators (CORP)", "points": points}


def cluster_bootstrap(y, p, clusters, ref=None, boot=1000, seed=7) -> dict:
    """Percentile intervals for intercept, slope and Brier skill, resampling whole clusters."""
    np = _np()
    y = np.asarray(y, dtype=float)
    p = np.asarray(p, dtype=float)
    labels, inverse = np.unique(np.asarray(clusters).astype(str), return_inverse=True)
    groups = labels.size
    members = [np.flatnonzero(inverse == g) for g in range(groups)]
    rng = np.random.default_rng(seed)
    keys = ["calibration_intercept", "calibration_slope", "brier_skill_vs_base_rate"]
    if ref is not None:
        keys.append("brier_skill_vs_reference")
        ref = np.asarray(ref, dtype=float)
    draws = {key: [] for key in keys}
    failed = 0
    for _ in range(boot):
        rows = np.concatenate([members[g] for g in rng.integers(0, groups, size=groups)])
        fit = intercept_slope(y[rows], p[rows])
        skills = brier_skills(y[rows], p[rows], None if ref is None else ref[rows])
        if fit is None or skills["vs_base_rate"] is None or (ref is not None and skills.get("vs_reference") is None):
            failed += 1
            continue
        draws["calibration_intercept"].append(fit["calibration_intercept"])
        draws["calibration_slope"].append(fit["calibration_slope"])
        draws["brier_skill_vs_base_rate"].append(skills["vs_base_rate"])
        if ref is not None:
            draws["brier_skill_vs_reference"].append(skills["vs_reference"])
    ci95 = {
        key: ([float(np.percentile(values, 2.5)), float(np.percentile(values, 97.5))] if values else None)
        for key, values in draws.items()
    }
    return {"G": int(groups), "boot": int(boot), "seed": int(seed), "failed_resamples": int(failed), "ci95": ci95}


def calibration_block(y, p, clusters=None, ref=None, boot=1000, seed=7) -> dict:
    block = {"definition": "logistic fit of the outcome on logit(p); intercept with slope fixed at 1 (target 0), slope from the joint fit (target 1)"}
    fit = intercept_slope(y, p)
    if fit is None:
        block["error"] = "logistic fit did not converge (separation or constant outcome)"
        return block
    block.update(fit)
    if clusters is None:
        block.update({"G": None, "ci95": None, "note": "pass --cluster-col naming the replication unit to get intervals"})
        return block
    block.update(cluster_bootstrap(y, p, clusters, ref, boot, seed))
    return block


def validate_numeric_inputs(frame, target: str, probability: str, extra_probabilities=()):
    """Return a copy with validated numeric target and probability columns."""
    import pandas as pd

    clean = frame.copy()
    for column in (target, probability, *extra_probabilities):
        converted = pd.to_numeric(clean[column], errors="coerce")
        invalid = clean[column].notna() & converted.isna()
        if invalid.any():
            raise SystemExit(f"{column!r} must contain numeric values")
        clean[column] = converted
    if not set(clean[target].dropna().unique()) <= {0, 1}:
        raise SystemExit(f"{target!r} must contain only 0/1 values")
    for column in (probability, *extra_probabilities):
        finite_probability = clean[column].dropna().map(math.isfinite)
        if not finite_probability.all() or not clean[column].dropna().between(0, 1).all():
            raise SystemExit(f"{column!r} must contain finite values between 0 and 1")
    return clean


def add_conservative_assessment(report: dict, min_bin_n: int, has_groups: bool) -> None:
    sparse_bins = [row["bin"] for row in report["calibration_table"] if row["n"] < min_bin_n]
    flags = ["held_out_and_walk_forward_provenance_not_verified_by_helper"]
    if report["n"] < 100:
        flags.append("small_overall_sample")
    if sparse_bins:
        flags.append("sparse_populated_bins")
    if has_groups:
        flags.append("group_stability_requires_manual_review")
    report["helper_assessment"] = {
        "status": "descriptive-summary-only",
        "min_bin_n": min_bin_n,
        "sparse_populated_bins": sparse_bins,
        "flags": flags,
    }
    report["verdict"] = "manual-review-required"


def display_gate(report: dict) -> dict:
    """Facts the display rule needs; the verdict word itself stays a manual call."""
    block = report["calibration_intercept_slope"]
    interval = (block.get("ci95") or {}).get("brier_skill_vs_base_rate")
    return {
        "rule": "show a probability only at usable-with-caveats or better on the verdict scale and with "
        "a Brier-skill interval at the replication unit whose lower bound is above zero; otherwise "
        "show the base rate or a coarser tier",
        "brier_skill_vs_base_rate_ci95": interval,
        "interval_lower_bound_above_zero": None if interval is None else bool(interval[0] > 0),
        "verdict_required": "usable-with-caveats or better (manual)",
    }


def selftest() -> int:
    """Synthetic checks of the slope, PAV, decomposition and binning math. Exit 1 on failure."""
    np = _np()
    failures = []

    def check(label, ok, detail=""):
        print(("  ok   " if ok else "  FAIL ") + label + (f" ({detail})" if detail else ""))
        if not ok:
            failures.append(label)

    rng = np.random.default_rng(7)
    # Exact-frequency design: each true rate r in 0.05..0.95 gets 400 rows with exactly 400r
    # events, so the full-sample fit of y on logit(r) is exactly intercept 0 and slope 1.
    levels = np.arange(1, 20) / 20.0
    per_level = 400
    true_p = np.repeat(levels, per_level)
    events = [int(round(rate * per_level)) for rate in levels]
    y = np.concatenate([np.r_[np.ones(k), np.zeros(per_level - k)] for k in events])
    clusters = rng.permutation(true_p.size) % 60

    calibrated = calibration_block(y, true_p, clusters, None, boot=400, seed=7)
    slope = calibrated["calibration_slope"]
    low, high = calibrated["ci95"]["calibration_slope"]
    check("calibrated generator: slope near 1", abs(slope - 1.0) < 0.05, f"slope {slope:.4f}")
    check("calibrated generator: interval covers 1", low <= 1.0 <= high, f"[{low:.3f}, {high:.3f}], G {calibrated['G']}")
    check("calibrated generator: intercept near 0", abs(calibrated["calibration_intercept"]) < 0.05,
          f"intercept {calibrated['calibration_intercept']:.4f}")

    overconfident = sigmoid(2.0 * logit(true_p))
    over = calibration_block(y, overconfident, clusters, None, boot=400, seed=7)
    slope = over["calibration_slope"]
    low, high = over["ci95"]["calibration_slope"]
    check("overconfident generator (logit doubled): slope near 0.5", abs(slope - 0.5) < 0.05, f"slope {slope:.4f}")
    check("overconfident generator: interval excludes 1", high < 1.0, f"[{low:.3f}, {high:.3f}]")

    noisy_p = rng.uniform(0.02, 0.98, 3000)
    noisy_y = (rng.uniform(size=3000) < noisy_p).astype(float)
    decomposition, points = corp(noisy_y, noisy_p)
    recal = [point["recalibrated"] for point in points["points"]]
    check("PAV output is monotone", all(a <= b + 1e-15 for a, b in zip(recal, recal[1:])), f"{len(recal)} blocks")
    kept = sum(point["recalibrated"] * point["n"] for point in points["points"])
    check("PAV keeps the event count", abs(kept - noisy_y.sum()) < 1e-6, f"{kept:.6f} vs {noisy_y.sum():.0f}")
    identity = decomposition["miscalibration"] - decomposition["discrimination"] + decomposition["uncertainty"]
    check("miscalibration - discrimination + uncertainty equals Brier", abs(identity - decomposition["brier"]) < 1e-9,
          f"gap {abs(identity - decomposition['brier']):.2e}")
    check("miscalibration and discrimination are non-negative",
          decomposition["miscalibration"] >= -1e-12 and decomposition["discrimination"] >= -1e-12)

    base = np.full(noisy_y.size, noisy_y.mean())
    check("a constant base-rate forecast has zero Brier skill",
          abs(proper_scores(noisy_y, base)["brier_skill_vs_base_rate"]) < 1e-12)

    table = metrics({"y": noisy_y, "p": noisy_p}, "y", "p", 10, strategy="quantile")["calibration_table"]
    counts = [row["n"] for row in table]
    check("quantile bins hold equal mass", len(counts) == 10 and max(counts) - min(counts) <= 1, f"counts {counts}")

    print("selftest: " + ("PASS" if not failures else f"FAIL ({len(failures)} failed)"))
    return 1 if failures else 0


def main(argv=None) -> int:
    args = parse_args(argv)
    if args.selftest:
        return selftest()
    if args.bins < 2:
        raise SystemExit("--bins must be at least 2")
    if args.min_bin_n < 1:
        raise SystemExit("--min-bin-n must be at least 1")
    if args.boot < 1:
        raise SystemExit("--boot must be at least 1")
    if bool(args.filter_col) != bool(args.filter_value):
        raise SystemExit("--filter-col and --filter-value must be provided together")
    if args.target == args.probability:
        raise SystemExit("--target and --probability must name different columns")
    if args.reference_col and args.reference_col in (args.target, args.probability):
        raise SystemExit("--reference-col must differ from --target and --probability")
    df = load_frame(args.input)
    required = [args.target, args.probability]
    required += [args.reference_col] if args.reference_col else []
    required += [args.cluster_col] if args.cluster_col else []
    required += [args.group_col] if args.group_col else []
    required += [args.filter_col] if args.filter_col else []
    required = list(dict.fromkeys(required))
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise SystemExit(f"missing required columns: {', '.join(missing)}")
    input_rows = int(len(df))
    extra = (args.reference_col,) if args.reference_col else ()
    clean = validate_numeric_inputs(df[required], args.target, args.probability, extra)
    clean = clean.dropna().copy()
    if args.filter_col:
        clean = clean[clean[args.filter_col].astype(str) == args.filter_value].copy()
    if clean.empty:
        raise SystemExit("no complete rows to evaluate")
    report = metrics(clean, args.target, args.probability, args.bins, args.bins_strategy)
    report["bins_strategy"] = args.bins_strategy
    report["row_accounting"] = {
        "input_rows": input_rows,
        "evaluated_rows": int(len(clean)),
        "rows_excluded_by_missing_values_or_filter": input_rows - int(len(clean)),
    }
    if args.filter_col:
        report["filter"] = {"column": args.filter_col, "value": args.filter_value}
    if args.group_col:
        report["groups"] = {
            str(group): metrics(part, args.target, args.probability, args.bins, args.bins_strategy)
            for group, part in clean.groupby(args.group_col, sort=True)
        }
    y = clean[args.target].to_numpy(dtype=float)
    p = clean[args.probability].to_numpy(dtype=float)
    ref = clean[args.reference_col].to_numpy(dtype=float) if args.reference_col else None
    clusters = clean[args.cluster_col].astype(str).to_numpy() if args.cluster_col else None
    report["proper_scores"] = proper_scores(y, p, ref)
    if args.reference_col:
        report["proper_scores"]["reference_col"] = args.reference_col
    decomposition, corp_points = corp(y, p)
    report["brier_decomposition"] = decomposition
    report["corp"] = corp_points
    report["calibration_intercept_slope"] = calibration_block(y, p, clusters, ref, args.boot, args.seed)
    if args.cluster_col:
        report["calibration_intercept_slope"]["cluster_col"] = args.cluster_col
    report["display_gate"] = display_gate(report)
    add_conservative_assessment(report, args.min_bin_n, bool(args.group_col))
    flags = report["helper_assessment"]["flags"]
    block = report["calibration_intercept_slope"]
    if not args.cluster_col:
        flags.append("no_replication_unit_given_so_no_intervals")
    elif block.get("G") is not None and block["G"] < 20:
        flags.append("few_clusters_below_about_20_bootstrap_interval_is_optimistic")
    if block.get("failed_resamples"):
        flags.append("some_bootstrap_resamples_failed_to_fit")
    if report["display_gate"]["interval_lower_bound_above_zero"] is False:
        flags.append("brier_skill_interval_not_above_zero_do_not_show_probability")
    if report["n"] < 1000:
        flags.append("under_about_1000_cases_recalibrate_by_logistic_intercept_and_slope_not_isotonic")
    text = json.dumps(report, indent=2)
    print(text)
    if args.out:
        out = Path(args.out)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(text + "\n", encoding="utf-8")
        print(f"wrote {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
