from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from scripts.run_analysis import describe_winner, missing_share_table, save_missingness_chart


def _frame() -> pd.DataFrame:
    # seed is missing for the two non-tournament rows only, as in the training CSV.
    return pd.DataFrame(
        {
            "seed": [1.0, 8.0, 16.0, np.nan, np.nan],
            "Barthag": [0.95, 0.80, 0.40, 0.30, np.nan],
            "AdjOE": [120.0, 112.0, 100.0, 98.0, 95.0],
        }
    )


def test_missing_share_table_reports_all_rows_and_modeled_rows() -> None:
    df = _frame()
    modeled = df["seed"].notna()
    out = missing_share_table(df, ["seed", "Barthag", "AdjOE"], modeled).set_index("feature")
    assert out.loc["seed", "missing_all_rows_pct"] == pytest.approx(40.0)
    assert out.loc["seed", "missing_modeled_rows_pct"] == pytest.approx(0.0)
    assert out.loc["Barthag", "missing_all_rows_pct"] == pytest.approx(20.0)
    assert out.loc["AdjOE", "missing_all_rows_pct"] == pytest.approx(0.0)


def test_save_missingness_chart_writes_a_figure(tmp_path: Path) -> None:
    df = _frame()
    table = missing_share_table(df, list(df.columns), df["seed"].notna())
    path = tmp_path / "missing.png"
    save_missingness_chart(table, path, n_all=len(df), n_modeled=3)
    assert path.stat().st_size > 0


def _collinear_design(n: int = 400) -> tuple[pd.DataFrame, pd.Series]:
    rng = np.random.default_rng(3)
    seed = rng.integers(1, 17, n).astype(float)
    X = pd.DataFrame({"seed": seed, "seed_2": seed**2, "noise": rng.normal(size=n)})
    y = pd.Series(4.0 - 0.6 * seed + 0.02 * seed**2 + rng.normal(0, 0.5, n))
    return X, y


def test_describe_winner_gives_linear_winners_signed_coefficients() -> None:
    # The old table was |coef| labelled importance, which hid that seed and seed_2 pull opposite ways.
    X, y = _collinear_design()
    out, _ = describe_winner("ridge", {"alpha": 0.1}, X, y, random_state=0)
    assert list(out.columns) == ["feature", "coef"]
    coef = out.set_index("feature")["coef"]
    assert coef["seed"] < 0 < coef["seed_2"]
    assert out["coef"].abs().is_monotonic_decreasing


def test_describe_winner_keeps_the_sign_of_permutation_importance() -> None:
    X, y = _collinear_design()
    out, _ = describe_winner("gbr", {"n_estimators": 20, "max_depth": 2, "learning_rate": 0.1}, X, y, random_state=0)
    assert list(out.columns) == ["feature", "mse_increase", "mse_increase_std"]
    assert out["mse_increase"].is_monotonic_decreasing
    assert out.iloc[0]["feature"] in {"seed", "seed_2"}
