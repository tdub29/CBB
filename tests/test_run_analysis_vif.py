from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from scripts.run_analysis import compute_vif


def _correlated_columns(n: int = 2000) -> pd.DataFrame:
    # Uncentred like the real features (AdjOE and AdjDE sit near 103, Barthag near 0.5).
    rng = np.random.default_rng(7)
    z = rng.normal(size=(n, 3))
    return pd.DataFrame(
        {
            "a": 103.0 + 7.0 * z[:, 0],
            "b": 100.0 + 5.0 * (0.8 * z[:, 0] + 0.6 * z[:, 1]),
            "c": 0.5 + 0.2 * (0.3 * z[:, 1] + 0.95 * z[:, 2]),
        }
    )


def test_compute_vif_matches_the_inverse_correlation_matrix() -> None:
    # Closed form: VIF_j is the j-th diagonal entry of the inverse correlation matrix.
    X = _correlated_columns()
    expected = dict(zip(X.columns, np.diag(np.linalg.inv(np.corrcoef(X.to_numpy(), rowvar=False)))))
    got = compute_vif(X).set_index("feature")["vif"]
    for col in X.columns:
        assert got[col] == pytest.approx(expected[col], rel=1e-9)


def test_compute_vif_two_columns_is_one_over_one_minus_r_squared() -> None:
    X = _correlated_columns()[["a", "b"]]
    r = np.corrcoef(X["a"], X["b"])[0, 1]
    got = compute_vif(X).set_index("feature")["vif"]
    assert got["a"] == pytest.approx(1.0 / (1.0 - r**2), rel=1e-9)
    assert got["b"] == pytest.approx(1.0 / (1.0 - r**2), rel=1e-9)


def test_compute_vif_does_not_depend_on_column_offsets() -> None:
    X = _correlated_columns()
    shifted = X + np.array([-103.0, 1000.0, 50.0])
    a = compute_vif(X).set_index("feature")["vif"].sort_index()
    b = compute_vif(shifted).set_index("feature")["vif"].sort_index()
    pd.testing.assert_series_equal(a, b, rtol=1e-9)
