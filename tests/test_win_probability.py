from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression

from src.models.win_probability import compute_strength, p_win, seed_fallback_strength


def test_p_win_is_bounded_and_symmetric() -> None:
    p_a = p_win(1.75, -0.25, k=1.2)
    p_b = p_win(-0.25, 1.75, k=1.2)
    assert 0.0 <= p_a <= 1.0
    assert 0.0 <= p_b <= 1.0
    assert np.isclose(p_a + p_b, 1.0, atol=1e-10)


def test_compute_strength_builds_power_features() -> None:
    df = pd.DataFrame({"x": [1.0, 2.0, 3.0]})
    X_train = pd.DataFrame({"x": [1.0, 2.0, 3.0], "x_2": [1.0, 4.0, 9.0]})
    y_train = np.array([2.0, 3.0, 4.0])
    model = LinearRegression().fit(X_train, y_train)

    preds = compute_strength(df, model, ["x", "x_2"])
    assert len(preds) == 3
    assert preds.notna().all()


def test_seed_fallback_strength_prefers_better_seeds() -> None:
    one_seed = seed_fallback_strength(1)
    sixteen_seed = seed_fallback_strength(16)
    assert one_seed > sixteen_seed
