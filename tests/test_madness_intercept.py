import numpy as np

from src.pipelines import madness as mad


def test_regression_model_keeps_its_intercept():
    rng = np.random.default_rng(0)
    X = rng.normal(size=(60, 3))
    y = 2.0 + X @ np.array([0.5, -0.3, 0.1])  # mean tournament wins sits well above 0
    for alpha in (0.0, 1.0):
        pred = mad._build_regression_model(alpha).fit(X, y).predict(X)
        assert abs(pred.mean() - y.mean()) < 1e-9
    legacy = mad._build_regression_model(1.0, fit_intercept=False).fit(X, y).predict(X)
    assert abs(legacy.mean()) < 0.05  # the old bug: centred on 0 wins
