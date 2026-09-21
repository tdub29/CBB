from __future__ import annotations

import numpy as np
import pandas as pd

from src.models.simulate import ROUND_COLUMNS, simulate_bracket


def _sample_bracket() -> pd.DataFrame:
    rows = []
    for region in ["East", "West", "South", "Midwest"]:
        for seed in range(1, 17):
            rows.append({"Team": f"{region}_{seed}", "Seed": seed, "Region": region})
    return pd.DataFrame(rows)


def _strength_lookup(df: pd.DataFrame) -> dict[str, float]:
    # Stronger values for lower seed numbers.
    return {row.Team: float(20 - row.Seed) for row in df.itertuples(index=False)}


def test_simulate_bracket_outputs_probabilities() -> None:
    bracket = _sample_bracket()
    lookup = _strength_lookup(bracket)
    out = simulate_bracket(bracket, lookup, n_sims=250, random_state=42)

    assert len(out) == 64
    for col in ROUND_COLUMNS:
        assert col in out.columns
        assert out[col].between(0.0, 1.0).all()

    assert np.isclose(out["p_Champ"].sum(), 1.0, atol=1e-12)
    assert out["expected_round"].between(0.0, 6.0).all()


def test_stronger_seed_has_higher_title_probability() -> None:
    bracket = _sample_bracket()
    lookup = _strength_lookup(bracket)
    out = simulate_bracket(bracket, lookup, n_sims=300, random_state=7)
    champ_probs = dict(zip(out["Team"], out["p_Champ"], strict=False))

    assert champ_probs["East_1"] > champ_probs["East_16"]
