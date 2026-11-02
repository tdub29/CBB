"""Topic loaders keep completed tournaments before the projected field's season.

The team-history cache carries 2026 seeds and finishes, so a rebuilt topic sample has seeded 2026 rows.
Those must not enter the 2008-2025 statistics or the seed-line baselines the 2026 field is compared with.
"""

from __future__ import annotations

import importlib
from pathlib import Path
import sys

import pandas as pd
import pytest

SCRIPTS_DIR = Path(__file__).resolve().parents[1] / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

TOPICS = ["experience", "free_throw_rate", "rebounding", "size", "tempo", "three_point", "turnovers"]


def _sample() -> pd.DataFrame:
    common = importlib.import_module("experience_analysis_common")
    df = pd.DataFrame(
        {
            "Team": ["Alpha", "Beta", "Gamma", "Delta"],
            "year": [2019, 2020, 2025, 2026],
            "seed": [1.0, 2.0, 3.0, 4.0],
            "finish": ["R64", "R32", "Sweet Sixteen", "R32"],
            "Conf": ["ACC", "SEC", "B10", "B12"],
        }
    )
    for col in ["EFFHGT", "AVGHGT", "Elite SOS", "conf_avg_barthag", "Barthag",
                common.PLAYER_PRIMARY_METRIC, common.PLAYER_SUPPORT_METRIC]:
        df[col] = 1.0
    return df


@pytest.mark.parametrize("topic", TOPICS)
def test_topic_loader_drops_the_projected_field_season(topic: str) -> None:
    common = importlib.import_module(f"{topic}_analysis_common")
    out = common.load_tournament_data(_sample())
    assert out["year"].tolist() == [2019, 2025]


def test_sos_loaders_drop_the_projected_field_season(tmp_path: Path) -> None:
    import sos_analysis_02_04 as sos
    import sos_analysis_05_07 as sos57

    path = tmp_path / "sos_sample.csv"
    _sample().to_csv(path, index=False)
    assert sos.load_tournament_data(path)["year"].tolist() == [2019, 2025]
    assert sos57.load_tournament_data(sos57.load_data(path))["year"].tolist() == [2019, 2025]
