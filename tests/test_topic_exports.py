"""Topic report exports carry repo-relative logo paths, not the path of the machine that ran them."""

from __future__ import annotations

from pathlib import Path
import sys

import numpy as np
import pandas as pd

SCRIPTS_DIR = Path(__file__).resolve().parents[1] / "scripts"
if str(SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPTS_DIR))

import size_analysis_common as sac  # noqa: E402


def test_repo_relative_path() -> None:
    logo = sac.REPO_ROOT / "data" / "external" / "team_logos" / "espn_ncaam" / "2006.png"
    assert sac.repo_relative_path(logo) == "data/external/team_logos/espn_ncaam/2006.png"
    assert sac.repo_relative_path(str(logo)) == "data/external/team_logos/espn_ncaam/2006.png"
    assert sac.repo_relative_path(None) is None
    assert np.isnan(sac.repo_relative_path(np.nan))


def test_export_copy_leaves_the_plotting_table_alone() -> None:
    logo = str(sac.REPO_ROOT / "data" / "external" / "team_logos" / "espn_ncaam" / "2006.png")
    table = pd.DataFrame({"team": ["Akron", "Unknown"], "logo_path": [logo, None]})
    exported = sac.with_repo_relative_logo_paths(table)
    assert exported.loc[0, "logo_path"] == "data/external/team_logos/espn_ncaam/2006.png"
    assert pd.isna(exported.loc[1, "logo_path"])
    assert table.loc[0, "logo_path"] == logo
    no_logo = pd.DataFrame({"team": ["Akron"]})
    assert sac.with_repo_relative_logo_paths(no_logo) is no_logo
