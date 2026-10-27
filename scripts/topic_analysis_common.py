#!/usr/bin/env python3
"""Constants and helpers shared by the per-topic *_analysis_common.py modules.

Each of these was copy-pasted unchanged into several topic modules. Those modules now import
them from here and re-export them, so the topic scripts' imports did not change.
size_analysis_common keeps its own styled_box, normalize_team_key and load_tournament_data,
and experience_analysis_common its own load_tournament_data, because those versions differ.
"""
from __future__ import annotations

from pathlib import Path
import re
import sys
import unicodedata

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from src.visualization.palette import PROJECT_PALETTE

FINISH_ORDER = [
    "R64",
    "R32",
    "Sweet Sixteen",
    "Elite Eight",
    "Final Four",
    "Finals",
    "CHAMPS",
]
FINISH_DEPTH = {
    "R64": 1,
    "R32": 2,
    "Sweet Sixteen": 3,
    "Elite Eight": 4,
    "Final Four": 5,
    "Finals": 6,
    "CHAMPS": 7,
}
FINISH_DISPLAY = {
    "R64": "R64",
    "R32": "R32",
    "Sweet Sixteen": "Sweet 16",
    "Elite Eight": "Elite 8",
    "Final Four": "Final 4",
    "Finals": "Finals",
    "CHAMPS": "Champ",
}
SEED_GROUP_MAP = {
    1: "1-2",
    2: "1-2",
    3: "3-4",
    4: "3-4",
    5: "5-6",
    6: "5-6",
    7: "7-8",
    8: "7-8",
    9: "9-10",
    10: "9-10",
    11: "11-13",
    12: "11-13",
    13: "11-13",
    14: "14-16",
    15: "14-16",
    16: "14-16",
}
SEED_GROUP_ORDER = ["1-2", "3-4", "5-6", "7-8", "9-10", "11-13", "14-16"]
CURRENT_FIELD_YEAR = 2026
TRANSFER_PORTAL_YEAR = 2022
ERAS = [
    ("2008-2011", 2008, 2011),
    ("2012-2021", 2012, 2021),
    ("2022-2025", 2022, 2025),
]
PROJECTED_FIELD_ALIASES = {
    "Connecticut": "UConn",
    "Miami FL": "Miami",
    "McNeese St.": "McNeese",
}

ACCENT = PROJECT_PALETTE["signal_blue"]
SECONDARY = PROJECT_PALETTE["steel"]
HIGHLIGHT = PROJECT_PALETTE["sage"]
TEXT_DARK = PROJECT_PALETTE["fog"]
POSITIVE = PROJECT_PALETTE["sage"]
NEGATIVE = "#B26B67"


def styled_box(
    facecolor: str | None = None,
    edgecolor: str | None = None,
    linewidth: float = 1.15,
    alpha: float = 0.97,
) -> dict[str, object]:
    return {
        "facecolor": facecolor or PROJECT_PALETTE["midnight"],
        "edgecolor": edgecolor or PROJECT_PALETTE["sky_blue"],
        "linewidth": linewidth,
        "alpha": alpha,
        "boxstyle": "round,pad=0.24",
    }


def format_year_tick(value: float, _: object) -> str:
    return str(int(value)) if float(value).is_integer() else ""


def clean_projected_team_name(value: object) -> str:
    text = str(value).replace("\xa0", " ").strip()
    text = re.sub(r"\*+", "", text)
    text = re.sub(r"\s+\d+$", "", text).strip()
    return text


def normalize_team_key(value: object) -> str:
    text = unicodedata.normalize("NFKD", str(value)).encode("ascii", "ignore").decode("ascii")
    text = text.lower().replace("&", "and")
    text = re.sub(r"\bsaint\b", "st", text)
    text = re.sub(r"\bstate\b", "st", text)
    text = re.sub(r"\bmount\b", "mt", text)
    text = re.sub(r"\buniversity\b", "", text)
    text = re.sub(r"\bof\b", "", text)
    text = re.sub(r"[^a-z0-9]+", "", text)
    alias_map = {
        "connecticut": "uconn",
        "uconn": "uconn",
        "miamifl": "miami",
        "miami": "miami",
        "mcneesest": "mcneese",
        "mcneese": "mcneese",
    }
    return alias_map.get(text, text)


def load_tournament_data(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out = out[out["year"] != 2020].copy()
    out = out[out["seed"].notna() & out["finish"].notna()].copy()
    out = out[out["finish"] != "R68"].copy()
    out = out[out["finish"].isin(FINISH_ORDER)].copy()
    out["seed_int"] = out["seed"].astype(int)
    out["finish_depth"] = out["finish"].map(FINISH_DEPTH)
    out["finish_label"] = out["finish"].map(FINISH_DISPLAY)
    out["seed_group"] = out["seed_int"].map(SEED_GROUP_MAP)
    return out
