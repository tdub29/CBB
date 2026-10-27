"""Tournament outcome encodings: Torvik finish labels, result codes, rounds and seeds."""

from __future__ import annotations

import re
from typing import Any

import numpy as np
import pandas as pd


def _finish_to_result(finish: Any) -> float:
    """Map Torvik finish labels to result code used by pipeline."""
    s = str(finish).strip().lower()
    if not s or s in ("nan", "na", "proj.", "(proj.)"):
        return np.nan
    if "champ" in s:
        return 1.0
    if "finals" in s or "runner" in s or "2nd" in s:
        return 2.0
    if "final four" in s or s == "f4":
        return 4.0
    if "elite eight" in s or s == "e8":
        return 8.0
    if "sweet sixteen" in s or "sweet 16" in s or s == "s16":
        return 16.0
    if "r32" in s or s == "32":
        return 32.0
    if "r64" in s or "first four" in s or s == "64":
        return 64.0
    return np.nan


def _finish_to_wl(finish: Any) -> tuple[int, int]:
    """Map finish string to NCAA tournament wins/losses."""
    r = _finish_to_result(finish)
    if pd.isna(r):
        return (0, 0)
    rv = int(r)
    if rv == 1:
        return (6, 0)
    if rv == 2:
        return (5, 1)
    if rv == 4:
        return (4, 1)
    if rv == 8:
        return (3, 1)
    if rv == 16:
        return (2, 1)
    if rv == 32:
        return (1, 1)
    return (0, 1)


def _extract_seed(team: str) -> float:
    """Extract seed from Team string (e.g. 'Duke 1 seed' -> 1)."""
    m = re.search(r"(\d+)\s*seed", str(team), re.I)
    return float(m.group(1)) if m else np.nan


def _result_to_round(result: Any) -> float:
    """Map result code to round (64->0, 32->1, ..., 1->6)."""
    m = {"64": 0, "32": 1, "16": 2, "8": 3, "4": 4, "2": 5, "1": 6}
    return m.get(str(int(result)) if pd.notna(result) else "", np.nan)
