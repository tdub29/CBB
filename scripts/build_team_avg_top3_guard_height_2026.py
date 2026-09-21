#!/usr/bin/env python3
"""
Build a per-team stat: average height (inches) of the 3 non-C / non-PF/C players
with the most minutes on each team, for year 2026 only.

Reads: data/raw/player_stats/player_advanced_all.csv
Writes: data/processed/team_avg_top3_guard_height_2026.csv (team, avg_top3_guard_height_in)
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

import pandas as pd

project_root = Path(__file__).resolve().parent.parent
PLAYER_ADVANCED = project_root / "data" / "raw" / "player_stats" / "player_advanced_all.csv"
OUT_CSV = project_root / "data" / "processed" / "team_avg_top3_guard_height_2026.csv"
YEAR = 2026
BIG_POSITIONS = {"C", "PF/C"}

# Height format "6-9" -> inches
HEIGHT_PATTERN = re.compile(r"^(\d)-(\d{1,2})$")


def height_to_inches(height) -> float | None:
    if pd.isna(height) or height is None or not str(height).strip():
        return None
    s = str(height).strip()
    m = HEIGHT_PATTERN.match(s)
    if not m:
        return None
    feet, inches = int(m.group(1)), int(m.group(2))
    return feet * 12 + inches


def main() -> int:
    if not PLAYER_ADVANCED.exists():
        print(f"Missing {PLAYER_ADVANCED}")
        return 1

    df = pd.read_csv(PLAYER_ADVANCED, low_memory=False)
    df = df[df["year"] == YEAR].copy()
    if df.empty:
        print(f"No rows for year {YEAR}")
        return 1

    # Non-big only (exclude C and PF/C)
    df = df[~df["pos"].isin(BIG_POSITIONS)].copy()
    df["height_in"] = df["height"].map(height_to_inches)
    df = df[df["height_in"].notna()].copy()
    if df.empty:
        print("No non-C/PF/C rows with valid height for 2026")
        return 1

    # Top 3 by minutes per team
    df = df.sort_values("min", ascending=False)
    top3 = df.groupby("team").head(3)
    agg = top3.groupby("team").agg(
        avg_top3_guard_height_in=("height_in", "mean"),
        n_players_used=("height_in", "count"),
    ).reset_index()
    agg["avg_top3_guard_height_in"] = agg["avg_top3_guard_height_in"].round(2)

    OUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    agg.to_csv(OUT_CSV, index=False)
    print(f"Wrote {OUT_CSV} ({len(agg)} teams)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
