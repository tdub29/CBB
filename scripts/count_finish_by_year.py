#!/usr/bin/env python3
"""Row count by year where finish != R68 (and != R64). Identify missing seeds per year."""
import pandas as pd
from pathlib import Path

path = Path(__file__).resolve().parent.parent / "data/raw/assembled/all_trank_team_tables_tournament.csv"
df = pd.read_csv(path)

# Filter to finish != R68 (main bracket only, 64 teams expected per year)
sub = df[df["finish"].astype(str) != "R68"].copy()
sub["seed"] = sub["seed"].astype(int)

# For each year, expected seeds 1-16 each with count 4. Find missing or undercounted.
print("=== Missing seeds by year (finish != R68; expected 4 of each seed 1-16) ===\n")
for year in sorted(sub["Year"].unique()):
    y = sub[sub["Year"] == year]
    cnt = y.groupby("seed").size()
    expected_seeds = set(range(1, 17))
    present = set(cnt.index)
    missing = sorted(expected_seeds - present)
    short = [s for s in expected_seeds if s in present and cnt[s] < 4]
    short_str = [f"seed {s}(n={cnt[s]})" for s in short]
    missing_str = f"missing seeds: {missing}" if missing else "none missing"
    short_str = f"; short: {short_str}" if short_str else ""
    print(f"{year}: {len(y)} rows. {missing_str}{short_str}")
