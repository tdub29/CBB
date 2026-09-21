#!/usr/bin/env python3
"""Cross-check short seeds in tournament file against team_history_cache."""
import pandas as pd
from pathlib import Path

root = Path(__file__).resolve().parent.parent
tour = pd.read_csv(root / "data/raw/assembled/all_trank_team_tables_tournament.csv")
hist = pd.read_csv(root / "data/raw/team_history_cache.csv")

tour_no68 = tour[tour["finish"].astype(str) != "R68"].copy()
tour_no68["seed"] = tour_no68["seed"].astype(int)

hist_t = hist[hist["seed"].notna()].copy()
hist_t["seed"] = hist_t["seed"].astype(int)
hist_t = hist_t[(hist_t["year"] != 2020) & (hist_t["finish"] != "R68")]

for year in sorted(tour_no68["Year"].unique()):
    ty = tour_no68[tour_no68["Year"] == year]
    seed_counts = ty.groupby("seed").size()
    short_seeds = [s for s in range(1, 17) if seed_counts.get(s, 0) < 4]
    if not short_seeds:
        continue

    print(f"--- {year}: {len(ty)} rows (expected 64) ---")
    for s in short_seeds:
        n = seed_counts.get(s, 0)
        hist_ys = hist_t[(hist_t["year"] == year) & (hist_t["seed"] == s)]
        tour_teams = set(ty[ty["seed"] == s]["Team"])
        hist_teams = set(hist_ys["team"])
        missing = hist_teams - tour_teams
        present_str = ", ".join(sorted(tour_teams))
        print(f"  Seed {s}: {n}/4 in tournament file. Present: [{present_str}]")
        if missing:
            for team in sorted(missing):
                row = hist_ys[hist_ys["team"] == team].iloc[0]
                print(f"    MISSING: {row['team']} (finish={row['finish']}, conf={row['conf']}, coach={row['coach']})")
        else:
            print(f"    History also only has {len(hist_teams)} for this seed — gap is in history too")
    print()
