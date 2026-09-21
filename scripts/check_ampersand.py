#!/usr/bin/env python3
"""Check raw bytes of A&M team names in postseason vs history."""
import pandas as pd
from pathlib import Path

root = Path(__file__).resolve().parent.parent
post = pd.read_csv(root / "data/raw/trank_postseason_final.csv", header=None)
hist = pd.read_csv(root / "data/raw/team_history_cache.csv")

post_tam = sorted(post[post[0].str.contains("Texas A", na=False)][0].unique())
hist_tam = sorted(hist[hist["team"].str.contains("Texas A", na=False)]["team"].unique())

print("Postseason 'Texas A*' names:")
for t in post_tam:
    print(f"  {repr(t)}  hex: {t.encode('utf-8').hex()}")

print("\nHistory 'Texas A*' names:")
for t in hist_tam:
    print(f"  {repr(t)}  hex: {t.encode('utf-8').hex()}")

# Also check William & Mary
post_wm = sorted(post[post[0].str.contains("William", na=False)][0].unique())
hist_wm = sorted(hist[hist["team"].str.contains("William", na=False)]["team"].unique())
print("\nPostseason 'William*':", [repr(t) for t in post_wm])
print("History 'William*':", [repr(t) for t in hist_wm])
