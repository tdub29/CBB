#!/usr/bin/env python3
"""Filter all_trank_team_tables.csv to rows where finish has a value. Writes all_trank_team_tables_tournament.csv."""
import pandas as pd
from pathlib import Path

path = Path(__file__).resolve().parent.parent / "data/raw/assembled/all_trank_team_tables.csv"
out_path = path.parent / "all_trank_team_tables_tournament.csv"
df = pd.read_csv(path)
mask = df["finish"].notna() & (df["finish"].astype(str).str.strip() != "")
df_filtered = df[mask]
df_filtered.to_csv(out_path, index=False)
print(f"{len(df)} -> {len(df_filtered)} rows. Saved: {out_path}")
