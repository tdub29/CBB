#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path
import re
import unicodedata

import pandas as pd


def normalize_team_key(value: object) -> str:
    text = unicodedata.normalize("NFKD", str(value)).encode("ascii", "ignore").decode("ascii")
    text = text.lower().replace("&", "and")
    text = re.sub(r"\bsaint\b", "st", text)
    text = re.sub(r"\bstate\b", "st", text)
    text = re.sub(r"\bmount\b", "mt", text)
    text = re.sub(r"\buniversity\b", "", text)
    text = re.sub(r"\bof\b", "", text)
    text = re.sub(r"[^a-z0-9]+", "", text)
    return text


def position_has_token(value: object, token: str) -> bool:
    if pd.isna(value):
        return False
    parts = [part for part in re.split(r"[^A-Za-z]+", str(value).upper()) if part]
    return token.upper() in parts


def compute_max_porpag_feature(
    player_stats_path: Path,
    position_token: str,
    feature_name: str,
) -> pd.DataFrame:
    players = pd.read_csv(player_stats_path, usecols=["team", "year", "pos", "porpag"])
    players["year"] = pd.to_numeric(players["year"], errors="coerce")
    players["porpag"] = pd.to_numeric(players["porpag"], errors="coerce")
    players = players[players["year"].notna() & players["porpag"].notna()].copy()
    players = players[players["pos"].apply(lambda value: position_has_token(value, position_token))].copy()
    feature_df = (
        players.groupby(["team", "year"], as_index=False)["porpag"]
        .max()
        .rename(columns={"team": "team_raw", "porpag": feature_name})
    )
    feature_df["year"] = feature_df["year"].astype(int)
    feature_df["team_key"] = feature_df["team_raw"].map(normalize_team_key)
    feature_df = (
        feature_df.sort_values(feature_name, ascending=False, kind="stable")
        .drop_duplicates(["team_key", "year"], keep="first")
        .reset_index(drop=True)
    )
    return feature_df


def merge_feature_into_dataset(
    df: pd.DataFrame,
    feature_df: pd.DataFrame,
    feature_name: str,
    team_col: str = "Team",
    year_col: str = "year",
    fill_value: float = 0.0,
) -> pd.DataFrame:
    out = df.copy()
    out["_team_key"] = out[team_col].map(normalize_team_key)
    out[year_col] = pd.to_numeric(out[year_col], errors="coerce")
    merged = out.merge(
        feature_df[["team_key", "year", feature_name]],
        left_on=["_team_key", year_col],
        right_on=["team_key", "year"],
        how="left",
        sort=False,
    )
    merged[feature_name] = pd.to_numeric(merged[feature_name], errors="coerce").fillna(fill_value)
    merged = merged.drop(columns=["_team_key", "team_key"])
    return merged
