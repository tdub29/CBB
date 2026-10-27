"""Training-frame assembly: table joins, numeric cleanup, outcome columns and the clean dataset."""

from __future__ import annotations

import re
from pathlib import Path

import numpy as np
import pandas as pd

from src.features.outcomes import _extract_seed, _result_to_round


def _parse_rec_w(rec: pd.Series, multi_digit: bool = False) -> pd.Series:
    """Parse wins from Rec string (e.g. '32-4' -> 32)."""
    digits = "01234" if multi_digit else "01"
    out = []
    for v in rec.astype(str):
        if len(v) >= 2 and v[1] in digits:
            out.append(v[:2] if len(v) > 1 else v[0])
        else:
            out.append(v[0] if v else "0")
    return pd.Series(out, index=rec.index).astype(float)


def _parse_rec_l(rec: pd.Series, multi_digit: bool = False) -> pd.Series:
    """Parse losses from Rec string."""
    digits = "01234" if multi_digit else "01"
    out = []
    for v in rec.astype(str):
        if len(v) >= 4 and v[1] in digits:
            out.append(v[3] if len(v) > 3 else "0")
        elif len(v) >= 3:
            out.append(v[2])
        else:
            out.append("0")
    return pd.Series(out, index=rec.index).astype(float)


def _clean_wab(val: str) -> str:
    """Clean WAB string (rank suffix, sign handling)."""
    val = str(val).strip()
    if not val:
        return "0"
    sign = val[0] if val[0] in "+-" else ""
    decimal_pos = val.find(".")
    if decimal_pos < 0:
        decimal_pos = len(val)
    if sign == "-":
        if decimal_pos == 3:
            return val[:4]
        if decimal_pos >= 4:
            return val[:5]
    if sign == "+" or (decimal_pos == 3 and sign != "-"):
        return val[1:4] if len(val) > 1 else val
    return val[2:5] if len(val) > 2 else val


def _truncate_numeric_col(val: str, has_decimal: bool, is_adj: bool = False) -> str:
    """Truncate numeric string for parsing (mimics R substr logic)."""
    val = str(val)
    if has_decimal:
        return val[:4]
    if is_adj and val.startswith("1"):
        return val[:5]
    return val[:4] if is_adj else val[:2]


def _get_rec_column(df: pd.DataFrame) -> str:
    """Find Rec column (handles multi-level or renamed columns like 'D-I Avg:_Rec')."""
    for c in df.columns:
        s = str(c).strip().lower()
        if s == "rec" or s.endswith("_rec") or "_rec" in s:
            return str(c)
    if "Rec" in df.columns:
        return "Rec"
    raise KeyError(f"No 'Rec' column found. Columns: {list(df.columns)}")


def build_features(
    master: pd.DataFrame,
    conf: pd.DataFrame,
    tourn: pd.DataFrame,
    ext_data_dir: Path,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Join tables, engineer features, return (mastfin, newmastfin)."""
    if master.empty or conf.empty or tourn.empty:
        raise ValueError("Missing scraped data")

    # Tournament W/L
    tourn = tourn.copy()
    rec_t = _get_rec_column(tourn)
    rec_series = tourn[rec_t].squeeze() if hasattr(tourn[rec_t], "squeeze") else tourn[rec_t]
    if isinstance(rec_series, pd.DataFrame):
        rec_series = rec_series.iloc[:, 0]
    tourn["tourn_w"] = _parse_rec_w(rec_series, multi_digit=False)
    tourn["tourn_l"] = _parse_rec_l(rec_series, multi_digit=False)

    # Conf W/L
    conf = conf.copy()
    rec_c = _get_rec_column(conf)
    rec_series_c = conf[rec_c].squeeze() if hasattr(conf[rec_c], "squeeze") else conf[rec_c]
    if isinstance(rec_series_c, pd.DataFrame):
        rec_series_c = rec_series_c.iloc[:, 0]
    conf["conf_w"] = _parse_rec_w(rec_series_c, multi_digit=True)
    conf["conf_l"] = _parse_rec_l(rec_series_c, multi_digit=True)

    # Join conf + tourn
    joined = conf.merge(
        tourn[["Team", "year", "tourn_w", "tourn_l"]],
        on=["Team", "year"],
        how="left",
    )
    joined["W"] = joined["conf_w"] - joined["tourn_w"].fillna(0)
    joined["L"] = joined["conf_l"] - joined["tourn_l"].fillna(0)

    # Join master + conf W/L
    final = master.merge(
        joined[["Team", "year", "W", "L"]],
        on=["Team", "year"],
        how="left",
    )
    # Deduplicate columns (merge can create duplicates with some table structures)
    final = final.loc[:, ~final.columns.duplicated()]

    return _finalize_training_frame(final, ext_data_dir, skip_ext_merge=False)


def _finalize_training_frame(
    final: pd.DataFrame,
    ext_data_dir: Path,
    skip_ext_merge: bool = False,
    skip_coach_merge: bool = False,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Shared finalization: sort, seed/result, numerics, round, optional ext/coach merge, Power6, training_data split."""
    final = final.loc[:, ~final.columns.duplicated()]
    # Sort by primary stat (needed for downstream); use fallback if AdjOE missing
    sort_col = None
    for col in ("AdjOE", "Barthag", "WAB"):
        if col in final.columns and final[col].notna().any():
            sort_col = col
            break
    if sort_col is not None:
        # Stable sort: ties keep their input order. The default quicksort may order ties
        # differently across numpy versions and CPUs, so the written CSV would not reproduce.
        final = final.sort_values(sort_col, ascending=False, kind="mergesort").reset_index(drop=True)

    # Seed from Team (if not already present from team-table + tourn merge)
    if "seed" not in final.columns or final["seed"].isna().all():
        final["seed"] = final["Team"].apply(_extract_seed)

    # Result/round from Team string (if not already present)
    result_map = {
        "R32": 32, "R64": 64, "Finals": 2, "CHAMPS": 1,
        "Elite Eight": 8, "Sweet Sixteen": 16, "Final Four": 4,
    }
    if "result" not in final.columns or final["result"].isna().all():
        final["result"] = np.nan
        for pat, val in result_map.items():
            final.loc[final["Team"].astype(str).str.contains(pat, regex=False), "result"] = val

    # Clean numeric columns (WAB, AdjOE, AdjDE, etc.)
    if "WAB" in final.columns:
        final["WAB"] = final["WAB"].astype(str).apply(_clean_wab)

    numeric_cols = [
        "AdjOE", "AdjDE", "Barthag", "EFG%", "EFGD%", "TOR", "TORD",
        "ORB", "DRB", "FTR", "FTRD", "2P%", "2P%D", "3P%", "3P%D", "Adj T.",
    ]
    for c in numeric_cols:
        if c in final.columns:
            final[c] = pd.to_numeric(final[c], errors="coerce")

    if "WAB" in final.columns:
        final["WAB"] = pd.to_numeric(final["WAB"], errors="coerce")
    final["seed"] = pd.to_numeric(final["seed"], errors="coerce")

    # Round encoding
    final["round"] = final["result"].apply(
        lambda x: _result_to_round(x) if pd.notna(x) else np.nan
    )

    final["W"] = final["W"].fillna(1)
    final["L"] = final["L"].fillna(1)

    # Clean Team (remove seed suffix)
    final["Team"] = final["Team"].astype(str).str.replace(
        r"\s+\d+\s*seed.*$", "", regex=True
    ).str.strip()

    ext_cols = ["WINS", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED", "EFFHGT", "EXP.", "TALENT", "ELITESOS"]
    if not skip_ext_merge:
        # Optional external data (team-table CSVs merged into scraped base)
        col_names_ext = [
            "TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES",
            "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%",
            "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%",
            "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR",
            "PAKE", "PASE", "TALENT", "blank", "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS",
        ]
        combined = pd.DataFrame(columns=col_names_ext)
        ext_files = list(ext_data_dir.glob("trank_team_table_data*.csv"))
        for f in ext_files:
            try:
                df = pd.read_csv(f, header=None)
                df.columns = col_names_ext[: len(df.columns)]
                if "BARTHAG" in df.columns and "YEAR" in df.columns:
                    df["Rk"] = df.groupby("YEAR")["BARTHAG"].rank(ascending=False)
                combined = pd.concat([combined, df], ignore_index=True)
            except Exception:
                pass
        core_from_ext = ["ADJOE", "ADJDE", "BARTHAG"]
        if not combined.empty and "YEAR" in combined.columns and "TEAM" in combined.columns:
            merge_cols = [c for c in ext_cols if c in combined.columns]
            ext_core = [c for c in core_from_ext if c in combined.columns]
            right_cols = ["TEAM", "YEAR"] + [c for c in (merge_cols + ext_core) if c in combined.columns]
            final = final.merge(
                combined[right_cols],
                left_on=["Team", "year"],
                right_on=["TEAM", "YEAR"],
                how="left",
                suffixes=("", "_ext"),
            )
            for dst, src in [("AdjOE", "ADJOE"), ("AdjDE", "ADJDE"), ("Barthag", "BARTHAG")]:
                if dst in final.columns and src in final.columns:
                    final[dst] = final[dst].fillna(pd.to_numeric(final[src], errors="coerce"))
            for c in merge_cols:
                if f"{c}_ext" in final.columns:
                    if c in final.columns:
                        final[c] = final[c].fillna(final[f"{c}_ext"])
                    else:
                        final[c] = final[f"{c}_ext"]
                    final = final.drop(columns=[f"{c}_ext"], errors="ignore")
                elif c not in final.columns:
                    final[c] = 0
            final = final.drop(columns=["TEAM", "YEAR"] + [c for c in core_from_ext if c in final.columns], errors="ignore")
            final = final.loc[:, ~final.columns.duplicated()]
    for c in ext_cols:
        if c not in final.columns:
            final[c] = 0

    # Optional coach tournament experience (last 5 years: appearances, wins, losses)
    coach_cols = ["coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y", "coach_years_with_program"]
    if not skip_coach_merge and not any(c in final.columns for c in coach_cols):
        coach_files = list(ext_data_dir.rglob("coach_tournament_experience*.csv"))
        for f in coach_files:
            try:
                coach_df = pd.read_csv(f)
                merge_coach = [c for c in coach_cols if c in coach_df.columns]
                if "Team" in coach_df.columns and "Year" in coach_df.columns and merge_coach:
                    right = coach_df[["Team", "Year"] + merge_coach].rename(columns={"Year": "Year_coach"})
                    final = final.merge(
                        right,
                        left_on=["Team", "year"],
                        right_on=["Team", "Year_coach"],
                        how="left",
                    )
                    final = final.drop(columns=["Year_coach"], errors="ignore")
                    break
            except Exception:
                pass
    for c in coach_cols:
        if c not in final.columns:
            final[c] = 0

    # Derived coach features (from appearances, wins, losses)
    wins = final["coach_tourn_wins_5y"].fillna(0).astype(float)
    losses = final["coach_tourn_losses_5y"].fillna(0).astype(float)
    apps = final["coach_tourn_appearances_5y"].fillna(0).astype(float)
    games = wins + losses
    final["coach_tourn_games_5y"] = games
    final["coach_tourn_win_pct_5y"] = np.where(games > 0, wins / games, 0.0)
    final["coach_avg_wins_per_appearance"] = np.where(apps > 0, wins / apps, 0.0)

    # Power 6 / conference flags
    if "Conf" not in final.columns:
        final["Conf"] = ""
    power6 = ["B12", "B10", "SEC", "ACC", "P12", "BE"]
    final["Power6"] = (final["Conf"].isin(power6)).astype(int)
    for conf_abbr in ["B12", "B10", "SEC", "ACC", "P12", "BE", "MWC"]:
        final[conf_abbr] = (final["Conf"] == conf_abbr).astype(int)

    if "Rec" in final.columns:
        final["Rec"] = final["Rec"].astype(str).str[:2]

    training_data_full = final.copy()

    # Drop cols for modeling
    drop_cols = ["result", "TEAM"] + (
        [final.columns[0], final.columns[1], final.columns[2]] if len(final.columns) >= 3 else []
    )
    training_data = final.drop(columns=[c for c in drop_cols if c in final.columns], errors="ignore")

    training_data["Round2"] = (training_data["round"] >= 1).astype(int)
    training_data["S16"] = (training_data["round"] >= 2).astype(int)
    training_data["E8"] = (training_data["round"] >= 3).astype(int)
    training_data["F4"] = (training_data["round"] >= 4).astype(int)
    training_data["Ship"] = (training_data["round"] >= 5).astype(int)
    training_data["Champ"] = (training_data["round"] == 6).astype(int)

    round_cols = ["Round2", "S16", "E8", "F4", "Ship", "Champ"]
    for c in round_cols:
        if c in training_data.columns:
            training_data[c] = training_data[c].fillna(0)

    training_data["round"] = training_data["round"].fillna(0)

    optional = [
        "WINS", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED",
        "EFFHGT", "EXP.", "TALENT", "ELITESOS", "WAB",
        "coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y",
        "coach_years_with_program",
        "coach_tourn_games_5y", "coach_tourn_win_pct_5y", "coach_avg_wins_per_appearance",
    ]
    for c in optional:
        if c in training_data.columns:
            training_data[c] = training_data[c].fillna(0)

    training_data = training_data.dropna()

    for c in ["G", "3PR", "3PRD", "Rec"]:
        if c in training_data.columns:
            training_data[c] = pd.to_numeric(training_data[c], errors="coerce")

    return training_data, training_data_full


def build_clean_training_dataset(full: pd.DataFrame) -> pd.DataFrame:
    """Produce one clean, prepped dataset: no duplicate cols, no duplicate rows, all variables and outcomes.

    - Drops duplicate column names (keeps first).
    - Drops redundant 3P columns: keeps '3P%', '3PRATE', '3PRATED'; drops '3P Rate *_3PR' / '3P Rate *_3PRD'.
    - One row per (Team, year); drops duplicate rows.
    - Ensures outcome columns: result, round, Round2, S16, E8, F4, Ship, Champ.
    """
    out = full.copy()

    # Drop duplicate column names (keep first)
    out = out.loc[:, ~out.columns.duplicated()]

    # Drop redundant 3P columns (year-specific 3P line distance variants); keep canonical 3P%, 3PRATE, 3PRATED
    drop_3p = [c for c in out.columns if isinstance(c, str) and re.match(r"3P Rate .*_(3PR|3PRD)$", c.strip())]
    out = out.drop(columns=[c for c in drop_3p if c in out.columns], errors="ignore")

    # One row per (Team, year)
    if "Team" in out.columns and "year" in out.columns:
        out = out.drop_duplicates(subset=["Team", "year"], keep="first").reset_index(drop=True)

    # Ensure outcome columns from round
    if "round" in out.columns:
        r = pd.to_numeric(out["round"], errors="coerce").fillna(-1)
        out["Round2"] = (r >= 1).astype(int)
        out["S16"] = (r >= 2).astype(int)
        out["E8"] = (r >= 3).astype(int)
        out["F4"] = (r >= 4).astype(int)
        out["Ship"] = (r >= 5).astype(int)
        out["Champ"] = (r == 6).astype(int)

    # Split "Postseason rec" (e.g. "4–1") into postseason_wins and postseason_losses
    if "Postseason rec" in out.columns:
        rec = out["Postseason rec"].astype(str).str.strip()
        parts = rec.str.split(r"[–\-]", n=1, regex=True)
        wins = parts.str.get(0).replace("", float("nan"))
        losses = parts.str.get(1).replace("", float("nan"))
        out["postseason_wins"] = pd.to_numeric(wins, errors="coerce")
        out["postseason_losses"] = pd.to_numeric(losses, errors="coerce")
        out = out.drop(columns=["Postseason rec"])

    # Derive conference tournament W/L by subtracting NCAA tournament W/L from postseason totals
    if "postseason_wins" in out.columns and "round" in out.columns:
        r = pd.to_numeric(out["round"], errors="coerce")
        ncaa_wins = r.fillna(0)                                  # round = NCAA wins
        ncaa_losses = pd.Series(0, index=out.index, dtype=float)
        in_tourney = r.notna()
        ncaa_losses[in_tourney & (r < 6)] = 1.0                  # everyone except champ loses once
        # ncaa_losses stays 0 for champ (round==6) and non-NCAA (round==NaN)

        out["conf_tourn_wins"] = out["postseason_wins"] - ncaa_wins
        out["conf_tourn_losses"] = out["postseason_losses"] - ncaa_losses
        out = out.drop(columns=["postseason_wins", "postseason_losses"], errors="ignore")

    return out


def _tourn_team_clean(team_str: str) -> str:
    """Strip seed and result suffixes from tournament Team string for matching."""
    s = str(team_str).strip()
    s = re.sub(r"\s+\d+\s*seed.*$", "", s, flags=re.I).strip()
    for pat in ["R32", "R64", "Finals", "CHAMPS", "Elite Eight", "Sweet Sixteen", "Final Four"]:
        s = s.replace(pat, "").strip()
    return s.strip() or team_str


def merge_seed_result_from_tourn(base: pd.DataFrame, tourn: pd.DataFrame) -> pd.DataFrame:
    """Merge seed and result from tournament table into base (Team, year)."""
    if tourn.empty or "Team" not in tourn.columns or "year" not in tourn.columns:
        return base
    result_map = {
        "R32": 32, "R64": 64, "Finals": 2, "CHAMPS": 1,
        "Elite Eight": 8, "Sweet Sixteen": 16, "Final Four": 4,
    }
    tourn = tourn.copy()
    tourn["team_clean"] = tourn["Team"].astype(str).apply(_tourn_team_clean)
    tourn["seed"] = tourn["Team"].apply(_extract_seed)
    tourn["result"] = np.nan
    for pat, val in result_map.items():
        tourn.loc[tourn["Team"].astype(str).str.contains(pat, regex=False), "result"] = val
    enrich = tourn[["team_clean", "year", "seed", "result"]].drop_duplicates(subset=["team_clean", "year"], keep="first")
    enrich = enrich.rename(columns={"team_clean": "Team"})
    base = base.merge(enrich, on=["Team", "year"], how="left", suffixes=("", "_tourn"))
    for c in ["seed", "result"]:
        if f"{c}_tourn" in base.columns:
            base[c] = base[c].fillna(base[f"{c}_tourn"])
            base = base.drop(columns=[f"{c}_tourn"], errors="ignore")
    return base


# Stats from main Bart page (R table) to fill into team-table primary when missing
_REGULAR_TABLE_FILL_COLS = [
    "Rk", "Conf", "G", "Rec", "AdjOE", "AdjDE", "Barthag", "EFG%", "EFGD%",
    "TOR", "TORD", "ORB", "DRB", "FTR", "FTRD", "2P%", "2P%D", "3P%", "3P%D",
    "Adj T.", "WAB",
]


def merge_regular_into_base(base: pd.DataFrame, regular_df: pd.DataFrame) -> pd.DataFrame:
    """Merge all stat columns from regular-season (R) table into base on (Team, year). Fills base where missing."""
    if regular_df.empty or "Team" not in regular_df.columns or "year" not in regular_df.columns:
        return base
    r = regular_df.copy()
    r["Team"] = r["Team"].astype(str).str.replace(r"\s+\d+\s*seed.*$", "", regex=True).str.strip()
    # Merge all non-key columns from R (prefer _REGULAR_TABLE_FILL_COLS order, then any others)
    all_fill = [c for c in _REGULAR_TABLE_FILL_COLS if c in r.columns]
    extra = [c for c in r.columns if c not in ("Team", "year") and c not in all_fill]
    merge_cols = all_fill + extra
    if not merge_cols:
        return base
    r = r[["Team", "year"] + merge_cols].drop_duplicates(subset=["Team", "year"], keep="first")
    base = base.merge(r, on=["Team", "year"], how="left", suffixes=("", "_r"))
    for c in merge_cols:
        r_col = f"{c}_r"
        if r_col not in base.columns:
            continue
        r_vals = base[r_col]
        if c in base.columns:
            base_vals = base[c]
            if pd.api.types.is_numeric_dtype(base_vals):
                base[c] = base_vals.fillna(r_vals)
            else:
                mask = base_vals.isna() | (base_vals.astype(str).str.strip() == "")
                base.loc[mask, c] = r_vals
        else:
            base[c] = r_vals
        base = base.drop(columns=[r_col], errors="ignore")
    return base


def _validate_training_data(df: pd.DataFrame) -> None:
    """Print a short validation summary of the training dataset."""
    if df.empty:
        print("Validation: no rows.")
        return
    n = len(df)
    cols = list(df.columns)
    required = ["Team", "year", "round", "seed"]
    missing = [c for c in required if c not in cols]
    outcome_cols = ["round", "Round2", "S16", "E8", "F4", "Ship", "Champ"]
    has_outcomes = [c for c in outcome_cols if c in cols]
    years = pd.to_numeric(df["year"], errors="coerce").dropna()
    year_range = (int(years.min()), int(years.max())) if len(years) else (None, None)
    print("Validation:")
    print(f"  Rows: {n}  Columns: {len(cols)}")
    print(f"  Year range: {year_range[0]}–{year_range[1]}")
    if missing:
        print(f"  Missing required columns: {missing}")
    else:
        print("  Required columns: OK (Team, year, round, seed)")
    print(f"  Outcome columns: {has_outcomes}")
