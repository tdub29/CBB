"""Team-table CSVs as the training base: data/raw/assembled/all_trank_team_tables.csv and per-season exports."""

from __future__ import annotations

import logging
from pathlib import Path

import numpy as np
import pandas as pd

LOGGER = logging.getLogger(__name__)


# Team tables as primary source (data/external/trank_team_table_data_YYYY.csv)
# Headless (positional) columns from fetch script:
TEAM_TABLE_COL_NAMES = [
    "TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES",
    "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%",
    "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%",
    "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR",
    "PAKE", "PASE", "TALENT", "blank", "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS",
]

# Map team-table CSV columns (headless/positional names) to pipeline canonical
TEAM_TABLE_TO_CANONICAL = {
    "TEAM": "Team", "YEAR": "year", "ADJOE": "AdjOE", "ADJDE": "AdjDE", "BARTHAG": "Barthag",
    "RECORD": "Rec", "WINS": "WINS", "GAMES": "GAMES",
    "EFG": "EFG%", "EFGD.": "EFGD%", "FTRATE": "FTR", "FTRATED": "FTRD",
    "TOV%": "TOR", "TOV%D": "TORD", "OREB%": "ORB", "OPOREB%": "DRB",
    "RAWT": "Adj T.", "2P%": "2P%", "2P%D": "2P%D", "3P%": "3P%", "3P%D": "3P%D",
    "BLK%": "BLK%", "BLKED%": "BLKED%", "AST%": "AST%", "OPAST%": "OPAST%",
    "3PRATE": "3PRATE", "3PRATED": "3PRATED", "ADJ.T": "Adj T.",
    "EFFHGT": "EFFHGT", "EXP.": "EXP.", "TALENT": "TALENT", "ELITESOS": "ELITESOS",
}

# Manually compiled team tables with header row (normalized key = strip + lower + collapse spaces)
TEAM_TABLE_HEADER_TO_CANONICAL = {
    "team": "Team", "year": "year", "adj oe": "AdjOE", "adj de": "AdjDE", "barthag": "Barthag",
    "record": "Rec", "wins": "WINS", "games": "GAMES",
    "efg": "EFG%", "efg d.": "EFGD%", "ft rate": "FTR", "ft rate d": "FTRD",
    "tov%": "TOR", "tov% d": "TORD", "o reb%": "ORB", "op oreb%": "DRB",
    "raw t": "Adj T.", "2p %": "2P%", "2p % d.": "2P%D", "3p %": "3P%", "3p % d.": "3P%D",
    "blk %": "BLK%", "blked %": "BLKED%", "ast %": "AST%", "op ast %": "OPAST%",
    "3p rate": "3PRATE", "3p rate d": "3PRATED", "adj. t": "Adj T.",
    "avg hgt.": "AVGHGT", "eff. hgt.": "EFFHGT", "exp.": "EXP.",
    "pake": "PAKE", "pase": "PASE", "talent": "TALENT",
    "ft%": "FT%", "op. ft%": "OPFT%", "ppp off.": "PPPOFF", "ppp def.": "PPPDEF", "elite sos": "ELITESOS",
}


def _normalize_header(s: str) -> str:
    """Normalize header for lookup: strip, lower, collapse spaces."""
    return " ".join(str(s).strip().lower().split())


def load_all_trank_base(
    all_tables_path: Path,
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
) -> pd.DataFrame:
    """Load all_trank_team_tables.csv as the primary training base."""
    all_tables_path = Path(all_tables_path)
    if not all_tables_path.exists():
        raise FileNotFoundError(f"Missing all-tables base file: {all_tables_path}")

    df = pd.read_csv(all_tables_path, header=None)
    if df.empty or len(df.columns) < 4:
        return pd.DataFrame()
    n = min(len(TEAM_TABLE_COL_NAMES), len(df.columns))
    extra_cols = [f"_extra_{i}" for i in range(n, len(df.columns))]
    df.columns = list(TEAM_TABLE_COL_NAMES[:n]) + extra_cols
    df["YEAR"] = pd.to_numeric(df["YEAR"], errors="coerce")
    skip = set(exclude_years)
    df = df.dropna(subset=["YEAR"]).copy()
    df["YEAR"] = df["YEAR"].astype(int)
    df = df[(df["YEAR"] >= year_start) & (df["YEAR"] <= year_end) & (~df["YEAR"].isin(skip))]
    if df.empty:
        return pd.DataFrame()

    rename = {
        k: v for k, v in TEAM_TABLE_TO_CANONICAL.items() if k in df.columns and k not in ("WINS", "GAMES")
    }
    df = df.rename(columns=rename)
    df = df.loc[:, ~df.columns.duplicated()]
    if "year" not in df.columns and "YEAR" in df.columns:
        df["year"] = df["YEAR"]
    if "Team" not in df.columns and "TEAM" in df.columns:
        df["Team"] = df["TEAM"]

    if "WINS" in df.columns and "GAMES" in df.columns:
        df["W"] = pd.to_numeric(df["WINS"], errors="coerce")
        df["L"] = pd.to_numeric(df["GAMES"], errors="coerce") - df["W"]
    else:
        df["W"] = np.nan
        df["L"] = np.nan
    df["G"] = df["GAMES"] if "GAMES" in df.columns else (df["W"] + df["L"])

    rank_col = df.get("Barthag") if "Barthag" in df.columns else df.get("BARTHAG")
    if rank_col is not None:
        df["Rk"] = pd.to_numeric(rank_col, errors="coerce")
        df["Rk"] = df.groupby("year")["Rk"].rank(ascending=False)

    if "Conf" not in df.columns:
        df["Conf"] = ""

    df["Team"] = df["Team"].astype(str).str.strip()
    before = len(df)
    df = df.drop_duplicates(subset=["Team", "year"], keep="first").reset_index(drop=True)
    dropped = before - len(df)
    if dropped > 0:
        LOGGER.warning(
            {"event": "dedupe_all_tables_base", "dropped_rows": dropped, "remaining_rows": len(df)}
        )

    for c in ["TEAM", "YEAR"]:
        if c in df.columns and c not in ("Team", "year"):
            df = df.drop(columns=[c], errors="ignore")
    return df


def load_team_tables_as_primary(
    ext_data_dir: Path,
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
) -> pd.DataFrame:
    """Load and combine team-table CSVs as primary data source. Supports headless (positional) or header-row CSVs."""
    ext_data_dir = Path(ext_data_dir)
    skip = set(exclude_years)
    combined = pd.DataFrame()
    for f in sorted(ext_data_dir.glob("trank_team_table_data*.csv")):
        try:
            # Try with header first (manually compiled tables with column names)
            df = pd.read_csv(f, header=0)
            if df.empty or len(df.columns) < 4:
                continue
            raw_cols = [str(c).strip() for c in df.columns]
            norm_keys = {_normalize_header(c) for c in raw_cols}
            use_header = "team" in norm_keys or "year" in norm_keys or "barthag" in norm_keys
            if use_header and any(nk in TEAM_TABLE_HEADER_TO_CANONICAL for nk in norm_keys):
                rename = {c: TEAM_TABLE_HEADER_TO_CANONICAL[_normalize_header(c)] for c in raw_cols if _normalize_header(c) in TEAM_TABLE_HEADER_TO_CANONICAL}
                df = df.rename(columns=rename)
                df = df.loc[:, ~df.columns.duplicated()]
                if "year" in df.columns:
                    df["YEAR"] = pd.to_numeric(df["year"], errors="coerce")
                elif "Year" in df.columns:
                    df["YEAR"] = pd.to_numeric(df["Year"], errors="coerce")
                else:
                    raise ValueError("no year column")
                if "Team" not in df.columns and "TEAM" in df.columns:
                    df["Team"] = df["TEAM"]
            else:
                df = pd.read_csv(f, header=None)
                if df.empty or len(df.columns) < 4:
                    continue
                df.columns = TEAM_TABLE_COL_NAMES[: len(df.columns)]
                df["YEAR"] = pd.to_numeric(df["YEAR"], errors="coerce")
            df = df.dropna(subset=["YEAR"])
            df["YEAR"] = df["YEAR"].astype(int)
            df = df[(df["YEAR"] >= year_start) & (df["YEAR"] <= year_end) & (~df["YEAR"].isin(skip))]
            if df.empty:
                continue
            if "TEAM" not in df.columns and "Team" in df.columns:
                df["TEAM"] = df["Team"]
            elif "Team" not in df.columns and "TEAM" in df.columns:
                df["Team"] = df["TEAM"]
            combined = pd.concat([combined, df], ignore_index=True)
        except Exception:
            try:
                df = pd.read_csv(f, header=None)
                if df.empty or len(df.columns) < 4:
                    continue
                df.columns = TEAM_TABLE_COL_NAMES[: len(df.columns)]
                if "YEAR" not in df.columns or "TEAM" not in df.columns:
                    continue
                df["YEAR"] = pd.to_numeric(df["YEAR"], errors="coerce")
                df = df.dropna(subset=["YEAR"])
                df["YEAR"] = df["YEAR"].astype(int)
                df = df[(df["YEAR"] >= year_start) & (df["YEAR"] <= year_end) & (~df["YEAR"].isin(skip))]
                if df.empty:
                    continue
                combined = pd.concat([combined, df], ignore_index=True)
            except Exception:
                continue
    if combined.empty:
        return combined
    # Rename to canonical pipeline columns (may produce duplicate names e.g. Adj T.; keep first)
    rename = {k: v for k, v in TEAM_TABLE_TO_CANONICAL.items() if k in combined.columns and k not in ("WINS", "GAMES")}
    combined = combined.rename(columns=rename)
    combined = combined.loc[:, ~combined.columns.duplicated()]
    if "year" not in combined.columns and "YEAR" in combined.columns:
        combined["year"] = combined["YEAR"]
    if "Team" not in combined.columns and "TEAM" in combined.columns:
        combined["Team"] = combined["TEAM"]
    # W, L from WINS and GAMES (full-season; team table doesn't separate conf vs tourn)
    if "WINS" in combined.columns and "GAMES" in combined.columns:
        combined["W"] = pd.to_numeric(combined["WINS"], errors="coerce")
        combined["L"] = pd.to_numeric(combined["GAMES"], errors="coerce") - combined["W"]
    else:
        combined["W"] = np.nan
        combined["L"] = np.nan
    combined["G"] = combined["GAMES"] if "GAMES" in combined.columns else (combined["W"] + combined["L"])
    rank_col = combined.get("Barthag") if "Barthag" in combined.columns else combined.get("BARTHAG")
    if rank_col is not None:
        combined["Rk"] = combined.groupby("year")[rank_col.name].rank(ascending=False)
    # Team tables don't have Conf or WAB; add placeholders (enrichment can fill from R table)
    if "Conf" not in combined.columns:
        combined["Conf"] = ""
    if "WAB" not in combined.columns:
        combined["WAB"] = np.nan
    # Drop any leftover TEAM/YEAR duplicates from rename
    for c in ["TEAM", "YEAR"]:
        if c in combined.columns and c not in ("Team", "year"):
            combined = combined.drop(columns=[c], errors="ignore")
    return combined
