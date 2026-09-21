"""
March Madness pipeline: ingestion, features, modeling (Python).

Replicates and optimizes madness.R with vectorized pandas and config-driven modeling.
"""

from __future__ import annotations

import logging
import re
import time
from io import StringIO
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import requests
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold, LeaveOneOut, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression, Ridge

from src.models.bracket import load_bracket
from src.models.simulate import simulate_bracket
from src.models.win_probability import build_strength_lookup, compute_strength

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------

LOGGER = logging.getLogger(__name__)
if not LOGGER.handlers:
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s")

DEFAULT_CONFIG = {
    "paths": {
        "output_training_csv": "data/processed/cbbtrainingdata",
        "output_training_clean": "data/processed/cbb_training_clean",
        "data_external": "data/external",
        "data_processed": "data/processed",
        "bracket_csv": "",
        "models": "models",
        "reports": "reports",
    },
    "model": {
        "k_bradley_terry": 1.0,
        "n_sims": 10000,
        "ridge_alpha": 1.0,
        "optimize_ridge_alpha": True,
        "ridge_alpha_grid": [0.0, 0.01, 0.1, 0.3, 1.0, 3.0, 10.0],
    },
}


def _deep_merge(base: dict, override: dict) -> dict:
    """Deep-merge two dictionaries."""
    out = dict(base)
    for key, value in (override or {}).items():
        if isinstance(value, dict) and isinstance(out.get(key), dict):
            out[key] = _deep_merge(out[key], value)
        else:
            out[key] = value
    return out


def load_config() -> dict:
    """Load config from YAML; fallback to defaults."""
    cfg = dict(DEFAULT_CONFIG)
    try:
        import yaml

        cfg_path = Path("configs/default.yaml")
        if cfg_path.exists():
            with open(cfg_path) as f:
                loaded = yaml.safe_load(f) or {}
                cfg = _deep_merge(cfg, loaded)
    except ImportError:
        LOGGER.warning("PyYAML not installed; using default config.")
    return cfg


def _get_cfg_path(cfg: dict, key: str, default: str) -> str:
    return cfg.get("paths", {}).get(key, default)


def _get_cfg_model(cfg: dict, key: str, default: float | int) -> float | int:
    return cfg.get("model", {}).get(key, default)


def _validate_runtime_config(cfg: dict) -> dict[str, Any]:
    """Validate simulation-related runtime settings."""
    n_sims = int(_get_cfg_model(cfg, "n_sims", DEFAULT_CONFIG["model"]["n_sims"]))
    k_bradley_terry = float(
        _get_cfg_model(cfg, "k_bradley_terry", DEFAULT_CONFIG["model"]["k_bradley_terry"])
    )
    ridge_alpha = float(
        _get_cfg_model(cfg, "ridge_alpha", DEFAULT_CONFIG["model"]["ridge_alpha"])
    )
    optimize_ridge_alpha = bool(
        _get_cfg_model(cfg, "optimize_ridge_alpha", DEFAULT_CONFIG["model"]["optimize_ridge_alpha"])
    )
    alpha_grid_raw = cfg.get("model", {}).get(
        "ridge_alpha_grid",
        DEFAULT_CONFIG["model"]["ridge_alpha_grid"],
    )
    if not isinstance(alpha_grid_raw, list):
        raise ValueError("model.ridge_alpha_grid must be a list of non-negative numbers.")
    alpha_grid: list[float] = []
    for val in alpha_grid_raw:
        try:
            alpha_grid.append(float(val))
        except (TypeError, ValueError) as exc:
            raise ValueError(f"model.ridge_alpha_grid contains non-numeric value: {val!r}") from exc
    alpha_grid = sorted(set(alpha_grid))

    if n_sims <= 0:
        raise ValueError(f"model.n_sims must be > 0 (got {n_sims})")
    if not (0 < k_bradley_terry <= 10):
        raise ValueError(
            f"model.k_bradley_terry must be in (0, 10] (got {k_bradley_terry})"
        )
    if ridge_alpha < 0:
        raise ValueError(f"model.ridge_alpha must be >= 0 (got {ridge_alpha})")
    if any(a < 0 for a in alpha_grid):
        raise ValueError("model.ridge_alpha_grid must contain only non-negative values.")
    if optimize_ridge_alpha and len(alpha_grid) == 0:
        raise ValueError("model.optimize_ridge_alpha is true but model.ridge_alpha_grid is empty.")

    return {
        "n_sims": n_sims,
        "k_bradley_terry": k_bradley_terry,
        "ridge_alpha": ridge_alpha,
        "optimize_ridge_alpha": optimize_ridge_alpha,
        "ridge_alpha_grid": alpha_grid,
    }


# -----------------------------------------------------------------------------
# Ingestion
# -----------------------------------------------------------------------------

BASE_URL = "https://barttorvik.com/trank.php"
USER_AGENT = "Mozilla/5.0"


def _get_trank_html(url: str, playwright_page=None) -> str | None:
    """Get HTML for trank URL. Tries HTTP first, then Playwright if blocked."""
    headers = {
        "User-Agent": USER_AGENT,
        "Accept": "text/html,application/xhtml+xml",
        "Referer": "https://barttorvik.com/",
    }
    try:
        resp = requests.get(url, headers=headers, timeout=30)
        resp.raise_for_status()
        text = resp.text
        if "Verifying Browser" in text or "Verifying your browser" in text:
            text = None
    except Exception:
        text = None

    if text is not None:
        return text

    if playwright_page is not None:
        try:
            playwright_page.goto(url, wait_until="domcontentloaded", timeout=30000)
            playwright_page.wait_for_timeout(3500)
            html = playwright_page.content()
            if "Verifying Browser" in html:
                return None
            return html
        except Exception:
            return None
    return None


# Map multi-level/flattened column names to canonical trank names
_TRANK_COL_MAP = {
    "rk": "Rk", "team": "Team", "conf": "Conf", "rec": "Rec", "g": "G",
    "adjoe": "AdjOE", "adjde": "AdjDE", "barthag": "Barthag",
    "efg%": "EFG%", "efgd%": "EFGD%", "tor": "TOR", "tord": "TORD",
    "orb": "ORB", "drb": "DRB", "ftr": "FTR", "ftrd": "FTRD",
    "2p%": "2P%", "2p%d": "2P%D", "3p%": "3P%", "3p%d": "3P%D",
    "adj t.": "Adj T.", "wab": "WAB",
}


def _normalize_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Flatten multi-level columns and map to canonical trank names."""
    if isinstance(df.columns, pd.MultiIndex):
        new_cols = []
        for col in df.columns:
            parts = [str(c).strip() for c in col if c]
            s = "_".join(parts) if parts else str(col[0])
            new_cols.append(s)
        df.columns = new_cols

    new_names = {}
    for c in df.columns:
        s = str(c).strip().lower()
        for canon_key, canon_val in _TRANK_COL_MAP.items():
            if canon_key in s or s.endswith("_" + canon_key):
                new_names[c] = canon_val
                break
        if c not in new_names and "_rec" in s:
            new_names[c] = "Rec"
        elif c not in new_names and "_team" in s:
            new_names[c] = "Team"
        elif c not in new_names and "_rk" in s or (s == "rk"):
            new_names[c] = "Rk"
    df = df.rename(columns={k: v for k, v in new_names.items() if k in df.columns})
    return df


def _fetch_table(
    year: int, table_type: str, playwright_page=None
) -> pd.DataFrame | None:
    """Fetch one Bart Torvik table (R=regular, P=postseason, T=tournament)."""
    url = f"{BASE_URL}?year={year}&sort=&top=0&conlimit=All&venue=All&type={table_type}#"
    html = _get_trank_html(url, playwright_page)
    if html is None:
        return None
    try:
        tables = pd.read_html(StringIO(html))
        if not tables:
            return None
        df = tables[0].copy()
        df = _normalize_columns(df)
        rk_col = next((c for c in df.columns if str(c).strip().upper() == "RK"), None)
        if rk_col and (df[rk_col].astype(str) == "Rk").any():
            df = df[df[rk_col].astype(str) != "Rk"].copy()
        rec_col = next((c for c in df.columns if "_rec" in str(c).lower() or str(c).lower().endswith("rec")), None)
        if rec_col and str(rec_col) != "Rec":
            df = df.rename(columns={rec_col: "Rec"})
        df["year"] = year
        df = df.loc[:, ~df.columns.duplicated()]
        return df
    except Exception:
        return None


def scrape_bart_torvik(
    year_start: int = 2008,
    year_end: int = 2025,
    exclude_years: tuple[int, ...] = (2020,),
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Scrape regular season, postseason, and tournament data. Uses Playwright fallback when HTTP is blocked."""
    import time

    master = []
    conf = []
    tourn = []
    skip = set(exclude_years)
    playwright_page = None

    try:
        from playwright.sync_api import sync_playwright

        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            playwright_page = browser.new_page()
            for year in range(year_start, year_end + 1):
                if year in skip:
                    continue
                r = _fetch_table(year, "R", playwright_page)
                if r is not None:
                    master.append(r)

                p_ = _fetch_table(year, "P", playwright_page)
                if p_ is not None:
                    conf.append(p_)

                t = _fetch_table(year, "T", playwright_page)
                if t is not None:
                    step = 25
                    idx_drop = list(range(0, len(t), step))
                    if idx_drop:
                        t = t.drop(index=t.index[idx_drop]).reset_index(drop=True)
                    t["year"] = year
                    tourn.append(t)

                time.sleep(2.5)  # Rate limit
            browser.close()
    except ImportError:
        for year in range(year_start, year_end + 1):
            if year in skip:
                continue
            r = _fetch_table(year, "R")
            if r is not None:
                master.append(r)
            p_ = _fetch_table(year, "P")
            if p_ is not None:
                conf.append(p_)
            t = _fetch_table(year, "T")
            if t is not None:
                step = 25
                idx_drop = list(range(0, len(t), step))
                if idx_drop:
                    t = t.drop(index=t.index[idx_drop]).reset_index(drop=True)
                t["year"] = year
                tourn.append(t)
            time.sleep(2.5)
    except Exception as e:
        print(f"Playwright failed ({e}), trying HTTP-only fallback...")
        for year in range(year_start, year_end + 1):
            if year in skip:
                continue
            r = _fetch_table(year, "R")
            if r is not None:
                master.append(r)
            p_ = _fetch_table(year, "P")
            if p_ is not None:
                conf.append(p_)
            t = _fetch_table(year, "T")
            if t is not None:
                step = 25
                idx_drop = list(range(0, len(t), step))
                if idx_drop:
                    t = t.drop(index=t.index[idx_drop]).reset_index(drop=True)
                t["year"] = year
                tourn.append(t)
            time.sleep(2.5)

    master_df = pd.concat(master, ignore_index=True) if master else pd.DataFrame()
    conf_df = pd.concat(conf, ignore_index=True) if conf else pd.DataFrame()
    tourn_df = pd.concat(tourn, ignore_index=True) if tourn else pd.DataFrame()

    return master_df, conf_df, tourn_df


# -----------------------------------------------------------------------------
# Feature Engineering
# -----------------------------------------------------------------------------


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


def _extract_seed(team: str) -> float:
    """Extract seed from Team string (e.g. 'Duke 1 seed' -> 1)."""
    m = re.search(r"(\d+)\s*seed", str(team), re.I)
    return float(m.group(1)) if m else np.nan


def _result_to_round(result: Any) -> float:
    """Map result code to round (64->0, 32->1, ..., 1->6)."""
    m = {"64": 0, "32": 1, "16": 2, "8": 3, "4": 4, "2": 5, "1": 6}
    return m.get(str(int(result)) if pd.notna(result) else "", np.nan)


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

    final = final.sort_values("AdjOE", ascending=False).reset_index(drop=True)

    # Seed from Team
    final["seed"] = final["Team"].apply(_extract_seed)

    # Result/round from Team string
    result_map = {
        "R32": 32, "R64": 64, "Finals": 2, "CHAMPS": 1,
        "Elite Eight": 8, "Sweet Sixteen": 16, "Final Four": 4,
    }
    final["result"] = np.nan
    for pat, val in result_map.items():
        final.loc[final["Team"].astype(str).str.contains(pat, regex=False), "result"] = val

    # Clean numeric columns (WAB, AdjOE, AdjDE, etc.)
    final["WAB"] = final["WAB"].astype(str).apply(_clean_wab)

    numeric_cols = [
        "AdjOE", "AdjDE", "Barthag", "EFG%", "EFGD%", "TOR", "TORD",
        "ORB", "DRB", "FTR", "FTRD", "2P%", "2P%D", "3P%", "3P%D", "Adj T.",
    ]
    for c in numeric_cols:
        if c in final.columns:
            final[c] = pd.to_numeric(final[c], errors="coerce")

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

    # Optional external data
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

    ext_cols = ["WINS", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED", "EFFHGT", "EXP.", "TALENT", "ELITESOS"]
    if not combined.empty and "YEAR" in combined.columns and "TEAM" in combined.columns:
        merge_cols = [c for c in ext_cols if c in combined.columns]
        final = final.merge(
            combined[["TEAM", "YEAR"] + merge_cols],
            left_on=["Team", "year"],
            right_on=["TEAM", "YEAR"],
            how="left",
        )
        final = final.drop(columns=["TEAM", "YEAR"], errors="ignore")
    for c in ext_cols:
        if c not in final.columns:
            final[c] = 0

    # Optional coach tournament experience (last 5 years: appearances, wins, losses)
    coach_cols = ["coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y", "coach_years_with_program"]
    coach_files = list(ext_data_dir.glob("coach_tournament_experience*.csv"))
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
        "EFFHGT", "EXP.", "TALENT", "ELITESOS",
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

    return out


# -----------------------------------------------------------------------------
# Modeling
# -----------------------------------------------------------------------------

# General model features (R: round ~ ... + optional coach experience when present)
GEN_FEATURES = [
    "WAB", "TOR", "Barthag", "AdjOE", "ORB", "EFGD%", "3P%", "AdjDE",
    "seed", "TALENT", "AST%",
    "coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_years_with_program",
    "coach_tourn_win_pct_5y", "coach_tourn_games_5y", "coach_avg_wins_per_appearance",
]
GEN_POLY = {"Barthag": 2, "AdjDE": 2, "seed": 3}

# Seed model config: (hist_seeds, holdout_seeds, features)
SEED_MODELS = [
    ([1, 2], [1, 2], ["WAB", "Barthag", "AdjOE", "OPAST%", "EXP.", "AdjDE", "seed", "predround"]),
    ([1, 2, 3], [1, 2, 3], ["WAB", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([2, 3, 4], [2, 3, 4], ["WAB", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([3, 4, 5], [3, 4, 5], ["WAB", "WAB_3", "AST%", "Barthag", "AdjOE", "2P%", "TORD", "AdjDE", "seed", "predround"]),
    ([4, 5, 6], [4, 5, 6], ["WAB", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([5, 6, 7], [5, 6, 7], ["WAB", "FTRD", "AdjOE", "AdjDE", "seed", "predround"]),
    ([6, 7, 8], [6, 7, 8], ["WAB", "Barthag", "AdjOE", "TOR", "Adj T.", "2P%", "seed", "predround"]),
    ([7, 8, 9], [7, 8, 9], ["WAB", "Barthag", "AdjOE", "AdjDE", "TOR", "BLKED%", "AST%", "OPAST%", "seed", "predround"]),
    ([8, 9, 10], [8, 9, 10], ["WAB", "Barthag", "AdjOE", "AdjDE", "seed", "predround", "TOR", "BLKED%", "AST%", "3PRATE", "ELITESOS"]),
    ([9, 10, 11], [9, 10, 11], ["WAB", "Barthag", "AdjOE", "AdjDE", "seed", "predround", "2P%D", "WINS", "3PRATED", "TALENT", "ELITESOS"]),
    ([10, 11, 12], [10, 11, 12], ["FTR", "AST%", "OPAST%", "ELITESOS", "Barthag", "AdjDE", "AdjOE", "seed", "predround"]),
    ([11, 12, 13], [11, 12, 13], ["BLK%", "AST%", "OPAST%", "EFFHGT", "TALENT", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([12, 13, 14], [12, 13, 14], ["W", "FTR", "FTRD", "TOR", "TORD", "DRB", "TALENT", "ELITESOS", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([13, 14, 15], [13, 14, 15], ["FTR", "TORD", "DRB", "EXP.", "ELITESOS", "Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
]


def _add_poly_features(df: pd.DataFrame, poly: dict) -> pd.DataFrame:
    """Add polynomial columns to df."""
    df = df.copy()
    for feat, deg in poly.items():
        if feat in df.columns:
            df[f"{feat}_2"] = df[feat].astype(float) ** 2
            if deg >= 3:
                df[f"{feat}_3"] = df[feat].astype(float) ** 3
    return df


def _resolve_features(features: list[str], poly: dict, df: pd.DataFrame) -> list[str]:
    """Expand feature list with poly terms; return only columns that exist in df."""
    expanded = list(features)
    for base, deg in poly.items():
        if f"{base}_2" in df.columns and f"{base}_2" not in expanded:
            expanded.append(f"{base}_2")
        if deg >= 3 and f"{base}_3" in df.columns and f"{base}_3" not in expanded:
            expanded.append(f"{base}_3")
    return [c for c in expanded if c in df.columns]


def _get_Xy(
    df: pd.DataFrame,
    features: list[str],
    target: str = "round",
    poly: dict | None = None,
) -> tuple[pd.DataFrame, pd.Series]:
    df = df.copy()
    poly = poly or {}
    df = _add_poly_features(df, poly)
    avail = _resolve_features(features, poly, df)
    if not avail:
        return pd.DataFrame(), df[target] if target in df.columns else pd.Series(dtype=float)
    X = df[avail].copy()
    for c in X.columns:
        X[c] = pd.to_numeric(X[c], errors="coerce")
    X = X.fillna(0)
    y = df[target] if target in df.columns else pd.Series(dtype=float)
    return X, y


def jackknife_loo(
    df: pd.DataFrame,
    features: list[str],
    poly: dict | None = None,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
) -> tuple[Pipeline, float, np.ndarray, np.ndarray]:
    """Leave-one-out cross-validation; return best model, MSE, pred, actual."""
    model_template = _build_regression_model(ridge_alpha)
    X, y = _get_Xy(df, features, poly=poly)
    if X.empty or len(X) < 3:
        return model_template, np.inf, np.array([]), np.array([])

    loo = LeaveOneOut()
    preds, actuals = [], []

    for train_idx, test_idx in loo.split(X):
        X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
        y_train = y.iloc[train_idx]
        model = _build_regression_model(ridge_alpha)
        model.fit(X_train, y_train)
        pred = model.predict(X_test)[0]
        actual = y.iloc[test_idx].values[0]
        preds.append(pred)
        actuals.append(actual)

    preds_arr = np.array(preds)
    actuals_arr = np.array(actuals)
    mse_total = mean_squared_error(actuals_arr, preds_arr)
    final_model = _build_regression_model(ridge_alpha)
    final_model.fit(X, y)
    return final_model, mse_total, preds_arr, actuals_arr


def _build_regression_model(ridge_alpha: float) -> Pipeline:
    """Build a scaled linear model (Ridge when alpha>0, else OLS)."""
    if ridge_alpha > 0:
        estimator = Ridge(alpha=ridge_alpha, fit_intercept=False, random_state=42)
    else:
        estimator = LinearRegression(fit_intercept=False)
    return Pipeline([("scaler", StandardScaler()), ("model", estimator)])


def _cv_label(cv: Any) -> str:
    name = type(cv).__name__
    if isinstance(cv, KFold):
        return f"KFold(n_splits={cv.n_splits}, shuffle={cv.shuffle})"
    return name


def tune_ridge_alpha(
    df: pd.DataFrame,
    features: list[str],
    poly: dict | None,
    alpha_grid: list[float],
    random_state: int = 42,
) -> dict[str, Any]:
    """
    Pick ridge alpha by CV MSE on a grid.

    Uses KFold(5, shuffle) when n>50, otherwise LeaveOneOut.
    Returns a dict with alpha_selected and a compact CV summary.
    """
    X, y = _get_Xy(df, features, poly=poly)
    n = len(X)
    if n < 3:
        return {
            "tuned": False,
            "reason": f"insufficient_rows({n})",
            "alpha_selected": None,
            "cv": None,
            "results": [],
        }

    cv = LeaveOneOut() if n <= 50 else KFold(n_splits=5, shuffle=True, random_state=random_state)
    results: list[dict[str, float]] = []
    for alpha in alpha_grid:
        model = _build_regression_model(float(alpha))
        scores = cross_val_score(
            model,
            X,
            y,
            cv=cv,
            scoring="neg_mean_squared_error",
            n_jobs=1,
        )
        mse_scores = (-scores).astype(float)
        results.append(
            {
                "alpha": float(alpha),
                "mean_mse": float(np.mean(mse_scores)),
                "std_mse": float(np.std(mse_scores)),
            }
        )

    best = min(results, key=lambda r: r["mean_mse"])
    # Keep the report small: only return the full grid if it's short.
    max_keep = 15
    kept = results if len(results) <= max_keep else sorted(results, key=lambda r: r["mean_mse"])[:max_keep]
    return {
        "tuned": True,
        "alpha_selected": float(best["alpha"]),
        "cv": _cv_label(cv),
        "results": kept,
    }


def train_general_model(
    training_data: pd.DataFrame,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    optimize_ridge_alpha: bool = DEFAULT_CONFIG["model"]["optimize_ridge_alpha"],
    ridge_alpha_grid: list[float] | None = None,
) -> tuple[Pipeline, list[str], dict]:
    """Train general model with jackknife LOO."""
    alpha_used = float(ridge_alpha)
    tuning: dict[str, Any] = {"tuned": False, "alpha_selected": None, "cv": None, "results": []}
    grid = ridge_alpha_grid or list(DEFAULT_CONFIG["model"]["ridge_alpha_grid"])
    if optimize_ridge_alpha:
        try:
            tuning = tune_ridge_alpha(training_data, GEN_FEATURES, poly=GEN_POLY, alpha_grid=grid)
            if tuning.get("tuned") and tuning.get("alpha_selected") is not None:
                alpha_used = float(tuning["alpha_selected"])
        except Exception as exc:
            tuning = {"tuned": False, "reason": str(exc), "alpha_selected": None, "cv": None, "results": []}

    model, mse, _, _ = jackknife_loo(
        training_data,
        GEN_FEATURES,
        poly=GEN_POLY,
        ridge_alpha=alpha_used,
    )
    df = _add_poly_features(training_data.copy(), GEN_POLY)
    all_feats = _resolve_features(GEN_FEATURES, GEN_POLY, df)
    return model, all_feats, {"mse": mse, "ridge_alpha": alpha_used, "tuning": tuning}


def _ensure_derived_features(df: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """Add derived columns (e.g. WAB_3) if in features but missing."""
    df = df.copy()
    if "WAB_3" in features and "WAB_3" not in df.columns and "WAB" in df.columns:
        df["WAB_3"] = df["WAB"].astype(float) ** 3
    return df


def train_seed_models(
    training_data: pd.DataFrame,
    ridge_alpha: float = DEFAULT_CONFIG["model"]["ridge_alpha"],
    optimize_ridge_alpha: bool = DEFAULT_CONFIG["model"]["optimize_ridge_alpha"],
    ridge_alpha_grid: list[float] | None = None,
) -> tuple[dict[int, tuple[Pipeline, list[str]]], dict[int, dict[str, Any]]]:
    """Train all 14 seed-stratified models. Returns {seed: (model, features)}."""
    seed_models = {}
    seed_meta: dict[int, dict[str, Any]] = {}
    grid = ridge_alpha_grid or list(DEFAULT_CONFIG["model"]["ridge_alpha_grid"])
    for i, (hist_seeds, _, features) in enumerate(SEED_MODELS, 1):
        sub = training_data[training_data["seed"].isin(hist_seeds)].copy()
        sub = _ensure_derived_features(sub, features)
        if len(sub) < 3:
            continue
        avail = [f for f in features if f in sub.columns]
        if "predround" not in avail and "predround" in sub.columns:
            avail.append("predround")
        if not avail:
            continue

        alpha_used = float(ridge_alpha)
        tuning: dict[str, Any] = {"tuned": False, "alpha_selected": None, "cv": None, "results": []}
        if optimize_ridge_alpha:
            try:
                tuning = tune_ridge_alpha(sub, avail, poly=None, alpha_grid=grid)
                if tuning.get("tuned") and tuning.get("alpha_selected") is not None:
                    alpha_used = float(tuning["alpha_selected"])
            except Exception as exc:
                tuning = {"tuned": False, "reason": str(exc), "alpha_selected": None, "cv": None, "results": []}

        model, loo_mse, _, _ = jackknife_loo(sub, avail, poly=None, ridge_alpha=alpha_used)
        seed_models[i] = (model, avail)
        seed_meta[i] = {
            "model_id": i,
            "hist_seeds": list(hist_seeds),
            "holdout_seeds": list(SEED_MODELS[i - 1][1]),
            "n_train": int(len(sub)),
            "ridge_alpha": alpha_used,
            "loo_mse": float(loo_mse),
            "tuning": tuning,
            "features": list(avail),
        }
    return seed_models, seed_meta


def _predict_with_seed_models(
    df: pd.DataFrame,
    seed_models: dict[int, tuple[Pipeline, list[str]]],
) -> pd.DataFrame:
    """Apply seed models to overwrite general predictions for configured seed groups."""
    out = df.copy()
    out["seed"] = pd.to_numeric(out.get("seed", np.nan), errors="coerce")
    for idx, (_, holdout_seeds, _) in enumerate(SEED_MODELS, 1):
        if idx not in seed_models:
            continue
        model, features = seed_models[idx]
        mask = out["seed"].isin(holdout_seeds)
        if not mask.any():
            continue
        sub = _ensure_derived_features(out.loc[mask].copy(), features)
        X_sub = sub.reindex(columns=features, fill_value=0).copy()
        for col in X_sub.columns:
            X_sub[col] = pd.to_numeric(X_sub[col], errors="coerce")
        X_sub = X_sub.fillna(0.0)
        out.loc[mask, "predround"] = model.predict(X_sub)
    out["predround"] = pd.to_numeric(out["predround"], errors="coerce").fillna(0.0).clip(0.0, 6.0)
    return out


def _save_model_artifacts(
    gen_model: Pipeline,
    seed_models: dict[int, tuple[Pipeline, list[str]]],
    models_dir: Path,
    run_tag: str,
    meta: dict[str, Any] | None = None,
) -> None:
    """Best-effort model artifact persistence."""
    try:
        import joblib
    except ImportError:
        LOGGER.warning("joblib not available; skipping model artifact save.")
        return

    models_dir.mkdir(parents=True, exist_ok=True)
    general_path = models_dir / f"general_model_{run_tag}.joblib"
    seed_path = models_dir / f"seed_models_{run_tag}.joblib"
    meta_path = models_dir / f"model_meta_{run_tag}.joblib"
    joblib.dump(gen_model, general_path)
    joblib.dump(seed_models, seed_path)
    if meta is not None:
        joblib.dump(meta, meta_path)
    LOGGER.info(
        {
            "event": "model_artifacts_saved",
            "general_model_path": str(general_path),
            "seed_models_path": str(seed_path),
            "model_meta_path": str(meta_path) if meta is not None else None,
        }
    )


def _build_narrative_summary(
    round_predictions: pd.DataFrame,
    n_sims: int,
    k_bradley_terry: float,
    bracket_path: str,
) -> str:
    """Create a concise model summary for the simulation output."""
    ordered = round_predictions.sort_values("p_Champ", ascending=False).reset_index(drop=True)
    top4_share = ordered["p_Champ"].head(4).sum()
    top4_lines = [
        f"{row.Team} ({row.p_Champ:.1%})"
        for row in ordered.head(4).itertuples(index=False)
    ]
    upset_pool = round_predictions[pd.to_numeric(round_predictions["Seed"], errors="coerce") >= 10]
    upset_line = "N/A"
    if not upset_pool.empty:
        upset = upset_pool.sort_values("p_S16", ascending=False).iloc[0]
        upset_line = f"{upset['Team']} (seed {int(upset['Seed'])}) with {upset['p_S16']:.1%} to reach S16"

    lines = [
        f"Hook: Top 4 title probabilities account for {top4_share:.1%} of simulated championships.",
        f"Context: bracket={bracket_path}, n_sims={n_sims:,}, k={k_bradley_terry:.2f}.",
        f"Insight: Top title contenders -> {', '.join(top4_lines)}.",
        f"Recommendation: Best upset candidate by S16 odds -> {upset_line}.",
        "Call to action: rerun with --bracket PATH and adjusted --n-sims/--k for scenario testing.",
    ]
    return "\n".join(lines)


def _md_table(headers: list[str], rows: list[list[str]]) -> str:
    """Render a simple GitHub-flavored Markdown table."""
    header_line = "| " + " | ".join(headers) + " |"
    sep_line = "| " + " | ".join(["---"] * len(headers)) + " |"
    body = ["| " + " | ".join(r) + " |" for r in rows]
    return "\n".join([header_line, sep_line] + body)


def _write_optimization_report(
    report_path: Path,
    *,
    run_tag: str,
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
    runtime_cfg: dict[str, Any],
    gen_feats: list[str],
    gen_meta: dict[str, Any],
    seed_meta: dict[int, dict[str, Any]],
    round_predictions_path: str | None,
    bracket_path: str,
) -> None:
    """Write a Markdown report showing per-model optimization evidence."""
    lines: list[str] = []
    lines.append(f"# Model Optimization Report ({run_tag})")
    lines.append("")
    lines.append("## How To Tell Models Are Optimized")
    lines.append("- Each model runs a Ridge `alpha` grid search (CV MSE) and picks the best `alpha`.")
    lines.append("- The report shows baseline vs best CV MSE (lower is better) per model.")
    lines.append("- Final training metric is Leave-One-Out (LOO) MSE for the selected `alpha`.")
    lines.append("")
    lines.append("## Run Context")
    lines.append(f"- Training years: {year_start}-{year_end} (excluded: {', '.join(map(str, exclude_years)) or 'none'})")
    lines.append(f"- Ridge alpha tuning: {runtime_cfg['optimize_ridge_alpha']}")
    lines.append(f"- Alpha grid: {runtime_cfg['ridge_alpha_grid']}")
    lines.append(f"- Bradley-Terry k: {runtime_cfg['k_bradley_terry']}")
    lines.append(f"- Monte Carlo sims: {runtime_cfg['n_sims']:,}")
    lines.append(f"- Bracket CSV: {bracket_path or '(none)'}")
    if round_predictions_path:
        lines.append(f"- Round predictions: {round_predictions_path}")
    lines.append("")

    lines.append("## General Model")
    lines.append(f"- Selected `alpha`: {gen_meta.get('ridge_alpha')}")
    lines.append(f"- LOO MSE: {gen_meta.get('mse'):.6f}")
    lines.append(f"- Features ({len(gen_feats)}): {', '.join(gen_feats)}")
    tuning = gen_meta.get("tuning", {}) or {}
    if tuning.get("tuned") and tuning.get("results"):
        cv_label = tuning.get("cv") or "CV"
        rows = []
        for r in tuning["results"]:
            a = r["alpha"]
            mean_mse = r["mean_mse"]
            std_mse = r["std_mse"]
            marker = " (selected)" if float(a) == float(gen_meta.get("ridge_alpha")) else ""
            rows.append([f"{a:g}{marker}", f"{mean_mse:.6f}", f"{std_mse:.6f}", cv_label])
        lines.append("")
        lines.append("Tuning results (mean CV MSE):")
        lines.append(_md_table(["alpha", "mean_mse", "std_mse", "cv"], rows))
    else:
        reason = tuning.get("reason")
        if reason:
            lines.append(f"- Tuning skipped: {reason}")
    lines.append("")

    lines.append("## Seed Models (Summary)")
    if not seed_meta:
        lines.append("- No seed models trained (insufficient rows).")
    else:
        rows = []
        for model_id in sorted(seed_meta):
            m = seed_meta[model_id]
            tune = m.get("tuning", {}) or {}
            baseline = ""
            best = ""
            cv_label = tune.get("cv") or ""
            results = tune.get("results") or []
            if results:
                baseline_row = next((r for r in results if float(r["alpha"]) == 0.0), None)
                best_row = min(results, key=lambda r: r["mean_mse"])
                baseline = f"{baseline_row['mean_mse']:.6f}" if baseline_row else ""
                best = f"{best_row['mean_mse']:.6f}" if best_row else ""
            rows.append(
                [
                    str(model_id),
                    ",".join(map(str, m["hist_seeds"])),
                    ",".join(map(str, m["holdout_seeds"])),
                    str(m["n_train"]),
                    f"{m['ridge_alpha']:g}",
                    f"{m['loo_mse']:.6f}",
                    baseline,
                    best,
                    cv_label,
                ]
            )
        lines.append(
            _md_table(
                [
                    "model_id",
                    "train_seeds",
                    "apply_seeds",
                    "n_train",
                    "alpha",
                    "loo_mse",
                    "cv_mse_alpha0",
                    "cv_mse_best",
                    "cv",
                ],
                rows,
            )
        )
    lines.append("")
    lines.append("## Notes")
    lines.append("- `cv_mse_alpha0` is the baseline (OLS) mean CV MSE when `alpha=0` is in the grid.")
    lines.append("- `loo_mse` is computed with Leave-One-Out CV using the selected `alpha`.")
    lines.append("- If you want to tune `k`, you need historical bracket + actual outcomes for calibration.")

    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


# -----------------------------------------------------------------------------
# Main pipeline
# -----------------------------------------------------------------------------


def run_pipeline(
    bracket_csv: str | None = None,
    n_sims: int | None = None,
    k_bradley_terry: float | None = None,
    ridge_alpha: float | None = None,
    optimize_ridge_alpha: bool | None = None,
    ridge_alpha_grid: list[float] | None = None,
) -> None:
    """Run full March Madness pipeline."""
    cfg = load_config()
    if bracket_csv is not None:
        cfg.setdefault("paths", {})["bracket_csv"] = bracket_csv
    if n_sims is not None:
        cfg.setdefault("model", {})["n_sims"] = int(n_sims)
    if k_bradley_terry is not None:
        cfg.setdefault("model", {})["k_bradley_terry"] = float(k_bradley_terry)
    if ridge_alpha is not None:
        cfg.setdefault("model", {})["ridge_alpha"] = float(ridge_alpha)
    if optimize_ridge_alpha is not None:
        cfg.setdefault("model", {})["optimize_ridge_alpha"] = bool(optimize_ridge_alpha)
    if ridge_alpha_grid is not None:
        cfg.setdefault("model", {})["ridge_alpha_grid"] = ridge_alpha_grid

    runtime_cfg = _validate_runtime_config(cfg)
    out_prefix = _get_cfg_path(cfg, "output_training_csv", "data/processed/cbbtrainingdata")
    ext_dir = Path(_get_cfg_path(cfg, "data_external", "data/external"))
    data_processed_dir = Path(_get_cfg_path(cfg, "data_processed", "data/processed"))
    models_dir = Path(_get_cfg_path(cfg, "models", "models"))
    bracket_path = _get_cfg_path(cfg, "bracket_csv", "")
    year_range = cfg.get("ingestion", {}).get("year_range", [2008, 2025])
    year_start, year_end = year_range[0], year_range[1]
    exclude_years = tuple(cfg.get("ingestion", {}).get("exclude_years", [2020]))
    run_tag = pd.Timestamp.now().strftime("%Y%m%d")
    start_time = time.time()
    LOGGER.info(
        {
            "event": "pipeline_start",
            "year_start": year_start,
            "year_end": year_end,
            "exclude_years": exclude_years,
            "n_sims": runtime_cfg["n_sims"],
            "k_bradley_terry": runtime_cfg["k_bradley_terry"],
            "ridge_alpha": runtime_cfg["ridge_alpha"],
        }
    )

    print("Scraping Bart Torvik...")
    master, conf, tourn = scrape_bart_torvik(year_start, year_end, exclude_years=exclude_years)

    print("Building features...")
    training_data, training_data_full = build_features(master, conf, tourn, ext_dir)

    out_path = Path(f"{out_prefix}_{run_tag}.csv")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    training_data_full.to_csv(out_path, index=False)
    print(f"Training data written to {out_path}")

    # One clean, prepped dataset: no duplicate vars, no duplicate rows, all outcomes
    clean_prefix = _get_cfg_path(cfg, "output_training_clean", "data/processed/cbb_training_clean")
    clean_path = Path(f"{clean_prefix}.csv")
    clean_path.parent.mkdir(parents=True, exist_ok=True)
    clean_df = build_clean_training_dataset(training_data_full)
    clean_df.to_csv(clean_path, index=False)
    print(f"Clean training dataset written to {clean_path} ({len(clean_df)} rows, {len(clean_df.columns)} columns)")

    # Research/feature-engineering cut: last N seasons only
    research_seasons = cfg.get("research_last_n_seasons", 2)
    if research_seasons and research_seasons > 0:
        year_col = training_data_full["year"]
        year_numeric = pd.to_numeric(year_col, errors="coerce")
        min_year = int(year_numeric.max()) - research_seasons + 1
        mask = year_numeric >= min_year
        research_full = training_data_full.loc[mask]
        if not research_full.empty:
            research_prefix = _get_cfg_path(cfg, "output_training_research", "data/processed/cbbtrainingdata_last2seasons")
            research_path = Path(f"{research_prefix}_{run_tag}.csv")
            research_full.to_csv(research_path, index=False)
            print(f"Research cut (last {research_seasons} seasons) written to {research_path} ({len(research_full)} rows)")

    print("Training general model...")
    gen_model, gen_feats, gen_meta = train_general_model(
        training_data,
        ridge_alpha=runtime_cfg["ridge_alpha"],
        optimize_ridge_alpha=runtime_cfg["optimize_ridge_alpha"],
        ridge_alpha_grid=runtime_cfg["ridge_alpha_grid"],
    )
    training_data["predround"] = compute_strength(training_data, gen_model, gen_feats)

    print("Training seed-stratified models...")
    seed_models, seed_meta = train_seed_models(
        training_data,
        ridge_alpha=runtime_cfg["ridge_alpha"],
        optimize_ridge_alpha=runtime_cfg["optimize_ridge_alpha"],
        ridge_alpha_grid=runtime_cfg["ridge_alpha_grid"],
    )
    training_data = _predict_with_seed_models(training_data, seed_models)

    # Create latest-team strength table used for bracket simulation.
    full_predictions = training_data_full.copy()
    full_predictions["predround"] = compute_strength(full_predictions, gen_model, gen_feats)
    full_predictions = _predict_with_seed_models(full_predictions, seed_models)
    strength_lookup = build_strength_lookup(full_predictions)
    _save_model_artifacts(
        gen_model,
        seed_models,
        models_dir,
        run_tag,
        meta={"general": gen_meta, "seed_meta": seed_meta},
    )

    # 2023 holdout (if available)
    holdout_2023 = training_data_full[
        (training_data_full["year"] == 2023) & (training_data_full["seed"].notna())
    ]

    if not holdout_2023.empty and len(seed_models) > 0:
        print("2023 holdout: predictions available from general model.")
    else:
        print("Skipping 2023 holdout (no 2023 data or seed models).")

    round_out_path: Path | None = None
    if bracket_path:
        try:
            bracket_df = load_bracket(bracket_path)
            progress_every = runtime_cfg["n_sims"] // 10 if runtime_cfg["n_sims"] >= 2000 else 0
            round_probs = simulate_bracket(
                bracket_df=bracket_df,
                strength_lookup=strength_lookup,
                n_sims=runtime_cfg["n_sims"],
                k=runtime_cfg["k_bradley_terry"],
                progress_every=progress_every,
            )

            latest_predround = (
                full_predictions[["Team", "year", "predround"]]
                .copy()
                .assign(year=lambda d: pd.to_numeric(d["year"], errors="coerce").fillna(-1))
                .sort_values(["Team", "year"])
                .groupby("Team", as_index=False)
                .tail(1)[["Team", "predround"]]
            )
            round_output = (
                bracket_df.merge(round_probs, on="Team", how="left")
                .merge(latest_predround, on="Team", how="left")
                .loc[
                    :,
                    [
                        "Team",
                        "Seed",
                        "Region",
                        "p_R64",
                        "p_R32",
                        "p_S16",
                        "p_E8",
                        "p_F4",
                        "p_Finals",
                        "p_Champ",
                        "expected_round",
                        "predround",
                    ],
                ]
            )
            data_processed_dir.mkdir(parents=True, exist_ok=True)
            round_out_path = data_processed_dir / f"round_predictions_{run_tag}.csv"
            round_output.to_csv(round_out_path, index=False)
            print(f"Round-probability predictions written to {round_out_path}")
            print(
                _build_narrative_summary(
                    round_output,
                    n_sims=runtime_cfg["n_sims"],
                    k_bradley_terry=runtime_cfg["k_bradley_terry"],
                    bracket_path=bracket_path,
                )
            )
        except Exception as exc:
            LOGGER.warning({"event": "simulation_skipped", "reason": str(exc)})
            print(f"Bracket simulation skipped: {exc}")
    else:
        print("No bracket CSV configured; skipping round-probability simulation.")

    try:
        reports_dir = Path(_get_cfg_path(cfg, "reports", "reports"))
        report_path = reports_dir / f"model_optimization_{run_tag}.md"
        _write_optimization_report(
            report_path,
            run_tag=run_tag,
            year_start=year_start,
            year_end=year_end,
            exclude_years=exclude_years,
            runtime_cfg=runtime_cfg,
            gen_feats=gen_feats,
            gen_meta=gen_meta,
            seed_meta=seed_meta,
            round_predictions_path=str(round_out_path) if round_out_path else None,
            bracket_path=bracket_path,
        )
        print(f"Model optimization report written to {report_path}")
    except Exception as exc:
        LOGGER.warning({"event": "report_write_failed", "reason": str(exc)})

    training_data["sweetsixteen"] = (training_data["round"] >= 2).astype(int)
    elapsed = round(time.time() - start_time, 2)
    LOGGER.info(
        {
            "event": "pipeline_complete",
            "status": "success",
            "elapsed_seconds": elapsed,
            "rows_training": int(len(training_data)),
            "rows_full": int(len(training_data_full)),
            "general_mse": float(gen_meta["mse"]),
            "seed_models_trained": int(len(seed_models)),
        }
    )
    print("Pipeline complete.")


if __name__ == "__main__":
    run_pipeline()
