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
from urllib.parse import quote_plus

import numpy as np
import pandas as pd
import requests
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold, LeaveOneOut, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression, Ridge

from src.frozen_inputs import write_csv
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
        "data_raw": "data/raw",
        "data_processed": "data/processed",
        "all_trank_team_tables": "data/raw/assembled/all_trank_team_tables.csv",
        "team_history_cache": "data/raw/team_history_cache.csv",
        "bracket_csv": "",
        "models": "models",
        "reports": "reports",
    },
    "ingestion": {
        "team_history_delay_sec": 0.8,
        "team_history_force_playwright": False,
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
TEAM_HISTORY_URL = "https://barttorvik.com/team-history.php"


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
            playwright_page.goto(url, wait_until="load", timeout=30000)
            playwright_page.wait_for_selector("body", timeout=10000)
            # Give verification + JS table render time (inspect_trank_page.py: ~8s gets full table)
            playwright_page.wait_for_timeout(8000)
            try:
                playwright_page.wait_for_selector("table", timeout=5000)
                playwright_page.wait_for_timeout(2000)
            except Exception:
                pass
            html = playwright_page.content()
            if "Verifying Browser" in html or "Verifying your browser" in html:
                return None
            return html
        except Exception:
            return None
    return None


# Map multi-level/flattened column names to canonical trank names (incl. Bart page variants like "Adj O", "Adj D")
_TRANK_COL_MAP = {
    "rk": "Rk", "team": "Team", "conf": "Conf", "rec": "Rec", "g": "G",
    "adjoe": "AdjOE", "adjde": "AdjDE", "adj o": "AdjOE", "adj d": "AdjDE",
    "adj o.": "AdjOE", "adj d.": "AdjDE", "barthag": "Barthag",
    "efg%": "EFG%", "efgd%": "EFGD%", "efg": "EFG%", "efgd": "EFGD%",
    "tor": "TOR", "tord": "TORD", "orb": "ORB", "drb": "DRB",
    "ftr": "FTR", "ftrd": "FTRD", "ftr.": "FTR", "ftrd.": "FTRD",
    "2p%": "2P%", "2p%d": "2P%D", "3p%": "3P%", "3p%d": "3P%D",
    "adj t.": "Adj T.", "adj t": "Adj T.", "wab": "WAB",
    "3prd": "3PRATED", "3pr": "3PRATE",  # 3prd before 3pr so "3prd" matches first
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
        s = str(c).strip().lower().replace(" ", "")
        s_raw = str(c).strip().lower()
        for canon_key, canon_val in _TRANK_COL_MAP.items():
            key_nospace = canon_key.replace(" ", "")
            if key_nospace in s or canon_key in s_raw or s_raw.endswith("_" + canon_key):
                new_names[c] = canon_val
                break
        if c not in new_names and ("_rec" in s_raw or "rec" == s_raw):
            new_names[c] = "Rec"
        elif c not in new_names and "_team" in s_raw:
            new_names[c] = "Team"
        elif c not in new_names and ("_rk" in s_raw or s_raw == "rk"):
            new_names[c] = "Rk"
    df = df.rename(columns={k: v for k, v in new_names.items() if k in df.columns})
    return df


def _strip_rank_suffix(ser: pd.Series) -> pd.Series:
    """Parse cells like '124.3 8' or '+11.5 2' to numeric (take first token, strip +)."""
    out = []
    for v in ser:
        v = str(v).strip()
        if not v or v in ("nan", ""):
            out.append(np.nan)
            continue
        parts = v.replace("+", "").strip().split()
        if not parts:
            out.append(np.nan)
            continue
        try:
            out.append(float(parts[0]))
        except (ValueError, TypeError):
            out.append(np.nan)
    return pd.Series(out, index=ser.index)


# Canonical columns for T-Rank tables so concat across years aligns (no duplicate/variant cols)
_TRANK_CANONICAL_COLS = [
    "Rk", "Team", "Conf", "G", "Rec", "AdjOE", "AdjDE", "Barthag", "EFG%", "EFGD%",
    "TOR", "TORD", "ORB", "DRB", "FTR", "FTRD", "2P%", "2P%D", "3P%", "3P%D", "Adj T.", "WAB", "year",
]


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
        # Pick the table with the most stat columns (Team + core stat) to capture full trank.php data
        best_df = None
        best_n = 0
        for tbl in tables:
            try:
                t = tbl.copy()
                t = _normalize_columns(t)
                t = t.loc[:, ~t.columns.duplicated()]
                has_core = any(
                    c in t.columns for c in ("AdjOE", "AdjDE", "Barthag", "WAB", "EFG%")
                )
                if has_core and "Team" in t.columns and len(t) > 1:
                    n_canon = sum(1 for c in _TRANK_CANONICAL_COLS if c in t.columns)
                    if n_canon > best_n:
                        best_n = n_canon
                        best_df = t
            except Exception:
                continue
        if best_df is None:
            # Fallback: try largest table with Team (after normalization) in case core stats use different labels
            for tbl in sorted(tables, key=lambda x: -len(x.columns)):
                try:
                    t = tbl.copy()
                    t = _normalize_columns(t)
                    t = t.loc[:, ~t.columns.duplicated()]
                    if "Team" in t.columns and len(t) > 1:
                        best_df = t
                        break
                except Exception:
                    continue
        if best_df is None:
            best_df = tables[0].copy()
            best_df = _normalize_columns(best_df)
            best_df = best_df.loc[:, ~best_df.columns.duplicated()]
        df = best_df
        rk_col = next((c for c in df.columns if str(c).strip().upper() == "RK"), None)
        if rk_col and (df[rk_col].astype(str) == "Rk").any():
            df = df[df[rk_col].astype(str) != "Rk"].copy()
        rec_col = next((c for c in df.columns if "_rec" in str(c).lower() or str(c).lower().endswith("rec")), None)
        if rec_col and str(rec_col) != "Rec":
            df = df.rename(columns={rec_col: "Rec"})
        df["year"] = year
        df = df.loc[:, ~df.columns.duplicated()]
        # Keep all normalized columns; ensure canonical cols exist (add as NaN if missing)
        keep_cols = [c for c in _TRANK_CANONICAL_COLS if c in df.columns]
        extra = [c for c in df.columns if c not in _TRANK_CANONICAL_COLS]
        df = df.reindex(columns=keep_cols + extra)
        # Strip rank suffixes from numeric cells (e.g. "124.3 8" -> 124.3)
        numeric_like = [c for c in df.columns if c not in ("Team", "Conf", "Rec", "year") and c in df.columns]
        for c in numeric_like:
            try:
                df[c] = _strip_rank_suffix(df[c])
            except Exception:
                pass
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


def save_raw_trank_tables(
    master: pd.DataFrame,
    conf: pd.DataFrame,
    tourn: pd.DataFrame,
    data_raw_dir: Path,
    run_tag: str,
) -> None:
    """Persist raw scraped R, P, T tables to data/raw/pipeline_runs/ for debugging and reuse."""
    data_raw_dir = Path(data_raw_dir)
    run_dir = data_raw_dir / "pipeline_runs"
    run_dir.mkdir(parents=True, exist_ok=True)
    if not master.empty:
        master.to_csv(run_dir / f"trank_regular_{run_tag}.csv", index=False)
    if not conf.empty:
        conf.to_csv(run_dir / f"trank_postseason_{run_tag}.csv", index=False)
    if not tourn.empty:
        tourn.to_csv(run_dir / f"trank_tournament_{run_tag}.csv", index=False)


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


TEAM_HISTORY_CANONICAL_COLS = [
    "team",
    "year",
    "t_rk",
    "coach",
    "seed",
    "finish",
    "conf",
    "ov_rec",
    "conf_rec",
    "adj_o",
    "adj_d",
    "efg_o",
    "efg_d",
    "to_percent",
    "tod_percent",
    "or_percent",
    "dr_percent",
    "ftr",
    "ftrd",
    "ft_percent",
    "two_pct",
    "two_pct_d",
    "three_pct",
    "adj_t",
]

TEAM_HISTORY_NUMERIC_COLS = [
    "year",
    "t_rk",
    "seed",
    "adj_o",
    "adj_d",
    "efg_o",
    "efg_d",
    "to_percent",
    "tod_percent",
    "or_percent",
    "dr_percent",
    "ftr",
    "ftrd",
    "ft_percent",
    "two_pct",
    "two_pct_d",
    "three_pct",
    "adj_t",
]

TEAM_HISTORY_HEADER_MAP = {
    "year": "year",
    "t-rk": "t_rk",
    "coach": "coach_raw",
    "conf": "conf",
    "rec.": "rec",
    "rec": "rec",
    "adjo": "adj_o",
    "adjd": "adj_d",
    "efgo": "efg_o",
    "efgd": "efg_d",
    "to%": "to_percent",
    "tod%": "tod_percent",
    "or%": "or_percent",
    "dr%": "dr_percent",
    "ftr": "ftr",
    "ftrd": "ftrd",
    "ft%": "ft_percent",
    "2p%": "two_pct",
    "2p%d": "two_pct_d",
    "3p%": "three_pct",
    "adj t": "adj_t",
    "adjt": "adj_t",
}


def _team_history_empty() -> pd.DataFrame:
    return pd.DataFrame(columns=TEAM_HISTORY_CANONICAL_COLS)


def _split_record_cell(rec: Any) -> tuple[str, str]:
    """Split Torvik record cell into overall and conference records."""
    s = str(rec).strip()
    if not s or s.lower() == "nan":
        return "", ""
    m = re.match(r"^\s*([0-9]+-[0-9]+)\s*(?:\(([^)]*)\))?\s*$", s)
    if m:
        ov = (m.group(1) or "").strip()
        conf = (m.group(2) or "").strip()
        return ov, conf
    return s, ""


def _split_coach_seed_finish(coach_cell: Any) -> tuple[str, float, str]:
    """Parse coach cell from team history into (coach, seed, finish)."""
    s = str(coach_cell).strip()
    if not s or s.lower() == "nan":
        return "", np.nan, ""

    # Examples:
    # "Jon Scheyer 1 seed, Final Four"
    # "Mike Krzyzewski 3 seed (Proj.)"
    # "Dan Hurley8 seed, R32"
    m = re.match(r"^(.+?)\s*(\d+)\s*seed(?:\s*\(([^)]*)\))?(?:\s*,\s*(.*))?$", s, re.I)
    if m:
        coach = (m.group(1) or "").strip()
        seed = float(m.group(2)) if m.group(2) else np.nan
        proj = (m.group(3) or "").strip()
        finish = (m.group(4) or "").strip()
        if not finish and proj:
            finish = proj
        return coach, seed, finish

    m2 = re.match(r"^(.+?)\s+(\d+)[,\s]+(.+)$", s)
    if m2:
        return (m2.group(1) or "").strip(), float(m2.group(2)), (m2.group(3) or "").strip()

    return s, np.nan, ""


def _parse_team_history_numeric(col: pd.Series) -> pd.Series:
    """Parse numeric history cells that may contain rank suffixes (e.g. '128.6 5')."""
    parsed = _strip_rank_suffix(col)
    return pd.to_numeric(parsed, errors="coerce")


def parse_team_history_to_schema(html: str, team: str) -> pd.DataFrame:
    """
    Parse Torvik team-history HTML into the bart_team_history-like canonical schema.

    Returns one row per season with TEAM_HISTORY_CANONICAL_COLS.
    """
    if not html:
        return _team_history_empty()
    try:
        tables = pd.read_html(StringIO(html))
    except Exception:
        return _team_history_empty()
    if not tables:
        return _team_history_empty()

    # Prefer table that has year + something coach-like.
    def _norm(s: str) -> str:
        return _normalize_header(s).replace(" ", "").replace("–", "-")

    chosen = None
    for tbl in tables:
        cols = {_norm(c): c for c in tbl.columns}
        has_year = "year" in cols
        has_coach = "coach" in cols or any("coach" in k for k in cols)
        if has_year and has_coach:
            chosen = tbl.copy()
            break
    if chosen is None:
        chosen = tables[0].copy()

    rename: dict[Any, str] = {}
    coach_col = None
    for c in chosen.columns:
        raw = _norm(c)
        if raw in TEAM_HISTORY_HEADER_MAP:
            rename[c] = TEAM_HISTORY_HEADER_MAP[raw]
        elif "coach" in raw and coach_col is None:
            rename[c] = "coach_raw"
            coach_col = c
    df = chosen.rename(columns=rename)
    if "year" not in df.columns or "coach_raw" not in df.columns:
        return _team_history_empty()

    if "rec" not in df.columns:
        df["rec"] = ""
    if "conf" not in df.columns:
        df["conf"] = ""

    coach_split = df["coach_raw"].apply(_split_coach_seed_finish)
    df["coach"] = coach_split.apply(lambda x: x[0])
    df["seed"] = coach_split.apply(lambda x: x[1])
    df["finish"] = coach_split.apply(lambda x: x[2])

    rec_split = df["rec"].apply(_split_record_cell)
    df["ov_rec"] = rec_split.apply(lambda x: x[0])
    df["conf_rec"] = rec_split.apply(lambda x: x[1])

    for col in TEAM_HISTORY_NUMERIC_COLS:
        if col in df.columns:
            df[col] = _parse_team_history_numeric(df[col])

    out = _team_history_empty()
    for c in TEAM_HISTORY_CANONICAL_COLS:
        if c in df.columns:
            out[c] = df[c]
    out["team"] = str(team).strip()

    out = out.dropna(subset=["year"]).copy()
    out["year"] = pd.to_numeric(out["year"], errors="coerce")
    out = out.dropna(subset=["year"])
    out["year"] = out["year"].astype(int)
    out = out.loc[:, TEAM_HISTORY_CANONICAL_COLS]
    out = out.sort_values("year").reset_index(drop=True)
    return out


def fetch_team_history(
    team: str,
    session: requests.Session | None = None,
    playwright_page=None,
    force_playwright: bool = False,
    user_agent: str = USER_AGENT,
) -> pd.DataFrame:
    """Fetch and parse one team from Torvik team-history.php."""
    team_str = str(team).strip()
    if not team_str:
        return _team_history_empty()
    # URL-encode so "Texas A&M" -> Texas+A%26M (ampersand must be %26)
    team_query = quote_plus(team_str)
    url = f"{TEAM_HISTORY_URL}?team={team_query}"
    html = None

    if not force_playwright:
        sess = session or requests.Session()
        headers = {
            "User-Agent": user_agent,
            "Accept": "text/html,application/xhtml+xml",
            "Referer": "https://barttorvik.com/",
        }
        try:
            resp = sess.get(url, headers=headers, timeout=20)
            resp.raise_for_status()
            if "Verifying Browser" not in resp.text and "Verifying your browser" not in resp.text:
                html = resp.text
        except Exception:
            html = None

    if html is None and playwright_page is not None:
        try:
            playwright_page.goto(url, wait_until="load", timeout=45000)
            try:
                playwright_page.wait_for_selector("table", timeout=10000)
            except Exception:
                pass
            playwright_page.wait_for_timeout(8000)
            maybe_html = playwright_page.content()
            if "Verifying Browser" not in maybe_html and "Verifying your browser" not in maybe_html:
                html = maybe_html
        except Exception:
            html = None

    return parse_team_history_to_schema(html or "", team=team_str)


def build_team_history_all(
    teams: list[str],
    cache_path: Path,
    refresh: bool = False,
    delay_sec: float = 0.8,
    force_playwright: bool = False,
    user_agent: str = USER_AGENT,
) -> tuple[pd.DataFrame, list[str]]:
    """
    Build/refresh team history cache and return history rows for requested teams.

    Returns (history_df, failed_teams).
    """
    cache_path = Path(cache_path)
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    team_list = sorted({str(t).strip() for t in teams if str(t).strip()})
    if not team_list:
        return _team_history_empty(), []

    cached = _team_history_empty()
    if cache_path.exists() and not refresh:
        try:
            cached = pd.read_csv(cache_path)
            for c in TEAM_HISTORY_CANONICAL_COLS:
                if c not in cached.columns:
                    cached[c] = np.nan
            cached = cached[TEAM_HISTORY_CANONICAL_COLS]
            cached["team"] = cached["team"].astype(str).str.strip()
            cached["year"] = pd.to_numeric(cached["year"], errors="coerce")
            cached = cached.dropna(subset=["year"])
            cached["year"] = cached["year"].astype(int)
        except Exception:
            cached = _team_history_empty()

    cached_teams = set(cached["team"].unique()) if not cached.empty else set()
    missing_teams = team_list if refresh else [t for t in team_list if t not in cached_teams]
    if not missing_teams:
        out = cached[cached["team"].isin(team_list)].copy()
        out = out.drop_duplicates(subset=["team", "year"], keep="last")
        return out, []

    session = requests.Session()
    session.headers.update({"User-Agent": user_agent})
    failed: list[str] = []
    fetched_rows: list[pd.DataFrame] = []
    use_playwright = force_playwright
    playwright_mgr = None
    browser = None
    page = None
    try:
        for i, team in enumerate(missing_teams, start=1):
            if i == 1 or i % 25 == 0:
                LOGGER.info(
                    {
                        "event": "team_history_progress",
                        "fetched": i,
                        "total": len(missing_teams),
                        "team": team,
                    }
                )
            if use_playwright and page is None:
                try:
                    from playwright.sync_api import sync_playwright

                    playwright_mgr = sync_playwright().start()
                    browser = playwright_mgr.chromium.launch(headless=True)
                    page = browser.new_page()
                except Exception:
                    page = None

            hist = fetch_team_history(
                team,
                session=session,
                playwright_page=page,
                force_playwright=use_playwright,
                user_agent=user_agent,
            )
            if hist.empty and not use_playwright:
                # Retry this team with Playwright fallback and keep it for remaining teams.
                use_playwright = True
                if page is None:
                    try:
                        from playwright.sync_api import sync_playwright

                        playwright_mgr = sync_playwright().start()
                        browser = playwright_mgr.chromium.launch(headless=True)
                        page = browser.new_page()
                    except Exception:
                        page = None
                hist = fetch_team_history(
                    team,
                    session=session,
                    playwright_page=page,
                    force_playwright=use_playwright,
                    user_agent=user_agent,
                )
            if hist.empty:
                failed.append(team)
            else:
                fetched_rows.append(hist)
            if delay_sec > 0:
                time.sleep(delay_sec)
    finally:
        try:
            session.close()
        except Exception:
            pass
        if browser is not None:
            try:
                browser.close()
            except Exception:
                pass
        if playwright_mgr is not None:
            try:
                playwright_mgr.stop()
            except Exception:
                pass

    fetched = pd.concat(fetched_rows, ignore_index=True) if fetched_rows else _team_history_empty()
    combined = pd.concat([cached, fetched], ignore_index=True)
    if combined.empty:
        return combined, failed
    combined = combined.drop_duplicates(subset=["team", "year"], keep="last")
    combined = combined.sort_values(["team", "year"]).reset_index(drop=True)
    combined.to_csv(cache_path, index=False)
    out = combined[combined["team"].isin(team_list)].copy()
    return out, failed


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


def _years_last_5_tournaments(season_year: int, exclude_years: tuple[int, ...]) -> list[int]:
    """Seasons in the 5-year look-back before season_year, minus exclude_years (not backfilled).

    The current season is never included: its finish is the prediction target. E.g. 2024 -> 2019,2021,2022,2023.
    """
    skip = set(exclude_years)
    return [y for y in range(season_year - 5, season_year) if y not in skip]


def compute_coach_stats_from_history(
    team_year: pd.DataFrame,
    history: pd.DataFrame,
    exclude_years: tuple[int, ...] = (2020,),
) -> pd.DataFrame:
    """Compute coach tournament experience features from team history.

    Appearances and W-L count the season's head coach at any program over _years_last_5_tournaments(year),
    so only pre-tournament information is used. coach_years_with_program counts consecutive seasons with this
    coach at this team, current included. Excluded years (2020) lost only the tournament, so they still count
    toward tenure; if history has no row for one, the streak bridges it when the same coach is on both sides.
    """
    if team_year.empty:
        return pd.DataFrame(
            columns=[
                "Team",
                "year",
                "coach",
                "coach_tourn_appearances_5y",
                "coach_tourn_wins_5y",
                "coach_tourn_losses_5y",
                "coach_years_with_program",
            ]
        )

    hist = history.copy()
    if hist.empty:
        out = team_year.copy()
        out["coach"] = ""
        out["coach_tourn_appearances_5y"] = 0
        out["coach_tourn_wins_5y"] = 0
        out["coach_tourn_losses_5y"] = 0
        out["coach_years_with_program"] = 0
        return out

    hist["team"] = hist["team"].astype(str).str.strip()
    hist["coach"] = hist["coach"].fillna("").astype(str).str.strip()
    hist["year"] = pd.to_numeric(hist["year"], errors="coerce")
    hist = hist.dropna(subset=["year"]).copy()
    hist["year"] = hist["year"].astype(int)
    wl = hist["finish"].apply(_finish_to_wl)
    hist["tourn_w"] = wl.apply(lambda x: x[0])
    hist["tourn_l"] = wl.apply(lambda x: x[1])
    hist["is_tournament"] = (hist["tourn_w"] + hist["tourn_l"]) > 0

    rows = []
    skip = set(exclude_years)
    min_year = int(hist["year"].min()) if not hist.empty else 2008
    for _, r in team_year.iterrows():
        team = str(r["Team"]).strip()
        year = int(r["year"])
        current = hist[(hist["team"] == team) & (hist["year"] == year)]
        if current.empty:
            rows.append(
                {
                    "Team": team,
                    "year": year,
                    "coach": "",
                    "coach_tourn_appearances_5y": 0,
                    "coach_tourn_wins_5y": 0,
                    "coach_tourn_losses_5y": 0,
                    "coach_years_with_program": 0,
                }
            )
            continue
        coach = str(current.iloc[0]["coach"]).strip()
        prior = _years_last_5_tournaments(year, exclude_years=exclude_years)
        coach_prior = hist[
            (hist["coach"] == coach) & (hist["year"].isin(prior)) & (hist["is_tournament"])
        ]
        appearances = int(len(coach_prior))
        wins = int(coach_prior["tourn_w"].sum())
        losses = int(coach_prior["tourn_l"].sum())

        years_with = 0
        bridged = 0
        for y in range(year, min_year - 1, -1):
            one = hist[(hist["team"] == team) & (hist["year"] == y)]
            if one.empty and y in skip:
                bridged += 1
                continue
            if one.empty or str(one.iloc[0]["coach"]).strip() != coach:
                break
            years_with += 1 + bridged
            bridged = 0

        rows.append(
            {
                "Team": team,
                "year": year,
                "coach": coach,
                "coach_tourn_appearances_5y": appearances,
                "coach_tourn_wins_5y": wins,
                "coach_tourn_losses_5y": losses,
                "coach_years_with_program": years_with,
            }
        )
    return pd.DataFrame(rows)


def _write_failed_teams_report(failed_teams: list[str], data_raw_dir: Path, run_tag: str) -> Path | None:
    """Write unresolved team names to a report file."""
    if not failed_teams:
        return None
    data_raw_dir = Path(data_raw_dir)
    run_dir = data_raw_dir / "pipeline_runs"
    run_dir.mkdir(parents=True, exist_ok=True)
    path = run_dir / f"team_history_failed_{run_tag}.txt"
    lines = sorted({t for t in failed_teams if t})
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


def build_training_from_all_tables(
    *,
    all_tables_path: Path,
    team_history_cache_path: Path,
    ext_data_dir: Path,
    data_raw_dir: Path,
    run_tag: str,
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
    refresh_team_history: bool = False,
    team_history_delay_sec: float = 0.8,
    team_history_force_playwright: bool = False,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Build training frame from all-trank base + Torvik team-history enrichment."""
    base = load_all_trank_base(all_tables_path, year_start, year_end, exclude_years)
    if base.empty:
        raise ValueError(f"No rows loaded from base file: {all_tables_path}")

    teams = sorted(base["Team"].dropna().astype(str).str.strip().unique().tolist())
    history_all, failed = build_team_history_all(
        teams=teams,
        cache_path=team_history_cache_path,
        refresh=refresh_team_history,
        delay_sec=team_history_delay_sec,
        force_playwright=team_history_force_playwright,
        user_agent=USER_AGENT,
    )

    skip = set(exclude_years)
    history_all = history_all.copy()
    # Coach stats only look back from each season, so they get the untrimmed history: excluded seasons (2020)
    # still count toward tenure, and _years_last_5_tournaments drops them from the tournament window.
    coach_history = history_all
    if not history_all.empty:
        history_all["year"] = pd.to_numeric(history_all["year"], errors="coerce")
        history_all = history_all.dropna(subset=["year"])
        history_all["year"] = history_all["year"].astype(int)
        coach_history = history_all
        history_all = history_all[
            (history_all["year"] >= year_start)
            & (history_all["year"] <= year_end)
            & (~history_all["year"].isin(skip))
        ]
        history_all["result"] = history_all["finish"].apply(_finish_to_result)
        history_all["round"] = history_all["result"].apply(
            lambda x: _result_to_round(x) if pd.notna(x) else np.nan
        )

    history_merge_cols = ["team", "year", "coach", "seed", "finish", "conf", "ov_rec", "conf_rec", "result", "round"]
    hist_for_merge = history_all[history_merge_cols].rename(
        columns={"team": "Team", "conf": "Conf_hist"}
    ) if not history_all.empty else pd.DataFrame(columns=["Team", "year"])
    merged = base.merge(hist_for_merge, on=["Team", "year"], how="left")
    if "Conf_hist" in merged.columns:
        merged["Conf"] = merged["Conf_hist"].fillna(merged.get("Conf", ""))
        merged = merged.drop(columns=["Conf_hist"], errors="ignore")

    coach_stats = compute_coach_stats_from_history(
        merged[["Team", "year"]].drop_duplicates(),
        coach_history,
        exclude_years=exclude_years,
    )
    merged = merged.merge(
        coach_stats,
        on=["Team", "year"],
        how="left",
        suffixes=("", "_coachcalc"),
    )
    if "coach_coachcalc" in merged.columns:
        merged["coach"] = merged["coach"].replace("", np.nan).fillna(merged["coach_coachcalc"])
        merged = merged.drop(columns=["coach_coachcalc"], errors="ignore")

    # Preserve one row per Team/year before finalization.
    merged = merged.drop_duplicates(subset=["Team", "year"], keep="first").reset_index(drop=True)

    for c in [
        "coach_tourn_appearances_5y",
        "coach_tourn_wins_5y",
        "coach_tourn_losses_5y",
        "coach_years_with_program",
    ]:
        if c not in merged.columns:
            merged[c] = 0
        merged[c] = pd.to_numeric(merged[c], errors="coerce").fillna(0).astype(int)

    report_path = _write_failed_teams_report(failed, data_raw_dir=data_raw_dir, run_tag=run_tag)
    if report_path is not None:
        LOGGER.warning(
            {
                "event": "team_history_missing_teams",
                "missing_count": len(failed),
                "report_path": str(report_path),
            }
        )

    return _finalize_training_frame(
        merged,
        ext_data_dir,
        skip_ext_merge=True,
        skip_coach_merge=True,
    )


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


def fetch_tournament_table_only(
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
) -> pd.DataFrame:
    """Fetch only the tournament (T) table via HTTP from main Bart page (trank.php) for seed/result enrichment."""
    import time
    tourn = []
    skip = set(exclude_years)
    for year in range(year_start, year_end + 1):
        if year in skip:
            continue
        t = _fetch_table(year, "T")  # HTTP only (no Playwright) — main Bart page
        if t is not None:
            step = 25
            idx_drop = list(range(0, len(t), step))
            if idx_drop:
                t = t.drop(index=t.index[idx_drop]).reset_index(drop=True)
            t["year"] = year
            tourn.append(t)
        time.sleep(2.5)
    return pd.concat(tourn, ignore_index=True) if tourn else pd.DataFrame()


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


def fetch_regular_table_for_enrichment(
    year_start: int,
    year_end: int,
    exclude_years: tuple[int, ...],
    use_playwright: bool = False,
) -> pd.DataFrame:
    """Fetch regular-season (R) table from main Bart page (trank.php). Tries HTTP first; use_playwright=True for fallback."""
    import time
    out = []
    skip = set(exclude_years)

    def _fetch_loop(page=None):
        for year in range(year_start, year_end + 1):
            if year in skip:
                continue
            r = _fetch_table(year, "R", page)
            if r is not None:
                out.append(r)
            time.sleep(2.5)

    if use_playwright:
        try:
            from playwright.sync_api import sync_playwright
            with sync_playwright() as p:
                browser = p.chromium.launch(headless=True)
                page = browser.new_page()
                _fetch_loop(page)
                browser.close()
        except Exception:
            _fetch_loop(None)
    else:
        _fetch_loop(None)

    return pd.concat(out, ignore_index=True) if out else pd.DataFrame()


def _regular_table_has_data(r_df: pd.DataFrame) -> bool:
    """True if R table has usable stats (e.g. AdjOE or WAB populated)."""
    if r_df is None or r_df.empty or len(r_df) < 2:
        return False
    for col in ("AdjOE", "AdjDE", "WAB"):
        if col in r_df.columns and r_df[col].notna().any():
            return True
    return False


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
        final = final.sort_values(sort_col, ascending=False).reset_index(drop=True)

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


# -----------------------------------------------------------------------------
# Modeling
# -----------------------------------------------------------------------------

# General model features (R: round ~ ... + optional coach experience when present)
GEN_FEATURES = [
    "TOR", "Barthag", "AdjOE", "ORB", "EFGD%", "3P%", "AdjDE",
    "seed", "TALENT", "AST%",
    "coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_years_with_program",
    "coach_tourn_win_pct_5y", "coach_tourn_games_5y", "coach_avg_wins_per_appearance",
]
GEN_POLY = {"Barthag": 2, "AdjDE": 2, "seed": 3}

# Seed model config: (hist_seeds, holdout_seeds, features)
SEED_MODELS = [
    ([1, 2], [1, 2], ["Barthag", "AdjOE", "OPAST%", "EXP.", "AdjDE", "seed", "predround"]),
    ([1, 2, 3], [1, 2, 3], ["Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([2, 3, 4], [2, 3, 4], ["Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([3, 4, 5], [3, 4, 5], ["AST%", "Barthag", "AdjOE", "2P%", "TORD", "AdjDE", "seed", "predround"]),
    ([4, 5, 6], [4, 5, 6], ["Barthag", "AdjOE", "AdjDE", "seed", "predround"]),
    ([5, 6, 7], [5, 6, 7], ["FTRD", "AdjOE", "AdjDE", "seed", "predround"]),
    ([6, 7, 8], [6, 7, 8], ["Barthag", "AdjOE", "TOR", "Adj T.", "2P%", "seed", "predround"]),
    ([7, 8, 9], [7, 8, 9], ["Barthag", "AdjOE", "AdjDE", "TOR", "BLKED%", "AST%", "OPAST%", "seed", "predround"]),
    ([8, 9, 10], [8, 9, 10], ["Barthag", "AdjOE", "AdjDE", "seed", "predround", "TOR", "BLKED%", "AST%", "3PRATE", "ELITESOS"]),
    ([9, 10, 11], [9, 10, 11], ["Barthag", "AdjOE", "AdjDE", "seed", "predround", "2P%D", "WINS", "3PRATED", "TALENT", "ELITESOS"]),
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
    if X.empty or len(y) == 0:
        return model_template, np.inf, np.array([]), np.array([])
    if len(X) < 3:
        # Too few for meaningful LOO; still fit on all data so returned model is fitted
        model_template.fit(X, y)
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


def _build_regression_model(ridge_alpha: float, fit_intercept: bool = True) -> Pipeline:
    """Build a scaled linear model (Ridge when alpha>0, else OLS).

    The intercept stays on: inputs are standardised, so without it every prediction centres
    on 0 wins inside each seed band and clips to 0 (53% of the 2025 field). fit_intercept=False
    exists only to reproduce that old behaviour in scripts/run_powered_comparison.py.
    """
    if ridge_alpha > 0:
        estimator = Ridge(alpha=ridge_alpha, fit_intercept=fit_intercept, random_state=42)
    else:
        estimator = LinearRegression(fit_intercept=fit_intercept)
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
    """Add any required derived feature columns if needed."""
    df = df.copy()
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


def run_pipeline(
    bracket_csv: str | None = None,
    n_sims: int | None = None,
    k_bradley_terry: float | None = None,
    ridge_alpha: float | None = None,
    optimize_ridge_alpha: bool | None = None,
    ridge_alpha_grid: list[float] | None = None,
    data_only: bool = False,
    year_start: int | None = None,
    year_end: int | None = None,
    refresh_team_history: bool = False,
    use_trank_only: bool = False,
) -> None:
    """Run full March Madness pipeline. If data_only=True, stop after compiling and validating training data (no models)."""
    cfg = load_config()
    if year_start is not None or year_end is not None:
        cfg.setdefault("ingestion", {})
        start = year_start if year_start is not None else cfg.get("ingestion", {}).get("year_range", [2008, 2025])[0]
        end = year_end if year_end is not None else cfg.get("ingestion", {}).get("year_range", [2008, 2025])[1]
        cfg["ingestion"]["year_range"] = [start, end]
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
    data_raw_dir = Path(_get_cfg_path(cfg, "data_raw", "data/raw"))
    data_processed_dir = Path(_get_cfg_path(cfg, "data_processed", "data/processed"))
    all_tables_path = Path(_get_cfg_path(cfg, "all_trank_team_tables", "data/raw/assembled/all_trank_team_tables.csv"))
    team_history_cache_path = Path(_get_cfg_path(cfg, "team_history_cache", "data/raw/team_history_cache.csv"))
    models_dir = Path(_get_cfg_path(cfg, "models", "models"))
    bracket_path = _get_cfg_path(cfg, "bracket_csv", "")
    year_range = cfg.get("ingestion", {}).get("year_range", [2008, 2025])
    year_start, year_end = year_range[0], year_range[1]
    exclude_years = tuple(cfg.get("ingestion", {}).get("exclude_years", [2020]))
    team_history_delay_sec = float(cfg.get("ingestion", {}).get("team_history_delay_sec", 0.8))
    team_history_force_playwright = bool(
        cfg.get("ingestion", {}).get("team_history_force_playwright", False)
    )
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

    if use_trank_only:
        print("Note: --trank-only is deprecated and ignored. Using all_trank_team_tables + team-history mode.")

    print("Building training data from all_trank_team_tables.csv + team-history.php...")
    training_data, training_data_full = build_training_from_all_tables(
        all_tables_path=all_tables_path,
        team_history_cache_path=team_history_cache_path,
        ext_data_dir=ext_dir,
        data_raw_dir=data_raw_dir,
        run_tag=run_tag,
        year_start=year_start,
        year_end=year_end,
        exclude_years=exclude_years,
        refresh_team_history=refresh_team_history,
        team_history_delay_sec=team_history_delay_sec,
        team_history_force_playwright=team_history_force_playwright,
    )

    out_path = write_csv(training_data_full, f"{out_prefix}_{run_tag}.csv")
    print(f"Training data written to {out_path}")

    # One clean, prepped dataset: no duplicate vars, no duplicate rows, all outcomes. Run-tagged:
    # data/processed/cbb_training_clean.csv is a frozen input of the log5 test (src/frozen_inputs.py),
    # cannot be rebuilt from live Torvik, and a default run covers only the configured year range.
    clean_prefix = _get_cfg_path(cfg, "output_training_clean", "data/processed/cbb_training_clean")
    clean_df = build_clean_training_dataset(training_data_full)
    clean_path = write_csv(clean_df, f"{clean_prefix}_{run_tag}.csv")
    print(
        f"Clean training dataset written to {clean_path} ({len(clean_df)} rows, {len(clean_df.columns)} columns, "
        f"years {year_start}-{year_end}); the frozen cbb_training_clean.csv is untouched"
    )

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
            research_path = write_csv(research_full, f"{research_prefix}_{run_tag}.csv")
            print(f"Research cut (last {research_seasons} seasons) written to {research_path} ({len(research_full)} rows)")

    if data_only:
        _validate_training_data(clean_df)
        print("Data-only run complete. No models trained.")
        return

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
