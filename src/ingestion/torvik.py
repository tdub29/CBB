"""Bart Torvik T-Rank pages (trank.php): fetch over HTTP, or Playwright when blocked, and parse the stat tables."""

from __future__ import annotations

from io import StringIO
from pathlib import Path

import numpy as np
import pandas as pd
import requests

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
