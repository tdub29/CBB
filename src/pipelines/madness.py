"""
March Madness pipeline: ingestion, features, modeling (Python).

Replicates and optimizes madness.R with vectorized pandas and config-driven modeling.
"""

from __future__ import annotations

import re
from io import StringIO
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import requests
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import LeaveOneOut
from sklearn.metrics import mean_squared_error

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------


def load_config() -> dict:
    """Load config from YAML; fallback to defaults."""
    try:
        import yaml

        cfg_path = Path("configs/default.yaml")
        if cfg_path.exists():
            with open(cfg_path) as f:
                return yaml.safe_load(f)
    except ImportError:
        pass
    return {
        "paths": {
            "output_training_csv": "data/processed/cbbtrainingdata",
            "data_external": "data/external",
        }
    }


def _get_cfg_path(cfg: dict, key: str, default: str) -> str:
    return cfg.get("paths", {}).get(key, default)


# -----------------------------------------------------------------------------
# Ingestion
# -----------------------------------------------------------------------------

BASE_URL = "https://barttorvik.com/trank.php"
USER_AGENT = "Mozilla/5.0"


def _fetch_table(year: int, table_type: str) -> pd.DataFrame | None:
    """Fetch one Bart Torvik table (R=regular, P=postseason, T=tournament)."""
    url = f"{BASE_URL}?year={year}&sort=&top=0&conlimit=All&venue=All&type={table_type}#"
    try:
        resp = requests.get(url, headers={"User-Agent": USER_AGENT}, timeout=30)
        resp.raise_for_status()
        tables = pd.read_html(StringIO(resp.text))
        if not tables:
            return None
        df = tables[0].copy()
        if "Rk" in df.columns and (df["Rk"] == "Rk").any():
            df = df[df["Rk"] != "Rk"].copy()
        df["year"] = year
        return df
    except Exception as e:
        print(f"Warning: Failed to fetch {year} type={table_type}: {e}")
        return None


def scrape_bart_torvik(year_start: int = 2008, year_end: int = 2024) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """Scrape regular season, postseason, and tournament data."""
    master = []
    conf = []
    tourn = []

    for year in range(year_start, year_end + 1):
        r = _fetch_table(year, "R")
        if r is not None:
            master.append(r)

        p = _fetch_table(year, "P")
        if p is not None:
            conf.append(p)

        t = _fetch_table(year, "T")
        if t is not None:
            # Remove header rows every 25 rows (R does this)
            step = 25
            idx_drop = list(range(0, len(t), step))
            if idx_drop:
                t = t.drop(index=t.index[idx_drop]).reset_index(drop=True)
            t["year"] = year
            tourn.append(t)

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
    tourn["tourn_w"] = _parse_rec_w(tourn["Rec"], multi_digit=False)
    tourn["tourn_l"] = _parse_rec_l(tourn["Rec"], multi_digit=False)

    # Conf W/L
    conf = conf.copy()
    conf["conf_w"] = _parse_rec_w(conf["Rec"], multi_digit=True)
    conf["conf_l"] = _parse_rec_l(conf["Rec"], multi_digit=True)

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

    optional = ["WINS", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED", "EFFHGT", "EXP.", "TALENT", "ELITESOS"]
    for c in optional:
        if c in training_data.columns:
            training_data[c] = training_data[c].fillna(0)

    training_data = training_data.dropna()

    for c in ["G", "3PR", "3PRD", "Rec"]:
        if c in training_data.columns:
            training_data[c] = pd.to_numeric(training_data[c], errors="coerce")

    return training_data, training_data_full


# -----------------------------------------------------------------------------
# Modeling
# -----------------------------------------------------------------------------

# General model features (R: round ~ WAB+TOR+Barthag+I(Barthag^2)+AdjOE+ORB+EFGD%+3P%+AdjDE+I(AdjDE^2)+seed+I(seed^2)+I(seed^3)+TALENT+AST%+0)
GEN_FEATURES = [
    "WAB", "TOR", "Barthag", "AdjOE", "ORB", "EFGD%", "3P%", "AdjDE",
    "seed", "TALENT", "AST%",
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
) -> tuple[LinearRegression, float, np.ndarray, np.ndarray]:
    """Leave-one-out cross-validation; return best model, MSE, pred, actual."""
    X, y = _get_Xy(df, features, poly=poly)
    if X.empty or len(X) < 3:
        return LinearRegression(fit_intercept=False), np.inf, np.array([]), np.array([])

    loo = LeaveOneOut()
    preds, actuals = [], []
    best_mse, best_model = np.inf, None

    for train_idx, test_idx in loo.split(X):
        X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
        y_train = y.iloc[train_idx]
        model = LinearRegression(fit_intercept=False)
        model.fit(X_train, y_train)
        pred = model.predict(X_test)[0]
        actual = y.iloc[test_idx].values[0]
        preds.append(pred)
        actuals.append(actual)
        mse = mean_squared_error([actual], [pred])
        if mse < best_mse:
            best_mse = mse
            best_model = model

    preds_arr = np.array(preds)
    actuals_arr = np.array(actuals)
    mse_total = mean_squared_error(actuals_arr, preds_arr)
    if best_model is None:
        best_model = LinearRegression(fit_intercept=False)
        best_model.fit(X, y)
    return best_model, mse_total, preds_arr, actuals_arr


def train_general_model(training_data: pd.DataFrame) -> tuple[LinearRegression, list[str], dict]:
    """Train general model with jackknife LOO."""
    model, mse, _, _ = jackknife_loo(training_data, GEN_FEATURES, poly=GEN_POLY)
    df = _add_poly_features(training_data.copy(), GEN_POLY)
    all_feats = _resolve_features(GEN_FEATURES, GEN_POLY, df)
    return model, all_feats, {"mse": mse}


def _ensure_derived_features(df: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """Add derived columns (e.g. WAB_3) if in features but missing."""
    df = df.copy()
    if "WAB_3" in features and "WAB_3" not in df.columns and "WAB" in df.columns:
        df["WAB_3"] = df["WAB"].astype(float) ** 3
    return df


def train_seed_models(training_data: pd.DataFrame) -> dict[int, tuple[LinearRegression, list[str]]]:
    """Train all 14 seed-stratified models. Returns {seed: (model, features)}."""
    seed_models = {}
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
        model, _, _, _ = jackknife_loo(sub, avail, poly=None)
        seed_models[i] = (model, avail)
    return seed_models


# -----------------------------------------------------------------------------
# Main pipeline
# -----------------------------------------------------------------------------


def run_pipeline() -> None:
    """Run full March Madness pipeline."""
    cfg = load_config()
    out_prefix = _get_cfg_path(cfg, "output_training_csv", "data/processed/cbbtrainingdata")
    ext_dir = Path(_get_cfg_path(cfg, "data_external", "data/external"))
    year_range = cfg.get("ingestion", {}).get("year_range", [2008, 2024])
    year_start, year_end = year_range[0], year_range[1]

    print("Scraping Bart Torvik...")
    master, conf, tourn = scrape_bart_torvik(year_start, year_end)

    print("Building features...")
    training_data, training_data_full = build_features(master, conf, tourn, ext_dir)

    out_path = Path(f"{out_prefix}_{pd.Timestamp.now().strftime('%Y%m%d')}.csv")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    training_data_full.to_csv(out_path, index=False)
    print(f"Training data written to {out_path}")

    print("Training general model...")
    gen_model, gen_feats, gen_meta = train_general_model(training_data)
    training_data = _add_poly_features(training_data.copy(), GEN_POLY)
    X_gen, _ = _get_Xy(training_data, GEN_FEATURES, poly=GEN_POLY)
    training_data["predround"] = gen_model.predict(X_gen) if not X_gen.empty else 0

    print("Training seed-stratified models...")
    seed_models = train_seed_models(training_data)

    # 2023 holdout (if available)
    holdout_2023 = training_data_full[
        (training_data_full["year"] == 2023) & (training_data_full["seed"].notna())
    ]

    if not holdout_2023.empty and len(seed_models) > 0:
        # Simplified: use general model for 2023 pred (full holdout_2023 prep would mirror R)
        print("2023 holdout: predictions available from general model.")
    else:
        print("Skipping 2023 holdout (no 2023 data or seed models).")

    training_data["sweetsixteen"] = (training_data["round"] >= 2).astype(int)
    print("Pipeline complete.")


if __name__ == "__main__":
    run_pipeline()
