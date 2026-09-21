#!/usr/bin/env python3
"""
Small-scale test: fetch trank.php and report all tables and columns available.
Run from project root: python scripts/inspect_trank_page.py
"""

from __future__ import annotations

import sys
from io import StringIO
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

import pandas as pd

# Reuse pipeline helpers
BASE_URL = "https://barttorvik.com/trank.php"
USER_AGENT = "Mozilla/5.0"


def get_html_playwright(url: str, timeout_ms: int = 30000) -> str | None:
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("Playwright not installed. pip install playwright && playwright install chromium")
        return None

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        try:
            page.goto(url, wait_until="load", timeout=timeout_ms)
            # Wait for body then for verification to disappear (safe check)
            page.wait_for_selector("body", timeout=10000)
            # Give verification + JS table render time (trank loads data after verification)
            page.wait_for_timeout(8000)
            try:
                page.wait_for_selector("table", timeout=5000)
                page.wait_for_timeout(2000)
            except Exception:
                pass
        except Exception as e:
            print(f"Wait/nav: {e}")
        html = page.content()
        browser.close()

    if "Verifying Browser" in html or "Verifying your browser" in html:
        print("Page still showing verification; HTML may be incomplete.")
    return html


def main() -> None:
    year = 2025
    table_type = "R"
    url = f"{BASE_URL}?year={year}&sort=&top=0&conlimit=All&venue=All&type={table_type}#"
    print(f"Fetching: {url}\n")

    html = get_html_playwright(url)
    if not html:
        print("No HTML returned.")
        return

    # Save raw HTML and CSV sample for manual inspection (optional)
    out_dir = project_root / "data" / "raw" / "samples"
    out_dir.mkdir(parents=True, exist_ok=True)
    html_path = out_dir / "trank_page_sample.html"
    html_path.write_text(html, encoding="utf-8")
    print(f"Saved raw HTML to {html_path} ({len(html):,} chars)\n")

    try:
        tables = pd.read_html(StringIO(html))
    except ValueError as e:
        print(f"No tables in HTML: {e}")
        print(f"HTML length: {len(html):,} chars")
        print("First 1500 chars:", html[:1500])
        return

    print(f"Found {len(tables)} table(s).\n")

    # Import normalize after path is set
    from src.pipelines.madness import _normalize_columns, _TRANK_CANONICAL_COLS

    for i, tbl in enumerate(tables):
        print(f"=== Table {i} ===")
        print(f"  Shape: {tbl.shape}")
        raw_cols = list(tbl.columns)
        print(f"  Raw columns ({len(raw_cols)}):")
        for j, c in enumerate(raw_cols):
            if isinstance(c, tuple):
                c = " | ".join(str(x) for x in c)
            print(f"    [{j}] {repr(c)}")
        try:
            t = tbl.copy()
            t = _normalize_columns(t)
            t = t.loc[:, ~t.columns.duplicated()]
            norm_cols = list(t.columns)
            print(f"  After normalize ({len(norm_cols)}): {norm_cols}")
            n_canon = sum(1 for c in _TRANK_CANONICAL_COLS if c in t.columns)
            print(f"  Canonical cols matched: {n_canon} / {len(_TRANK_CANONICAL_COLS)}")
            if not t.empty and "Team" in t.columns:
                sample = t.head(2)
                print(f"  Sample (first 2 rows, first 8 cols):\n{sample.iloc[:, :8].to_string()}")
        except Exception as e:
            print(f"  Normalize error: {e}")
        print()

    # Pick "best" table and save a CSV slice for inspection
    best = None
    best_n = 0
    for tbl in tables:
        try:
            t = tbl.copy()
            t = _normalize_columns(t)
            t = t.loc[:, ~t.columns.duplicated()]
            if "Team" in t.columns and len(t) > 1:
                n = sum(1 for c in _TRANK_CANONICAL_COLS if c in t.columns)
                if n > best_n:
                    best_n = n
                    best = t
        except Exception:
            continue
    if best is not None:
        sample_path = out_dir / "trank_table_sample.csv"
        best.head(20).to_csv(sample_path, index=False)
        print(f"Saved best table sample (20 rows) to {sample_path}")
        print(f"Best table columns: {list(best.columns)}")


if __name__ == "__main__":
    main()
