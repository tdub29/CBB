#!/usr/bin/env python3
"""
Fetch Bart Torvik team-tables CSV via Playwright using your already-open browser.

Connects to Chrome (or Edge) with remote debugging enabled and loops through
team-tables_each.php?year=YEAR&...&type=P&yax=3&csv=1 for 2008–2025 (excluding 2020),
saving each year's CSV to data/external/.

Either:
  A) Already have Chrome open with debugging: run script and it connects.
  B) Let the script launch Chrome:  python scripts/fetch_team_tables_browser.py --launch-chrome

Manual launch (close all Chrome first):
  Chrome:  "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe" --remote-debugging-port=9222
  Edge:    "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe" --remote-debugging-port=9222
"""

from __future__ import annotations

import argparse
import subprocess
import sys
import time
from pathlib import Path

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Years to fetch: 2008–2025 excluding 2020
YEARS = [y for y in range(2008, 2026) if y != 2020]

BASE_URL = (
    "https://www.barttorvik.com/team-tables_each.php"
    "?year={year}&top=0&conlimit=All&venue=All&type=P&yax=3&csv=1"
)
OUT_DIR = project_root / "data" / "external"
DELAY_SEC = 2.5
DEFAULT_CDP_PORT = 9222


def main() -> int:
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("Playwright not installed. Run: pip install playwright && playwright install chromium")
        return 1

    parser = argparse.ArgumentParser(description="Fetch Bart Torvik team-tables via your browser (CDP).")
    parser.add_argument(
        "--port",
        type=int,
        default=DEFAULT_CDP_PORT,
        help=f"Chrome/Edge remote-debugging port (default {DEFAULT_CDP_PORT})",
    )
    parser.add_argument("--cdp-url", type=str, help="Full CDP URL (overrides --port), e.g. http://localhost:9222")
    parser.add_argument(
        "--launch-chrome",
        action="store_true",
        help="Start Chrome with remote-debugging-port then connect (close other Chrome windows first)",
    )
    args = parser.parse_args()

    if args.launch_chrome:
        chrome_paths = [
            Path(r"C:\Program Files\Google\Chrome\Application\chrome.exe"),
            Path(r"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe"),
        ]
        chrome_exe = None
        for p in chrome_paths:
            if p.exists():
                chrome_exe = p
                break
        if not chrome_exe:
            print("Chrome not found. Start it manually with --remote-debugging-port=9222")
            return 1
        print(f"Starting Chrome with debugging on port {args.port}...")
        subprocess.Popen(
            [str(chrome_exe), f"--remote-debugging-port={args.port}"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        print("Waiting for Chrome CDP (up to 30s)... Close all other Chrome windows if this hangs.")
        time.sleep(5)

    cdp_url = args.cdp_url or f"http://127.0.0.1:{args.port}"
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    with sync_playwright() as p:
        browser = None
        last_err = None
        for attempt in range(6):
            try:
                browser = p.chromium.connect_over_cdp(cdp_url)
                break
            except Exception as e:
                last_err = e
                if attempt < 5:
                    time.sleep(3)
        if browser is None:
            print(
                f"Could not connect to browser at {cdp_url}.\n"
                "If using --launch-chrome: close ALL Chrome windows, then run again.\n"
                "Or start Chrome manually: chrome.exe --remote-debugging-port=9222"
            )
            print(f"Error: {last_err}")
            return 1

        # Use existing page if possible, else create one
        contexts = browser.contexts
        if contexts and contexts[0].pages:
            page = contexts[0].pages[0]
        else:
            page = browser.new_page()

        for year in YEARS:
            url = BASE_URL.format(year=year)
            print(f"Fetching {year}...", end=" ", flush=True)
            try:
                resp = page.goto(url, wait_until="load", timeout=60000)
                if resp and resp.ok:
                    body = resp.text()
                    if body and ("Team," in body or "team," in body or "TEAM," in body):
                        out_path = OUT_DIR / f"trank_team_table_data_{year}.csv"
                        out_path.write_text(body, encoding="utf-8")
                        print(f"OK -> {out_path.name}")
                    else:
                        print("skipped (no CSV body or wrong format)")
                else:
                    print(f"skipped (status {getattr(resp, 'status', '?')})")
            except Exception as e:
                print(f"error: {e}")
            time.sleep(DELAY_SEC)

        # Disconnect only; do not close the user's browser
        browser.close()

    print("Done.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
