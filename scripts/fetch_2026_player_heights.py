#!/usr/bin/env python3
"""
Fetch player heights for 2026 tournament teams from Bart Torvik team pages via Playwright,
then join heights into data/raw/player_stats/player_advanced_all.csv.

Team list: data/raw/2026 expected seeds.xlsx (column "Team"). If missing, uses unique
teams from player_advanced_all where year=2026.

Usage:
  Default - headless browser (no Chrome to open; runs in background):
    python scripts/fetch_2026_player_heights.py
    python scripts/fetch_2026_player_heights.py --team Duke

  Optional - use your already-open Chrome (for debugging):
    chrome.exe --remote-debugging-port=9222
    python scripts/fetch_2026_player_heights.py --cdp-url http://127.0.0.1:9222

  Join only (skip fetch, use existing heights CSV):
    python scripts/fetch_2026_player_heights.py --join-only

  Show full roster for one team from existing heights CSV:
    python scripts/fetch_2026_player_heights.py --show-team Duke
"""

from __future__ import annotations

import argparse
import re
import sys
import time
from pathlib import Path
from urllib.parse import quote_plus

import pandas as pd

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

YEAR = 2026
BASE_URL = "https://barttorvik.com/team.php?team={team}&year={year}"
HEIGHTS_CSV = project_root / "data" / "raw" / "player_stats" / "player_heights_2026.csv"
PLAYER_ADVANCED_ALL = project_root / "data" / "raw" / "player_stats" / "player_advanced_all.csv"
SEEDS_2026_XLSX = project_root / "data" / "raw" / "2026 expected seeds.xlsx"
DELAY_SEC = 2.5

# Height pattern: e.g. 6-9, 6-2, 6-10, 5-11
HEIGHT_RE = re.compile(r"\b(\d-\d{1,2})\b")


def clean_team_name(value: object) -> str:
    """Strip Excel artifacts: ***, \\xa0, trailing seed digits."""
    text = str(value).replace("\xa0", " ").strip()
    text = re.sub(r"\*+", "", text)
    text = re.sub(r"\s+\d+$", "", text).strip()
    return text


def get_2026_teams(players_df: pd.DataFrame) -> list[str]:
    """Expected 2026 tournament teams: from seeds xlsx, else unique teams in player_advanced for year 2026."""
    if SEEDS_2026_XLSX.exists():
        try:
            field = pd.read_excel(SEEDS_2026_XLSX)
            if "Team" in field.columns:
                raw = field["Team"].dropna().astype(str)
                teams = [clean_team_name(t) for t in raw.unique() if clean_team_name(t)]
                return sorted(set(teams))
        except Exception as e:
            print(f"Warning: could not read {SEEDS_2026_XLSX.name}: {e}")
    # Fallback: all teams that have 2026 player data
    y2026 = players_df[players_df["year"] == YEAR]
    return sorted(y2026["team"].dropna().unique().tolist())


def team_to_url_param(team: str) -> str:
    """Encode team name for URL (spaces -> +, e.g. Texas+Tech)."""
    return quote_plus(team)


def parse_player_height_from_text(text: str) -> tuple[str | None, str | None]:
    """
    Extract height (e.g. 6-9) and player name from a cell that may look like:
      "#12 Fr 6-9 Cameron Boozer" or "Cameron Boozer #12, 6-9, Fr"
    Returns (height, name) or (None, None).
    """
    if not text or not text.strip():
        return None, None
    text = " ".join(text.split())
    match = HEIGHT_RE.search(text)
    if not match:
        return None, None
    height = match.group(1)
    # Name: remove #digits, height, and class (Fr/So/Jr/Sr) tokens, take the rest
    name_part = HEIGHT_RE.sub(" ", text)
    name_part = re.sub(r"#\s*\d+\s*", " ", name_part)
    name_part = re.sub(r"\b(Fr|So|Jr|Sr)\b", " ", name_part, flags=re.I)
    name_part = re.sub(r"[,.]", " ", name_part)
    tokens = [t for t in name_part.split() if t and not t.isdigit() and len(t) > 1]
    name = " ".join(tokens).strip() if tokens else None
    return height, name


def scrape_team_roster(page, team: str) -> list[dict]:
    """Use Playwright page to load team.php and return list of {player, team, height}."""
    url = BASE_URL.format(team=team_to_url_param(team), year=YEAR)
    try:
        resp = page.goto(url, wait_until="networkidle", timeout=45000)
        if resp and resp.status != 200:
            return []
    except Exception:
        return []
    time.sleep(3)
    try:
        page.wait_for_selector("table", timeout=20000)
    except Exception:
        pass
    def parse_table_into_rows(table_locator):
        out = []
        try:
            trs = table_locator.locator("tbody tr").all()
            if not trs:
                trs = table_locator.locator("tr").all()
        except Exception:
            return out
        for tr in trs:
            try:
                cells = tr.locator("td").all()
            except Exception:
                continue
            if not cells:
                continue
            try:
                first_cell_text = cells[0].inner_text(timeout=5000).strip()
            except Exception:
                continue
            if not first_cell_text:
                continue
            height_val, name_val = parse_player_height_from_text(first_cell_text)
            if not name_val and len(cells) >= 3:
                for i in range(1, min(4, len(cells))):
                    try:
                        t = cells[i].inner_text(timeout=3000).strip()
                    except Exception:
                        continue
                    if HEIGHT_RE.match(t):
                        height_val = height_val or HEIGHT_RE.match(t).group(1)
                        if i + 1 < len(cells):
                            try:
                                name_val = cells[i + 1].inner_text(timeout=3000).strip()
                            except Exception:
                                pass
                        break
            if not height_val or not name_val or len(name_val) < 3:
                continue
            skip_keywords = ("games", "projected", "current", "close", "trace", "tue", "mon", "wed", "thu", "fri", "sat", "sun")
            if any(k in name_val.lower() for k in skip_keywords):
                continue
            name_parts = name_val.split()
            if len(name_parts) < 2:
                continue
            out.append({"player": name_val.strip(), "team": team, "height": height_val})
        return out

    rows: list[dict] = []
    try:
        # Try table with "Min%" first (player stats); if none or empty, try every table and take the one with most players
        roster_table = page.locator("table:has-text('Min%')").first
        try:
            roster_table.wait_for(state="visible", timeout=2000)
            rows = parse_table_into_rows(roster_table)
        except Exception:
            rows = []
        if not rows:
            tables = page.locator("table").all()
            for tbl in tables:
                candidate = parse_table_into_rows(tbl)
                if len(candidate) > len(rows):
                    rows = candidate
    except Exception as e:
        print(f"  parse error: {e}")
    return rows


def fetch_all_heights(teams: list[str], *, cdp_url: str | None = None) -> pd.DataFrame:
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        print("Playwright not installed. Run: pip install playwright && playwright install chromium")
        sys.exit(1)

    all_rows: list[dict] = []

    with sync_playwright() as p:
        if cdp_url:
            # Attach to existing Chrome (user opened with --remote-debugging-port=9222)
            browser = None
            for attempt in range(6):
                try:
                    browser = p.chromium.connect_over_cdp(cdp_url)
                    break
                except Exception as e:
                    if attempt < 5:
                        time.sleep(3)
                    else:
                        print(f"Could not connect to browser at {cdp_url}. Error: {e}")
                        sys.exit(1)
            context = browser.contexts[0] if browser.contexts else browser.new_context()
            page = context.pages[0] if context.pages else context.new_page()
        else:
            # Default: headless Chromium (no Chrome to open)
            browser = p.chromium.launch(headless=True)
            context = browser.new_context()
            page = context.new_page()

        for i, team in enumerate(teams):
            print(f"[{i+1}/{len(teams)}] {team}...", end=" ", flush=True)
            roster = scrape_team_roster(page, team)
            if roster:
                all_rows.extend(roster)
                print(f"{len(roster)} players")
                # Print full roster when testing a single team so user can verify
                if len(teams) == 1:
                    print("\n--- Full roster (player, height) ---")
                    for r in roster:
                        print(f"  {r['player']}\t{r['height']}")
                    print("---\n")
            else:
                print("(none)")
            time.sleep(DELAY_SEC)

        browser.close()

    return pd.DataFrame(all_rows).drop_duplicates(subset=["player", "team"], keep="first")


def join_heights_to_advanced(heights_df: pd.DataFrame, advanced_path: Path) -> None:
    """Add height column to player_advanced_all for year 2026; write back to CSV."""
    df = pd.read_csv(advanced_path)
    h = heights_df[["player", "team", "height"]].copy()
    h["player"] = h["player"].astype(str).str.strip()
    h["team"] = h["team"].astype(str).str.strip()
    df["player"] = df["player"].astype(str).str.strip()
    df["team"] = df["team"].astype(str).str.strip()

    if "height" in df.columns:
        df = df.drop(columns=["height"])
    merged = df.merge(
        h.drop_duplicates(subset=["player", "team"]),
        on=["player", "team"],
        how="left",
    )
    # Only keep height for year 2026 (avoid writing 2026 height onto other years)
    merged["height"] = merged["height"].where(merged["year"] == YEAR)
    merged.to_csv(advanced_path, index=False)
    matched = merged.loc[merged["year"] == YEAR, "height"].notna().sum()
    total_2026 = (merged["year"] == YEAR).sum()
    print(f"Joined heights: {matched} of {total_2026} year-{YEAR} rows matched.")


def main() -> int:
    # Avoid UnicodeEncodeError when printing player names to Windows console
    if hasattr(sys.stdout, "reconfigure"):
        try:
            sys.stdout.reconfigure(encoding="utf-8")
        except Exception:
            pass
    parser = argparse.ArgumentParser(description="Fetch 2026 player heights from Bart Torvik and join to player_advanced_all.")
    parser.add_argument("--join-only", action="store_true", help="Skip fetch; only join existing heights CSV to player_advanced_all.")
    parser.add_argument("--team", type=str, metavar="NAME", help="Fetch only this team (e.g. Duke) for testing.")
    parser.add_argument("--show-team", type=str, metavar="NAME", help="Print full roster for one team from existing player_heights_2026.csv (no fetch).")
    parser.add_argument("--dry-run", action="store_true", help="Parse sample Duke table only; no browser. Shows expected output format.")
    parser.add_argument("--cdp-url", type=str, help="Use existing Chrome: pass e.g. http://127.0.0.1:9222 (Chrome must be started with --remote-debugging-port=9222).")
    args = parser.parse_args()

    advanced_path = PLAYER_ADVANCED_ALL
    if not advanced_path.exists():
        print(f"Missing {advanced_path}")
        return 1

    players_df = pd.read_csv(advanced_path)

    if args.join_only:
        if not HEIGHTS_CSV.exists():
            print(f"Missing {HEIGHTS_CSV}. Run without --join-only to fetch heights first.")
            return 1
        heights_df = pd.read_csv(HEIGHTS_CSV)
        join_heights_to_advanced(heights_df, advanced_path)
        return 0

    if args.show_team:
        if not HEIGHTS_CSV.exists():
            print(f"Missing {HEIGHTS_CSV}. Fetch with --team {args.show_team} first.")
            return 1
        df = pd.read_csv(HEIGHTS_CSV)
        team_name = args.show_team.strip()
        team_df = df[df["team"].astype(str).str.strip().str.lower() == team_name.lower()]
        if team_df.empty:
            print(f"No rows for team {team_name!r} in {HEIGHTS_CSV.name}")
            return 1
        print(f"--- Full roster: {team_name} ({YEAR}) ({len(team_df)} players) ---")
        for _, r in team_df.iterrows():
            print(f"  {r['player']}\t{r['height']}")
        print("---")
        return 0

    if args.dry_run:
        # Sample row text similar to Bart Torvik team.php (e.g. Duke 2026)
        sample_rows = [
            "#12 Fr 6-9 Cameron Boozer",
            "#5 So 6-5 Kon Knueppel",
            "#2 Jr 6-2 Caleb Foster",
        ]
        print("Dry run: parsing sample player rows (Duke-style)\n")
        out = []
        for text in sample_rows:
            height, name = parse_player_height_from_text(text)
            row = {"player": name, "team": "Duke", "height": height}
            out.append(row)
            print(f"  {text!r} -> player={name!r} height={height!r}")
        print("\nResulting DataFrame (first 3 rows):")
        print(pd.DataFrame(out).to_string(index=False))
        return 0

    if args.team:
        teams = [args.team.strip()]
        print(f"Testing single team: {teams[0]}")
    else:
        teams = get_2026_teams(players_df)
    print(f"Fetching heights for {len(teams)} teams (year={YEAR})...")
    # Default: headless. Pass --cdp-url to attach to existing Chrome instead.
    heights_df = fetch_all_heights(teams, cdp_url=args.cdp_url)
    if heights_df.empty:
        print("No heights scraped.")
        return 1
    HEIGHTS_CSV.parent.mkdir(parents=True, exist_ok=True)
    heights_df.to_csv(HEIGHTS_CSV, index=False)
    print(f"Wrote {HEIGHTS_CSV} ({len(heights_df)} rows).")
    join_heights_to_advanced(heights_df, advanced_path)
    return 0


if __name__ == "__main__":
    sys.exit(main())
