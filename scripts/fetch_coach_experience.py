#!/usr/bin/env python3
"""
Build coach_tournament_experience.csv for the pipeline.

Head coach NCAA tournament experience over the 5 years prior to the current season:
- coach_tourn_appearances_5y: number of tournaments in the 5 years before current season
- coach_tourn_wins_5y / coach_tourn_losses_5y: tournament W-L in those years

Year 2020 is excluded (no NCAA tournament). Output is written to data/external/
and merged by the pipeline on (Team, year). Missing rows are filled with 0.

Data source: Bart Torvik team history (same as toRvik's bart_team_history):
https://www.torvik.dev/reference/bart_team_history.php
https://barttorvik.com/team-history.php?team=Team+Name

Usage:
  python scripts/fetch_coach_experience.py
      [--training-csv PATH] [--out-dir PATH]
      [--fetch] [--source torvik|sr] [--year-start YYYY] [--year-end YYYY]

Without --fetch: generates a template CSV (zeros). With --fetch: fills from
Bart Torvik (default, same source as pipeline) or Sports Reference (--source sr).
"""

from __future__ import annotations

import argparse
import re
import sys
import time
from pathlib import Path
from urllib.parse import quote_plus

project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

import pandas as pd

# Columns expected by pipeline
COACH_COLS = ["coach_tourn_appearances_5y", "coach_tourn_wins_5y", "coach_tourn_losses_5y", "coach_years_with_program"]
EXCLUDE_YEAR = 2020  # No NCAA tournament


def _find_latest_training_csv(processed_dir: Path) -> Path | None:
    """Return path to most recent full export, cbbtrainingdata_YYYYMMDD.csv (not the _last2seasons_ cut)."""
    processed_dir = Path(processed_dir)
    if not processed_dir.exists():
        return None
    files = list(processed_dir.glob("cbbtrainingdata_[0-9]*.csv"))
    if not files:
        return None
    return max(files, key=lambda p: p.stat().st_mtime)


def _team_year_from_training(csv_path: Path) -> pd.DataFrame:
    """Get unique (Team, Year) from training CSV."""
    df = pd.read_csv(csv_path)
    if "Team" not in df.columns:
        raise ValueError(f"Training CSV must have 'Team' column: {csv_path}")
    year_col = "year" if "year" in df.columns else "Year"
    if year_col not in df.columns:
        raise ValueError(f"Training CSV must have 'year' or 'Year' column: {csv_path}")
    out = df[["Team", year_col]].drop_duplicates()
    out = out.rename(columns={year_col: "Year"})
    out["Year"] = pd.to_numeric(out["Year"], errors="coerce")
    out = out.dropna().astype({"Year": int})
    return out


def build_template(training_csv: Path) -> pd.DataFrame:
    """Build coach experience table with zeros from training (Team, Year) list."""
    team_year = _team_year_from_training(training_csv)
    for c in COACH_COLS:
        team_year[c] = 0
    return team_year[["Team", "Year"] + COACH_COLS]


def _years_prior_5(season_year: int) -> list[int]:
    """Five tournament years prior to season_year, excluding the current year and EXCLUDE_YEAR."""
    out = []
    for y in range(season_year - 5, season_year):
        if y != EXCLUDE_YEAR:
            out.append(y)
    return out


# Bart Torvik team-history.php: finish string -> (wins, losses) in NCAA tournament
FINISH_TO_WL: dict[str, tuple[int, int]] = {
    "r64": (0, 1), "64": (0, 1), "first four": (0, 1),
    "r32": (1, 1), "32": (1, 1),
    "s16": (2, 1), "sweet 16": (2, 1), "16": (2, 1),
    "e8": (3, 1), "elite eight": (3, 1), "8": (3, 1),
    "f4": (4, 1), "final four": (4, 1), "4": (4, 1),
    "2nd": (5, 1), "runner": (5, 1), "finals": (5, 1),
    "champion": (6, 0), "champ": (6, 0), "1": (6, 0), "1st": (6, 0),
}


def _finish_to_wl(finish: str) -> tuple[int, int]:
    """Map Torvik finish string to (tournament_wins, tournament_losses)."""
    if not finish or not isinstance(finish, str):
        return (0, 0)
    s = finish.strip().lower()
    for key, wl in FINISH_TO_WL.items():
        if key in s:
            return wl
    if re.search(r"^\d+-\d+$", s):  # e.g. "2-1"
        parts = s.split("-")
        return (int(parts[0]), int(parts[1]))
    return (0, 0)


def fetch_from_torvik(
    team_year: pd.DataFrame,
    user_agent: str = "Mozilla/5.0 (compatible; CBB pipeline; +https://github.com)",
    delay_sec: float = 0.8,
    force_playwright: bool = False,
) -> pd.DataFrame:
    """
    Fill coach experience from Bart Torvik team-history.php (same as toRvik's bart_team_history).
    Returns DataFrame with Team, Year, coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_tourn_losses_5y.
    """
    try:
        import requests
        from bs4 import BeautifulSoup
    except ImportError:
        print("Install requests and beautifulsoup4: pip install requests beautifulsoup4", file=sys.stderr)
        return team_year.assign(**{c: 0 for c in COACH_COLS})

    base = "https://barttorvik.com/team-history.php"
    session = requests.Session()
    session.headers.update({
        "User-Agent": user_agent,
        "Accept": "text/html,application/xhtml+xml",
        "Referer": "https://barttorvik.com/",
    })

    use_playwright = [force_playwright]  # mutable so closure can update

    def _get_html(url: str) -> str | None:
        if not use_playwright[0]:
            try:
                r = session.get(url, timeout=15)
                r.raise_for_status()
                if "Verifying Browser" in r.text or "Verifying your browser" in r.text:
                    use_playwright[0] = True
                    print("  Torvik bot check detected, switching to Playwright...", file=sys.stderr)
                    return _get_html_playwright(url)
                return r.text
            except Exception:
                use_playwright[0] = True
                return _get_html_playwright(url)
        return _get_html_playwright(url)

    pw_state = {"p": None, "browser": None, "page": None}

    def _get_html_playwright(url: str) -> str | None:
        try:
            from playwright.sync_api import sync_playwright
            if pw_state["browser"] is None:
                pw_state["p"] = sync_playwright().start()
                pw_state["browser"] = pw_state["p"].chromium.launch(headless=True)
                pw_state["page"] = pw_state["browser"].new_page()
            page = pw_state["page"]
            page.goto(url, wait_until="networkidle", timeout=30000)
            page.wait_for_timeout(3500)
            html = page.content()
            if "Verifying Browser" in html:
                return None
            return html
        except Exception:
            return None

    def _close_pw():
        if pw_state.get("browser"):
            try:
                pw_state["browser"].close()
            except Exception:
                pass
            pw_state["browser"] = None
        if pw_state.get("p"):
            try:
                pw_state["p"].stop()
            except Exception:
                pass
            pw_state["p"] = None

    # 1) Fetch team history for each unique team; build (team, year, coach, tourn_w, tourn_l)
    team_history_rows: list[dict] = []
    teams = team_year["Team"].unique().tolist()
    n_teams = len(teams)
    try:
        for i, team in enumerate(teams):
            if (i + 1) % 25 == 0 or i == 0:
                print(f"  Fetching team {i+1}/{n_teams}: {team}", file=sys.stderr)
            team_str = str(team).strip()
            if not team_str:
                continue
            q = quote_plus(team_str)  # "Texas A&M" -> Texas+A%26M
            url = f"{base}?team={q}"
            try:
                html = _get_html(url)
                if not html:
                    time.sleep(delay_sec)
                    continue
                soup = BeautifulSoup(html, "html.parser")
                tables = soup.find_all("table")
                if not tables:
                    time.sleep(delay_sec)
                    continue
                table = tables[0]
                # Table: year (col 0), t_rk, coach (name + seed + finish in one cell), rec, ...
                for tr in table.find_all("tr")[1:]:  # skip header
                    cells = tr.find_all(["td", "th"])
                    if len(cells) < 3:
                        continue
                    try:
                        year_val = int(cells[0].get_text(strip=True))
                    except (ValueError, IndexError):
                        continue
                    coach_cell = cells[2].get_text(strip=True) if len(cells) > 2 else ""
                    if not coach_cell:
                        continue
                    # Parse coach cell: "Dan Hurley8 seed, R32" or "Dan Hurley 3 R32" -> coach name, finish
                    coach_match = re.match(r"^(.+?)(\d+)\s*seed\s*[,]?\s*(.*)$", coach_cell)
                    if coach_match:
                        coach_name = coach_match.group(1).strip()
                        finish = coach_match.group(3).strip()
                    else:
                        coach_match = re.match(r"^(.+?)\s+(\d+)[,\s]+(.+)$", coach_cell)
                        if coach_match:
                            coach_name = coach_match.group(1).strip()
                            finish = coach_match.group(3).strip()
                        else:
                            coach_name = coach_cell
                            finish = ""
                    w, l = _finish_to_wl(finish)
                    team_history_rows.append({
                        "team": team_str,
                        "year": year_val,
                        "coach": coach_name,
                        "tourn_w": w,
                        "tourn_l": l,
                    })
                time.sleep(delay_sec)
            except Exception:
                time.sleep(delay_sec)
                continue
    finally:
        _close_pw()

    if not team_history_rows:
        out = team_year.copy()
        for c in COACH_COLS:
            out[c] = 0
        return out

    hist = pd.DataFrame(team_history_rows)
    # 2) For each (team, year) in team_year: get coach at that team/year, then sum that coach's prior-5 stats
    rows = []
    for _, r in team_year.iterrows():
        team, year = r["Team"], int(r["Year"])
        prior = _years_prior_5(year)
        # Coach at this team in this year
        coach_row = hist[(hist["team"] == team) & (hist["year"] == year)]
        if coach_row.empty:
            rows.append({"Team": team, "Year": year, **{c: 0 for c in COACH_COLS}, "coach_years_with_program": 0})
            continue
        coach_name = coach_row.iloc[0]["coach"]
        # All tournament rows for this coach in prior 5 years (any team)
        coach_prior = hist[(hist["coach"] == coach_name) & (hist["year"].isin(prior))]
        coach_tourn = coach_prior[(coach_prior["tourn_w"] + coach_prior["tourn_l"]) > 0]
        appearances = len(coach_tourn)
        wins = int(coach_prior["tourn_w"].sum())
        losses = int(coach_prior["tourn_l"].sum())
        # Years with program: consecutive years (going back) this coach at this team
        years_with = 0
        for y in range(year, 2007, -1):
            r = hist[(hist["team"] == team) & (hist["year"] == y)]
            if r.empty or r.iloc[0]["coach"] != coach_name:
                break
            years_with += 1
        rows.append({
            "Team": team,
            "Year": year,
            "coach_tourn_appearances_5y": appearances,
            "coach_tourn_wins_5y": wins,
            "coach_tourn_losses_5y": losses,
            "coach_years_with_program": years_with,
        })
    return pd.DataFrame(rows)


# Sports Reference: map pipeline team names to SR school slug (URL segment)
TEAM_TO_SR_SLUG: dict[str, str] = {
    "Connecticut": "connecticut",
    "UConn": "connecticut",
    "St. John's": "st-johns-ny",
    "St. John's (NY)": "st-johns-ny",
    "Saint John's": "st-johns-ny",
    "St Joseph's": "saint-josephs",
    "Saint Joseph's": "saint-josephs",
    "St. Joseph's": "saint-josephs",
    "UNC": "north-carolina",
    "North Carolina": "north-carolina",
    "NC State": "north-carolina-state",
    "North Carolina State": "north-carolina-state",
    "BYU": "brigham-young",
    "Brigham Young": "brigham-young",
    "LSU": "louisiana-state",
    "Louisiana State": "louisiana-state",
    "Ole Miss": "mississippi",
    "Mississippi": "mississippi",
    "Southern California": "southern-california",
    "USC": "southern-california",
    "UCF": "central-florida",
    "Central Florida": "central-florida",
    "UNLV": "nevada-las-vegas",
    "Nevada-Las Vegas": "nevada-las-vegas",
    "SMU": "southern-methodist",
    "Southern Methodist": "southern-methodist",
    "TCU": "texas-christian",
    "Texas Christian": "texas-christian",
    "VCU": "virginia-commonwealth",
    "Virginia Commonwealth": "virginia-commonwealth",
    "UMBC": "maryland-baltimore-county",
    "LIU": "long-island-university",
    "LIU Brooklyn": "long-island-university",
    "St. Mary's": "saint-marys-ca",
    "Saint Mary's": "saint-marys-ca",
    "St. Mary's (CA)": "saint-marys-ca",
    "Massachusetts": "massachusetts",
    "UMass": "massachusetts",
    "Texas A&M": "texas-am",
    "Texas A&M-Corpus Christi": "texas-am-corpus-christi",
    "Miami (FL)": "miami-fl",
    "Miami": "miami-fl",
    "Pitt": "pittsburgh",
    "Pittsburgh": "pittsburgh",
    "Penn": "pennsylvania",
    "Pennsylvania": "pennsylvania",
    "UIC": "illinois-chicago",
    "Illinois-Chicago": "illinois-chicago",
    "FIU": "florida-international",
    "Florida International": "florida-international",
    "Cal": "california",
    "California": "california",
    "UC Irvine": "california-irvine",
    "UC Santa Barbara": "california-santa-barbara",
    "San Diego State": "san-diego-state",
    "Colorado State": "colorado-state",
    "Florida State": "florida-state",
    "Michigan State": "michigan-state",
    "Ohio State": "ohio-state",
    "Oregon State": "oregon-state",
    "Washington State": "washington-state",
    "Iowa State": "iowa-state",
    "Oklahoma State": "oklahoma-state",
    "Kansas State": "kansas-state",
    "Boise State": "boise-state",
    "Utah State": "utah-state",
    "New Mexico State": "new-mexico-state",
    "Murray State": "murray-state",
    "Weber State": "weber-state",
    "Wichita State": "wichita-state",
    "Kent State": "kent-state",
    "Ball State": "ball-state",
    "Appalachian State": "appalachian-state",
    "Grand Canyon": "grand-canyon",
    "Loyola Chicago": "loyola-chicago",
    "Loyola (IL)": "loyola-chicago",
    "Loyola Marymount": "loyola-marymount",
    "George Mason": "george-mason",
}


def _team_to_slug(team: str) -> str:
    """Convert pipeline Team name to Sports Reference school slug."""
    t = str(team).strip()
    if t in TEAM_TO_SR_SLUG:
        return TEAM_TO_SR_SLUG[t]
    # Fallback: lowercase, replace spaces/punctuation with hyphen
    slug = re.sub(r"[^\w\s-]", "", t.lower())
    slug = re.sub(r"[-\s]+", "-", slug).strip("-")
    return slug or t.lower()


def fetch_from_sports_reference(
    team_year: pd.DataFrame,
    user_agent: str = "Mozilla/5.0 (compatible; CBB pipeline; +https://github.com)",
    delay_sec: float = 1.0,
) -> pd.DataFrame:
    """
    Attempt to fill coach experience from Sports Reference.
    Returns DataFrame with Team, Year, coach_tourn_appearances_5y, coach_tourn_wins_5y, coach_tourn_losses_5y.
    """
    try:
        import requests
        from bs4 import BeautifulSoup
    except ImportError:
        print("Install requests and beautifulsoup4 for --fetch: pip install requests beautifulsoup4", file=sys.stderr)
        return team_year

    base = "https://www.sports-reference.com"
    session = requests.Session()
    session.headers["User-Agent"] = user_agent

    rows = []
    for _, r in team_year.iterrows():
        team, year = r["Team"], int(r["Year"])
        slug = _team_to_slug(team)
        prior_years = _years_prior_5(year)
        appearances, wins, losses = 0, 0, 0

        # School season page -> coach link
        url = f"{base}/cbb/schools/{slug}/men/{year}.html"
        try:
            resp = session.get(url, timeout=15)
            resp.raise_for_status()
            soup = BeautifulSoup(resp.text, "html.parser")
            # Find coach link: often in a p tag "Coach: ..." or in meta
            coach_link = soup.select_one('a[href*="/cbb/coaches/"]')
            if not coach_link:
                time.sleep(delay_sec)
                rows.append({"Team": team, "Year": year, **{c: 0 for c in COACH_COLS}})
                continue
            coach_href = coach_link.get("href", "")
            coach_slug = coach_href.replace("/cbb/coaches/", "").strip("/").replace(".html", "")
            if not coach_slug:
                time.sleep(delay_sec)
                rows.append({"Team": team, "Year": year, **{c: 0 for c in COACH_COLS}})
                continue
        except Exception:
            time.sleep(delay_sec)
            rows.append({"Team": team, "Year": year, **{c: 0 for c in COACH_COLS}})
            continue

        time.sleep(delay_sec)

        # Coach page -> season table
        coach_url = f"{base}/cbb/coaches/{coach_slug}.html"
        try:
            resp = session.get(coach_url, timeout=15)
            resp.raise_for_status()
            soup = BeautifulSoup(resp.text, "html.parser")
            table = soup.select_one("table#coach-per_game, table#coach_per_game, table.stats_table")
            if not table:
                rows.append({"Team": team, "Year": year, "coach_tourn_appearances_5y": 0, "coach_tourn_wins_5y": 0, "coach_tourn_losses_5y": 0})
                continue
            tbody = table.find("tbody")
            if not tbody:
                rows.append({"Team": team, "Year": year, "coach_tourn_appearances_5y": 0, "coach_tourn_wins_5y": 0, "coach_tourn_losses_5y": 0})
                continue
            for tr in tbody.find_all("tr"):
                year_cell = tr.find("td", {"data-stat": "year_id"}) or tr.find("th", {"data-stat": "season"})
                if not year_cell:
                    continue
                try:
                    # year_id can be "2024" or "2023-24"
                    y_str = year_cell.get_text(strip=True)
                    if "-" in y_str:
                        y = int(y_str.split("-")[0])
                    else:
                        y = int(y_str)
                except ValueError:
                    continue
                if y not in prior_years:
                    continue
                # NCAA tournament: data-stat="ncaa_tournament" or similar; value like "1-1" or "Champion"
                ncaa_cell = tr.find("td", {"data-stat": "ncaa_tournament"}) or tr.find("td", {"data-stat": "ncaa_tournament"})
                if not ncaa_cell:
                    for td in tr.find_all("td"):
                        if "NCAA" in (td.get_text() or "") or (td.get("data-stat") and "tournament" in (td.get("data-stat") or "").lower()):
                            ncaa_cell = td
                            break
                if ncaa_cell:
                    text = ncaa_cell.get_text(strip=True)
                    if "NCAA" in text or re.search(r"\d+-\d+", text):
                        appearances += 1
                        # Parse W-L
                        match = re.search(r"(\d+)-(\d+)", text)
                        if match:
                            w, l = int(match.group(1)), int(match.group(2))
                            wins += w
                            losses += l
        except Exception:
            pass
        rows.append({
            "Team": team,
            "Year": year,
            "coach_tourn_appearances_5y": appearances,
            "coach_tourn_wins_5y": wins,
            "coach_tourn_losses_5y": losses,
        })
        time.sleep(delay_sec)

    return pd.DataFrame(rows)


def main() -> int:
    parser = argparse.ArgumentParser(description="Build coach_tournament_experience.csv for pipeline")
    parser.add_argument("--training-csv", type=Path, help="Path to cbbtrainingdata_*.csv (default: latest in data/processed)")
    parser.add_argument("--out-dir", type=Path, default=None, help="Output directory (default: data/external)")
    parser.add_argument("--fetch", action="store_true", help="Fetch coach experience from remote source")
    parser.add_argument("--source", choices=("torvik", "sr"), default="torvik",
                        help="With --fetch: use Bart Torvik (default) or Sports Reference (sr)")
    parser.add_argument("--year-start", type=int, default=None, help="Limit to years >= this (with --fetch)")
    parser.add_argument("--year-end", type=int, default=None, help="Limit to years <= this (with --fetch)")
    parser.add_argument("--max-teams", type=int, default=None, help="Limit teams to fetch (for testing)")
    parser.add_argument("--playwright", action="store_true", help="Use Playwright from start (if Torvik blocks HTTP)")
    args = parser.parse_args()

    out_dir = args.out_dir or project_root / "data" / "external"
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    training_csv = args.training_csv
    if not training_csv:
        processed = project_root / "data" / "processed"
        training_csv = _find_latest_training_csv(processed)
    if not training_csv or not Path(training_csv).exists():
        print("No training CSV found. Run the pipeline first or pass --training-csv PATH.", file=sys.stderr)
        return 1

    training_csv = Path(training_csv)
    df = build_template(training_csv)

    if args.fetch:
        subset = df[["Team", "Year"]].drop_duplicates()
        if args.year_start is not None:
            subset = subset[subset["Year"] >= args.year_start]
        if args.year_end is not None:
            subset = subset[subset["Year"] <= args.year_end]
        if subset.empty:
            print("No rows after year filter.", file=sys.stderr)
            return 1
        if args.source == "torvik":
            print("Fetching coach experience from Bart Torvik (team-history.php)...", file=sys.stderr)
            if args.max_teams:
                teams_sub = subset["Team"].unique()[: args.max_teams]
                subset = subset[subset["Team"].isin(teams_sub)]
                print(f"  Limited to {len(teams_sub)} teams", file=sys.stderr)
            fetched = fetch_from_torvik(subset, force_playwright=args.playwright)
        else:
            print("Fetching coach experience from Sports Reference (rate-limited)...", file=sys.stderr)
            fetched = fetch_from_sports_reference(subset)
        # Merge fetched values back into full template (keep all Team/Year rows)
        df = df.drop(columns=COACH_COLS, errors="ignore")
        df = df.merge(
            fetched[["Team", "Year"] + COACH_COLS],
            on=["Team", "Year"],
            how="left",
        )
        for c in COACH_COLS:
            df[c] = df[c].fillna(0).astype(int)

    out_path = out_dir / "coach_tournament_experience.csv"
    df = df[["Team", "Year"] + COACH_COLS]
    df.to_csv(out_path, index=False)
    print(f"Wrote {len(df)} rows to {out_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
