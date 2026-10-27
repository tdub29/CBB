"""Coach tournament-experience features, computed from team history over a look-back window."""

from __future__ import annotations

import pandas as pd

from src.features.outcomes import _finish_to_wl


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
