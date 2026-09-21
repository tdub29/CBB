"""Bracket loading, validation, and matchup construction."""

from __future__ import annotations

from pathlib import Path

import pandas as pd

REQUIRED_BRACKET_COLUMNS = ("Team", "Seed", "Region")
EXPECTED_SEEDS = tuple(range(1, 17))
FIRST_ROUND_PAIRINGS = (
    (1, 16),
    (8, 9),
    (5, 12),
    (4, 13),
    (6, 11),
    (3, 14),
    (7, 10),
    (2, 15),
)


def _prepare_bracket(df: pd.DataFrame) -> pd.DataFrame:
    """Normalize basic bracket fields for validation/simulation."""
    out = df.copy()
    out["Team"] = out["Team"].astype(str).str.strip()
    out["Region"] = out["Region"].astype(str).str.strip()
    out["Seed"] = pd.to_numeric(out["Seed"], errors="coerce")
    return out


def validate_bracket(df: pd.DataFrame) -> tuple[list[str], list[str]]:
    """
    Validate bracket shape and seed distribution.

    Returns (errors, warnings).
    """
    errors: list[str] = []
    warnings: list[str] = []

    missing_cols = [c for c in REQUIRED_BRACKET_COLUMNS if c not in df.columns]
    if missing_cols:
        errors.append(f"Missing required columns: {', '.join(missing_cols)}")
        return errors, warnings

    work = _prepare_bracket(df[list(REQUIRED_BRACKET_COLUMNS)])

    if work["Team"].eq("").any():
        errors.append("Bracket contains blank team names.")
    if work["Region"].eq("").any():
        errors.append("Bracket contains blank region names.")

    if work["Seed"].isna().any():
        errors.append("Bracket contains non-numeric or missing seeds.")
    else:
        non_int = (work["Seed"] % 1 != 0).any()
        if non_int:
            errors.append("Seeds must be integers.")

    if work["Team"].duplicated().any():
        dupes = sorted(work.loc[work["Team"].duplicated(), "Team"].unique().tolist())
        errors.append(f"Duplicate teams found: {', '.join(dupes)}")

    n_teams = len(work)
    if n_teams not in (64, 68):
        warnings.append(f"Expected 64 teams (or 68 with First Four), found {n_teams}.")

    regions = sorted(work["Region"].unique().tolist())
    if len(regions) != 4:
        warnings.append(f"Expected 4 regions for standard bracket simulation, found {len(regions)}.")

    for region in regions:
        region_df = work[work["Region"] == region]
        if len(region_df) != 16:
            warnings.append(f"Region '{region}' has {len(region_df)} teams (expected 16).")

        if not region_df["Seed"].isna().all():
            seed_counts = region_df["Seed"].value_counts()
            dupes = seed_counts[seed_counts > 1].index.tolist()
            if dupes:
                errors.append(f"Region '{region}' has duplicate seeds: {', '.join(map(str, dupes))}")

            present = set(region_df["Seed"].dropna().astype(int).tolist())
            missing = [s for s in EXPECTED_SEEDS if s not in present]
            if missing:
                warnings.append(f"Region '{region}' missing seeds: {', '.join(map(str, missing))}")

    return errors, warnings


def load_bracket(csv_path: str | Path) -> pd.DataFrame:
    """Load and validate bracket CSV."""
    path = Path(csv_path)
    if not path.exists():
        raise FileNotFoundError(f"Bracket CSV not found: {path}")

    df = pd.read_csv(path)
    errors, warnings = validate_bracket(df)
    if errors:
        msg = "; ".join(errors)
        raise ValueError(f"Invalid bracket: {msg}")
    if warnings:
        print("Bracket warnings:")
        for warning in warnings:
            print(f"- {warning}")

    out = _prepare_bracket(df[list(REQUIRED_BRACKET_COLUMNS)])
    out["Seed"] = out["Seed"].astype(int)
    return out


def build_first_round_matchups(bracket_df: pd.DataFrame) -> list[dict[str, str | int]]:
    """Build Round of 64 matchups from a validated bracket."""
    errors, _ = validate_bracket(bracket_df)
    if errors:
        raise ValueError("Cannot build matchups from invalid bracket.")

    matchups: list[dict[str, str | int]] = []
    work = _prepare_bracket(bracket_df[list(REQUIRED_BRACKET_COLUMNS)])

    for region in sorted(work["Region"].unique().tolist()):
        region_df = work[work["Region"] == region]
        seed_to_team = {int(row.Seed): row.Team for row in region_df.itertuples(index=False)}
        for seed_a, seed_b in FIRST_ROUND_PAIRINGS:
            if seed_a not in seed_to_team or seed_b not in seed_to_team:
                raise ValueError(
                    f"Region '{region}' missing required first-round seed pair {seed_a} vs {seed_b}."
                )
            matchups.append(
                {
                    "round": "R64",
                    "region": region,
                    "seed_a": seed_a,
                    "seed_b": seed_b,
                    "team_a": seed_to_team[seed_a],
                    "team_b": seed_to_team[seed_b],
                }
            )
    return matchups


def advance_round(winners: list[str]) -> list[tuple[str, str]]:
    """Pair winners in bracket order for the next round."""
    if len(winners) % 2 != 0:
        raise ValueError("Winners list must contain an even number of teams.")
    return [(winners[i], winners[i + 1]) for i in range(0, len(winners), 2)]
