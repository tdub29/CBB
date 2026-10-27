"""Ingestion from Bart Torvik: T-Rank pages, team-table CSVs and team-history pages."""

from .team_history import (
    TEAM_HISTORY_CANONICAL_COLS,
    build_team_history_all,
    fetch_team_history,
    parse_team_history_to_schema,
)
from .team_tables import (
    TEAM_TABLE_COL_NAMES,
    load_all_trank_base,
    load_team_tables_as_primary,
)
from .torvik import (
    fetch_regular_table_for_enrichment,
    fetch_tournament_table_only,
    save_raw_trank_tables,
    scrape_bart_torvik,
)

__all__ = [
    "TEAM_HISTORY_CANONICAL_COLS",
    "build_team_history_all",
    "fetch_team_history",
    "parse_team_history_to_schema",
    "TEAM_TABLE_COL_NAMES",
    "load_all_trank_base",
    "load_team_tables_as_primary",
    "fetch_regular_table_for_enrichment",
    "fetch_tournament_table_only",
    "save_raw_trank_tables",
    "scrape_bart_torvik",
]
