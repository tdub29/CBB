# Inspect trank page (small-scale test)

Use this to see what tables and columns are available at the Bart Torvik trank URL **before** running the full pipeline.

## Run

From project root:

```bash
python scripts/inspect_trank_page.py
```

## What it does

1. Fetches `https://barttorvik.com/trank.php?year=2025&type=R` with Playwright (same as pipeline fallback).
2. Waits for the data table to load (~8s after load).
3. Parses all `<table>` elements with `pd.read_html`.
4. For each table, prints:
   - Shape and **raw column names** (as on the page).
   - **After normalize**: column names after pipeline mapping.
   - How many canonical columns were matched.
   - A small sample of the data.
5. Writes:
   - `data/raw/samples/trank_page_sample.html` – full HTML for manual inspection.
   - `data/raw/samples/trank_table_sample.csv` – first 20 rows of the “best” table.

## Use this to

- See exactly which columns the trank page provides.
- Debug missing columns (compare raw vs normalized names).
- Adjust `_TRANK_COL_MAP` / `_normalize_columns` in `src/pipelines/madness.py` if a column exists on the page but isn’t mapped.
- Confirm the pipeline’s Playwright wait is long enough (if HTML is small or “Verifying Browser” appears, increase the wait in the script and in `_get_trank_html`).

## Requirements

- Playwright: `pip install playwright && playwright install chromium`
