# Coach tournament experience data

`coach_tournament_experience.csv` provides head coach NCAA tournament experience over the last 5 years (appearances, wins, losses) and years with program per (Team, Year). The pipeline merges it automatically when present.

## Current status

- **Template exists**: 5,607 rows (Team, Year) with coach columns. Values are 0 until fetched.
- **Fetch blocked**: Bart Torvik serves "Verifying Browser" to automated HTTP requests; both R (httr) and Python (requests) get blocked.
- **Playwright option**: Python script can use Playwright to bypass.

## Estimated runtime

- **Full fetch (~367 teams)**: **~1.5–2 hours** (observed ~92 min in practice).
- **Test run** (e.g. `--max-teams 20`): **~5–10 min**.

## How to populate

### Option A: R + toRvik (recommended if you have Rtools)

1. Install Rtools 4.4 from https://cran.r-project.org/bin/windows/Rtools/
2. In R: `remotes::install_github("andreweatherman/toRvik")`
3. Run: `Rscript scripts/fetch_coach_experience.R`

### Option B: Python + Playwright

1. `pip install playwright && playwright install chromium`
2. Run: `python scripts/fetch_coach_experience.py --fetch --playwright`
3. Full run (~367 teams) takes ~1.5–2 hours. Test with `--max-teams 20` first (~5–10 min).

### Option C: R without toRvik (httr + rvest)

The R script falls back to httr+rvest when toRvik is missing, but Torvik blocks this too. Use Playwright instead.

### Option D: Manual

1. Run `python scripts/fetch_coach_experience.py` (no --fetch) to ensure template exists.
2. Fill values from [Sports Reference coach pages](https://www.sports-reference.com/cbb/coaches/).
