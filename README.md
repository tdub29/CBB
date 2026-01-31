# March Madness (Men's) Analysis and Prediction

Predict NCAA Men's March Madness tournament advancement (round reached) using Bart Torvik team stats and seed-stratified linear models.

## Project goals

- **Ingest** regular-season, postseason, and tournament data from [Bart Torvik](https://barttorvik.com) (2008–2024).
- **Engineer features** including conference tournament W/L, round encoding (0–6), Power 6 conference flags, and seed-stratified indicators.
- **Model** tournament round advancement with a general linear model plus 14 seed-specific models, selected via jackknife leave-one-out cross-validation.
- **Output** training CSVs, model artifacts, and optional 2023 holdout validation.

## Modeling approach

- **Target**: `round` (0 = Round of 64, 6 = Champion).
- **Features**: AdjOE, AdjDE, Barthag, WAB, seed, conference flags (B12, B10, SEC, etc.), optional team depth (EFFHGT, TALENT, AST%, etc.) when external data is available.
- **Models**: One general LM + 14 seed-stratified LMs for finer prediction by seed tier.
- **Validation**: Jackknife LOO cross-validation; Metrics package for bias and RMSE.

## Setup

Pipelines are available in **R** and **Python**. Use either language.

### R

- R (4.0+ recommended)
- Required packages: `rvest`, `httr`, `dplyr`, `sqldf`, `readxl`, `stringr`, `Metrics`, `ggplot2`, `yaml`
- Optional: `toRvik` (for exploratory injury-impact analysis)

```r
Rscript scripts/install_deps.R
```

### Python

- Python 3.9+
- Install: `pip install -r requirements.txt`

```
pandas, numpy, requests, scikit-learn, PyYAML, lxml, html5lib
```

## How to run

From the project root:

**R:**
```bash
Rscript scripts/run_pipeline.R
```

**Python:**
```bash
python scripts/run_pipeline.py
```

Or `python -m src.pipelines.madness`

### Outputs

| Output | Location |
|--------|----------|
| Training data | `data/processed/cbbtrainingdata_YYYYMMDD.csv` |
| Model artifacts | In-memory (general model, seed1–14 models); extend to save in `models/` as needed |
| Figures | Plots rendered during run; extend to `reports/figures/` as needed |

## Data sources

| Source | Description |
|--------|-------------|
| **Bart Torvik** | [barttorvik.com](https://barttorvik.com) – regular-season (R), postseason (P), and tournament (T) tables. Primary source; scraped automatically. |
| **Optional external files** | Placed in `data/external/` when available: `trank_team_table_data*.csv`, `cleanteam.xlsx`, `teamdata23.csv`. See [docs/data_sources.md](docs/data_sources.md). |

## Known limitations

- **Optional external data**: Richer features (EFFHGT, TALENT, etc.) require external CSVs. Pipeline runs without them; optional columns are filled with 0.
- **2023 holdout**: Needs `cleanteam.xlsx` and `teamdata23.csv` in `data/external/`, or falls back to 2023 teams from the scrape.
- **`bart_injuryimpact`**: Exploratory only; requires optional `toRvik` package. Does not affect main pipeline.

## Project structure

```
.
├── configs/                 # Configuration (paths, year range, etc.)
├── data/
│   ├── external/            # Optional third-party data (see docs)
│   ├── raw/                 # Raw ingested data
│   ├── interim/             # Intermediate cleaned/joined data
│   └── processed/           # Final training tables
├── docs/                    # Data dictionaries, design notes
├── models/                  # Serialized models (to be populated)
├── notebooks/               # Exploratory notebooks
├── reports/figures/         # Plots and figures
├── scripts/                 # install_deps.R, run_pipeline.R, run_pipeline.py
├── src/
│   ├── pipelines/           # madness.R, madness.py – end-to-end pipelines
│   ├── ingestion/           # (Planned) Scrapers
│   ├── features/            # (Planned) Feature engineering
│   ├── modeling/            # (Planned) Model training
│   └── ...
└── tests/
```

## Current implementation

- **`src/pipelines/madness.R`**: R end-to-end pipeline (ingestion → features → models → outputs).
- **`src/pipelines/madness.py`**: Python equivalent; vectorized pandas, config-driven seed models, sklearn LOO.

## Configuration

- Default config: `configs/default.yaml` (paths, year range, Bart Torvik base URL).
- Keep local overrides in `configs/local.yaml` (gitignored) if needed.

### Reproducible dependencies

**R (renv):**
```r
renv::init(); renv::snapshot()  # Commit renv.lock
```

**Python:** `pip freeze > requirements.txt` or use `uv` / `poetry`.

## Naming conventions

- **Data frames**: Descriptive snake_case (e.g., `training_data`, `holdout_2023`, `regular_season_raw`). See [docs/naming_conventions.md](docs/naming_conventions.md).
- **Directories**: lowercase, hyphenated (e.g., `data/raw`, `reports/figures`).
- **Scripts**: snake_case (e.g., `run_pipeline.R`).
- **Data files**: source + date (e.g., `cbbtrainingdata_20240131.csv`).
