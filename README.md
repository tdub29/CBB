# CBB

This repository is being reorganized into a production-quality data science layout so the work is reproducible, testable, and ready for collaboration. The new structure separates ingestion, feature engineering, modeling, evaluation, and deployment artifacts while keeping data and documentation discoverable.

## Proposed project structure

```
.
├── configs/                 # Centralized configuration files (YAML/JSON) for paths, seeds, and model params
├── data/                    # Data storage (notebooks/scripts read from here; outputs go to processed)
│   ├── external/            # Third-party data sources as-is (immutable)
│   ├── raw/                 # Raw ingested data (immutable)
│   ├── interim/             # Intermediate data with light cleaning or joins
│   └── processed/           # Final feature tables used for modeling
├── docs/                    # Project documentation (design notes, decisions, data dictionaries)
├── models/                  # Serialized model artifacts and experiment outputs
├── notebooks/               # Exploratory analysis notebooks (kept lightweight and reproducible)
├── reports/                 # Reporting outputs (figures, write-ups)
│   └── figures/             # Plots and images referenced by reports
├── scripts/                 # One-off scripts, CLI helpers, and automation tasks
├── src/                     # Core, reusable code
│   ├── ingestion/           # Scrapers and ingestion pipelines
│   ├── features/            # Feature engineering and transforms
│   ├── modeling/            # Model training and selection code
│   ├── evaluation/          # Metrics, validation, and model comparison
│   ├── deployment/          # Packaging, scoring, and deployment hooks
│   ├── pipelines/           # End-to-end workflows that orchestrate modules
│   └── utils/               # Shared utilities (I/O helpers, constants, etc.)
├── tests/                   # Unit/integration tests
└── README.md                # Project overview and onboarding
```

### Why this layout
- **configs/**: Enables reproducibility by putting all file paths, seeds, and hyperparameters in versioned config files.
- **data/**: Encourages a clean data lifecycle with immutable raw/external inputs and explicit processed outputs.
- **docs/**: Keeps design docs, data dictionaries, and decisions close to code.
- **models/**: Makes experiment artifacts and model binaries easy to track.
- **notebooks/**: Supports exploration without mixing ad-hoc work into production code.
- **reports/**: Centralizes figures and write-ups for sharing results.
- **scripts/**: Captures automation and CLI tasks that don't belong in core modules.
- **src/**: Keeps production code modular, testable, and reusable.
- **tests/**: Ensures regressions are caught early and builds trust in results.

## Naming conventions
- **Directories**: lowercase, hyphenated if multiple words (e.g., `data/raw`, `reports/figures`).
- **R scripts**: snake_case (e.g., `build_features.R`, `train_model.R`).
- **Configs**: `*.yaml` for configuration (e.g., `configs/default.yaml`).
- **Data files**: include source + date when possible (e.g., `barttorvik_2024_raw.csv`).

## README content recommendations
Include at minimum:
1. **Project goals** and a short summary of the modeling approach.
2. **Setup** instructions (R version, package dependencies, optional `renv`).
3. **How to run** ingestion, feature engineering, training, and evaluation pipelines.
4. **Data sources** and any usage constraints or citations.
5. **Experiment tracking** expectations (where to store models and metrics).

## Configuration management recommendations
- Store default configuration in `configs/default.yaml` and keep environment-specific overrides (e.g., `configs/local.yaml`) out of version control.
- Add a single entrypoint script in `scripts/` that reads config, runs ingestion → features → train → evaluate.
- Use `renv` for deterministic R dependencies and record the lockfile in git.

## Current scripts
- `src/pipelines/madness.R` is the original end-to-end pipeline script. It should be incrementally split into ingestion, features, and modeling modules under `src/` as the refactor proceeds.
