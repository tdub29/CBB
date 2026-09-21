# Data Directory

| Subdirectory | Purpose |
|--------------|---------|
| `raw/` | Raw ingested data (immutable) |
| `interim/` | Intermediate data after light cleaning or joins |
| `processed/` | Final feature tables for modeling (`cbbtrainingdata_YYYYMMDD.csv`); research cut (`cbbtrainingdata_last2seasons_YYYYMMDD.csv`, last N seasons) for feature engineering |
| `external/` | Optional third-party data; see [external/README.md](external/README.md) |

Data lifecycle: external/raw → interim → processed. Outputs from the pipeline go to `processed/`.
