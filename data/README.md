# Data Directory

| Subdirectory | Purpose |
|--------------|---------|
| `raw/` | Raw ingested data (immutable) |
| `interim/` | Intermediate data after light cleaning or joins |
| `processed/` | Final feature tables used for modeling (e.g. `cbbtrainingdata_YYYYMMDD.csv`) |
| `external/` | Optional third-party data; see [external/README.md](external/README.md) |

Data lifecycle: external/raw → interim → processed. Outputs from the pipeline go to `processed/`.
