# Data Sources

## Primary: Bart Torvik

**URL**: [https://barttorvik.com](https://barttorvik.com)

The pipeline scrapes three table types from Bart Torvik's T-Rank pages:

| Type | URL pattern | Description |
|------|-------------|-------------|
| Regular season (R) | `...?year=YYYY&...&type=R#` | Full regular-season team stats |
| Postseason (P) | `...?year=YYYY&...&type=P#` | Postseason (incl. conference tournaments) |
| Tournament (T) | `...?year=YYYY&...&type=T#` | NCAA Tournament games only |

**Year range**: 2008–2024 (configurable in `configs/default.yaml`).

**Key columns (examples)**: Rk, Team, Conf, AdjOE, AdjDE, Barthag, EFG%, EFGD%, TOR, TORD, ORB, DRB, FTR, FTRD, 2P%, 2P%D, 3P%, 3P%D, Adj T., WAB, Rec, G.

**Usage constraints**: Scraping for personal/research use. Respect Bart Torvik's terms of use and rate limits.

---

## Optional: External team-depth data

Place these in `data/external/`. The pipeline runs without them; they add features like EFFHGT, TALENT, EXP., etc.

### 1. `trank_team_table_data*.csv`

- **Pattern**: `trank_team_table_data (N).csv` or `trank_team_table_data*.csv`
- **Source**: Bart Torvik team-tables export (per year or combined).
- **Provides**: WINS, BLK%, BLKED%, AST%, OPAST%, 3PRATE, 3PRATED, EFFHGT, EXP., TALENT, ELITESOS.
- **Usage**: Joined to master table by Team and Year.

### 2. `cleanteam.xlsx`

- **Source**: Prepared Excel of 2023 tournament teams (or holdout year).
- **Provides**: Pre-cleaned team roster/performance for holdout validation.
- **Usage**: Loaded as `cbb2023` for prediction and validation.

### 3. `teamdata23.csv`

- **Source**: Team-level data for 2023 (aligned with cleanteam.xlsx).
- **Provides**: Same structure as `trank_team_table_data` for the holdout year.
- **Usage**: Joined to `cbb2023` for full feature set.

---

## `holdout_actual_results` (optional)

A manually maintained data frame or CSV with actual tournament results for the holdout year (Team, round). Used to compute out-of-sample RMSE for 2023 predictions. If absent, that validation step is skipped.
