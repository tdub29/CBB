---
name: data-prep-optimization
description: Data cleaning, transformation, imputation, encoding, deduplication, and feature engineering for tabular data. Use when preparing raw data for modeling, cleaning messy datasets, engineering features, handling missing values, encoding categoricals, or optimizing data pipelines for quality and performance.
---

# Data Prep & Optimization

Systematic data preparation — cleaning, transformation, and feature engineering — to get from raw data to model-ready features.

## When to Use This Skill

- Cleaning a messy or raw dataset for analysis or modeling
- Deciding how to handle missing values (imputation strategy)
- Encoding categorical variables
- Engineering new features from existing columns
- Deduplicating records
- Normalizing or scaling numeric features
- Optimizing a data pipeline for correctness and performance
- Preparing holdout/validation splits

## Data Prep Workflow

### Stage 1: Load and Inspect

Before cleaning, understand what you have (see data-analysis-profiling skill for full EDA). Minimum checks:

```python
df = pd.read_csv("data.csv")
print(df.shape, df.dtypes.value_counts())
print(df.isnull().sum().sort_values(ascending=False).head(10))
print(df.duplicated().sum())
```

### Stage 2: Type Casting

Fix types early — everything downstream depends on correct types.

```python
# Numeric columns stored as strings
for col in ["price", "score", "rating"]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

# Dates
df["date"] = pd.to_datetime(df["date"], errors="coerce")

# Categoricals with known levels
df["status"] = pd.Categorical(df["status"], categories=["low", "med", "high"], ordered=True)
```

**Common traps:**
- CSV loads everything as object/string — always verify
- `"N/A"`, `"null"`, `"-"`, `""` may not be recognized as null by pandas
- Integers with nulls become float64 (use `Int64` nullable type if needed)

### Stage 3: Deduplication

```python
# Full-row duplicates
df = df.drop_duplicates()

# Key-based duplicates (keep first occurrence)
df = df.drop_duplicates(subset=["id", "date"], keep="first")

# Audit before dropping
dupes = df[df.duplicated(subset=["id", "date"], keep=False)]
print(f"Dropping {len(dupes)} duplicate key rows")
```

**Decision guide:**

| Scenario | Strategy |
|----------|----------|
| Exact duplicate rows | Drop (keep first) |
| Same key, different values | Investigate — may need aggregation or recency logic |
| Same key, one row has more nulls | Keep the more complete row |
| Intentional duplicates (time series) | Don't dedupe — use composite key including timestamp |

### Stage 4: Handling Missing Values

**Decision tree for imputation:**

```
Is the column >50% null?
├── Yes → Drop the column (or flag as "has_X" binary)
└── No
    ├── Is it the target variable?
    │   └── Drop the rows (never impute target)
    ├── Is it categorical?
    │   ├── Few nulls → Mode or "Unknown" category
    │   └── Many nulls → "Missing" as explicit level
    └── Is it numeric?
        ├── Normally distributed → Mean imputation
        ├── Skewed → Median imputation
        ├── Time series → Forward fill or interpolate
        └── Has group structure → Group-specific median
```

**Implementation patterns:**

```python
# Simple: median fill
df["score"] = df["score"].fillna(df["score"].median())

# Group-specific: median within category
df["score"] = df.groupby("category")["score"].transform(
    lambda x: x.fillna(x.median())
)

# Indicator + fill: preserve missingness signal
df["score_missing"] = df["score"].isnull().astype(int)
df["score"] = df["score"].fillna(0)

# Forward fill (time series)
df["value"] = df.groupby("id")["value"].ffill()
```

**When NOT to impute:**
- The missingness is informative (e.g., "no tournament appearance" = NaN seed)
- The column is >50% null — imputing invents too much data
- It's the target variable

### Stage 5: Encoding Categoricals

**Encoding selection guide:**

| Method | When to use | Example |
|--------|-------------|---------|
| **One-hot (dummy)** | Low cardinality (<15 levels), no ordinal relationship | Conference flags, region |
| **Label encoding** | Ordinal categories | low/med/high, education level |
| **Target encoding** | High cardinality + supervised task | Team name, ZIP code |
| **Binary/hash encoding** | Very high cardinality | Free-text categories, IDs |
| **Frequency encoding** | Cardinality matters, tree-based models | Any categorical |
| **Leave as-is** | Tree models (LightGBM, CatBoost) handle categoricals natively | N/A |

```python
# One-hot
df = pd.get_dummies(df, columns=["region"], drop_first=True)

# Label encoding (ordinal)
order_map = {"low": 0, "med": 1, "high": 2}
df["level_encoded"] = df["level"].map(order_map)

# Frequency encoding
freq = df["category"].value_counts(normalize=True)
df["category_freq"] = df["category"].map(freq)

# Target encoding (use with cross-validation to avoid leakage)
from sklearn.model_selection import KFold
def target_encode(df, col, target, n_splits=5):
    df[f"{col}_te"] = np.nan
    kf = KFold(n_splits=n_splits, shuffle=True, random_state=42)
    for train_idx, val_idx in kf.split(df):
        means = df.iloc[train_idx].groupby(col)[target].mean()
        df.loc[df.index[val_idx], f"{col}_te"] = df.iloc[val_idx][col].map(means)
    df[f"{col}_te"] = df[f"{col}_te"].fillna(df[target].mean())
    return df
```

### Stage 6: Feature Engineering

**Common transformations:**

| Technique | When to use | Example |
|-----------|-------------|---------|
| **Ratios** | Two related metrics | win_rate = wins / games |
| **Differences** | Compare to baseline | adj_margin = AdjOE - AdjDE |
| **Log transform** | Right-skewed positive values | log_revenue = np.log1p(revenue) |
| **Polynomial** | Known non-linear relationship | seed^2, seed^3 |
| **Binning** | Non-linear effect, reduce noise | age_group = pd.cut(age, bins) |
| **Interaction** | Combined effect matters | feature_A * feature_B |
| **Rolling/lag** | Time series or sequential data | rolling_mean_3y |
| **Rank** | Ordinal position matters more than magnitude | rank within group |

```python
# Ratio
df["win_pct"] = df["wins"] / df["games"].replace(0, np.nan)

# Difference
df["net_rating"] = df["off_rating"] - df["def_rating"]

# Log transform (use log1p to handle zeros)
df["log_value"] = np.log1p(df["value"])

# Polynomial
df["seed_sq"] = df["seed"] ** 2

# Interaction
df["off_x_pace"] = df["off_rating"] * df["pace"]

# Rank within group
df["rank_in_conf"] = df.groupby("conference")["rating"].rank(ascending=False)
```

### Stage 7: Scaling and Normalization

**When to scale:**
- Linear models, SVM, KNN, neural nets: **always scale**
- Tree-based models (RF, XGBoost, LightGBM): **scaling not needed**
- PCA / clustering: **always scale**

**Methods:**

| Method | Formula | When |
|--------|---------|------|
| **StandardScaler** | (x - mean) / std | Default choice for linear models |
| **MinMaxScaler** | (x - min) / (max - min) | Need bounded [0, 1] range |
| **RobustScaler** | (x - median) / IQR | Many outliers |
| **Log transform** | log(x + 1) | Right-skewed features |
| **None** | Raw values | Tree-based models |

```python
from sklearn.preprocessing import StandardScaler, RobustScaler

# Fit on training, transform both train and test
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)  # never fit on test
```

### Stage 8: Train/Test Split Best Practices

```python
from sklearn.model_selection import train_test_split

# Random split (i.i.d. data)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Stratified split (preserve class balance)
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, stratify=y, random_state=42
)

# Time-based split (temporal data — never shuffle)
cutoff = df["year"].max()
train = df[df["year"] < cutoff]
test = df[df["year"] == cutoff]
```

**Rules:**
- Never let test data influence training (no fit on test, no target encoding leakage)
- For time series: always split by time, never randomly
- For grouped data (same entity in multiple rows): keep all rows of an entity in the same split

## Data Pipeline Optimization

### Performance Tips

| Problem | Solution |
|---------|----------|
| Slow CSV reads (>100 MB) | Use `pd.read_csv(..., dtype={...})` to pre-specify types; or use Parquet |
| Repeated expensive transforms | Cache intermediate results to disk |
| Memory pressure | Use `category` dtype for low-cardinality strings; downcast ints/floats |
| Slow groupby | Sort by group key first; use `.transform()` over `.apply()` |
| Chained `.apply()` | Vectorize with numpy/pandas operations |

### Idempotency Checklist

A good prep pipeline should produce the same output if run twice:

- Deduplication is stable (same row kept each time)
- Random seeds are fixed
- File paths use consistent naming
- No append-only writes without clearing first

## Common Pitfalls

| Pitfall | Why it matters | Fix |
|---------|---------------|-----|
| Target leakage | Feature contains future information | Audit all features for temporal validity |
| Imputing before splitting | Test set statistics leak into training | Always split first, then fit imputer on train only |
| Dropping rows carelessly | Loses signal, may introduce bias | Document every row drop with reason |
| Over-engineering features | More features ≠ better model | Start simple; add features guided by error analysis |
| Ignoring the "zero" problem | Filling NaN with 0 changes meaning (0 ≠ missing) | Use indicator columns alongside fill |
| Scaling the target | Usually wrong for regression | Scale features only, not target |

## Reference Documentation

- Imputation strategies: `references/imputation_strategies.md`
- Feature engineering patterns: `references/feature_engineering_catalog.md`
