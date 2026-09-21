---
name: data-analysis-profiling
description: Exploratory data analysis, statistical profiling, data quality assessment, and diagnostic insights for tabular data. Use when profiling datasets, checking data quality, identifying outliers, analyzing distributions, computing correlations, or performing EDA before modeling or reporting.
---

# Data Analysis & Profiling

Systematic approach to exploratory data analysis and data profiling for tabular datasets.

## When to Use This Skill

- Exploring a dataset for the first time
- Profiling shape, types, nulls, and distributions before modeling
- Checking data quality (duplicates, domain violations, unexpected missing values)
- Analyzing feature correlations and multicollinearity
- Identifying outliers and anomalous records
- Comparing distributions across groups or time periods
- Diagnosing why model performance changed after a data refresh
- Validating join integrity after merging tables

## EDA Workflow

A disciplined EDA follows these stages in order:

### Stage 1: Structural Overview

Before looking at values, understand the shape and schema.

```python
df.shape                          # (rows, columns)
df.dtypes                         # column types
df.info(memory_usage="deep")      # types + non-null counts + memory
df.columns.tolist()               # column names
df.duplicated().sum()             # full-row duplicates
```

**Key questions:**
- How many rows and columns?
- Which columns are numeric vs. categorical vs. datetime?
- Are there unexpected object-typed columns that should be numeric?
- Any fully-null columns?

### Stage 2: Missing Data Assessment

```python
null_pct = (df.isnull().mean() * 100).sort_values(ascending=False)
null_pct[null_pct > 0]            # only columns with nulls
```

**Patterns to look for:**
- **MCAR** (Missing Completely At Random): nulls scattered uniformly
- **MAR** (Missing At Random): nulls correlate with another column
- **MNAR** (Missing Not At Random): missingness depends on the missing value itself

Visualize missing patterns:

```python
import seaborn as sns
sns.heatmap(df.isnull(), cbar=False, yticklabels=False)
```

### Stage 3: Univariate Summaries

**Numeric columns:**

```python
df.describe()                     # count, mean, std, min, quartiles, max
df.select_dtypes(include="number").skew()   # skewness
df.select_dtypes(include="number").kurtosis()
```

What to check:
- Min/max within domain-valid range?
- High skew (>2 or <-2) suggests outliers or log-transform candidates
- Std = 0 means constant column (drop before modeling)
- Large gap between mean and median = skewed distribution

**Categorical columns:**

```python
for col in df.select_dtypes(exclude="number").columns:
    print(col, df[col].nunique(), df[col].value_counts().head())
```

What to check:
- Cardinality: too many unique values for one-hot encoding?
- Dominant class: >95% one value = near-zero variance
- Typos/inconsistencies: "USA", "usa", "U.S.A." as separate values

### Stage 4: Key Integrity

If your data has natural keys (like id, or a composite key):

```python
key_cols = ["id"]                                  # or ["team", "year"] etc.
df.duplicated(subset=key_cols).sum()               # should be 0
df.groupby(key_cols).size().value_counts()          # all should be 1
```

### Stage 5: Bivariate / Correlation Analysis

```python
corr = df.select_dtypes(include="number").corr()

# Flag highly correlated pairs
import numpy as np
upper = corr.where(np.triu(np.ones(corr.shape), k=1).astype(bool))
high_corr = [
    (upper.columns[j], upper.index[i], upper.iloc[i, j])
    for i in range(len(upper)) for j in range(len(upper.columns))
    if abs(upper.iloc[i, j]) > 0.85
]
```

**Rules of thumb:**
- |r| > 0.85: potential multicollinearity — consider dropping one or using regularization
- |r| > 0.95: near-redundant — almost certainly drop one
- Low correlation with target: feature may not be useful (but check non-linear relationships too)

### Stage 6: Outlier Detection

**IQR method** (robust to skew):

```python
def flag_iqr_outliers(series, k=1.5):
    q1, q3 = series.quantile(0.25), series.quantile(0.75)
    iqr = q3 - q1
    return (series < q1 - k * iqr) | (series > q3 + k * iqr)
```

**Z-score method** (assumes ~normal):

```python
from scipy import stats
z = np.abs(stats.zscore(df[col].dropna()))
outlier_mask = z > 3
```

**Decision framework:**

| Situation | Action |
|-----------|--------|
| Data entry error | Fix or remove |
| Legitimate extreme value | Keep, but note for stakeholders |
| Influential point in model | Test with and without; report both |
| Many outliers (>5%) | Distribution is heavy-tailed — transform, don't remove |

### Stage 7: Group Comparisons

Compare distributions across meaningful segments:

```python
# Numeric: group means
df.groupby("category")[numeric_cols].mean()

# Statistical test: are groups different?
from scipy.stats import mannwhitneyu, kruskal
stat, p = kruskal(*[group[col].dropna() for name, group in df.groupby("category")])
```

### Stage 8: Temporal Drift

If data spans time periods, check whether distributions shift:

```python
from scipy.stats import ks_2samp

def check_drift(df, feature, time_col, current_period):
    hist = df[df[time_col] != current_period][feature].dropna()
    curr = df[df[time_col] == current_period][feature].dropna()
    stat, p = ks_2samp(hist, curr)
    return {"feature": feature, "ks_stat": stat, "p_value": p, "drifted": p < 0.05}
```

## Data Quality Gate Checklist

Run these checks before any modeling or reporting:

| # | Check | How to verify | Severity |
|---|-------|---------------|----------|
| 1 | No full-row duplicates | `df.duplicated().sum() == 0` | Critical |
| 2 | Key uniqueness | `df.duplicated(subset=keys).sum() == 0` | Critical |
| 3 | No all-null columns | `df.isnull().all().sum() == 0` | Critical |
| 4 | Feature nulls < threshold | `(df.isnull().mean() < 0.05).all()` | Warning |
| 5 | Numeric cols are numeric dtype | Check dtypes after load | Warning |
| 6 | Values within domain bounds | Min/max per column | Warning |
| 7 | Categorical levels expected | `nunique()` within reason | Info |
| 8 | No constant columns | `df.nunique() > 1` for all features | Warning |
| 9 | Target variable complete | No nulls in target | Critical |
| 10 | Join didn't inflate rows | `len(merged) == len(left)` for left joins | Critical |

## Profiling Tools

**Quick profiling (no install):**
- `df.describe()`, `df.info()`, `df.isnull().sum()`

**Libraries (if available):**
- `ydata-profiling` (formerly pandas-profiling): `ProfileReport(df).to_file("report.html")`
- `sweetviz`: `sweetviz.analyze(df).show_html()`
- `dataprep`: `from dataprep.eda import create_report`

**Manual is fine.** These libraries are convenient but not required. The patterns above cover the same ground.

## Common Pitfalls

| Pitfall | Why it matters | Fix |
|---------|---------------|-----|
| Looking only at means | Hides bimodal distributions, outliers | Always check median, quartiles, histograms |
| Ignoring missing data patterns | MAR/MNAR can bias models | Profile missingness before imputing |
| Correlation ≠ causation | High r doesn't mean X causes Y | Note for stakeholders; use domain knowledge |
| Profiling after imputation | Hides the original data quality issues | Profile both raw and imputed versions |
| Not checking post-join row counts | Silent row multiplication from bad joins | Always assert row count after merge |
| Trusting dtype inference | "123" as string, dates as objects | Explicitly cast after loading |

## Reference Documentation

- Profiling workflow patterns: `references/profiling_patterns.md`
- Quality gate specifications: `references/quality_gates.md`
