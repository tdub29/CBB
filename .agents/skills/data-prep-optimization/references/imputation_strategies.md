# Imputation Strategies

How to handle missing values depending on data type, missingness mechanism, and downstream use.

## Missingness Mechanisms

Understanding *why* data is missing determines the right strategy:

| Mechanism | Definition | Example | Implication |
|-----------|-----------|---------|-------------|
| **MCAR** | Missing completely at random — no pattern | Random sensor dropout | Safe to drop or simple impute |
| **MAR** | Missing depends on observed data | Income missing for younger respondents | Use conditional imputation (group-based) |
| **MNAR** | Missing depends on the missing value itself | High earners skip income question | Missingness is informative — encode it |

## Strategy Selection

### Numeric Columns

| Strategy | When to use | Pros | Cons |
|----------|-------------|------|------|
| **Drop rows** | <1% null, MCAR | No information invented | Loses data; biased if not MCAR |
| **Mean** | Roughly normal, MCAR, low null rate | Simple, preserves mean | Reduces variance; pulls distribution inward |
| **Median** | Skewed distributions | Robust to outliers | Same variance reduction issue |
| **Group median** | MAR — value depends on a categorical | Respects group structure | Needs sufficient group sizes |
| **Forward/backward fill** | Time series, sequential data | Respects temporal structure | Can propagate stale values |
| **Linear interpolation** | Smooth time series | Good for gradual trends | Poor for volatile or seasonal data |
| **KNN imputer** | Complex relationships between features | Uses multivariate information | Slow on large datasets; sensitive to k |
| **Iterative (MICE)** | Multiple features with missing values | Theoretically principled | Computationally expensive; complex |
| **Zero fill** | When 0 has domain meaning (e.g., "no events") | Simple, interpretable | Wrong if 0 ≠ missing |
| **Indicator + fill** | Missingness is informative | Preserves signal | Adds a column |

### Categorical Columns

| Strategy | When to use |
|----------|-------------|
| **Mode** | Low null rate, MCAR |
| **"Unknown" / "Missing" level** | Missingness is meaningful or common |
| **Group mode** | MAR — value depends on another feature |
| **Drop rows** | Very few nulls and column is critical |

## Implementation Patterns

### Simple fill

```python
df["score"] = df["score"].fillna(df["score"].median())
df["category"] = df["category"].fillna("Unknown")
```

### Group-conditional fill

```python
df["score"] = df.groupby("region")["score"].transform(lambda x: x.fillna(x.median()))
```

### Indicator pattern (preserve missingness signal)

```python
df["score_was_missing"] = df["score"].isnull().astype(int)
df["score"] = df["score"].fillna(df["score"].median())
```

### Sklearn-compatible (fit on train, transform test)

```python
from sklearn.impute import SimpleImputer, KNNImputer

# Simple
imputer = SimpleImputer(strategy="median")
X_train_imp = imputer.fit_transform(X_train)
X_test_imp = imputer.transform(X_test)

# KNN
imputer = KNNImputer(n_neighbors=5)
X_train_imp = imputer.fit_transform(X_train)
X_test_imp = imputer.transform(X_test)
```

## Decision Checklist

1. What percentage of the column is null?
   - >50%: drop column or use binary indicator only
   - 5-50%: impute with group-based or model-based strategy
   - <5%: simple impute (median/mode) is usually fine
2. Is missingness random?
   - Yes (MCAR): any strategy works
   - Depends on other columns (MAR): use conditional/group imputation
   - Depends on itself (MNAR): encode the missingness as a feature
3. Will you be splitting train/test?
   - Yes: fit imputer on train only, transform both
   - No (EDA only): global imputation is fine
4. Does zero have meaning in this domain?
   - Yes: okay to fill with 0 when appropriate
   - No: use median or indicator pattern instead
