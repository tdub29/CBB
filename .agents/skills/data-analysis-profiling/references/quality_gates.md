# Data Quality Gates

Automated checks to run before any modeling or reporting. Each gate has a severity level and a clear pass/fail criterion.

## Gate Definitions

### Critical Gates (must pass — block pipeline)

| Gate | Check | Pass Criterion |
|------|-------|---------------|
| G01: No duplicate keys | `df.duplicated(subset=key_cols).sum()` | `== 0` |
| G02: Target not null | `df[target].isnull().sum()` | `== 0` |
| G03: No all-null columns | `df.isnull().all().any()` | `== False` |
| G04: Row count in range | `len(df)` | Within expected bounds |
| G05: No duplicate column names | `df.columns.duplicated().any()` | `== False` |

### Warning Gates (should pass — investigate if fails)

| Gate | Check | Pass Criterion |
|------|-------|---------------|
| G06: Feature nulls low | `df.isnull().mean()` per column | `< 0.05` for all features |
| G07: No constant columns | `df.nunique()` per column | `> 1` for all features |
| G08: Numeric types correct | `df.select_dtypes(include="object")` | No columns that should be numeric |
| G09: Values in domain | Min/max per column | Within configured bounds |
| G10: Year coverage | Unique years in data | All expected years present |

### Info Gates (nice to know — log only)

| Gate | Check | Insight |
|------|-------|---------|
| G11: Skewed features | `abs(skew) > 2` | Consider log transform |
| G12: High-correlation pairs | `abs(corr) > 0.85` | Potential multicollinearity |
| G13: Class balance | Value counts of target | Imbalanced target? |
| G14: Cardinality flags | Categorical columns with >100 unique values | Encoding strategy needed |

## Implementation Pattern

```python
def run_quality_gates(df, key_cols, target, bounds=None):
    results = []

    # G01
    dupes = df.duplicated(subset=key_cols).sum()
    results.append({"gate": "G01", "check": "duplicate_keys", "value": dupes, "pass": dupes == 0, "severity": "critical"})

    # G02
    target_nulls = df[target].isnull().sum()
    results.append({"gate": "G02", "check": "target_not_null", "value": target_nulls, "pass": target_nulls == 0, "severity": "critical"})

    # G03
    all_null = df.isnull().all().sum()
    results.append({"gate": "G03", "check": "no_all_null_cols", "value": all_null, "pass": all_null == 0, "severity": "critical"})

    # G06
    max_null_pct = df.isnull().mean().max()
    results.append({"gate": "G06", "check": "feature_nulls_low", "value": round(max_null_pct, 4), "pass": max_null_pct < 0.05, "severity": "warning"})

    # G07
    constants = (df.nunique() <= 1).sum()
    results.append({"gate": "G07", "check": "no_constant_cols", "value": constants, "pass": constants == 0, "severity": "warning"})

    return pd.DataFrame(results)
```

## Running Gates in Practice

1. **Development:** Run all gates, review info-level items
2. **CI / automated pipeline:** Run critical + warning gates; fail the job if any critical gate fails
3. **Post-refresh:** Focus on drift-related gates (G10, G09)
4. **Pre-presentation:** Run all gates so you can confidently say "data quality verified"
