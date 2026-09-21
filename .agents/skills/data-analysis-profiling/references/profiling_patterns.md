# Profiling Patterns

Reusable patterns for profiling tabular data at each stage of a project.

## Pattern 1: First-Look Profile

Run immediately after loading any new dataset.

**What to produce:**
1. Shape (rows x columns)
2. Column types breakdown (numeric / categorical / datetime / other)
3. Null percentage per column (sorted descending)
4. Full-row duplicate count
5. Key-column duplicate count (if applicable)
6. Memory usage

**Decision points:**
- Columns >50% null → flag for potential drop
- Object columns that should be numeric → flag for type casting
- Unexpected row count → check for header rows in data, or bad delimiter

## Pattern 2: Numeric Feature Profile

For each numeric column, compute:

| Metric | Why |
|--------|-----|
| count, mean, std | Basic centrality and spread |
| min, 25%, 50%, 75%, max | Distribution shape at a glance |
| skewness | >2 or <-2 = consider log transform |
| kurtosis | >7 = heavy tails, outlier-prone |
| % zero | High zero rate may indicate default fill |
| % null | Imputation needed? |
| unique count | 1 = constant (drop) |

## Pattern 3: Categorical Feature Profile

For each categorical column:

| Metric | Why |
|--------|-----|
| unique count (cardinality) | Encoding strategy depends on this |
| top value and frequency | Dominant class? |
| % null | Missing category handling |
| value list (if <20 unique) | Spot typos and inconsistencies |

**Cardinality thresholds:**
- 2 values → binary flag
- 3-15 values → one-hot encoding
- 16-100 → target or frequency encoding
- 100+ → hash encoding or embeddings

## Pattern 4: Correlation Audit

1. Compute pairwise Pearson correlation for all numeric features
2. Flag pairs with |r| > 0.85 as collinear
3. For each flagged pair, decide which to keep based on:
   - Domain relevance to the target
   - Interpretability
   - Null rate (prefer the more complete column)
4. Check Spearman correlation for non-linear monotonic relationships

## Pattern 5: Target Variable Profile

For the column you're predicting:

- Distribution shape (balanced? skewed?)
- Class balance (classification) or value range (regression)
- Relationship to key features (scatter plots, grouped means)
- Null count (must be zero for modeling)

## Pattern 6: Post-Merge Validation

After any join/merge operation:

1. Assert row count: `assert len(merged) == len(left_table)`
2. Check null inflation: compare null rates before vs. after merge
3. Spot-check: pick 5 random keys and verify values match source
4. Look for `_x` / `_y` suffix columns (unintended overlapping column names)

## Pattern 7: Drift Detection

When refreshing data or adding a new time period:

1. For each numeric feature, compare current period vs. historical using KS test
2. For categoricals, compare value distributions using chi-squared
3. Flag features with p < 0.05 as drifted
4. Investigate: real change (rule change, trend) or data quality issue?
