# Feature Engineering Catalog

Reusable feature engineering patterns organized by technique type.

## Arithmetic Features

### Ratios

Normalize one quantity by another to create rate-based features.

```python
df["win_rate"] = df["wins"] / df["games"].replace(0, np.nan)
df["efficiency"] = df["output"] / df["input"]
df["conversion_rate"] = df["conversions"] / df["visits"].clip(lower=1)
```

**When:** Two quantities have a meaningful relationship as a rate. Ratios are scale-invariant and often more predictive than raw counts.

### Differences / Margins

Capture net effect between two related metrics.

```python
df["net_score"] = df["offense"] - df["defense"]
df["margin"] = df["revenue"] - df["cost"]
df["delta"] = df["current"] - df["previous"]
```

**When:** The gap between two metrics matters more than either alone.

### Sums / Aggregates

Combine related features into a single signal.

```python
df["total_score"] = df["math"] + df["reading"] + df["science"]
df["total_experience"] = df["wins"] + df["losses"]
```

**When:** Individual components are noisy but their sum is stable.

## Transformation Features

### Log Transform

Compress right-skewed distributions; stabilize variance.

```python
df["log_revenue"] = np.log1p(df["revenue"])  # log1p handles zeros
```

**When:** Feature has right skew (long tail of large values). Common for: revenue, population, counts, prices.

### Power / Polynomial

Capture non-linear relationships with the target.

```python
df["score_sq"] = df["score"] ** 2
df["score_cu"] = df["score"] ** 3
df["sqrt_count"] = np.sqrt(df["count"])
```

**When:** You suspect the relationship between feature and target is curved. Use polynomial terms sparingly — they increase complexity.

### Clipping / Winsorization

Cap extreme values to reduce outlier influence without removing rows.

```python
lower, upper = df["value"].quantile(0.01), df["value"].quantile(0.99)
df["value_clipped"] = df["value"].clip(lower, upper)
```

**When:** Outliers are real but you don't want them dominating the model.

## Categorical Features

### Frequency Encoding

Replace category with its occurrence rate.

```python
freq = df["category"].value_counts(normalize=True)
df["category_freq"] = df["category"].map(freq)
```

**When:** Cardinality is too high for one-hot; frequency carries signal (common categories behave differently from rare ones).

### Binary / Flag Features

Create indicator variables from conditions.

```python
df["is_premium"] = (df["tier"] == "premium").astype(int)
df["is_weekend"] = df["day_of_week"].isin([5, 6]).astype(int)
df["has_feature_x"] = (df["feature_x"].notna() & (df["feature_x"] > 0)).astype(int)
```

**When:** A binary split captures the signal better than the raw value.

### Grouping / Bucketing

Reduce cardinality by combining similar categories.

```python
rare_cats = df["category"].value_counts()
rare = rare_cats[rare_cats < 50].index
df["category_grouped"] = df["category"].where(~df["category"].isin(rare), "Other")
```

**When:** Many rare categories add noise without signal.

## Temporal Features

### Extraction from Dates

```python
df["year"] = df["date"].dt.year
df["month"] = df["date"].dt.month
df["day_of_week"] = df["date"].dt.dayofweek
df["quarter"] = df["date"].dt.quarter
df["is_month_end"] = df["date"].dt.is_month_end.astype(int)
```

### Recency / Time Since

```python
df["days_since_event"] = (pd.Timestamp.now() - df["event_date"]).dt.days
df["years_active"] = df["current_year"] - df["start_year"]
```

### Rolling / Lag Features

```python
df["rolling_mean_3"] = df.groupby("id")["value"].transform(lambda x: x.rolling(3).mean())
df["lag_1"] = df.groupby("id")["value"].shift(1)
df["diff_1"] = df.groupby("id")["value"].diff(1)
```

**Warning:** Rolling/lag features require careful handling in train/test splits to avoid leakage. Always compute within the training window only.

## Rank Features

### Rank Within Group

```python
df["rank_in_group"] = df.groupby("group")["score"].rank(ascending=False)
df["percentile"] = df.groupby("group")["score"].rank(pct=True)
```

**When:** Relative position matters more than absolute value (e.g., "best in their group" vs. "scored 85").

## Interaction Features

### Multiplicative Interaction

```python
df["price_x_quantity"] = df["price"] * df["quantity"]
df["speed_x_time"] = df["speed"] * df["time"]
```

**When:** The combined effect of two features is stronger than either alone. Use sparingly — interactions multiply the feature space.

## Feature Selection After Engineering

After creating new features, prune:

1. **Drop zero-variance:** `df.loc[:, df.nunique() > 1]`
2. **Drop highly correlated:** keep one from each |r| > 0.95 pair
3. **Check feature importance:** use model-based importance or permutation importance
4. **Cross-validate:** more features ≠ better performance; validate that additions help
