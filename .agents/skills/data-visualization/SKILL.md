---
name: data-visualization
description: Data visualization selection, design, and implementation. Covers when to use which chart type, how to make visualizations clear and intuitive, color and layout principles, and implementation in Python (matplotlib, seaborn, plotly). Use when creating charts, choosing visualization types, improving chart clarity, or building dashboards.
---

# Data Visualization

How to pick the right chart, make it readable, and avoid common visual pitfalls.

## When to Use This Skill

- Choosing which chart type fits your data and question
- Making an existing chart clearer or more intuitive
- Building visualizations for reports, presentations, or dashboards
- Reviewing whether a visualization accurately represents the data
- Implementing charts in Python (matplotlib, seaborn, plotly)

## The Chart Selection Guide

**Start with your question, not your data.**

### What are you trying to show?

#### Comparison — "How does A compare to B?"

| Chart | Best for | Avoid when |
|-------|----------|------------|
| **Bar chart** (vertical) | Comparing values across categories (5-15 items) | Too many categories (>20) |
| **Bar chart** (horizontal) | Long category labels, or ranking | Data is continuous |
| **Grouped bar** | Comparing sub-groups within categories | More than 3-4 sub-groups |
| **Dot plot / lollipop** | Clean comparison with many items | Audience expects traditional bars |

#### Composition — "What makes up the whole?"

| Chart | Best for | Avoid when |
|-------|----------|------------|
| **Stacked bar** | Parts of a whole across categories | More than 5-6 segments |
| **100% stacked bar** | Comparing proportions across groups | Absolute values matter |
| **Pie chart** | One composition, 2-4 slices max | More than 5 slices, or comparing multiple pies |
| **Treemap** | Hierarchical composition, many parts | Precise comparison needed |
| **Waffle chart** | Intuitive part-of-100 display | More than 3 categories |

#### Trend — "How does it change over time?"

| Chart | Best for | Avoid when |
|-------|----------|------------|
| **Line chart** | Continuous change over time, 1-5 series | Too many overlapping lines (>5) |
| **Area chart** | Emphasize volume/magnitude over time | Comparing specific values |
| **Stacked area** | Composition changing over time | Precise reading of individual series |
| **Slope chart** | Change between two time points | More than 2 time points |
| **Sparklines** | Inline trend context in tables | Standalone analysis |

#### Distribution — "How is the data spread?"

| Chart | Best for | Avoid when |
|-------|----------|------------|
| **Histogram** | Shape of one continuous variable | Comparing many groups |
| **Box plot** | Comparing distributions across groups | Audience unfamiliar with box plots |
| **Violin plot** | Distribution shape + comparison | Audience unfamiliar with violins |
| **Strip/swarm plot** | Small datasets, show individual points | Thousands of points (overplotting) |
| **Density plot (KDE)** | Smooth distribution shape | Small samples or multimodal data |

#### Relationship — "How are X and Y related?"

| Chart | Best for | Avoid when |
|-------|----------|------------|
| **Scatter plot** | Two continuous variables, <5000 points | Massive overplotting |
| **Bubble chart** | Three dimensions (x, y, size) | More than 3 dimensions |
| **Hexbin / 2D histogram** | Dense scatter (>5000 points) | Few data points |
| **Heatmap** | Correlation matrix or 2D grid data | Small number of variables |
| **Pair plot** | Exploring many variable relationships at once | Presentation (too dense) |

#### Ranking — "What's the order?"

| Chart | Best for | Avoid when |
|-------|----------|------------|
| **Horizontal bar** (sorted) | Top N or bottom N | Unsorted data |
| **Bump chart** | Rank changes over time | Too many items (>10) |
| **Parallel coordinates** | Multivariate ranking comparison | Non-technical audience |

### Quick Decision Flowchart

```
What question are you answering?
│
├── How much? / Compare values?
│   ├── Across categories → Bar chart
│   └── Over time → Line chart
│
├── What proportion? / Part of whole?
│   ├── One snapshot → Pie (≤4 slices) or stacked bar
│   └── Over time → Stacked area or 100% stacked bar
│
├── How is it distributed?
│   ├── One variable → Histogram
│   └── Compare groups → Box plot or violin
│
├── How are they related?
│   ├── Two variables → Scatter plot
│   └── Many variables → Heatmap or pair plot
│
└── What's the order / rank?
    └── Horizontal bar (sorted)
```

## Design Principles for Clear Visualizations

### 1. Maximize the Data-Ink Ratio

Every element on the chart should serve a purpose. Remove anything that doesn't help the reader understand the data.

**Remove:**
- Gridlines (or lighten them significantly)
- Borders and boxes around the chart
- 3D effects (always)
- Decorative elements / chart junk
- Redundant legends (if labels are on the data)

**Keep:**
- Axis labels with units
- A clear title that states the insight
- Data labels when precision matters
- Annotation for key data points

### 2. Title = Insight, Not Description

```
BAD:  "Sales by Region Q1-Q4 2024"
GOOD: "West Region Sales Grew 40% While East Declined"

BAD:  "Distribution of Scores"
GOOD: "Most Scores Cluster Between 70-85, With a Long Right Tail"

BAD:  "Revenue Over Time"
GOOD: "Revenue Doubled After the March Launch"
```

The title should tell the reader what to see. The chart provides the evidence.

### 3. Color With Purpose

**Principles:**
- Use color to encode meaning, not for decoration
- Limit to 5-7 distinct colors maximum
- Use a single accent color to highlight the key point
- Ensure sufficient contrast for accessibility
- Don't rely on color alone — add labels or patterns for colorblind viewers

**Color palettes by use:**

| Purpose | Palette type | Example |
|---------|-------------|---------|
| Categorical (unordered groups) | Qualitative | Tab10, Set2, Paired |
| Sequential (low to high) | Sequential | Blues, Viridis, YlOrRd |
| Diverging (below/above center) | Diverging | RdBu, Coolwarm, BrBG |
| Highlighting one thing | Gray + one accent | Everything gray except the key series |

**Gray is your best friend.** Make everything gray first, then add color only where you want the eye to go.

```python
colors = ["#cccccc"] * len(categories)    # all gray
colors[highlight_idx] = "#e63946"          # one accent
```

### 4. Axis and Label Best Practices

- **Always label axes.** Include units: "Revenue ($M)", "Temperature (°C)"
- **Start numeric axes at zero** for bar charts (truncated axes distort comparisons)
- **Line charts can start elsewhere** when showing change magnitude matters
- **Don't rotate labels.** If labels don't fit horizontally, use a horizontal bar chart instead
- **Use human-readable numbers.** "$2.4M" not "$2,400,000". "15K" not "15,000"
- **Order categories intentionally.** Alphabetical is almost never the best order — sort by value, or use a natural order

### 5. Annotation > Legend

When practical, label data directly instead of using a legend:

```python
# Instead of a legend, label the line directly
ax.text(x[-1], y[-1], "Series A", va="center", fontsize=9)
```

- Legends force the reader to look back and forth
- Direct labels reduce cognitive load
- Use sparingly — 2-3 direct labels are clear, 10 are clutter

### 6. One Chart, One Message

If you need to make two points, make two charts. A chart that tries to show everything shows nothing.

```
BAD:  One chart with revenue, users, churn rate, NPS, and ARPU over time
GOOD: Three focused charts: revenue trend, user growth, churn analysis
```

### 7. Aspect Ratio Matters

- **Line charts:** wider than tall (16:9 or 2:1) — emphasizes trend
- **Bar charts:** depends on number of bars — give each bar breathing room
- **Scatter plots:** roughly square — preserves relationship perception
- **Small multiples:** consistent size across all panels

## Common Visualization Mistakes

| Mistake | Problem | Fix |
|---------|---------|-----|
| Pie chart with 8+ slices | Unreadable; humans are bad at comparing angles | Use bar chart |
| Dual Y-axis | Creates false correlations; easy to mislead | Use two separate charts |
| Truncated bar chart axis | Exaggerates differences | Start Y-axis at zero |
| Rainbow color scheme | No perceptual ordering; inaccessible | Use sequential or qualitative palette |
| 3D charts | Distort perception; add no information | Use 2D always |
| Spaghetti line chart (>5 lines) | Can't distinguish series | Highlight 1-2; gray the rest; or use small multiples |
| Area chart for comparison | Overlapping areas hide data | Use line chart or small multiples |
| Stacked bar with >5 segments | Only bottom segment is easy to compare | Separate bars or highlight key segments |
| Default matplotlib style | Looks unfinished; distracting grid | Apply a clean theme (seaborn, minimal) |
| No title or unlabeled axes | Reader doesn't know what they're looking at | Always title and label |

## Implementation Quick Reference

### matplotlib (fine-grained control)

```python
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(10, 6))
ax.bar(categories, values, color="#4a90d9", edgecolor="none")
ax.set_title("Key Insight Goes Here", fontsize=14, fontweight="bold", pad=12)
ax.set_xlabel("Category")
ax.set_ylabel("Value ($M)")
ax.spines[["top", "right"]].set_visible(False)
ax.grid(axis="y", alpha=0.3)
plt.tight_layout()
plt.savefig("chart.png", dpi=150, bbox_inches="tight")
```

### seaborn (statistical plots)

```python
import seaborn as sns

sns.set_theme(style="whitegrid", font_scale=1.1)

# Distribution
sns.histplot(data=df, x="score", hue="group", bins=30, alpha=0.6)

# Comparison
sns.boxplot(data=df, x="category", y="value", palette="Set2")

# Relationship
sns.scatterplot(data=df, x="feature_a", y="feature_b", hue="group", size="magnitude")

# Heatmap
sns.heatmap(df.corr(), annot=True, fmt=".2f", cmap="RdBu_r", center=0, square=True)
```

### plotly (interactive)

```python
import plotly.express as px

# Line chart
fig = px.line(df, x="date", y="value", color="group", title="Trend Over Time")
fig.update_layout(template="plotly_white")

# Scatter
fig = px.scatter(df, x="x", y="y", color="group", size="magnitude",
                 hover_data=["name"], title="Relationship")

# Bar
fig = px.bar(df, x="category", y="value", color="group", barmode="group",
             title="Comparison Across Groups")
```

### Small Multiples (faceting)

When one chart has too many groups, split into a grid:

```python
# seaborn FacetGrid
g = sns.FacetGrid(df, col="group", col_wrap=3, height=3, sharex=True, sharey=True)
g.map(sns.histplot, "value", bins=20)

# plotly facet
fig = px.scatter(df, x="x", y="y", facet_col="group", facet_col_wrap=3)
```

### Style Templates

**Clean minimal (good default):**

```python
plt.rcParams.update({
    "figure.facecolor": "white",
    "axes.facecolor": "white",
    "axes.grid": True,
    "grid.alpha": 0.2,
    "axes.spines.top": False,
    "axes.spines.right": False,
    "font.size": 11,
    "axes.titlesize": 13,
    "axes.titleweight": "bold",
})
```

## Accessibility Checklist

- [ ] Colors are distinguishable in grayscale (print-friendly)
- [ ] Uses colorblind-safe palette (avoid red/green only)
- [ ] Text is large enough to read (min ~10pt in final output)
- [ ] Key data is labeled directly, not just color-coded
- [ ] Alt text describes the chart's insight for screen readers
- [ ] Sufficient contrast between data elements and background

## Reference Documentation

- Chart selection decision tree: `references/chart_selection.md`
- Color palette recommendations: `references/color_palettes.md`
