# Chart Selection Reference

Detailed guidance for choosing the right chart type.

## Decision Matrix

For each combination of data type and question, the recommended chart:

| Question | 1 numeric | 2 numeric | 1 categorical | 1 cat + 1 num | 2 cat | Time series |
|----------|-----------|-----------|---------------|---------------|-------|-------------|
| Distribution | Histogram, KDE | Hexbin, 2D hist | Bar (counts) | Box/violin | Heatmap (counts) | — |
| Comparison | — | Scatter | Bar | Grouped bar | Grouped bar | Multi-line |
| Trend | — | — | — | — | — | Line chart |
| Composition | — | — | Pie (≤4), bar | Stacked bar | Stacked bar | Stacked area |
| Relationship | — | Scatter, line | — | Strip/swarm | Mosaic | Lag plot |
| Ranking | — | — | Horizontal bar | Horizontal bar | — | Bump chart |

## When to Use Each Chart

### Bar Chart

**Use when:**
- Comparing values across discrete categories
- Showing ranking or ordered comparison
- Displaying counts or frequencies

**Best practices:**
- Sort by value (not alphabetically) unless there's a natural order
- Use horizontal bars when labels are long
- Start Y-axis at zero
- Use consistent color; accent only for emphasis
- Limit to ~15 bars before considering grouping

**Avoid when:**
- Data is continuous (use histogram instead)
- Showing change over time (use line chart)

### Line Chart

**Use when:**
- Showing trends over continuous time
- Comparing trends for 2-5 groups
- Highlighting rate of change

**Best practices:**
- Time on X-axis (always left to right)
- Keep line count to 5 or fewer
- If >5, use small multiples or highlight + gray
- Annotate key events or inflection points
- Use consistent line thickness; accent the key series

**Avoid when:**
- X-axis is categorical / non-ordered
- Comparing magnitude (use bar chart)

### Scatter Plot

**Use when:**
- Exploring relationship between two continuous variables
- Looking for clusters, outliers, or patterns
- Dataset is <5000 points

**Best practices:**
- Add a trend line if showing correlation
- Use color for a third dimension (categorical grouping)
- Use size for a fourth dimension sparingly
- Add axis labels with units
- Use transparency (alpha) when points overlap

**Avoid when:**
- Massive overplotting (>10K points) — use hexbin or density
- Comparing categories — use bar or box plot

### Histogram

**Use when:**
- Understanding the distribution of one numeric variable
- Checking for normality, skewness, outliers
- Choosing bin count: start with sqrt(n), adjust for clarity

**Best practices:**
- 15-30 bins is usually right for most datasets
- Add KDE overlay for smooth shape
- Use consistent bin width
- Mark mean/median with vertical lines if relevant

### Box Plot

**Use when:**
- Comparing distributions across groups
- Showing median, spread, and outliers compactly
- Audience is data-literate

**Best practices:**
- Sort groups by median for easy comparison
- Add individual points (strip plot overlay) for small datasets
- Label what the box components mean if audience is mixed

### Heatmap

**Use when:**
- Showing correlation matrices
- Displaying values across two categorical axes
- Any matrix/grid data

**Best practices:**
- Annotate cells with values
- Use diverging colormap centered at meaningful value (0 for correlations)
- Order rows/columns by clustering or magnitude
- Keep it readable: max ~20x20 before it gets crowded

### Pie Chart (use sparingly)

**Only use when:**
- Showing parts of ONE whole
- 2-4 slices maximum
- Exact proportions are less important than the general split

**Never use when:**
- More than 5 slices
- Comparing across multiple groups (don't use multiple pies)
- Precision matters (humans are bad at comparing angles)
- There's any other reasonable option

## Small Multiples

When a single chart gets too busy, split into a grid of consistent small charts.

**Use when:**
- Comparing the same pattern across many groups (>5)
- Each sub-chart is simple (one line, one histogram)
- Shared axes make comparison easy

**Best practices:**
- Same scale on all panels (shared X and Y)
- Label each panel clearly
- Use a clean grid layout (3-4 columns)
- Keep individual panels simple
