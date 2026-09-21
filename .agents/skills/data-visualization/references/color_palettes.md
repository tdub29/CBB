# Color Palette Recommendations

Choosing colors that are effective, accessible, and purposeful.

## Palette Types

### Qualitative (categorical / unordered groups)

Use when each color represents a distinct category with no inherent order.

| Palette | Source | Good for |
|---------|--------|----------|
| Tab10 | matplotlib | Default, decent for up to 10 categories |
| Set2 | ColorBrewer | Softer tones, up to 8 categories |
| Paired | ColorBrewer | Paired comparisons (light/dark variants) |
| Dark2 | ColorBrewer | High contrast, up to 8 categories |

**Rule:** Never use more than 7 qualitative colors. Beyond that, use direct labels or small multiples instead.

### Sequential (low to high)

Use for a single variable going from low to high values.

| Palette | Best for |
|---------|----------|
| Viridis | Perceptually uniform; prints well in grayscale; colorblind safe |
| Blues / Greens | When you want a single-hue gradient |
| YlOrRd | Emphasizing high values as "hot" |
| Inferno / Magma | High contrast; works on both light and dark backgrounds |

**Default recommendation:** Viridis. It's colorblind-safe, perceptually uniform, and prints well in black and white.

### Diverging (deviation from center)

Use when there's a meaningful midpoint (zero, average, target).

| Palette | Best for |
|---------|----------|
| RdBu (Red-Blue) | Correlation matrices, positive/negative |
| Coolwarm | Subtle diverging, less harsh than RdBu |
| BrBG (Brown-Green) | Colorblind-safe alternative to RdBu |
| PiYG (Pink-Green) | Another colorblind-safe option |

**Always set `center=0`** (or the meaningful midpoint) so the neutral color aligns with the break point.

## Highlight Strategy

The most impactful technique: make everything gray, then add one accent color.

```python
import matplotlib.pyplot as plt

categories = ["A", "B", "C", "D", "E"]
values = [10, 25, 15, 30, 20]
highlight = 3  # index of the item to highlight

colors = ["#d0d0d0"] * len(categories)
colors[highlight] = "#e63946"

fig, ax = plt.subplots()
ax.bar(categories, values, color=colors)
ax.set_title("Category D Leads All Others")
```

**When to use:** You have one key insight and want the reader's eye to go directly to it.

## Colorblind Safety

~8% of men and ~0.5% of women have color vision deficiency. The most common type can't distinguish red from green.

**Safe combinations:**
- Blue + Orange
- Blue + Yellow
- Purple + Yellow/Green
- Dark blue + Light blue (intensity variation)

**Unsafe combinations:**
- Red + Green
- Red + Brown
- Green + Brown

**Test your charts:**
- Use a colorblind simulator (e.g., Coblis, Color Oracle)
- Or just check: does your chart make sense in grayscale?

**Don't rely on color alone:**
- Add text labels
- Use different line styles (solid, dashed, dotted)
- Use different marker shapes
- Add patterns (hatching) to bar fills

## Practical Defaults

### matplotlib

```python
# Qualitative
plt.cm.tab10(range(5))

# Sequential
plt.cm.viridis(np.linspace(0, 1, 10))

# Diverging
plt.cm.RdBu_r(np.linspace(0, 1, 11))  # _r = reversed so red=negative
```

### seaborn

```python
# Set a good default
sns.set_palette("Set2")

# For specific plots
sns.heatmap(data, cmap="RdBu_r", center=0)
sns.barplot(data=df, x="cat", y="val", palette="viridis")
```

### plotly

```python
# Sequential
fig.update_layout(coloraxis_colorscale="Viridis")

# Qualitative
px.bar(df, color="category", color_discrete_sequence=px.colors.qualitative.Set2)
```

## Hex Color Quick Reference

Useful accent colors:

| Color | Hex | Use |
|-------|-----|-----|
| Strong red | `#e63946` | Danger, negative, highlight |
| Calm blue | `#457b9d` | Primary, neutral emphasis |
| Dark navy | `#1d3557` | Text, strong emphasis |
| Warm yellow | `#f4a261` | Warning, secondary highlight |
| Soft green | `#2a9d8f` | Positive, success |
| Light gray | `#d0d0d0` | Background, non-highlighted data |
| Medium gray | `#8d99ae` | Secondary text, gridlines |
