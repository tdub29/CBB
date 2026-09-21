# Project Color Palette

Starting palette for CBB reports, charts, and any future app surfaces. The direction is blue-forward, dark, and atmospheric. Image-based reports should not default to white or off-white backgrounds; pale surfaces are reserved for print-style tables or utility docs.

## Core colors

| Token | Hex | Use |
| --- | --- | --- |
| `black` | `#111418` | Headings, strongest text, high-contrast anchors |
| `midnight` | `#0A1622` | Primary report background |
| `deep_ocean` | `#10263A` | Chart panel background |
| `storm` | `#1C3850` | Grid lines, panel borders, dividers |
| `ink` | `#1D2630` | Body text, outlines, dense chart elements |
| `slate` | `#5F6B78` | Secondary text, muted labels |
| `steel` | `#8793A1` | Light annotations, secondary series |
| `fog` | `#D6DEE6` | Grid lines, borders, subtle separators |
| `paper` | `#F5F7FA` | Print/table background only, not picture-led reports |
| `navy` | `#173A5E` | Dark primary blue |
| `signal_blue` | `#2A5B84` | Main project blue |
| `lake_blue` | `#5F89B2` | Secondary blue, alternate series |
| `sky_blue` | `#AFC4D6` | Light fill, heatmaps, glow accents |
| `sage` | `#7D9386` | Muted accent for highlights without disruption |

## Working guidance

- Use `midnight` plus `deep_ocean` as the default export background stack for image-based reports.
- Use `signal_blue` as the default brand/data color.
- Use `sage` sparingly for benchmarks, highlights, or one comparison series.
- Use `fog` and `steel` for typography on dark surfaces.
- Use `paper` only when the output is intentionally document-like rather than visual/story-led.

## Report direction

- Picture-based reports should feel like a broadcast graphic or scouting board, not a spreadsheet screenshot.
- Default contrast should come from dark layered backgrounds, light typography, and restrained accent color.
- Avoid large white fields unless a specific chart requires a print treatment.

## Python usage

Shared tokens live in [src/visualization/palette.py](/c:/Users/TrevorWhite/Downloads/CBBData/CBB/src/visualization/palette.py).

```python
from src.visualization.palette import PROJECT_PALETTE, set_project_theme

set_project_theme()
primary = PROJECT_PALETTE["signal_blue"]
accent = PROJECT_PALETTE["sage"]
```

## CSS variable starter

```css
:root {
  --cbb-black: #111418;
  --cbb-midnight: #0a1622;
  --cbb-deep-ocean: #10263a;
  --cbb-storm: #1c3850;
  --cbb-ink: #1d2630;
  --cbb-slate: #5f6b78;
  --cbb-steel: #8793a1;
  --cbb-fog: #d6dee6;
  --cbb-paper: #f5f7fa;
  --cbb-navy: #173a5e;
  --cbb-signal-blue: #2a5b84;
  --cbb-lake-blue: #5f89b2;
  --cbb-sky-blue: #afc4d6;
  --cbb-sage: #7d9386;
}
```
