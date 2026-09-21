# Data Viz Learnings

## What worked

- For the SOS conference chart, plotting conference `avg_barthag` over time worked better than plotting rank because the threshold line is also in Barthag units.
- A static matplotlib chart was sufficient. React was not needed because the problem was label strategy, not lack of interactivity.
- Sharing the x-axis between the top and bottom panels made the year positions line up correctly and improved readability.
- Removing gridlines entirely made the chart cleaner than trying to lighten them.
- A single thick vertical marker for the transfer-rules era reads better than multiple breakpoint markers.

## Labeling choices

- Use direct series labels for this chart, not a legend.
- Direct series labels need:
  - right-edge placement
  - connector lines from the series endpoint
  - vertical spacing logic to avoid collisions
  - filled, outlined label boxes
- Threshold and annotation labels should also use filled boxes with visible outlines.
- Keep threshold labels at the far right edge so they do not compete with the data region.
- Minimal labels work best for reference lines:
  - top threshold: `Power conf. threshold`
  - bottom dotted line: `Avg gap`
- The transfer-era label should be large and explicit: `Current transfer-rules era starts`.

## Text and annotation rules

- Make structural annotations noticeably larger than normal axis text.
- Use explicit signs for directional statistics in labels, e.g. `+18.0%` instead of `18.0%`.
- Verify annotation math against the exported CSV, not just the plotting code.
- For this chart, the widest-gap annotation should show signed percent change versus `2022`.

## Context-specific decisions

- Use `2022` as the single season-level transfer-rules breakpoint for this visualization.
- The top threshold is the median yearly 6th-place conference `avg_barthag`, but the chart label should stay simple rather than spelling out the full method.
- The bottom chart benefits from a short descriptive subtitle explaining what the gap represents in plain language.

## Process guardrails

- After each visual tweak, rerun the chart and inspect the rendered image rather than assuming the code change helped.
- When labels still look bad, fix the labeling strategy first before escalating to a different visualization stack.
