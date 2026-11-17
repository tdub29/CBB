#!/usr/bin/env python3
"""Write a blank sports validation charter markdown file.

The fields match the "Charter template" block in validation-design/SKILL.md, in
the same order; angle brackets mark what to fill in.
"""

from __future__ import annotations

import argparse
from pathlib import Path

TEMPLATE = """# Validation charter

- Target / grain / decision time T / eligible population:
- Split unit and ordering: season walk-forward
- Grouping rule:
- Minimum training history: min_train_seasons = 2
- Window: expanding
- Gap/embargo and availability rationale:
- Primary metric definition:
- Secondary diagnostics:
- Baselines:
- Inner tuning and early stopping: inside training only
- Final untouched holdout:
- Window ledger: <window | what scored it | who | when | commit; any entry means the window is seen, never "pre-registered", "untouched", "blind" or "scored once">
- Candidates at first score: <the list, stamped with the time the first test table printed; later additions are post hoc>
- Look budget: <families allowed per target | n so far | null-simulated bar for the n-th family | how computed; every pass reports n>
- Power card: <MDE at this n | date a forward test can resolve it; a null below the MDE is untestable>
- Slice plan: <slices named before results, each with its n>
- Thresholds (asserted in code at <file:line>): <every promotion or ship bar>
- Acceptance scope: <the program-level rule this study counts against>
- Decision block: <decision and loss at T | reference policy | executable limits | live version and the gate it passed | forward-scoring plan and stop rule>
- Primary claim: <the one number that may travel, with its test and unit>
- Common evaluation rows:
- Success / failure rule: beat baselines on mean primary metric AND on majority of folds
- Required leakage checks: feature-rules + leakage-audit
- Per-fold reporting and slice requirements: per-fold table + mean + decision
- Allowed reruns / versioning:
- Notes:
"""


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--out", required=True, help="Markdown output path")
    args = p.parse_args()
    path = Path(args.out)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(TEMPLATE, encoding="utf-8")
    print(f"wrote {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
