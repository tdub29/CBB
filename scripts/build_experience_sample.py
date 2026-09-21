#!/usr/bin/env python3
from __future__ import annotations

from experience_analysis_common import get_experience_base_paths, load_experience_data


def main() -> int:
    paths = get_experience_base_paths()
    sample = load_experience_data(paths["input_raw"])
    output_path = paths["input_sample"]
    output_path.parent.mkdir(parents=True, exist_ok=True)
    sample.to_csv(output_path, index=False)
    year_min = int(sample["year"].min())
    year_max = int(sample["year"].max())
    print(f"Wrote experience sample: {output_path} ({len(sample)} rows, {len(sample.columns)} cols)")
    print(f"Year range: {year_min}-{year_max}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
