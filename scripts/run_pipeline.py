#!/usr/bin/env python3
"""Entry point for the March Madness pipeline (Python)."""
import sys
from pathlib import Path

# Ensure project root is on path
project_root = Path(__file__).resolve().parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

# Change to project root
import os

os.chdir(project_root)

from src.pipelines.madness import run_pipeline

if __name__ == "__main__":
    run_pipeline()
