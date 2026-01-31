#!/usr/bin/env Rscript
# Entry point for the March Madness pipeline.
# Run from project root: Rscript scripts/run_pipeline.R

# Find project root and set working directory
if (file.exists("configs/default.yaml")) {
  proj_root <- getwd()
} else if (file.exists("../configs/default.yaml")) {
  proj_root <- normalizePath("..")
} else {
  stop("Run from project root or scripts/ directory. Expected configs/default.yaml")
}
setwd(proj_root)
message("Working directory: ", proj_root)

# Run the pipeline
source("src/pipelines/madness.R")
message("Pipeline complete.")
