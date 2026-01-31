#!/usr/bin/env Rscript
# Install required (and optional) R packages for the March Madness pipeline.
# Run from project root: Rscript scripts/install_deps.R

# Find project root (directory containing configs/)
if (file.exists("configs/default.yaml")) {
  proj_root <- getwd()
} else if (file.exists("../configs/default.yaml")) {
  proj_root <- normalizePath("..")
  setwd(proj_root)
} else {
  stop("Run from project root or scripts/ directory")
}
source("packages.R", local = TRUE)

install_if_missing <- function(pkgs, required = TRUE) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing ", pkg, "...")
      install.packages(pkg, repos = "https://cloud.r-project.org")
    } else {
      message(pkg, " already installed.")
    }
  }
}

message("Installing required packages...")
install_if_missing(required_packages)

message("\nInstalling optional packages (for injury impact analysis)...")
install_if_missing(optional_packages)

message("\nDone.")
