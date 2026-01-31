# Required packages for March Madness pipeline
# Install via: source("scripts/install_deps.R")

required_packages <- c(
  "rvest",
  "httr",
  "dplyr",
  "sqldf",
  "readxl",
  "stringr",
  "Metrics",
  "ggplot2",
  "yaml"
)

optional_packages <- c(
  "toRvik"   # For bart_injuryimpact (exploratory injury impact analysis)
)
