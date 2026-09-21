#!/usr/bin/env Rscript
# Fetch one team's history via toRvik::bart_team_history (R package).
# Usage: Rscript scripts/fetch_team_history_torvik_r.R [team_name]
# Example: Rscript scripts/fetch_team_history_torvik_r.R "Texas A&M"

team <- if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  paste(commandArgs(trailingOnly = TRUE), collapse = " ")
} else {
  "Texas A&M"
}

if (!requireNamespace("toRvik", quietly = TRUE)) {
  message("toRvik not installed. Install with: install.packages('toRvik') or remotes::install_github('andreweatherman/toRvik')")
  quit(save = "no", status = 1)
}

message("Fetching team history for: ", sQuote(team))
h <- tryCatch(toRvik::bart_team_history(team = team), error = function(e) {
  message("Error: ", conditionMessage(e))
  NULL
})

if (is.null(h) || (is.data.frame(h) && nrow(h) == 0)) {
  message("No data returned.")
  quit(save = "no", status = 1)
}

message("Rows: ", nrow(h))
message("Columns: ", paste(names(h), collapse = ", "))
print(h)
out <- file.path(getwd(), "data", "raw", "team_history_texas_am_torvik_r.csv")
if (!dir.exists(dirname(out))) out <- "team_history_texas_am_torvik_r.csv"
write.csv(h, out, row.names = FALSE)
message("Wrote: ", out)
