#!/usr/bin/env Rscript
# Build coach_tournament_experience.csv by fetching Bart Torvik team-history.php
# Same data source as toRvik::bart_team_history (no toRvik required)
# Run: Rscript scripts/fetch_coach_experience.R

EXCLUDE_YEAR <- 2020

finish_to_wl <- function(finish) {
  if (is.na(finish) || finish == "" || length(finish) == 0) return(c(0, 0))
  s <- tolower(trimws(as.character(finish)))
  if (grepl("champion|champ|1st", s)) return(c(6, 0))
  if (grepl("2nd|runner|final", s)) return(c(5, 1))
  if (grepl("final four|f4|\\b4\\b", s)) return(c(4, 1))
  if (grepl("elite eight|e8|\\b8\\b", s)) return(c(3, 1))
  if (grepl("sweet 16|s16|\\b16\\b", s)) return(c(2, 1))
  if (grepl("r32|\\b32\\b", s)) return(c(1, 1))
  if (grepl("r64|\\b64\\b|first four", s)) return(c(0, 1))
  m <- regmatches(s, regexec("^(\\d+)-(\\d+)$", s))[[1]]
  if (length(m) == 3) return(c(as.integer(m[2]), as.integer(m[3])))
  c(0, 0)
}

years_prior_5 <- function(y) {
  out <- (y - 5):(y - 1)  # five years prior, exclude current season
  out[out != EXCLUDE_YEAR]
}

fetch_team_history <- function(team) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("Install httr: install.packages('httr')")
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Install rvest: install.packages('rvest')")
  q <- gsub(" ", "+", trimws(team))
  url <- paste0("https://barttorvik.com/team-history.php?team=", q)
  resp <- httr::GET(url, httr::user_agent("Mozilla/5.0 (compatible; CBB pipeline)"))
  if (httr::status_code(resp) != 200) return(NULL)
  txt <- httr::content(resp, "text")
  if (grepl("Verifying Browser", txt, ignore.case = TRUE)) return(NULL)
  html <- rvest::read_html(txt)
  tables <- rvest::html_table(html)
  if (length(tables) == 0) return(NULL)
  tab <- tables[[1]]
  if (nrow(tab) < 2) return(NULL)
  # Columns: year, t_rk, coach (name seed finish), rec, ...
  nms <- names(tab)
  if (length(nms) < 3) return(NULL)
  year_col <- which(sapply(tab[, 1], function(x) grepl("^20[0-2][0-9]$", as.character(x))))[1]
  coach_col <- 3
  out <- data.frame(
    team = team,
    year = as.integer(tab[[1]]),
    coach = "",
    tourn_w = 0,
    tourn_l = 0,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(tab))) {
    y <- suppressWarnings(as.integer(tab[i, 1]))
    if (is.na(y) || y < 2008) next
    coach_cell <- as.character(tab[i, coach_col])
    if (is.na(coach_cell) || coach_cell == "") next
    # Parse "Dan Hurley8 seed, R32" (Torvik) or "CoachName 3 R32"
    m <- regmatches(coach_cell, regexec("^(.+?)(\\d+)\\s*seed\\s*[,]?\\s*(.*)$", coach_cell))[[1]]
    if (length(m) >= 4) {
      coach_name <- trimws(m[2])
      finish <- trimws(m[4])
    } else {
      m <- regmatches(coach_cell, regexec("^(.+?)\\s+(\\d+)[,\\s]+(.+)$", coach_cell))[[1]]
      if (length(m) >= 4) {
        coach_name <- trimws(m[2])
        finish <- trimws(m[4])
      } else {
        coach_name <- coach_cell
        finish <- ""
      }
    }
    wl <- finish_to_wl(finish)
    out$coach[i] <- coach_name
    out$tourn_w[i] <- wl[1]
    out$tourn_l[i] <- wl[2]
  }
  out
}

# Find latest training CSV
proj_root <- if (file.exists("configs/default.yaml")) getwd() else {
  if (file.exists("../configs/default.yaml")) { setwd(".."); getwd() } else stop("Run from project root")
}
proc_dir <- file.path(proj_root, "data", "processed")
files <- list.files(proc_dir, pattern = "^cbbtrainingdata_.*\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No cbbtrainingdata_*.csv found. Run pipeline first.")
training_path <- files[order(file.info(files)$mtime, decreasing = TRUE)[1]]

message("Reading ", basename(training_path))
training <- read.csv(training_path)
year_col <- if ("year" %in% names(training)) "year" else "Year"
team_year <- unique(training[, c("Team", year_col)])
names(team_year)[2] <- "Year"
team_year$Year <- as.integer(team_year$Year)
team_year <- team_year[!is.na(team_year$Year), , drop = FALSE]
teams <- unique(as.character(team_year$Team))

message("Fetching team history for ", length(teams), " teams...")
all_history <- list()
for (i in seq_along(teams)) {
  team <- teams[i]
  if (i %% 25 == 0) message("  ", i, "/", length(teams), ": ", team)
  h <- fetch_team_history(team)
  if (!is.null(h)) all_history[[team]] <- h
  Sys.sleep(0.6)
}

if (length(all_history) == 0) stop("No team history fetched (Torvik may be blocking). Try running from a browser session or use Python script with --playwright.")

hist_df <- do.call(rbind, all_history)
rownames(hist_df) <- NULL

message("Building coach experience...")
out_rows <- list()
for (i in seq_len(nrow(team_year))) {
  team <- as.character(team_year$Team[i])
  year <- team_year$Year[i]
  prior <- years_prior_5(year)
  coach_row <- hist_df[hist_df$team == team & hist_df$year == year, ]
  if (nrow(coach_row) == 0) {
    out_rows[[i]] <- data.frame(Team = team, Year = year,
      coach_tourn_appearances_5y = 0, coach_tourn_wins_5y = 0, coach_tourn_losses_5y = 0, coach_years_with_program = 0)
    next
  }
  coach_name <- coach_row$coach[1]
  coach_prior <- hist_df[hist_df$coach == coach_name & hist_df$year %in% prior, ]
  coach_tourn <- coach_prior[(coach_prior$tourn_w + coach_prior$tourn_l) > 0, ]
  appearances <- nrow(coach_tourn)
  wins <- sum(coach_prior$tourn_w, na.rm = TRUE)
  losses <- sum(coach_prior$tourn_l, na.rm = TRUE)
  years_with <- 0
  for (y in year:2008) {
    r <- hist_df[hist_df$team == team & hist_df$year == y, ]
    if (nrow(r) == 0 || r$coach[1] != coach_name) break
    years_with <- years_with + 1
  }
  out_rows[[i]] <- data.frame(Team = team, Year = year,
    coach_tourn_appearances_5y = appearances, coach_tourn_wins_5y = wins, coach_tourn_losses_5y = losses,
    coach_years_with_program = years_with)
}

out <- do.call(rbind, out_rows)
out_dir <- file.path(proj_root, "data", "external")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, "coach_tournament_experience.csv")
write.csv(out, out_path, row.names = FALSE)
message("Wrote ", nrow(out), " rows to ", out_path)

# Summary
nz <- sum(out$coach_tourn_appearances_5y > 0)
message("Rows with non-zero coach experience: ", nz)
