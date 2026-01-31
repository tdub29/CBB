# March Madness pipeline: ingestion, features, modeling
`%||%` <- function(x, y) if (is.null(x)) y else x
# Load config if available
if (requireNamespace("yaml", quietly = TRUE)) {
  cfg <- yaml::read_yaml("configs/default.yaml")
} else {
  cfg <- list(paths = list(output_training_csv = "data/processed/cbbtrainingdata", data_external = "data/external"))
}
output_csv_prefix <- cfg$paths$output_training_csv %||% "data/processed/cbbtrainingdata"
ext_data_dir <- cfg$paths$data_external %||% "data/external"

library(rvest)
library(httr)
library(dplyr)

regular_season_raw <- data.frame()

# Scrape regular season data from Bart Torvik
for (year in 2008:2024){
  url2<- as.character(year)
  url1 <- "https://barttorvik.com/trank.php?year="
  url3 <- "&sort=&top=0&conlimit=All&venue=All&type=R#"
  url <- paste(url1, url2, url3, sep = "")
  html <- read_html(httr::GET(url, user_agent("Mozilla/5.0")))
  
  # Extract the table
  ncaa_scrape <- html %>% html_node("table") %>% html_table()
  
  new_colnames <- as.character(ncaa_scrape[1,])
  colnames(ncaa_scrape) <- new_colnames
  # Remove the first row, which contains column headers
  # Define the starting row and step size
  
  filter(ncaa_scrape, ncaa_scrape$Rk != 'Rk') -> ncaa_scrape

  ncaa_scrape$year <- NA
  ncaa_scrape$year <- year
  regular_season_raw <- rbind(regular_season_raw, ncaa_scrape)
}

# Scrape postseason (conference tournament) data
postseason_raw <- data.frame()
for (year in 2008:2024){
  url2<- as.character(year)
  url1 <- "https://barttorvik.com/trank.php?year="
  url3 <- "&sort=&top=0&conlimit=All&venue=All&type=P#"
  url <- paste(url1, url2, url3, sep = "")
  html <- read_html(httr::GET(url, user_agent("Mozilla/5.0")))
  
  # Extract the table
  ncaa_scrapeconf <- html %>% html_node("table") %>% html_table()
  
  new_colnames <- as.character(ncaa_scrapeconf[1,])
  colnames(ncaa_scrapeconf) <- new_colnames
  
  # Loop over the rows to remove
  filter(ncaa_scrapeconf, ncaa_scrapeconf$Rk != 'Rk') -> ncaa_scrapeconf
  
  ncaa_scrapeconf$year <- NA
  ncaa_scrapeconf$year <- year
  postseason_raw <- rbind(postseason_raw, ncaa_scrapeconf)
}

# Scrape NCAA tournament data
ncaa_tournament_raw <- data.frame()
for (year in 2008:2024){
  url2<- as.character(year)
  url1 <- "https://barttorvik.com/trank.php?year="
  url3 <- "&sort=&top=0&conlimit=All&venue=All&type=T#"
  url <- paste(url1, url2, url3, sep = "")
  html <- read_html(httr::GET(url, user_agent("Mozilla/5.0")))
  
  # Extract the table
  ncaa_scrapetourn <- html %>% html_node("table") %>% html_table()
  
  new_colnames <- as.character(ncaa_scrapetourn[1,])
  colnames(ncaa_scrapetourn) <- new_colnames
  # Remove the first row, which contains column headers
  # Define the starting row and step size
  start_row <- 1
  step_size <- 25
  
  # Loop over the rows to remove
  for (i in seq(start_row, nrow(ncaa_scrapetourn), step_size)) {
    # Remove the current row
    ncaa_scrapetourn <- ncaa_scrapetourn[-i, ]
  }
  ncaa_scrapetourn$year <- NA
  ncaa_scrapetourn$year <- year
  ncaa_tournament_raw <- rbind(ncaa_tournament_raw, ncaa_scrapetourn)
}

# -----------------------------------------------------------------------------
# Feature engineering
# -----------------------------------------------------------------------------

# Parse NCAA tournament wins/losses
ncaa_tournament_raw$W <- substr(ncaa_tournament_raw$Rec, 1, ifelse(substr(ncaa_tournament_raw$Rec, 2, 2) %in% c("0", "1"), 2, 1))
ncaa_tournament_raw$L <- ifelse(substr(ncaa_tournament_raw$Rec, 2, 2) %in% c("0", "1"), substr(ncaa_tournament_raw$Rec, 4, 4), substr(ncaa_tournament_raw$Rec, 3, 3))

# Parse conference tournament wins/losses
postseason_raw$W <- substr(postseason_raw$Rec, 1, ifelse(substr(postseason_raw$Rec, 2, 2) %in% c("0", "1", "2", "3"), 2, 1))
postseason_raw$L <- ifelse(substr(postseason_raw$Rec, 2, 2) %in% c("0", "1", "2", "3"), substr(postseason_raw$Rec, 4, 4), substr(postseason_raw$Rec, 3, 3))

# Join postseason with NCAA tournament to get conference-only W/L
library(sqldf)
conf_tourney_with_ncaa <- sqldf("SELECT t.*, tf.W as tournw, tf.L as tournl
                       FROM postseason_raw t
                       LEFT JOIN ncaa_tournament_raw tf
                       ON t.Team = tf.Team AND t.year = tf.year")
conf_tourney_with_ncaa$tournw <- as.numeric(conf_tourney_with_ncaa$tournw)
conf_tourney_with_ncaa$tournl <- as.numeric(conf_tourney_with_ncaa$tournl)
conf_tourney_with_ncaa$L <- as.numeric(conf_tourney_with_ncaa$L)
conf_tourney_with_ncaa$W <- as.numeric(conf_tourney_with_ncaa$W)
conf_tourney_with_ncaa <- mutate(conf_tourney_with_ncaa, conftournw = W - tournw, conftournl = L - tournl)

# Join regular season with conference tournament W/L
teams_with_conf_tourney_wl <- sqldf("SELECT mt.*, jc.conftournw as W, jc.conftournl as L
                      FROM regular_season_raw mt
                      LEFT JOIN conf_tourney_with_ncaa jc
                      ON mt.Team = jc.Team AND mt.year = jc.year")

library(dplyr)
teams_annotated <- arrange(teams_with_conf_tourney_wl, desc(AdjOE))
# teams_annotated <- slice(mastertable_sorted, 2:nrow(mastertable_sorted))


# create a new column called "seed"
teams_annotated$seed <- NA
# Loop through each row of "training_data$Team"
for (i in seq_along(teams_annotated$Team)) {
  # Check if "seed" is present in the value of the "Team" column
  if (grepl("seed", teams_annotated$Team[i])) {
    # If "seed" is present, extract all numbers before "seed" using a regular expression
    seed_value <- gsub("\\D", "", sub("seed.*", "", teams_annotated$Team[i]))
    # Store the extracted value in the "seed" column for that row
    teams_annotated$seed[i] <- seed_value
  }
}




# create a new column called "result" TO CATEGORIZE ROUNDS RESULT
teams_annotated$result <- NA

# loop over each row of the teams_annotated data frame
for (i in seq_len(nrow(teams_annotated))) {
  # extract the team name from the "Team" column
  team_name <- as.character(teams_annotated$Team[i])
  
  # check if the team name contains a certain result string
  if (grepl("R32", team_name)) {
    teams_annotated$result[i] <- 32
  } else if (grepl("R64", team_name)) {
    teams_annotated$result[i] <- 64
  } else if (grepl("Finals", team_name)) {
    teams_annotated$result[i] <- 2
  } else if (grepl("CHAMPS", team_name)) {
    teams_annotated$result[i] <- 1
  } else if (grepl("Elite Eight", team_name)) {
    teams_annotated$result[i] <- 8
  } else if (grepl("Sweet Sixteen", team_name)) {
    teams_annotated$result[i] <- 16
  } else if (grepl("Final Four", team_name)) {
    teams_annotated$result[i] <- 4
  }
}

training_data <- teams_annotated

# Clean WAB column (rank suffix and sign handling)
training_data$WAB <- sapply(as.character(training_data$WAB), function(value) {
  # Check if the value starts with "+" or "-"
  sign_prefix <- substr(value, 1, 1)
  
  # Find the position of the decimal point
  decimal_pos <- regexpr("\\.", value)
  
  # Check if there is more than one digit before the decimal point (accounting for the sign)
  if (decimal_pos == 3 && sign_prefix == "-") {  # Single-digit negative value before the decimal
    return(substr(value, 1, 4))
  } else if (decimal_pos == 4 && sign_prefix == "-") {  # Two-digit negative value before the decimal
    return(substr(value, 1, 5))
  } else if (sign_prefix == "+" || (decimal_pos == 3 && sign_prefix != "-")) {  # Positive value or single-digit value not starting with "-"
    return(substr(value, 2, 4))
  } else {  # Other cases, presumably positive with two digits before the decimal
    return(substr(value, 2, 5))
  }
})






for (col in 9:23) {
  for (i in 1:nrow(training_data)) {
    value <- as.character(training_data[i, col])
    if (grepl("\\.", value)) {
      training_data[i, col] <- substr(value, 1, 4)
    } else {
      training_data[i, col] <- substr(value, 1, 2)
    }
  }
}
for(i in 1:nrow(training_data)){
  training_data[i, 8] <- substr(training_data[i, 8], 1, 5)}

for (col in 6:7) {
  for (i in 1:nrow(training_data)) {
    value <- as.character(training_data[i, col])
    if (startsWith(value, "1")) {
      training_data[i, col] <- substr(value, 1, 5)
    } else {
      training_data[i, col] <- substr(value, 1, 4)
    }
  }
}

as.numeric(training_data$AdjOE)->training_data$AdjOE
as.numeric(training_data$AdjDE)->training_data$AdjDE
as.numeric(training_data$Barthag)->training_data$Barthag
training_data$`EFG%` <- as.numeric(training_data$`EFG%`)
training_data$`EFGD%` <- as.numeric(training_data$`EFGD%`)
training_data$TOR <- as.numeric(training_data$TOR)
training_data$TORD <- as.numeric(training_data$TORD)
training_data$ORB <- as.numeric(training_data$ORB)
training_data$DRB <- as.numeric(training_data$DRB)
training_data$FTR <- as.numeric(training_data$FTR)
training_data$FTRD <- as.numeric(training_data$FTRD)
training_data$`2P%` <- as.numeric(training_data$`2P%`)
training_data$`2P%D` <- as.numeric(training_data$`2P%D`)
training_data$`3P%` <- as.numeric(training_data$`3P%`)
training_data$`3P%D` <- as.numeric(training_data$`3P%D`)
training_data$`Adj T.` <- as.numeric(training_data$`Adj T.`)
training_data$WAB <- as.numeric(training_data$WAB)
training_data$seed <- as.numeric(training_data$seed)

training_data$round <- ifelse(training_data$result == '64', 0,
                        ifelse(training_data$result == '32', 1,
                               ifelse(training_data$result == '16', 2,
                                      ifelse(training_data$result == '8', 3,
                                             ifelse(training_data$result == '4', 4,
                                                    ifelse(training_data$result == '2', 5,
                                                           ifelse(training_data$result == '1', 6, NA)))))))
# loop through training_data$W and training_data$L
for (i in 1:length(training_data$W)) {
  if (is.na(training_data$W[i])) {
    training_data$W[i] <- 1
  }
  if (is.na(training_data$L[i])) {
    training_data$L[i] <- 1
  }
}

library(dplyr)
library(stringr)

training_data <- training_data %>%
  mutate(Team = ifelse(str_detect(Team, "\\b\\d+\\s+seed"), 
                       str_trim(str_replace(Team, "\\s+\\d+\\s+seed.*$", "")),
                       Team))

# Optional: add team depth data (EFFHGT, TALENT, etc.) from external CSVs
col_names_ext <- c("TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES", "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%", "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR", "PAKE", "PASE", "TALENT", "blank", "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS")
external_team_depth <- setNames(data.frame(matrix(nrow = 0, ncol = length(col_names_ext))), col_names_ext)
ext_files <- list.files(ext_data_dir, pattern = "trank_team_table_data.*\\.csv", full.names = TRUE, ignore.case = TRUE)
if (length(ext_files) > 0) {
  for (f in ext_files) {
    data <- tryCatch(read.csv(f, header = FALSE), error = function(e) NULL)
    if (!is.null(data) && nrow(data) > 0) {
      colnames(data) <- col_names_ext[seq_len(ncol(data))]
      if ("BARTHAG" %in% colnames(data) && "YEAR" %in% colnames(data)) {
        data$Rk <- ave(-as.numeric(data$BARTHAG), data$YEAR, FUN = rank)
      }
      external_team_depth <- rbind(external_team_depth, data)
    }
  }
  if (nrow(external_team_depth) > 0) {
    external_team_depth$Rk <- ave(-as.numeric(external_team_depth$BARTHAG), external_team_depth$YEAR, FUN = rank)
  }
}
#read_excel("C:\\Users\\TrevorWhite\\Downloads\\cleanedtm.xlsx")->training_data
training_data_full <- sqldf("
  SELECT 
    m.*, 
    c.TEAM, c.WINS, c.`BLK%`, c.`BLKED%`, c.`AST%`, c.`OPAST%`, 
    c.`3PRATE`, c.`3PRATED`, c.EFFHGT, c.`EXP.`, c.TALENT, c.ELITESOS
  FROM 
    training_data AS m
    LEFT JOIN external_team_depth AS c ON m.Year = c.Year AND m.Team = c.TEAM
")

library(sqldf)

training_data <- training_data_full #assign new df to master df



# Define the list of Power 5 conference abbreviations
power6_confs <- c("B12", "B10", "SEC", "ACC", "P12","BE")

# Create the Power5 variable
training_data <- training_data %>%
  mutate(Power6 = ifelse(Conf %in% power6_confs, 1, 0))

# Create BOOLEAN FEATURES FOR P5 CONFERENCES
training_data <- training_data %>%
  mutate(B12 = ifelse(Conf == "B12", 1, 0),
         B10 = ifelse(Conf == "B10", 1, 0),
         SEC = ifelse(Conf == "SEC", 1, 0),
         ACC = ifelse(Conf == "ACC", 1, 0),
         P12 = ifelse(Conf == "P12", 1, 0),
         BE = ifelse(Conf == "BE", 1, 0),
         MWC = ifelse(Conf == "MWC", 1, 0))

training_data$Rec <- substr(training_data$Rec, 1, 2)

training_data_with_team <- training_data

training_data_with_team <- training_data_with_team %>%
  select(-TEAM)

training_data_backup<-training_data
training_data <- training_data %>%
  select(-1:-3, -result, -TEAM)

##select(training_data, 1:36) ->training_data
#CREATE BOOLEAN FEATURES FOR ROUNDS
training_data <- training_data %>%
  mutate(Round2 = ifelse(round >= 1, 1, 0),
         S16 = ifelse(round >= 2, 1, 0),
         E8 = ifelse(round >= 3, 1, 0),
         F4 = ifelse(round >= 4, 1, 0),
         Ship = ifelse(round >= 5, 1, 0),
         Champ = ifelse(round == 6, 1, 0))

seeded_teams_subset <- training_data %>%
  filter(!is.na(seed))

training_data <- training_data %>%
  mutate(across(45:50, ~ifelse(is.na(.), 0, .)))

# Fill NA values in 'round' column with 0
training_data <- training_data %>%
  mutate(round = ifelse(is.na(round), 0, round))

training_data ->training_data_analysis_backup
# Fill optional columns (from external data) with 0 when NA so pipeline runs without external files
optional_cols <- c("WINS", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED", "EFFHGT", "EXP.", "TALENT", "ELITESOS")
for (oc in optional_cols) {
  if (oc %in% colnames(training_data)) training_data[[oc]][is.na(training_data[[oc]])] <- 0
}
# Now remove rows with NA values in any column
training_data <- training_data %>%
  na.omit()

training_data <- training_data %>%
  mutate(G = as.numeric(G),
         `3PR` = as.numeric(`3PR`),
         `3PRD` = as.numeric(`3PRD`),
         Rec = as.numeric(Rec))




#training_data_full->training_data
library(readxl)
output_path <- paste0(output_csv_prefix, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(training_data_full, output_path, row.names = FALSE)
message("Training data written to ", output_path)

# Optional: 2023 holdout data for validation (requires cleanteam.xlsx and teamdata23.csv in data/external)
has_holdout <- FALSE
cleanteam_path <- file.path(ext_data_dir, "cleanteam.xlsx")
teamdata23_path <- file.path(ext_data_dir, "teamdata23.csv")
if (file.exists(cleanteam_path) && file.exists(teamdata23_path)) {
  holdout_2023 <- tryCatch({
    read_excel(cleanteam_path)
  }, error = function(e) { message("Could not read cleanteam.xlsx: ", e$message); NULL })
  if (!is.null(holdout_2023)) {
    combined23 <- tryCatch(read.csv(teamdata23_path, header = FALSE), error = function(e) NULL)
    if (!is.null(combined23)) {
      colnames(combined23) <- col_names_ext[seq_len(ncol(combined23))]
      holdout_2023_merged <- sqldf("
        SELECT m.*, c.TEAM, c.WINS, c.\"BLK%\", c.\"BLKED%\", c.\"AST%\", c.\"OPAST%\",
          c.\"3PRATE\", c.\"3PRATED\", c.EFFHGT, c.\"EXP.\", c.TALENT, c.ELITESOS
        FROM holdout_2023 AS m
        LEFT JOIN combined23 AS c ON m.Team = c.TEAM
      ")
      holdout_2023 <- holdout_2023_merged
      has_holdout <- TRUE
      message("2023 holdout data loaded for validation.")
    }
  }
} else {
  message("Skipping 2023 holdout: cleanteam.xlsx and teamdata23.csv not found in ", ext_data_dir)
  # Fallback: use 2023 tournament teams from our scrape for prediction
  if ("year" %in% colnames(teams_annotated)) {
    holdout_2023 <- filter(teams_annotated, !is.na(seed) & year == 2023)
    if (nrow(holdout_2023) > 0) {
      has_holdout <- TRUE
      message("Using 2023 tournament teams from scrape for validation.")
    }
  }
}






# =========================================================
# ==============building general model================================
# ==========================================
#JUST LINEAR REGRESSION


cor(training_data[6:40])->ncaacor

# Fit the model with intercept set to 1
model <- lm(round ~`3P%D`+`3P%`+ `EFG%`+ FTRD+WAB+ I(WAB^3)+`EFGD%`+`2P%D`+`2P%`+ ORB + TOR+ Barthag + AdjOE +I(AdjOE^2) + AdjDE+ I(AdjDE^2) + seed + I(seed^2)+L +0, data = training_data)
#

# Print the coefficients
coef(model)
summary(model)
model$fitted.values->training_data$fitted


cor(training_data[4:40])->ncaacor
# Initialize for jackknife model selection
best_general_mse <- Inf
currentmodel <- NULL
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 15, nrow = nrow(training_data))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_data))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_data))
# Loop through each row of the data
for (i in 1:nrow(training_data)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_data[-i, ]
  dataofone<- training_data[i,]
  
  # Fit the model to the subset of data
  fit <- lm(round ~WAB+TOR+
                      Barthag+ I(Barthag^2) + AdjOE + ORB+`EFGD%`+`3P%`
                     + AdjDE+ I(AdjDE^2) + seed + I(seed^2)+I(seed^3) + TALENT+`AST%`+0, 
                     data = data_subset)
 #+`2P%` +  + I(seed^2) `3P%`+ +L+`EFGD%``EFG%`+
 #options denied adjoe^2  + I(WAB^3) 2p%d tord+`3P%D`+W+FTR+FTRD L W adj t 
  
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit)
  predicted_round <- predict(fit, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
  
}
 

# Calculate the bias and standard error of each coefficient
bias <- bias(predvactual[,2],predvactual[,1])
mse<- rmse(predvactual[,2],predvactual[,1])^2

#mean out of sample error
summary(fit)

# Print the results
cbind(bias, mse)

best_general_mse-mse
  if ( mse < best_general_mse){
   mse -> best_general_mse
    fit->currentmodel
    print("keep")
  }


#graphinv#########
################
############3
###############
library(ggplot2)
ggplot(training_data, aes(x = fitted, y = round)) +
  geom_point() +
  geom_smooth(method = "lm")

winners <- subset(training_data, round == 6)
library(ggplot2)

# Create a data frame with the actual and fitted values
data <- data.frame(actual = training_data$round, fitted = predict(model))

# Plot the actual vs. fitted values
ggplot(data, aes(x = fitted, y = actual)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") 




###############################################################
######FILTERED LINEAR MODEL(differentiate between seeding)
#######################################################


#graphinv
library(ggplot2)
ggplot(training_data, aes(x = fitted, y = round)) +
  geom_point() +
  geom_smooth(method = "lm")

training_data_filtered <- subset(training_data, round == 6)
library(ggplot2)

# Create a data frame with the actual and fitted values
data <- data.frame(actual = training_data$round, fitted = predict(model))

# Plot the actual vs. fitted values
ggplot(data, aes(x = fitted, y = actual)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") 

# Process 2023 holdout for prediction (only when we have holdout_2023)
if (exists("holdout_2023") && nrow(holdout_2023) > 0) {

for (i in 1:nrow(holdout_2023)) {
  value <- as.character(holdout_2023[i, "WAB"])
  if (value != '131'& value != '111'& value != '104'){
    if (abs(as.numeric(value)) < 10){
      if (startsWith(value, "-")) {
        if (grepl("\\.", value)) {
          holdout_2023[i, "WAB"] <- substr(value, 1, 4)
        } else {
          holdout_2023[i, "WAB"] <- substr(value, 1, 2)
        }
      } else {
        if (grepl("\\.", value)) {
          holdout_2023[i, "WAB"] <- substr(value, 1, 3)
        } else {
          holdout_2023[i, "WAB"] <- substr(value, 1, 1)
        }
      }
    } else {
      if (startsWith(value, "-")) {
        if (grepl("\\.", value)) {
          holdout_2023[i, "WAB"] <- substr(value, 1, 5)
        } else {
          holdout_2023[i, "WAB"] <- substr(value, 1, 3)
        }
      } else {
        if (grepl("\\.", value)) {
          holdout_2023[i, "WAB"] <- substr(value, 1, 4)
        } else {
          holdout_2023[i, "WAB"] <- substr(value, 1, 2)
          
        }}}}}
for (i in 1:nrow(holdout_2023)) {
  value <- as.character(holdout_2023[i, "WAB"])
  if (!grepl("\\.", value)) {
    if (startsWith(value, "-")|value == '131'| value == '111'| value == '104') {
      holdout_2023[i, "WAB"] <- substr(value, 1, 2)
    } else {
      holdout_2023[i, "WAB"] <- substr(value, 1, 1)
    }
  }}






for (col in 9:21) {
  for (i in 1:nrow(holdout_2023)) {
    value <- as.character(holdout_2023[i, col])
    if (grepl("\\.", value)) {
      holdout_2023[i, col] <- substr(value, 1, 4)
    } else {
      holdout_2023[i, col] <- substr(value, 1, 2)
    }
  }
}
for(i in 1:nrow(holdout_2023)){
  holdout_2023[i, 8] <- substr(holdout_2023[i, 8], 1, 5)}

for (col in 6:7) {
  for (i in 1:nrow(holdout_2023)) {
    value <- as.character(holdout_2023[i, col])
    if (startsWith(value, "1")) {
      holdout_2023[i, col] <- substr(value, 1, 5)
    } else {
      holdout_2023[i, col] <- substr(value, 1, 4)
    }
  }
}

as.numeric(holdout_2023$AdjOE)->holdout_2023$AdjOE
as.numeric(holdout_2023$AdjDE)->holdout_2023$AdjDE
as.numeric(holdout_2023$Barthag)->holdout_2023$Barthag
holdout_2023$`EFG%` <- as.numeric(holdout_2023$`EFG%`)
holdout_2023$`EFGD%` <- as.numeric(holdout_2023$`EFGD%`)
holdout_2023$TOR <- as.numeric(holdout_2023$TOR)
holdout_2023$TORD <- as.numeric(holdout_2023$TORD)
holdout_2023$ORB <- as.numeric(holdout_2023$ORB)
holdout_2023$DRB <- as.numeric(holdout_2023$DRB)
holdout_2023$FTR <- as.numeric(holdout_2023$FTR)
holdout_2023$FTRD <- as.numeric(holdout_2023$FTRD)
holdout_2023$`2P%` <- as.numeric(holdout_2023$`2P%`)
holdout_2023$`2P%D` <- as.numeric(holdout_2023$`2P%D`)
holdout_2023$`3P%` <- as.numeric(holdout_2023$`3P%`)
holdout_2023$`3P%D` <- as.numeric(holdout_2023$`3P%D`)
holdout_2023$`Adj T.` <- as.numeric(holdout_2023$`Adj T.`)
holdout_2023$WAB <- as.numeric(holdout_2023$WAB)
holdout_2023$seed <- as.numeric(holdout_2023$seed)
holdout_2023$W <- as.numeric(holdout_2023$W)
holdout_2023$L <- as.numeric(holdout_2023$L)

holdout_2023$round<- NA
summary(currentmodel)
# Use the predict() function to generate predicted values based on the "model" and the variables in the "holdout_2023" data frame
predicted_values <- predict(currentmodel, newdata = holdout_2023)
training_data$predround<- predict(currentmodel, newdata = training_data)

holdout_2023$round <- predicted_values

if (requireNamespace("toRvik", quietly = TRUE)) {
  tryCatch({
    toRvik::bart_injuryimpact(year=2023, team='Tennessee', player='Zakai Zeigler')->wozakaitenn
  }, error = function(e) message("bart_injuryimpact (exploratory) skipped: ", e$message))
}
#> # A tibble: 2 × 5
#>   situation            adj_oe adj_de barthag    rk
#>   <chr>                 <dbl>  <dbl>   <dbl> <dbl>
#> 1 With Zion Williamson   120.   90.4   0.962     5
#> 2 Without                115.   94.7   0.905    17
#> 
#> 

###1seed model
best_general_mse1 <- Inf
model_seed_1 <- NULL
filter(training_data, seed == 1| seed == 2) -> training_seed_1
filter(holdout_2023, seed == 1| seed == 2)->training_seed_123
cor(training_seed_1[6:40])->seed1cors

# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 8, nrow = nrow(training_seed_1))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_1))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_1))
# Loop through each row of the data
for (i in 1:nrow(training_seed_1)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_1[-i, ]
  dataofone<- training_seed_1[i,]
  
  # Fit the model to the subset of data
  fit1 <- lm(round ~WAB+
              Barthag +AdjOE+`OPAST%`+`EXP.`
            + AdjDE+ seed+predround +0, 
            data = data_subset)
  #+`2P%` +  + I(seed^2) `3P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`3P%D`+W+FTR+FTRD L W adj t `EXP.`
  
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit1)
  predicted_round <- predict(fit1, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
  
}


# Calculate the bias and standard error of each coefficient
bias1 <- bias(predvactual[,2],predvactual[,1])
mse1<- rmse(predvactual[,2],predvactual[,1])^2

#mean out of sample error
summary(fit1)

# Print the results
cbind(bias1, mse1)

best_general_mse1-mse1
if ( mse1 < best_general_mse1){
  mse1 -> best_general_mse1
  fit1->model_seed_1
  print("keep")
}

rename(training_seed_123, 'predround' = 'round')->training_seed_123
training_seed_123$seedpred <-predict(model_seed_1, newdata = training_seed_123)
training_seed_1$seedpred <-predict(model_seed_1, newdata = training_seed_1)




###2seed model
best_mse_seed_2 <- Inf
model_seed_2 <- NULL
filter(training_data, seed == 1| seed == 2| seed == 3) -> training_seed_2
filter(holdout_2023, seed == 1| seed == 2|seed == 3)->holdout_seed_2
cor(training_seed_2[6:40])->seed2cors
View(seed2cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(training_seed_2))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_2))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_2))
# Loop through each row of the data
for (i in 1:nrow(training_seed_2)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_2[-i, ]
  dataofone<- training_seed_2[i,]
  
  # Fit the model to the subset of data
  fit2 <- lm(formula = round ~ WAB + Barthag + AdjOE + AdjDE + seed + predround + 
                0, data = data_subset)
  #+`2P%` +  + I(seed^2) `3P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`3P%D`+W+FTR+FTRD L W adj t 
  
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit2)
  predicted_round <- predict(fit2, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
  
}


# Calculate the bias and standard error of each coefficient
bias2 <- bias(predvactual[,2],predvactual[,1])
mse2<- rmse(predvactual[,2],predvactual[,1])^2

#mean out of sample error
summary(fit2)

# Print the results
cbind(bias2, mse2)

best_mse_seed_2-mse2
if ( mse2 <
     best_mse_seed_2){
  mse2 -> best_mse_seed_2
  fit2->model_seed_2
  print("keep")
}

rename(holdout_seed_2, 'predround' = 'round')->holdout_seed_2
holdout_seed_2$seedpred <-predict(model_seed_2, newdata = holdout_seed_2)
training_seed_2$seedpred <-predict(model_seed_2, newdata = training_seed_2)


library(Metrics)








###3seed model
best_general_mse3 <- Inf
seed3finalmodel <- NULL
filter(training_data, seed == 2| seed == 3|seed == 4)->training_seed_3
filter(holdout_2023, seed == 2| seed == 3|seed == 4)->holdout_seed_3
cor(training_seed_3[6:40])->seed3cors
View(seed3cors)

# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(training_seed_3))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_3))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_3))
# Loop through each row of the data
for (i in 1:nrow(training_seed_3)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_3[-i, ]
  dataofone<- training_seed_3[i,]
  # Fit the model to the subset of data
  fit3 <- lm(formula = round ~ WAB + Barthag + AdjOE + AdjDE + seed + predround + 
               0, data = data_subset)
  #+`2P%` +  + I(seed^2) `3P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`3P%D`+W+FTR+FTRD L W adj t 
  
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit3)
  predicted_round <- predict(fit3, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
  
}


# Calculate the bias and standard error of each coefficient
bias3 <- bias(predvactual[,2],predvactual[,1])
mse3<- rmse(predvactual[,2],predvactual[,1])^2

#mean out of sample error
summary(fit3)

# Print the results
cbind(bias3, mse3)


best_general_mse3-mse3
if ( mse3 <
     best_general_mse3){
  mse3 -> best_general_mse3
  fit3->seed3finalmodel
  print("keep")
}

rename(holdout_seed_3, 'predround' = 'round')->holdout_seed_3
holdout_seed_3$seedpred <-predict(seed3finalmodel, newdata = holdout_seed_3)
training_seed_3$seedpred <-predict(seed3finalmodel, newdata = training_seed_3)



###4seed model
best_mse_seed_4 <- Inf
seed4finalmodel <- NULL
filter(training_data, seed == 3| seed == 4|seed == 5)->training_seed_4
filter(holdout_2023, seed == 3| seed == 4|seed == 5)->training_seed_423
cor(training_seed_4[6:40])->seed4cors
View(seed4cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 10, nrow = nrow(training_seed_4))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_4))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_4))
# Loop through each row of the data
for (i in 1:nrow(training_seed_4)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_4[-i, ]
  dataofone<- training_seed_4[i,]
  # Fit the model to the subset of data
  fit4 <- lm(formula = round ~ WAB + I(WAB^3) + `AST%` + Barthag + AdjOE + 
               `2P%` + TORD + AdjDE + seed + predround + 0, data = data_subset)
  #+`2P%` +  + I(seed^2) `4P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`4P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit4)
  predicted_round <- predict(fit4, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias4 <- bias(predvactual[,2],predvactual[,1])
mse4<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit4)
# Print the results
cbind(bias4, mse4)
best_mse_seed_4-mse4
if ( mse4 <
     best_mse_seed_4){
  mse4 -> best_mse_seed_4
  fit4->seed4finalmodel
  print("keep")
}

rename(training_seed_423, 'predround' = 'round')->training_seed_423
training_seed_423$seedpred <-predict(seed4finalmodel, newdata = training_seed_423)
training_seed_4$seedpred <-predict(seed4finalmodel, newdata = training_seed_4)





###5seed model
best_general_mse5 <- Inf
model_seed_5 <- NULL
filter(training_data, seed == 4| seed == 5|seed == 6)->seed5analysis
filter(holdout_2023, seed == 6| seed == 4|seed == 5)->holdout_seed_5
cor(seed5analysis[6:29])->seed5cors
View(seed5cors)

# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(seed5analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed5analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed5analysis))
# Loop through each row of the data
for (i in 1:nrow(seed5analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed5analysis[-i, ]
  dataofone<- seed5analysis[i,]
  # Fit the model to the subset of data
  fit5 <- lm(formula = round ~ WAB + Barthag + AdjOE + AdjDE + seed + predround + 
               0, data = data_subset)
  #+`2P%` +  + I(seed^2) `5P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`5P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit5)
  predicted_round <- predict(fit5, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias5 <- bias(predvactual[,2],predvactual[,1])
mse5<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit5)
# Print the results
cbind(bias5, mse5)

best_general_mse5-mse5
if ( mse5 <
     best_general_mse5){
  mse5 -> best_general_mse5
  fit5->model_seed_5
  print("keep")
}


rename(holdout_seed_5, 'predround' = 'round')->holdout_seed_5
holdout_seed_5$seedpred <-predict(model_seed_5, newdata = holdout_seed_5)
seed5analysis$seedpred <-predict(model_seed_5, newdata = seed5analysis)









###6seed model
best_mse_seed_6 <- Inf
model_seed_6 <- NULL
filter(training_data, seed == 7| seed == 5| seed == 6) -> training_seed_6
filter(holdout_2023, seed == 6| seed == 7|seed == 5)->holdout_seed_6
cor(training_seed_6[6:29])->seed6cors
View(seed6cors)
  # Create an empty matrix to store the results
  jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(training_seed_6))
  modelerror <- matrix(NA, ncol = 1, nrow = nrow(training_seed_6))
  predvactual <- matrix(NA, ncol = 2, nrow = nrow(training_seed_6))
  
  # Loop through each row of the data
  for (j in 1:nrow(training_seed_6)) {
    # Subset the data, leaving out the jth observation
    data_subset <- training_seed_6[-j, ]
    dataofone <- training_seed_6[j, ]
    
    # Fit the model to the subset of data
    fit6 <- lm(formula = round ~ WAB + FTRD + AdjOE + AdjDE + seed + 
                 predround + 0, data = data_subset)
    
    # Store the coefficients in the jth row of the results matrix
    jackknife_results[j, ] <- coef(fit6)
    
    # Calculate the out-of-sample error for the jth observation
    predicted_round <- predict(fit6, newdata = dataofone)
    actual_round <- dataofone$round
    predicted_round - actual_round -> modelerror[j, ]
    predicted_round -> predvactual[j, 1]
    actual_round -> predvactual[j, 2]
  }
  library(Metrics)
  # Calculate the bias and mean squared error of the model
  bias6 <- bias(predvactual[, 2], predvactual[, 1])
  mse6 <- rmse(predvactual[, 2], predvactual[, 1])^2
  
  # Print the results
  print(paste0("Bias: ", bias6))
  print(paste0("Mean Squared Error: ", mse6))
  
  # Check if the current model has the lowest out-of-sample error so far
  if (mse6 < best_mse_seed_6) {
    mse6 -> best_mse_seed_6
    fit6 -> model_seed_6
    print("keep")
  } 



rename(holdout_seed_6, 'predround' = 'round')->holdout_seed_6
holdout_seed_6$seedpred <-predict(model_seed_6, newdata = holdout_seed_6)
training_seed_6$seedpred <-predict(model_seed_6, newdata = training_seed_6)






###7seed model
best_general_mse7 <- Inf
model_seed_7 <- NULL
filter(training_data, seed == 7| seed == 8|seed == 6)->training_seed_7
filter(holdout_2023, seed == 6| seed == 7|seed == 8)->holdout_seed_7
cor(training_seed_7[6:29])->seed7cors
View(seed7cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 8, nrow = nrow(training_seed_7))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_7))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_7))
# Loop through each row of the data
for (i in 1:nrow(training_seed_7)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_7[-i, ]
  dataofone<- training_seed_7[i,]
  # Fit the model to the subset of data
  fit7 <- lm(formula = round ~ WAB + Barthag + AdjOE + TOR + `Adj T.` + 
               `2P%` + seed + predround + 0, data = data_subset)
  #+`2P%` +  + I(seed^2) `7P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`7P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit7)
  predicted_round <- predict(fit7, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias7 <- bias(predvactual[,2],predvactual[,1])
mse7<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit7)
# Print the results
cbind(bias7, mse7)

best_general_mse7-mse7
if ( mse7 <
     best_general_mse7){
  mse7 -> best_general_mse7
  fit7->model_seed_7
  print("keep")
}

rename(holdout_seed_7, 'predround' = 'round')->holdout_seed_7
holdout_seed_7$seedpred <-predict(model_seed_7, newdata = holdout_seed_7)
training_seed_7$seedpred <-predict(model_seed_7, newdata = training_seed_7)



###8seed model
best_mse_seed_8 <- Inf
seed8finalmodel <- NULL
filter(training_data, seed == 7| seed == 8|seed == 9)->training_seed_8
filter(holdout_2023, seed == 9| seed == 7|seed == 8)->training_seed_823
cor(training_seed_8[6:40])->seed8cors
View(seed8cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 10, nrow = nrow(training_seed_8))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_8))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_8))
# Loop through each row of the data
for (i in 1:nrow(training_seed_8)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_8[-i, ]
  dataofone<- training_seed_8[i,]
  # Fit the model to the subset of data
  fit8 <- lm(round ~WAB +
               Barthag +AdjOE+AdjDE+TOR+`BLKED%`+`AST%`+`OPAST%`
             + seed+predround +0, 
             data = data_subset)
  #+`2P%` +  + I(seed^2) `8P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`8P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit8)
  predicted_round <- predict(fit8, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias8 <- bias(predvactual[,2],predvactual[,1])
mse8<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit8)
# Print the results
cbind(bias8, mse8)

best_mse_seed_8-mse8
if ( mse8 <
     best_mse_seed_8){
  mse8 -> best_mse_seed_8
  fit8->seed8finalmodel
  print("keep")
}

rename(training_seed_823, 'predround' = 'round')->training_seed_823
training_seed_823$seedpred <-predict(seed8finalmodel, newdata = training_seed_823)
training_seed_8$seedpred <-predict(seed8finalmodel, newdata = training_seed_8)












###9seed model
best_general_mse9 <- Inf
model_seed_9 <- NULL
filter(training_data, seed == 8| seed == 9|seed == 10)->training_seed_9
filter(holdout_2023, seed == 9| seed == 8|seed == 10)->holdout_seed_9
cor(training_seed_9[6:40])->seed9cors
View(seed9cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 12, nrow = nrow(training_seed_9))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_9))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_9))
# Loop through each row of the data
for (i in 1:nrow(training_seed_9)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_9[-i, ]
  dataofone<- training_seed_9[i,]
  # Fit the model to the subset of data
  fit9 <- lm(round ~WAB +
               Barthag +AdjOE+AdjDE+ seed+predround+TOR +W+`BLKED%`+`AST%`+`3PRATE`+`ELITESOS` +0, 
             data = data_subset)
  #+`2P%` +  + I(seed^2) `9P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`9P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit9)
  predicted_round <- predict(fit9, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias9 <- bias(predvactual[,2],predvactual[,1])
mse9<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit9)
# Print the results
cbind(bias9, mse9)

best_general_mse9-mse9
if ( mse9 <
     best_general_mse9){
  mse9 -> best_general_mse9
  fit9->model_seed_9
  print("keep")
}

rename(holdout_seed_9, 'predround' = 'round')->holdout_seed_9
holdout_seed_9$seedpred <-predict(model_seed_9, newdata = holdout_seed_9)
training_seed_9$seedpred <-predict(model_seed_9, newdata = training_seed_9)









###10seed model
best_general_mse10 <- Inf
model_seed_10 <- NULL
filter(training_data, seed == 11| seed == 9|seed == 10)->training_seed_10
filter(holdout_2023, seed == 9| seed == 11|seed == 10)->holdout_seed_10
cor(training_seed_10[6:40])->seed10cors
View(seed10cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 11, nrow = nrow(training_seed_10))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_10))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_10))
# Loop through each row of the data
for (i in 1:nrow(training_seed_10)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_10[-i, ]
  dataofone<- training_seed_10[i,]
  # Fit the model to the subset of data
  fit10 <- lm(round ~WAB +
               Barthag +AdjOE+AdjDE+ seed+predround+`2P%D`+ `WINS`+ `3PRATED`+ `TALENT`+ELITESOS+0, 
             data = data_subset)
  #+`2P%` +  + I(seed^2) `10P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`10P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit10)
  predicted_round <- predict(fit10, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias10 <- bias(predvactual[,2],predvactual[,1])
mse10<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit10)
# Print the results
cbind(bias10, mse10)

best_general_mse10-mse10
if ( mse10 <
     best_general_mse10){
  mse10 -> best_general_mse10
  fit10->model_seed_10
  print("keep")
}

rename(holdout_seed_10, 'predround' = 'round')->holdout_seed_10
holdout_seed_10$seedpred <-predict(model_seed_10, newdata = holdout_seed_10)
training_seed_10$seedpred <-predict(model_seed_10, newdata = training_seed_10)




###11seed model
best_mse_seed_11 <- Inf
model_seed_11 <- NULL
filter(training_data, seed == 11| seed == 10|seed == 12)->training_seed_11
filter(holdout_2023, seed == 12| seed == 11|seed == 10)->holdout_seed_11
cor(training_seed_11[6:40])->seed11cors
View(seed11cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 9, nrow = nrow(training_seed_11))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_11))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_11))
# Loop through each row of the data
for (i in 1:nrow(training_seed_11)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_11[-i, ]
  dataofone<- training_seed_11[i,]
  # Fit the model to the subset of data
  fit11 <- lm(round ~`FTR`  +`AST%`+`OPAST%` + `ELITESOS` +
                Barthag +AdjDE+AdjOE+ seed+predround+0, 
              data = data_subset)
  #+`2P%` +  + I(seed^2) `11P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`11P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit11)
  predicted_round <- predict(fit11, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias11 <- bias(predvactual[,2],predvactual[,1])
mse11<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit11)
# Print the results
cbind(bias11, mse11)

best_mse_seed_11-mse11
if ( mse11 <
     best_mse_seed_11){
  mse11 -> best_mse_seed_11
  fit11->model_seed_11
  print("keep")
}

rename(holdout_seed_11, 'predround' = 'round')->holdout_seed_11
holdout_seed_11$seedpred <-predict(model_seed_11, newdata = holdout_seed_11)
training_seed_11$seedpred <-predict(model_seed_11, newdata = training_seed_11)



###12seed model
best_mse_seed_12 <- Inf
seed12finalmodel <- NULL
filter(training_data, seed == 11| seed == 12|seed == 13)->training_seed_12
filter(holdout_2023, seed == 12| seed == 11|seed == 13)->holdout_seed_12
cor(training_seed_12[6:40])->seed12cors
View(seed12cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 11, nrow = nrow(training_seed_12))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_12))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_12))
# Loop through each row of the data
for (i in 1:nrow(training_seed_12)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_12[-i, ]
  dataofone<- training_seed_12[i,]
  # Fit the model to the subset of data
  fit12 <- lm(round ~`BLK%`+`AST%` +`OPAST%` +`EFFHGT`+`TALENT` +
                Barthag +AdjOE+AdjDE+ seed+predround+0, 
              data = data_subset)
  #+`2P%` +  + I(seed^2) `12P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`12P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit12)
  predicted_round <- predict(fit12, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias12 <- bias(predvactual[,2],predvactual[,1])
mse12<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit12)
# Print the results
cbind(bias12, mse12)

best_mse_seed_12-mse12
if ( mse12 <
     best_mse_seed_12){
  mse12 -> best_mse_seed_12
  fit12->seed12finalmodel
  print("keep")
}

rename(holdout_seed_12, 'predround' = 'round')->holdout_seed_12
holdout_seed_12$seedpred <-predict(seed12finalmodel, newdata = holdout_seed_12)
training_seed_12$seedpred <-predict(seed12finalmodel, newdata = training_seed_12)


##########13SEED MODEL
best_general_mse13 <- Inf
model_seed_13 <- NULL
filter(training_data, seed == 14| seed == 12|seed == 13)->training_seed_13
filter(holdout_2023, seed == 12| seed == 14|seed == 13)->training_seed_1323
cor(training_seed_13[6:40])->seed13cors
View(seed13cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 13, nrow = nrow(training_seed_13))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_13))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_13))
# Loop through each row of the data
for (i in 1:nrow(training_seed_13)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_13[-i, ]
  dataofone<- training_seed_13[i,]
  # Fit the model to the subset of data
  fit13 <- lm(round ~W+ FTR+  FTRD+ `TOR`  + `TORD`+  DRB+ `TALENT`+ELITESOS +
                Barthag +AdjOE+AdjDE+ seed+predround+0, 
              data = data_subset)
  #+`2P%` +  + I(seed^2) `13P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`13P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit13)
  predicted_round <- predict(fit13, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias13 <- bias(predvactual[,2],predvactual[,1])
mse13<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit13)
# Print the results
cbind(bias13, mse13)

best_general_mse13-mse13
if ( mse13 <
     best_general_mse13){
  mse13 -> best_general_mse13
  fit13->model_seed_13
  print("keep")
}

rename(training_seed_1323, 'predround' = 'round')->training_seed_1323
training_seed_1323$seedpred <-predict(model_seed_13, newdata = training_seed_1323)
training_seed_13$seedpred <-predict(model_seed_13, newdata = training_seed_13)




##########14SEED MODEL
best_mse_seed_14 <- Inf
model_seed_14 <- NULL
filter(training_data, seed == 14| seed == 15|seed == 13)->training_seed_14
filter(holdout_2023, seed == 15| seed == 14|seed == 13)->holdout_seed_14
cor(training_seed_14[6:40])->seed14cors
View(seed14cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 10, nrow = nrow(training_seed_14))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(training_seed_14))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(training_seed_14))
# Loop through each row of the data
for (i in 1:nrow(training_seed_14)) {
  # Subset the data, leaving out the ith observation
  data_subset <- training_seed_14[-i, ]
  dataofone<- training_seed_14[i,]
  # Fit the model to the subset of data
  fit14 <- lm(round ~`FTR` +TORD+ `DRB`+ `EXP.` + ELITESOS +
                Barthag +AdjOE+AdjDE+ seed+predround+0, 
              data = data_subset)
  #+`2P%` +  + I(seed^2) `14P%`+ +L+`EFGD%``EFG%`+
  #options denied adjoe^2  +  2p%d tord+`14P%D`+W+FTR+FTRD L W adj t 
  # Store the coefficients in the ith row of the results matrix
  jackknife_results[i, ] <- coef(fit14)
  predicted_round <- predict(fit14, newdata = dataofone)
  actual_round <- dataofone$round
  predicted_round-actual_round -> modelerror[i,]
  predicted_round ->predvactual[i,1]
  actual_round ->predvactual[i,2]
}
# Calculate the bias and standard error of each coefficient
bias14 <- bias(predvactual[,2],predvactual[,1])
mse14<- rmse(predvactual[,2],predvactual[,1])^2
#mean out of sample error
summary(fit14)
# Print the results
cbind(bias14, mse14)

best_mse_seed_14-mse14
if ( mse14 <
     best_mse_seed_14){
  mse14 -> best_mse_seed_14
  fit14->model_seed_14
  print("keep")
}

rename(holdout_seed_14, 'predround' = 'round')->holdout_seed_14
holdout_seed_14$seedpred <-predict(model_seed_14, newdata = holdout_seed_14)
training_seed_14$seedpred <-predict(model_seed_14, newdata = training_seed_14)












###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model
###1seed model


if (requireNamespace("toRvik", quietly = TRUE)) {
  tryCatch(toRvik::bart_injuryimpact(year=2023, team='UCLA', player='Jaylen Clark')->wojaylen, error = function(e) NULL)
}


# Load the required library
library(dplyr)

# Normalize holdout/training seed frame names for downstream combining
if (exists("training_seed_123") && !exists("holdout_seed_1")) holdout_seed_1 <- training_seed_123
if (exists("training_seed_423") && !exists("holdout_seed_4")) holdout_seed_4 <- training_seed_423
if (exists("training_seed_823") && !exists("holdout_seed_8")) holdout_seed_8 <- training_seed_823
if (exists("training_seed_1323") && !exists("holdout_seed_13")) holdout_seed_13 <- training_seed_1323
if (exists("seed5analysis") && !exists("training_seed_5")) training_seed_5 <- seed5analysis

# Combine holdout predictions by seed
holdout_predictions_combined <- data.frame()
for (i in 1:14) {
  df_name <- paste0("holdout_seed_", i)
  
  # Access the current data frame using the get() function
  if (!exists(df_name)) next
  current_df <- get(df_name)
  if (is.null(current_df) || nrow(current_df) == 0) next
  
  # Filter the current data frame to include only rows where the desired column equals i
  filtered_df <- current_df %>%
    filter(seed == i) # Replace 'desired_column' with the actual name of the column you want to filter on
  if (nrow(filtered_df) == 0) next
  
  holdout_predictions_combined <- rbind(holdout_predictions_combined, filtered_df)
}

# Combine historical predictions by seed
historical_predictions_combined <- data.frame()
for (i in 1:14) {
  df_name <- paste0("training_seed_", i)
  
  # Access the current data frame using the get() function
  if (!exists(df_name)) next
  current_df <- get(df_name)
  if (is.null(current_df) || nrow(current_df) == 0) next
  
  # Filter the current data frame to include only rows where the desired column equals i
  filtered_df <- current_df %>%
    filter(seed == i) # Replace 'desired_column' with the actual name of the column you want to filter on
  if (nrow(filtered_df) == 0) next
  
  # Combine the filtered data frame with the previous ones using rbind
  historical_predictions_combined <- rbind(historical_predictions_combined, filtered_df)
}

# The combined_df variable now contains the combined and filtered data frames

# Load the required library
library(dplyr)

# Calculate the average seedpred value for each seed value and store it in a temporary data frame
average_seedpred <- historical_predictions_combined %>%
  group_by(seed) %>%
  summarise(avg_seedpred = mean(seedpred))

# Join the average_seedpred data frame to the original data frame, combcbb
historical_predictions_combined_with_avg <- historical_predictions_combined %>%
  left_join(average_seedpred, by = "seed")

# Calculate the percentage difference and add a new column 'perc_diff'
historical_predictions_combined_with_perc_diff <- historical_predictions_combined_with_avg %>%
  mutate(perc_diff = ((seedpred - avg_seedpred)))

# The combcbb_with_perc_diff variable now contains the original data frame with an additional column named 'perc_diff'

filter(historical_predictions_combined, round == 6)->winsss
library(sqldf)

if (exists("holdout_actual_results")) {
  holdout_eval <- sqldf("SELECT pred.*, m.Team as team2, m.round
        FROM holdout_predictions_combined pred
        LEFT JOIN holdout_actual_results m ON pred.Team = m.Team")
  squared_diff <- matrix(data = NA, ncol = 1, nrow = nrow(holdout_eval))
  for (i in 1:nrow(holdout_eval)) {
    squared_diff[i] <- sqrt((holdout_eval[i, "seedpred"] - round(holdout_eval[i, "round"]))^2)
  }
  root_mean_squared_diff <- mean(squared_diff, na.rm = TRUE)
  message("2023 holdout RMSE: ", round(root_mean_squared_diff, 4))
} else {
  message("holdout_actual_results not found; skipping 2023 holdout RMSE calculation.")
}

}  # end if (exists("holdout_2023") && nrow(holdout_2023) > 0)

if (exists("collegebasketballprojectdata")) {
  df <- collegebasketballprojectdata
  df$L <- ifelse(df$L == 0, 1, 0)
  write.csv(df, file.path(dirname(output_path), "collegebasketballprojectdata.csv"), row.names = FALSE)
}

training_data$sweetsixteen <- ifelse(training_data$round >= 2, 1, 0)
