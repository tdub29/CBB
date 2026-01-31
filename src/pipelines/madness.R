#create historic data
library(rvest)
library(httr)
library(dplyr)

mastertable <- data.frame()

#scrape regular season data from barttorvik
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
  mastertable <- rbind(mastertable,ncaa_scrape)
}
 #create postseason and tourney data to create boolean variable for conf win
tableforconf <- data.frame()
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
  tableforconf <- rbind(tableforconf,ncaa_scrapeconf)
}

#create tournament df in order to isolate conf tourney stats later
tablefortourn <- data.frame()
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
  tablefortourn <- rbind(tablefortourn,ncaa_scrapetourn)
}


#FEATURE ENGINEERING

# Create new column "W" which is wins in tournaments
tablefortourn$W <- substr(tablefortourn$Rec, 1, ifelse(substr(tablefortourn$Rec, 2, 2) %in% c("0", "1"), 2, 1))

# Create new column "L" which is losses in tournaments aka 0 means they won
tablefortourn$L <- ifelse(substr(tablefortourn$Rec, 2, 2) %in% c("0", "1"), substr(tablefortourn$Rec, 4, 4), substr(tablefortourn$Rec, 3, 3))

# Create new column "W" which is wins in conference tourney
tableforconf$W <- substr(tableforconf$Rec, 1, ifelse(substr(tableforconf$Rec, 2, 2) %in% c("0", "1","2","3"), 2, 1))


# Create new column "L" which is losses in conf tournaments aka 0 means they won
tableforconf$L <- ifelse(substr(tableforconf$Rec, 2, 2) %in% c("0", "1", "2","3"), substr(tableforconf$Rec, 4, 4), substr(tableforconf$Rec, 3, 3))


#join tournament and playoffs data
library(sqldf)
joinedconf <- sqldf("SELECT t.*, tf.W as tournw, tf.L as tournl
                       FROM tableforconf t
                       LEFT JOIN tablefortourn tf 
                       ON t.Team = tf.Team AND t.year = tf.year")
as.numeric(joinedconf$tournw)->joinedconf$tournw
as.numeric(joinedconf$tournl)->joinedconf$tournl
as.numeric(joinedconf$L)->joinedconf$L
as.numeric(joinedconf$W)->joinedconf$W

#create variables for conference tournemnt w's and l's 
mutate(joinedconf, conftournw =W - tournw, conftournl = L- tournl )->joinedconf

#join conference win loss data to master table
finalmastertable <- sqldf("SELECT mt.*, jc.conftournw as W, jc.conftournl as L 
                      FROM mastertable mt
                      LEFT JOIN joinedconf jc 
                      ON mt.Team = jc.Team AND mt.year = jc.year")


finalmastertable23 <- sqldf("SELECT mt.*, jc.W, jc.L 
                      FROM mastertable mt
                      LEFT JOIN joinedconf jc 
                      ON mt.Team = jc.Team AND mt.year = jc.year")


library(dplyr)
mastertable_final <- arrange(finalmastertable, desc(AdjOE))
# mastertable_final <- slice(mastertable_sorted, 2:nrow(mastertable_sorted))


# create a new column called "seed"
mastertable_final$seed <- NA
# Loop through each row of "mastfin$Team"
for (i in seq_along(mastertable_final$Team)) {
  # Check if "seed" is present in the value of the "Team" column
  if (grepl("seed", mastertable_final$Team[i])) {
    # If "seed" is present, extract all numbers before "seed" using a regular expression
    seed_value <- gsub("\\D", "", sub("seed.*", "", mastertable_final$Team[i]))
    # Store the extracted value in the "seed" column for that row
    mastertable_final$seed[i] <- seed_value
  }
}




# create a new column called "result" TO CATEGORIZE ROUNDS RESULT
mastertable_final$result <- NA

# loop over each row of the mastertable_final data frame
for (i in seq_len(nrow(mastertable_final))) {
  # extract the team name from the "Team" column
  team_name <- as.character(mastertable_final$Team[i])
  
  # check if the team name contains a certain result string
  if (grepl("R32", team_name)) {
    mastertable_final$result[i] <- 32
  } else if (grepl("R64", team_name)) {
    mastertable_final$result[i] <- 64
  } else if (grepl("Finals", team_name)) {
    mastertable_final$result[i] <- 2
  } else if (grepl("CHAMPS", team_name)) {
    mastertable_final$result[i] <- 1
  } else if (grepl("Elite Eight", team_name)) {
    mastertable_final$result[i] <- 8
  } else if (grepl("Sweet Sixteen", team_name)) {
    mastertable_final$result[i] <- 16
  } else if (grepl("Final Four", team_name)) {
    mastertable_final$result[i] <- 4
  }
}

mastertable_final -> mastfin

#clean columns, the teams rank is stuck at the end of the number skewing results marginally

mastfin$WAB <- sapply(as.character(mastfin$WAB), function(value) {
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
  for (i in 1:nrow(mastfin)) {
    value <- as.character(mastfin[i, col])
    if (grepl("\\.", value)) {
      mastfin[i, col] <- substr(value, 1, 4)
    } else {
      mastfin[i, col] <- substr(value, 1, 2)
    }
  }
}
for(i in 1:nrow(mastfin)){
  mastfin[i, 8] <- substr(mastfin[i, 8], 1, 5)}

for (col in 6:7) {
  for (i in 1:nrow(mastfin)) {
    value <- as.character(mastfin[i, col])
    if (startsWith(value, "1")) {
      mastfin[i, col] <- substr(value, 1, 5)
    } else {
      mastfin[i, col] <- substr(value, 1, 4)
    }
  }
}

as.numeric(mastfin$AdjOE)->mastfin$AdjOE
as.numeric(mastfin$AdjDE)->mastfin$AdjDE
as.numeric(mastfin$Barthag)->mastfin$Barthag
mastfin$`EFG%` <- as.numeric(mastfin$`EFG%`)
mastfin$`EFGD%` <- as.numeric(mastfin$`EFGD%`)
mastfin$TOR <- as.numeric(mastfin$TOR)
mastfin$TORD <- as.numeric(mastfin$TORD)
mastfin$ORB <- as.numeric(mastfin$ORB)
mastfin$DRB <- as.numeric(mastfin$DRB)
mastfin$FTR <- as.numeric(mastfin$FTR)
mastfin$FTRD <- as.numeric(mastfin$FTRD)
mastfin$`2P%` <- as.numeric(mastfin$`2P%`)
mastfin$`2P%D` <- as.numeric(mastfin$`2P%D`)
mastfin$`3P%` <- as.numeric(mastfin$`3P%`)
mastfin$`3P%D` <- as.numeric(mastfin$`3P%D`)
mastfin$`Adj T.` <- as.numeric(mastfin$`Adj T.`)
mastfin$WAB <- as.numeric(mastfin$WAB)
mastfin$seed <- as.numeric(mastfin$seed)

mastfin$round <- ifelse(mastfin$result == '64', 0,
                        ifelse(mastfin$result == '32', 1,
                               ifelse(mastfin$result == '16', 2,
                                      ifelse(mastfin$result == '8', 3,
                                             ifelse(mastfin$result == '4', 4,
                                                    ifelse(mastfin$result == '2', 5,
                                                           ifelse(mastfin$result == '1', 6, NA)))))))
# loop through mastfin$W and mastfin$L
for (i in 1:length(mastfin$W)) {
  if (is.na(mastfin$W[i])) {
    mastfin$W[i] <- 1
  }
  if (is.na(mastfin$L[i])) {
    mastfin$L[i] <- 1
  }
}

library(dplyr)
library(stringr)

mastfin <- mastfin %>%
  mutate(Team = ifelse(str_detect(Team, "\\b\\d+\\s+seed"), 
                       str_trim(str_replace(Team, "\\s+\\d+\\s+seed.*$", "")),
                       Team))

#add additional data TO INCREASE # OF FEATURES TO CAPTURE THINGS 
#LIKE TEAM SIZE, PLAYER HEIGHT, AGE AND OTHER THINGS

library(httr)

htmltable<-read_html(httr::GET('https://barttorvik.com/team-tables_each.php?year=2022&top=0&conlimit=All&venue=All&type=All&yax=3', user_agent("Mozilla/5.0")))


tbl_scrape <- htmltable %>% html_node("table") %>% html_table()


# Create an empty data frame to hold the concatenated data
combined_data <- data.frame()

# Loop through values 10:24 except 12   29 - 44
for (i in 52:52) {
  if (i != 12) {
    # Read in the CSV file
    file_name <- paste0("C:/Users/TrevorWhite/Downloads/trank_team_table_data (", i, ").csv")
    data <- read.csv(file_name, header = FALSE)
    
    
    # Add the data to the combined data frame
    combined_data <- rbind(combined_data, data)
  }
}

# Set the column names of combined_data
colnames(combined_data) <- c("TEAM", "ADJOE", "ADJDE", "BARTHAG", "RECORD", "WINS", "GAMES", "EFG", "EFGD.", "FTRATE", "FTRATED", "TOV%", "TOV%D", "OREB%", "OPOREB%", "RAWT", "2P%", "2P%D", "3P%", "3P%D", "BLK%", "BLKED%", "AST%", "OPAST%", "3PRATE", "3PRATED", "ADJ.T", "AVGHGT", "EFFHGT", "EXP.", "YEAR", "PAKE", "PASE", "TALENT",'blank', "FT%", "OPFT%", "PPPOFF", "PPPDEF", "ELITESOS")

combined_data$Rk <- ave(-combined_data$BARTHAG, combined_data$YEAR, FUN = rank)
#read_excel("C:\\Users\\TrevorWhite\\Downloads\\cleanedtm.xlsx")->mastfin
NEWMASTFIN <- sqldf("
  SELECT 
    m.*, 
    c.TEAM, c.WINS, c.`BLK%`, c.`BLKED%`, c.`AST%`, c.`OPAST%`, 
    c.`3PRATE`, c.`3PRATED`, c.EFFHGT, c.`EXP.`, c.TALENT, c.ELITESOS
  FROM 
    mastfin AS m
    LEFT JOIN combined_data AS c ON m.Year = c.Year AND m.Team = c.TEAM
")

library(sqldf)

mastfin <- NEWMASTFIN #assign new df to master df



# Define the list of Power 5 conference abbreviations
power6_confs <- c("B12", "B10", "SEC", "ACC", "P12","BE")

# Create the Power5 variable
mastfin <- mastfin %>%
  mutate(Power6 = ifelse(Conf %in% power6_confs, 1, 0))

# Create BOOLEAN FEATURES FOR P5 CONFERENCES
mastfin <- mastfin %>%
  mutate(B12 = ifelse(Conf == "B12", 1, 0),
         B10 = ifelse(Conf == "B10", 1, 0),
         SEC = ifelse(Conf == "SEC", 1, 0),
         ACC = ifelse(Conf == "ACC", 1, 0),
         P12 = ifelse(Conf == "P12", 1, 0),
         BE = ifelse(Conf == "BE", 1, 0),
         MWC = ifelse(Conf == "MWC", 1, 0))

mastfin$Rec <- substr(mastfin$Rec, 1, 2)

mastfinwteam <- mastfin

mastfinwteam <- mastfinwteam %>%
  select(-TEAM)

mastfinallcopy<-mastfin
mastfin <- mastfin %>%
  select(-WINS, -1:-3, -result, -TEAM)

##select(mastfin, 1:36) ->mastfin
#CREATE BOOLEAN FEATURES FOR ROUNDS
mastfin <- mastfin %>%
  mutate(Round2 = ifelse(round >= 1, 1, 0),
         S16 = ifelse(round >= 2, 1, 0),
         E8 = ifelse(round >= 3, 1, 0),
         F4 = ifelse(round >= 4, 1, 0),
         Ship = ifelse(round >= 5, 1, 0),
         Champ = ifelse(round == 6, 1, 0))

madness24 <- mastfin %>%
  filter(!is.na(seed))

mastfin <- mastfin %>%
  mutate(across(45:50, ~ifelse(is.na(.), 0, .)))

# Fill NA values in 'round' column with 0
mastfin <- mastfin %>%
  mutate(round = ifelse(is.na(round), 0, round))

mastfin ->mastfinanalysiscopy
# Now remove rows with NA values in any column
mastfin <- mastfin %>%
  na.omit()

mastfin <- mastfin %>%
  mutate(G = as.numeric(G),
         `3PR` = as.numeric(`3PR`),
         `3PRD` = as.numeric(`3PRD`),
         Rec = as.numeric(Rec))




#NEWMASTFIN->mastfin
library(readxl)
write.csv(NEWMASTFIN, "cbbtrainingdata31224.csv")
read_excel('C:/Users/TrevorWhite/Downloads/cleanteam.xlsx')->cbb2023
read.csv("C:/Users/TrevorWhite/Downloads/teamdata23.csv", header = FALSE)->combined23
colnames(combined23)<-colnames(combined_data[1:40])
View(combined23)
NEWCBB23 <- sqldf("
  SELECT 
    m.*, 
    c.TEAM, c.WINS, c.`BLK%`, c.`BLKED%`, c.`AST%`, c.`OPAST%`, 
    c.`3PRATE`, c.`3PRATED`, c.EFFHGT, c.`EXP.`, c.TALENT, c.ELITESOS
  FROM 
    cbb2023 AS m
    LEFT JOIN combined23 AS c ON m.Team = c.TEAM
")
NEWCBB23->cbb2023






# =========================================================
# ==============building general model================================
# ==========================================
#JUST LINEAR REGRESSION


cor(mastfin[6:40])->ncaacor

# Fit the model with intercept set to 1
model <- lm(round ~`3P%D`+`3P%`+ `EFG%`+ FTRD+WAB+ I(WAB^3)+`EFGD%`+`2P%D`+`2P%`+ ORB + TOR+ Barthag + AdjOE +I(AdjOE^2) + AdjDE+ I(AdjDE^2) + seed + I(seed^2)+L +0, data = mastfin)
#

# Print the coefficients
coef(model)
summary(model)
model$fitted.values->mastfin$fitted


cor(mastfin[4:40])->ncaacor
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 15, nrow = nrow(mastfin))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(mastfin))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(mastfin))
# Loop through each row of the data
for (i in 1:nrow(mastfin)) {
  # Subset the data, leaving out the ith observation
  data_subset <- mastfin[-i, ]
  dataofone<- mastfin[i,]
  
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

currentbestoutofsampleerr-mse
  if ( mse < currentbestoutofsampleerr){
   mse -> currentbestoutofsampleerr
    fit->currentmodel
    print("keep")
  }


#graphinv#########
################
############3
###############
library(ggplot2)
ggplot(mastfin, aes(x = fitted, y = round)) +
  geom_point() +
  geom_smooth(method = "lm")

winners <- subset(mastfin, round == 6)
library(ggplot2)

# Create a data frame with the actual and fitted values
data <- data.frame(actual = mastfin$round, fitted = predict(model))

# Plot the actual vs. fitted values
ggplot(data, aes(x = fitted, y = actual)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") 




###############################################################
######FILTERED LINEAR MODEL(differentiate between seeding)
#######################################################


#graphinv
library(ggplot2)
ggplot(mastfin, aes(x = fitted, y = round)) +
  geom_point() +
  geom_smooth(method = "lm")

mastfin_filtered <- subset(mastfin, round == 6)
library(ggplot2)

# Create a data frame with the actual and fitted values
data <- data.frame(actual = mastfin$round, fitted = predict(model))

# Plot the actual vs. fitted values
ggplot(data, aes(x = fitted, y = actual)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") 


filter(mastertable_final, !is.na(seed)) ->cbb2023

for (i in 1:nrow(cbb2023)) {
  value <- as.character(cbb2023[i, "WAB"])
  if (value != '131'& value != '111'& value != '104'){
    if (abs(as.numeric(value)) < 10){
      if (startsWith(value, "-")) {
        if (grepl("\\.", value)) {
          cbb2023[i, "WAB"] <- substr(value, 1, 4)
        } else {
          cbb2023[i, "WAB"] <- substr(value, 1, 2)
        }
      } else {
        if (grepl("\\.", value)) {
          cbb2023[i, "WAB"] <- substr(value, 1, 3)
        } else {
          cbb2023[i, "WAB"] <- substr(value, 1, 1)
        }
      }
    } else {
      if (startsWith(value, "-")) {
        if (grepl("\\.", value)) {
          cbb2023[i, "WAB"] <- substr(value, 1, 5)
        } else {
          cbb2023[i, "WAB"] <- substr(value, 1, 3)
        }
      } else {
        if (grepl("\\.", value)) {
          cbb2023[i, "WAB"] <- substr(value, 1, 4)
        } else {
          cbb2023[i, "WAB"] <- substr(value, 1, 2)
          
        }}}}}
for (i in 1:nrow(cbb2023)) {
  value <- as.character(cbb2023[i, "WAB"])
  if (!grepl("\\.", value)) {
    if (startsWith(value, "-")|value == '131'| value == '111'| value == '104') {
      cbb2023[i, "WAB"] <- substr(value, 1, 2)
    } else {
      cbb2023[i, "WAB"] <- substr(value, 1, 1)
    }
  }}






for (col in 9:21) {
  for (i in 1:nrow(cbb2023)) {
    value <- as.character(cbb2023[i, col])
    if (grepl("\\.", value)) {
      cbb2023[i, col] <- substr(value, 1, 4)
    } else {
      cbb2023[i, col] <- substr(value, 1, 2)
    }
  }
}
for(i in 1:nrow(cbb2023)){
  cbb2023[i, 8] <- substr(cbb2023[i, 8], 1, 5)}

for (col in 6:7) {
  for (i in 1:nrow(cbb2023)) {
    value <- as.character(cbb2023[i, col])
    if (startsWith(value, "1")) {
      cbb2023[i, col] <- substr(value, 1, 5)
    } else {
      cbb2023[i, col] <- substr(value, 1, 4)
    }
  }
}

as.numeric(cbb2023$AdjOE)->cbb2023$AdjOE
as.numeric(cbb2023$AdjDE)->cbb2023$AdjDE
as.numeric(cbb2023$Barthag)->cbb2023$Barthag
cbb2023$`EFG%` <- as.numeric(cbb2023$`EFG%`)
cbb2023$`EFGD%` <- as.numeric(cbb2023$`EFGD%`)
cbb2023$TOR <- as.numeric(cbb2023$TOR)
cbb2023$TORD <- as.numeric(cbb2023$TORD)
cbb2023$ORB <- as.numeric(cbb2023$ORB)
cbb2023$DRB <- as.numeric(cbb2023$DRB)
cbb2023$FTR <- as.numeric(cbb2023$FTR)
cbb2023$FTRD <- as.numeric(cbb2023$FTRD)
cbb2023$`2P%` <- as.numeric(cbb2023$`2P%`)
cbb2023$`2P%D` <- as.numeric(cbb2023$`2P%D`)
cbb2023$`3P%` <- as.numeric(cbb2023$`3P%`)
cbb2023$`3P%D` <- as.numeric(cbb2023$`3P%D`)
cbb2023$`Adj T.` <- as.numeric(cbb2023$`Adj T.`)
cbb2023$WAB <- as.numeric(cbb2023$WAB)
cbb2023$seed <- as.numeric(cbb2023$seed)
cbb2023$W <- as.numeric(cbb2023$W)
cbb2023$L <- as.numeric(cbb2023$L)

cbb2023$round<- NA
summary(currentmodel)
# Use the predict() function to generate predicted values based on the "model" and the variables in the "cbb2023" data frame
predicted_values <- predict(currentmodel, newdata = cbb2023)
mastfin$predround<- predict(currentmodel, newdata = mastfin)

cbb2023$round <- predicted_values


bart_injuryimpact(year=2023, team='Tennessee', player='Zakai Zeigler')->wozakaitenn
#> # A tibble: 2 × 5
#>   situation            adj_oe adj_de barthag    rk
#>   <chr>                 <dbl>  <dbl>   <dbl> <dbl>
#> 1 With Zion Williamson   120.   90.4   0.962     5
#> 2 Without                115.   94.7   0.905    17
#> 
#> 

###1seed model

filter(mastfin, seed == 1| seed == 2) -> seed1analysis
filter(cbb2023, seed == 1| seed == 2)->seed1analysis23
cor(seed1analysis[6:40])->seed1cors

# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 8, nrow = nrow(seed1analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed1analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed1analysis))
# Loop through each row of the data
for (i in 1:nrow(seed1analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed1analysis[-i, ]
  dataofone<- seed1analysis[i,]
  
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

currentbestoutofsampleerr1-mse1
if ( mse1 < currentbestoutofsampleerr1){
  mse1 -> currentbestoutofsampleerr1
  fit1->seed1finalmodel
  print("keep")
}

rename(seed1analysis23, 'predround' = 'round')->seed1analysis23
seed1analysis23$seedpred <-predict(seed1finalmodel, newdata = seed1analysis23)
seed1analysis$seedpred <-predict(seed1finalmodel, newdata = seed1analysis)




###2seed model

filter(mastfin, seed == 1| seed == 2|seed == 3)->seed2analysis
filter(cbb2023, seed == 1| seed == 2|seed == 3)->seed2analysis23
cor(seed2analysis[6:40])->seed2cors
View(seed2cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(seed2analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed2analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed2analysis))
# Loop through each row of the data
for (i in 1:nrow(seed2analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed2analysis[-i, ]
  dataofone<- seed2analysis[i,]
  
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

currentbestoutofsampleerr2-mse2
if ( mse2 <
     currentbestoutofsampleerr2){
  mse2 -> currentbestoutofsampleerr2
  fit2->seed2finalmodel
  print("keep")
}

rename(seed2analysis23, 'predround' = 'round')->seed2analysis23
seed2analysis23$seedpred <-predict(seed2finalmodel, newdata = seed2analysis23)
seed2analysis$seedpred <-predict(seed2finalmodel, newdata = seed2analysis)


library(Metrics)








###3seed model
filter(mastfin, seed == 2| seed == 3|seed == 4)->seed3analysis
filter(cbb2023, seed == 2| seed == 3|seed == 4)->seed3analysis23
cor(seed3analysis[6:40])->seed3cors
View(seed3cors)

# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(seed3analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed3analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed3analysis))
# Loop through each row of the data
for (i in 1:nrow(seed3analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed3analysis[-i, ]
  dataofone<- seed3analysis[i,]
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


currentbestoutofsampleerr3-mse3
if ( mse3 <
     currentbestoutofsampleerr3){
  mse3 -> currentbestoutofsampleerr3
  fit3->seed3finalmodel
  print("keep")
}

rename(seed3analysis23, 'predround' = 'round')->seed3analysis23
seed3analysis23$seedpred <-predict(seed3finalmodel, newdata = seed3analysis23)
seed3analysis$seedpred <-predict(seed3finalmodel, newdata = seed3analysis)



###4seed model

filter(mastfin, seed == 3| seed == 4|seed == 5)->seed4analysis
filter(cbb2023, seed == 3| seed == 4|seed == 5)->seed4analysis23
cor(seed4analysis[6:40])->seed4cors
View(seed4cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 10, nrow = nrow(seed4analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed4analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed4analysis))
# Loop through each row of the data
for (i in 1:nrow(seed4analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed4analysis[-i, ]
  dataofone<- seed4analysis[i,]
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
currentbestoutofsampleerr4-mse4
if ( mse4 <
     currentbestoutofsampleerr4){
  mse4 -> currentbestoutofsampleerr4
  fit4->seed4finalmodel
  print("keep")
}

rename(seed4analysis23, 'predround' = 'round')->seed4analysis23
seed4analysis23$seedpred <-predict(seed4finalmodel, newdata = seed4analysis23)
seed4analysis$seedpred <-predict(seed4finalmodel, newdata = seed4analysis)





###5seed model


filter(mastfin, seed == 4| seed == 5|seed == 6)->seed5analysis
filter(cbb2023, seed == 6| seed == 4|seed == 5)->seed5analysis23
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

currentbestoutofsampleerr5-mse5
if ( mse5 <
     currentbestoutofsampleerr5){
  mse5 -> currentbestoutofsampleerr5
  fit5->seed5finalmodel
  print("keep")
}


rename(seed5analysis23, 'predround' = 'round')->seed5analysis23
seed5analysis23$seedpred <-predict(seed5finalmodel, newdata = seed5analysis23)
seed5analysis$seedpred <-predict(seed5finalmodel, newdata = seed5analysis)









###6seed model


filter(mastfin, seed == 7| seed == 5|seed == 6)->seed6analysis
filter(cbb2023, seed == 6| seed == 7|seed == 5)->seed6analysis23
cor(seed6analysis[6:29])->seed6cors
View(seed6cors)
  # Create an empty matrix to store the results
  jackknife_results <- matrix(NA, ncol = 6, nrow = nrow(seed6analysis))
  modelerror <- matrix(NA, ncol = 1, nrow = nrow(seed6analysis))
  predvactual <- matrix(NA, ncol = 2, nrow = nrow(seed6analysis))
  
  # Loop through each row of the data
  for (j in 1:nrow(seed6analysis)) {
    # Subset the data, leaving out the jth observation
    data_subset <- seed6analysis[-j, ]
    dataofone <- seed6analysis[j, ]
    
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
  if (mse6 < currentbestoutofsampleerr6) {
    mse6 -> currentbestoutofsampleerr6
    fit6 -> seed6finalmodel
    print("keep")
  } 



rename(seed6analysis23, 'predround' = 'round')->seed6analysis23
seed6analysis23$seedpred <-predict(seed6finalmodel, newdata = seed6analysis23)
seed6analysis$seedpred <-predict(seed6finalmodel, newdata = seed6analysis)






###7seed model

filter(mastfin, seed == 7| seed == 8|seed == 6)->seed7analysis
filter(cbb2023, seed == 6| seed == 7|seed == 8)->seed7analysis23
cor(seed7analysis[6:29])->seed7cors
View(seed7cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 8, nrow = nrow(seed7analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed7analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed7analysis))
# Loop through each row of the data
for (i in 1:nrow(seed7analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed7analysis[-i, ]
  dataofone<- seed7analysis[i,]
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

currentbestoutofsampleerr7-mse7
if ( mse7 <
     currentbestoutofsampleerr7){
  mse7 -> currentbestoutofsampleerr7
  fit7->seed7finalmodel
  print("keep")
}

rename(seed7analysis23, 'predround' = 'round')->seed7analysis23
seed7analysis23$seedpred <-predict(seed7finalmodel, newdata = seed7analysis23)
seed7analysis$seedpred <-predict(seed7finalmodel, newdata = seed7analysis)



###8seed model

filter(mastfin, seed == 7| seed == 8|seed == 9)->seed8analysis
filter(cbb2023, seed == 9| seed == 7|seed == 8)->seed8analysis23
cor(seed8analysis[6:40])->seed8cors
View(seed8cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 10, nrow = nrow(seed8analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed8analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed8analysis))
# Loop through each row of the data
for (i in 1:nrow(seed8analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed8analysis[-i, ]
  dataofone<- seed8analysis[i,]
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

currentbestoutofsampleerr8-mse8
if ( mse8 <
     currentbestoutofsampleerr8){
  mse8 -> currentbestoutofsampleerr8
  fit8->seed8finalmodel
  print("keep")
}

rename(seed8analysis23, 'predround' = 'round')->seed8analysis23
seed8analysis23$seedpred <-predict(seed8finalmodel, newdata = seed8analysis23)
seed8analysis$seedpred <-predict(seed8finalmodel, newdata = seed8analysis)












###9seed model


filter(mastfin, seed == 8| seed == 9|seed == 10)->seed9analysis
filter(cbb2023, seed == 9| seed == 8|seed == 10)->seed9analysis23
cor(seed9analysis[6:40])->seed9cors
View(seed9cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 12, nrow = nrow(seed9analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed9analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed9analysis))
# Loop through each row of the data
for (i in 1:nrow(seed9analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed9analysis[-i, ]
  dataofone<- seed9analysis[i,]
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

currentbestoutofsampleerr9-mse9
if ( mse9 <
     currentbestoutofsampleerr9){
  mse9 -> currentbestoutofsampleerr9
  fit9->seed9finalmodel
  print("keep")
}

rename(seed9analysis23, 'predround' = 'round')->seed9analysis23
seed9analysis23$seedpred <-predict(seed9finalmodel, newdata = seed9analysis23)
seed9analysis$seedpred <-predict(seed9finalmodel, newdata = seed9analysis)









###10seed model

filter(mastfin, seed == 11| seed == 9|seed == 10)->seed10analysis
filter(cbb2023, seed == 9| seed == 11|seed == 10)->seed10analysis23
cor(seed10analysis[6:40])->seed10cors
View(seed10cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 11, nrow = nrow(seed10analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed10analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed10analysis))
# Loop through each row of the data
for (i in 1:nrow(seed10analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed10analysis[-i, ]
  dataofone<- seed10analysis[i,]
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

currentbestoutofsampleerr10-mse10
if ( mse10 <
     currentbestoutofsampleerr10){
  mse10 -> currentbestoutofsampleerr10
  fit10->seed10finalmodel
  print("keep")
}

rename(seed10analysis23, 'predround' = 'round')->seed10analysis23
seed10analysis23$seedpred <-predict(seed10finalmodel, newdata = seed10analysis23)
seed10analysis$seedpred <-predict(seed10finalmodel, newdata = seed10analysis)




###11seed model

filter(mastfin, seed == 11| seed == 10|seed == 12)->seed11analysis
filter(cbb2023, seed == 12| seed == 11|seed == 10)->seed11analysis23
cor(seed11analysis[6:40])->seed11cors
View(seed11cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 9, nrow = nrow(seed11analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed11analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed11analysis))
# Loop through each row of the data
for (i in 1:nrow(seed11analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed11analysis[-i, ]
  dataofone<- seed11analysis[i,]
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

currentbestoutofsampleerr11-mse11
if ( mse11 <
     currentbestoutofsampleerr11){
  mse11 -> currentbestoutofsampleerr11
  fit11->seed11finalmodel
  print("keep")
}

rename(seed11analysis23, 'predround' = 'round')->seed11analysis23
seed11analysis23$seedpred <-predict(seed11finalmodel, newdata = seed11analysis23)
seed11analysis$seedpred <-predict(seed11finalmodel, newdata = seed11analysis)



###11seed model



filter(mastfin, seed == 11| seed == 12|seed == 13)->seed12analysis
filter(cbb2023, seed == 12| seed == 11|seed == 13)->seed12analysis23
cor(seed12analysis[6:40])->seed12cors
View(seed12cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 11, nrow = nrow(seed12analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed12analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed12analysis))
# Loop through each row of the data
for (i in 1:nrow(seed12analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed12analysis[-i, ]
  dataofone<- seed12analysis[i,]
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

currentbestoutofsampleerr12-mse12
if ( mse12 <
     currentbestoutofsampleerr12){
  mse12 -> currentbestoutofsampleerr12
  fit12->seed12finalmodel
  print("keep")
}

rename(seed12analysis23, 'predround' = 'round')->seed12analysis23
seed12analysis23$seedpred <-predict(seed12finalmodel, newdata = seed12analysis23)
seed12analysis$seedpred <-predict(seed12finalmodel, newdata = seed12analysis)


##########13SEED MODEL

filter(mastfin, seed == 14| seed == 12|seed == 13)->seed13analysis
filter(cbb2023, seed == 12| seed == 14|seed == 13)->seed13analysis23
cor(seed13analysis[6:40])->seed13cors
View(seed13cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 13, nrow = nrow(seed13analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed13analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed13analysis))
# Loop through each row of the data
for (i in 1:nrow(seed13analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed13analysis[-i, ]
  dataofone<- seed13analysis[i,]
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

currentbestoutofsampleerr13-mse13
if ( mse13 <
     currentbestoutofsampleerr13){
  mse13 -> currentbestoutofsampleerr13
  fit13->seed13finalmodel
  print("keep")
}

rename(seed13analysis23, 'predround' = 'round')->seed13analysis23
seed13analysis23$seedpred <-predict(seed13finalmodel, newdata = seed13analysis23)
seed13analysis$seedpred <-predict(seed13finalmodel, newdata = seed13analysis)




##########14SEED MODEL

filter(mastfin, seed == 14| seed == 15|seed == 13)->seed14analysis
filter(cbb2023, seed == 15| seed == 14|seed == 13)->seed14analysis23
cor(seed14analysis[6:40])->seed14cors
View(seed14cors)
# Create an empty matrix to store the results
jackknife_results <- matrix(NA, ncol = 10, nrow = nrow(seed14analysis))
modelerror<- matrix(NA,ncol = 1,nrow = nrow(seed14analysis))
predvactual<- matrix(NA,ncol = 2,nrow = nrow(seed14analysis))
# Loop through each row of the data
for (i in 1:nrow(seed14analysis)) {
  # Subset the data, leaving out the ith observation
  data_subset <- seed14analysis[-i, ]
  dataofone<- seed14analysis[i,]
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

currentbestoutofsampleerr14-mse14
if ( mse14 <
     currentbestoutofsampleerr14){
  mse14 -> currentbestoutofsampleerr14
  fit14->seed14finalmodel
  print("keep")
}

rename(seed14analysis23, 'predround' = 'round')->seed14analysis23
seed14analysis23$seedpred <-predict(seed14finalmodel, newdata = seed14analysis23)
seed14analysis$seedpred <-predict(seed14finalmodel, newdata = seed14analysis)












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


bart_injuryimpact(year=2023, team='UCLA', player='Jaylen Clark')->wojaylen


# Load the required library
library(dplyr)

# Initialize an empty data frame for the final result
combcbb <- data.frame()

# Loop through the numbers 1 to 14
for (i in 1:14) {
  # Get the name of the current data frame
  df_name <- paste0("seed", i, "analysis23")
  
  # Access the current data frame using the get() function
  current_df <- get(df_name)
  
  # Filter the current data frame to include only rows where the desired column equals i
  filtered_df <- current_df %>%
    filter(seed == i) # Replace 'desired_column' with the actual name of the column you want to filter on
  
  # Combine the filtered data frame with the previous ones using rbind
  combcbb <- rbind(combcbb, filtered_df)
}


combcbbhist <- data.frame()

# Loop through the numbers 1 to 14
for (i in 1:14) {
  # Get the name of the current data frame
  df_name <- paste0("seed", i, "analysis")
  
  # Access the current data frame using the get() function
  current_df <- get(df_name)
  
  # Filter the current data frame to include only rows where the desired column equals i
  filtered_df <- current_df %>%
    filter(seed == i) # Replace 'desired_column' with the actual name of the column you want to filter on
  
  # Combine the filtered data frame with the previous ones using rbind
  combcbbhist <- rbind(combcbbhist, filtered_df)
}

# The combined_df variable now contains the combined and filtered data frames

# Load the required library
library(dplyr)

# Calculate the average seedpred value for each seed value and store it in a temporary data frame
average_seedpred <- combcbbhist %>%
  group_by(seed) %>%
  summarise(avg_seedpred = mean(seedpred))

# Join the average_seedpred data frame to the original data frame, combcbb
combcbbhist_with_avg <- combcbbhist %>%
  left_join(average_seedpred, by = "seed")

# Calculate the percentage difference and add a new column 'perc_diff'
combcbbhist_with_perc_diff <- combcbbhist_with_avg %>%
  mutate(perc_diff = ((seedpred - avg_seedpred)))

# The combcbb_with_perc_diff variable now contains the original data frame with an additional column named 'perc_diff'

filter(combcbbhist, round == 6)->winsss
library(sqldf)

sqldf("SELECT cbb.*, m.Team as team2, m.round
      FROM combcbb as cbb
      LEFT JOIN mastfinthisyearresult as m
      ON cbb.Team = m.Team")->outofsampcbb23

# Initialize a vector to store the squared differences
squared_diff <- matrix(data = NA, ncol =1, nrow = nrow(outofsampcbb23))
matrix
# Loop through the rows of the data frame
for (i in 1:nrow(outofsampcbb23)) {
  # Calculate the squared difference between seedpred and round for each row
  squared_diff[i] <- sqrt((outofsampcbb23[i, "seedpred"] - round(outofsampcbb23[i, "round"]))^2)
}

# Take the mean of all the squared differences
root_mean_squared_diff <- mean(squared_diff, na.rm = T
                          )

df$L <- ifelse(df$L == 0, 1, 0)

collegebasketballprojectdata->df

mastfin$sweetsixteen <- ifelse(mastfin$round >= 2, 1, 0)


write.csv(df, "collegebasketballprojectdata.csv")
