# Load libraries
library(changepoint)
library(tidyverse)

# setwd("C:\\Users\\ljrwe\\OneDrive\\Desktop\\Capstone\\Catapult")

# Load Catapult data
catapult <- read_csv("catapult_condensed.csv") %>% 
  filter(total_distance != 0) %>% 
  select(-c(`...1`))

changepoint_func <- function(column){
  cpt_results <- cpt.meanvar(na.omit(as.numeric(column), na.rm = T), 
                             method="PELT", penalty="MBIC", minseglen=2)
}

# Splits data into a list of dfs by player
catapult_list <- split(catapult, catapult$About) 
catapult_list <- Filter(function(df) nrow(df) > 50, catapult_list)

# Conducts change point analysis on each data frame
results <- lapply(catapult_list, function(d){
  lapply(d %>% select(-c(About, Date, position, profile_max_velocity)), changepoint_func)
})

# Loads non-contact injury data
injury <- read_csv("Cleaned_Non-Contact_Injuries.csv") %>% 
  select(Position, Name, `Date of Injury`, `Body Part`, Injury, Side) %>%
  mutate(`Date of Injury` = as.Date(`Date of Injury`, format = "%m/%d/%Y")) %>% 
  filter(`Date of Injury` >= as.Date("2024-01-01"))

# Fix names
injury[29, "Name"] <- "William Watson"

# Filter catapult list by injured players
catapult_injured_players <- catapult_list[names(catapult_list) %in% injury$Name]
catapult_injured_players <- catapult_injured_players[!sapply(catapult_injured_players, is.null)]
names(catapult_injured_players) <- names(catapult_injured_players)


# Variable that will store count of variables that predated injury
injury_variable_count <- c(rep(0, 30))

injury_detection <- function(df) {
  player_name <- as.character(df[1, "About"]) # Get player name
  
  injury_date <- injury$`Date of Injury`[injury$Name == player_name] # Gets the injury date 
  injury_index <- which.min(abs(as.Date(df$Date) - as.Date(injury_date))) # Gets exact/nearest injury index
  
  threshold_date <- injury_date - 14 # Gets threshold date
  threshold_index <- which.min(abs(as.Date(df$Date) - as.Date(threshold_date))) # Gets exact/nearest threshold index
  
  player_result <- results[[player_name]]
  cpts_list <- lapply(names(player_result), function(variable) {
    return(cpts(player_result[[variable]]))
  })
  
  for (i in 1 : length(cpts_list)) {
    if (any(cpts_list[[i]] >= threshold_index & cpts_list[[i]] <= injury_date)) {
      injury_variable_count[i] <<- injury_variable_count[i] + 1
    }
  }
}

output <- lapply(catapult_injured_players, injury_detection)
injury_occurance <- data.frame(
  count = injury_variable_count,
  variable = names(results[["Ali Jennings"]])
)

barplot(injury_occurance$count, 
        names.arg = injury_occurance$variable, # Replace with the actual variable names
        las = 2, # Rotates text to be perpendicular
        cex.names = 0.5) # Adjusts text size for better fit