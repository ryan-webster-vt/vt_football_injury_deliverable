# Load libraries
library(tidyverse)
library(lubridate)
library(survival)
library(ggsurvfit)
library(dplyr)

# Load Catapult data
catapult_data <- read_csv("catapult_condensed.csv") %>% 
  filter(total_distance != 0) %>% 
  select(-1)

# Load non-contact injury data
injury_data <- read_csv("Cleaned_Non-Contact_Injuries.csv") %>% 
  select(Position, Name, `Date of Injury`, `Body Part`, Injury, Side) %>%
  mutate(`Date of Injury` = as.Date(`Date of Injury`, format = "%m/%d/%Y")) %>% 
  filter(`Date of Injury` >= as.Date("2024-01-01")) %>% 
  group_by(Name) %>% 
  slice_min(`Date of Injury`, with_ties = F) %>% 
  ungroup()

# Split Catapult data into lists of dfs by name, filter out small dfs
catapult_list <- split(catapult_data, catapult_data$About) 
catapult_list <- Filter(function(df) nrow(df) > 50, catapult_list)

# Assigns start and end values for each entry
create_time_variables <- function(df) {
  df <- df %>% arrange(Date)
  df$start <- as.numeric(difftime(df$Date, min(df$Date), units = "days"))
  df$end <- lead(df$start, n = 1)
  df$end[nrow(df)] <- df$start[nrow(df)] + 1
  df <- df %>% select(start, end, everything())
  return(df)
}
catapult_list <- lapply(catapult_list, create_time_variables)

# Adds indicator variable to injury date
find_injury_date <- function(df) {
  name <- df$About[1]
  if (!any(name == injury_data$Name)) {
    df$injury <- 0
    return(df)
  }
  
  injury_date <- injury_data$`Date of Injury`[injury_data$Name == name]
  df$injury <- 0
  injury_index <- which.min(abs(difftime(df$Date, injury_date, units = "days")))
  df$injury[injury_index] <- 1
  
  df <- df[1:injury_index, ]
  
  return(df)
}

catapult_list <- lapply(catapult_list,find_injury_date)

# Combines list into one df
catapult_final <- bind_rows(catapult_list)

# Run Model
library(MASS)

catapult_final <- catapult_final %>% drop_na()


full_cox_model <- coxph(Surv(as.numeric(start), as.numeric(end), injury) ~ . -Date -About -position -profile_max_velocity -Acceleration.Density.Index,
                        data = catapult_final)
summary(full_cox_model)

step_model <- stepAIC(full_cox_model, direction = "backward")

summary(step_model)

# Assumption Testing for Proportional Hazards (the effect of a 
# covariate on the hazard is constant over time)
zph <- cox.zph(step_model)
plot(zph)

# Test if the log hazard if a linear function of continuous predictors
resid_mart <- residuals(step_model, type = "martingale")
plot(catapult_final$total_player_load, resid_mart)
lines(lowess(catapult_final$total_player_load, resid_mart), col = "red")

plot(catapult_final$ima_decel_high_log, resid_mart)
lines(lowess(catapult_final$ima_decel_high, resid_mart), col = "red")

plot(catapult_final$ima_cod_left_high_log, resid_mart)
lines(lowess(catapult_final$ima_cod_left_high, resid_mart), col = "red")

plot(catapult_final$max_deceleration_sqrt, resid_mart)
lines(lowess(catapult_final$max_deceleration_sqrt, resid_mart), col = "red")

# Log data
catapult_final$ima_decel_high_sqrt <- sqrt(catapult_final$ima_decel_high)
catapult_final$ima_cod_left_high_sqrt <- sqrt(catapult_final$ima_cod_left_high)
catapult_final$ima_cod_righ_low_sqrt <- sqrt(catapult_final$ima_cod_righ_low)
catapult_final$max_deceleration_sqrt <- sqrt(abs(catapult_final$max_deceleation))

# Refit
step_model_copy <- coxph(Surv(as.numeric(start), as.numeric(end), injury) ~ 
                           total_player_load +
                           ima_decel_high_sqrt + 
                           ima_cod_left_high_sqrt + 
                           ima_cod_righ_low_sqrt + 
                           max_deceleration_sqrt +
                           cluster(as.factor(About)), data = catapult_final)

fit<- survfit(step_model_copy)
plot(fit$cumhaz)

player_fit <- data.frame(total_player_load = catapult_final$total_player_load[1:10],
                                         ima_decel_high_sqrt = catapult_final$ima_decel_high_sqrt[1:10],
                                         ima_cod_left_high_sqrt = catapult_final$ima_cod_left_high_sqrt[1:10],
                                         ima_cod_righ_low_sqrt = catapult_final$ima_cod_righ_low_sqrt[1:10], 
                                         max_deceleration_sqrt = catapult_final$max_deceleration_sqrt[1:10])

survival <- survfit(step_model_copy, newdata = player_fit)
plot(survival)
predict <- predict(step_model_copy, newdata = player_fit, type = "risk")


data_for_pred<-catapult_final %>%
  group_by(About) %>%
  slice(1) %>%
  ungroup()

# data_for_pred<-catapult_final %>%
#   group_by(About) %>%
#   summarise(
#     start = min(start),
#     end = max(end),
#     total = sum(injury),
#     ...
#     
#   )


playerfit2 <-  coxph(Surv(as.numeric(start), as.numeric(end), injury) ~ 
                       total_player_load +
                       ima_decel_high_sqrt + 
                       ima_cod_left_high_sqrt + 
                       ima_cod_righ_low_sqrt + 
                       max_deceleration_sqrt +
                       cluster(as.factor(About)), data = catapult_final)

survival2 <- survfit(playerfit2, newdata= data_for_pred)

prob<- summary(survival2, times=100)$surv
risk <- ifelse(prob > 0.9, "Low",
               ifelse(prob > 0.7, "Medium", "High"))

risk_for_athlete <- data.frame( name = data_for_pred$About,
                                 probability = prob,  
                                 risk_lvl =risk )



# No multicollinearity
library(car)
vif(step_model)

attr(step_model_copy$terms, ".Environment") <- NULL
environment(step_model_copy$formula) <- environment()
saveRDS(step_model_copy, "catapult_model.RDS")
