library(tidyverse)
library(dplyr)
library(readr)
library(survival)
library(lubridate)
library(MASS)
library(car)

# Load vald data
vald <- read_csv("/Users/matthewchoy/Desktop/CMDA-4864/Project/Data/cleaned_vald_data.csv") %>%
  filter(Side == "Left",           
         !is.na(About),           
         !is.na(Date)) %>%         
  mutate(Date = as.Date(Date, "%Y/%m/%d"))

# Load non-contact injury data
injury_data <- read_csv("/Users/matthewchoy/Desktop/CMDA-4864/Project/Data/Cleaned_Non-Contact_Injuries.csv") %>% 
  dplyr::select(Position, Name, `Date of Injury`, `Body Part`, Injury, Side) %>%
  mutate(`Date of Injury` = as.Date(`Date of Injury`, format = "%m/%d/%Y")) %>%
  filter(`Date of Injury` >= as.Date("2024-01-01")) %>%
  group_by(Name) %>%
  slice_min(`Date of Injury`, with_ties = F) %>%
  ungroup()

vald_list <- split(vald, vald$About)
vald_list <- Filter(function(df) nrow(df) > 3, vald_list)

create_time_vars <- function(df) {
  df <- df %>% arrange(Date)
  df$start <- as.numeric(difftime(df$Date, min(df$Date), units = "days"))
  df$end <- lead(df$start, n = 1)
  df$end[nrow(df)] <- df$start[nrow(df)] + 1
  df <- df %>% dplyr::select(start, end, everything())
  return(df)
}

vald_list <- lapply(vald_list, create_time_vars)

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

vald_list <- lapply(vald_list, find_injury_date)

# Combines list into one df
vald_final <- bind_rows(vald_list)
names(vald_final)

# Mark injury status
assign_injury <- function(df) {
  name <- df$About[1]
  if (!name %in% injury_data$Name) {
    df$injury <- 0
    return(df)
  }
  injury_date <- injury_data$`Date of Injury`[injury_data$Name == name]
  df$injury <- 0
  idx <- which.min(abs(difftime(df$Date, injury_date, units = "days")))
  df$injury[idx] <- 1
  return(df[1:idx, ])
}
vald_list <- lapply(vald_list, assign_injury)

# Combine into one dataframe and drop rows with missing values
vald_final <- bind_rows(vald_list) %>% drop_na()

# Pick key predictors
vald_model <- vald_final %>%
  dplyr::select(
    start, end, injury, Average.Force, Nordic.Left.MAX, Nordic.MEAN.Imbalance, Nordic.Left.Avg,
    Nordic.Mean.Median
  )
vald_model

# Fit full Cox PH model
full_model <- coxph(Surv(start, end, injury) ~ ., data = vald_model)
summary(full_model)

# Stepwise selection
step_model <- stepAIC(full_model, direction = "backward")
summary(step_model)

# Variables to check PH assumption for
vars <- c("Average.Force", "Nordic.Left.MAX",
          "Nordic.MEAN.Imbalance", "Nordic.Left.Avg", "Nordic.Mean.Median")

# Loop and run Schoenfeld test + plot for each
for (v in vars) {
  cat("\n--- PH assumption for", v, "---\n")
  
  # fit univariate Cox model
  f <- as.formula(paste("Surv(start, end, injury) ~", v))
  uni_mod <- coxph(f, data = vald_model)
  
  # Schoenfeld test
  zph_uni <- cox.zph(uni_mod)
  print(zph_uni)
  
  # plot scaled Schoenfeld residuals
  plot(zph_uni, var = 1, main = paste("Scaled Schoenfeld for", v))
}

# Log / sqrt transforms
vald_final <- vald_final %>%
  mutate(
    AverageForce_log = log(Average.Force + 1),
    NordicLeftAvg_sqrt = sqrt(Nordic.Left.Avg)
  )

# Refit with transformed variables and clustering by athlete
step_model_copy <- coxph(
  Surv(start, end, injury) ~
    AverageForce_log + NordicLeftAvg_sqrt +
    cluster(factor(About)),
  data = vald_final
)
summary(step_model_copy)

resid_mart <- residuals(step_model_copy, type = "martingale")
md <- model.frame(step_model_copy)

plot(md$AverageForce_log, resid_mart,
     main="Martingale vs Avg Force (log)", xlab="AverageForce_log")
lines(lowess(md$AverageForce_log, resid_mart), col="red")

plot(md$NordicLeftAvg_sqrt, resid_mart,
     main="Martingale vs Nordic Left Avg (sqrt)", xlab="NordicLeftAvg_sqrt")
lines(lowess(md$NordicLeftAvg_sqrt, resid_mart), col="red")

# Plot cumulative hazard
fit <- survfit(step_model_copy)
plot(fit$cumhaz)

# Survival curves for first 10 athletes
player_fit <- data.frame(
  AverageForce_log = vald_final$AverageForce_log[1:10],
  NordicLeftAvg_sqrt = vald_final$NordicLeftAvg_sqrt[1:10]
)
survival <- survfit(step_model_copy, newdata = player_fit)
plot(survival)
predict <- predict(step_model_copy, newdata = player_fit, type = "risk")

# Prepare data for group‐level risk prediction
data_for_pred <- vald_final %>%
  group_by(About) %>%
  slice(1) %>%
  ungroup()

playerfit2 <- coxph(
  Surv(start, end, injury) ~
    AverageForce_log + NordicLeftAvg_sqrt +
    cluster(factor(About)),
  data = vald_final
)
survival2 <- survfit(playerfit2, newdata = data_for_pred)

prob <- summary(survival2, times = 100)$surv
risk <- ifelse(prob > 0.9, "Low",
               ifelse(prob > 0.7, "Medium", "High"))
risk_for_athlete <- data.frame(
  name        = data_for_pred$About,
  probability = prob,
  risk_lvl    = risk
)

# Check multicollinearity
vif(step_model)

# Save final model
saveRDS(step_model_copy, "vald_model.RDS")