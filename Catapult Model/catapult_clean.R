library(ggplot2)
library(dplyr)
library(readr)

# Load Data
dfs <- list()
for (i in 1:14) {
  file_name <- paste0("catapult_p", i, ".csv")
  df <- read_csv(file_name)
  # Fixes error with incompatible column type for different data frames
  df$Bench.Time <- as.character(df$Bench.Time)
  dfs[[i]] <- df
}
rm(df, i, file_name)
catapult_main <- bind_rows(dfs)

# Cleaning
# Fix Date Format
catapult_main <- catapult_main %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Removing named entries with no information and duplicate entries
catapult_main <- catapult_main %>% 
  filter(!is.na(Start)) %>% 
  distinct()

# Select relevant columns
catapult_select <- catapult_main %>% 
  select(Date,
         About,
         Position,
         Total.Duration..min.,
         Total.Distance,
         Maximum.Velocity,
         Profile.Max.Velocity,
         Average.Velocity,
         Heart.Rate.Exertion,
         Total.Player.Load,
         Player.Load.min,
         Player.Load.Per.Metre,
         Total.Acceleration.Load,
         Acceleration.Density.Index,
         IMA.Accel.Low,
         IMA.Accel.Medium,
         IMA.Accel.High,
         IMA.Decel.Low,
         IMA.Decel.Medium,
         IMA.Decel.High,
         IMA.CoD.Left.Low,
         IMA.CoD.Left.Medium,
         IMA.CoD.Left.High,
         IMA.CoD.Right.Low,
         IMA.CoD.Right.Medium,
         IMA.CoD.Right.High,
         IMA.Accel.Total,
         IMA.Decel.Total,
         Explosive.Efforts,
         Average.GNSS.Quality,
         Max.Acceleration,
         Max.Deceleration,
         Player.Load..1D.Fwd.,
         Player.Load..1D.Side.,
         Player.Load..1D.Up.)

catapult_condensed <- catapult_select %>% 
  group_by(Date, About, Position) %>% 
  summarize(
    total_duration_min = sum(Total.Duration..min.),
    total_distance = sum(Total.Distance),
    max_velocity = max(Maximum.Velocity),
    profile_max_velocity = Profile.Max.Velocity,
    average_velocity = mean(Average.Velocity),
    heart_rate_exertion = sum(Heart.Rate.Exertion),
    total_player_load = sum(Total.Player.Load),
    player_load_per_min = sum(Total.Player.Load) / sum(Total.Duration..min.),
    player_load_per_meter = sum(Total.Player.Load) / sum(Total.Distance),
    total_acceleration_load = sum(Total.Acceleration.Load),
    Acceleration.Density.Index = mean(Acceleration.Density.Index),
    ima_accel_low = sum(IMA.Accel.Low),
    ima_accel_med = sum(IMA.Accel.Medium),
    ima_accel_high = sum(IMA.Accel.High),
    ima_decel_low = sum(IMA.Decel.Low),
    ima_decel_med = sum(IMA.Decel.Medium),
    ima_decel_high = sum(IMA.Decel.High),
    ima_cod_left_low = sum(IMA.CoD.Left.Low),
    ima_cod_left_med = sum(IMA.CoD.Left.Medium),
    ima_cod_left_high = sum(IMA.CoD.Left.High),
    ima_cod_right_low = sum(IMA.CoD.Right.Low),
    ima_cod_right_med = sum(IMA.CoD.Right.Medium),
    ima_cod_right_high = sum(IMA.CoD.Right.High),
    ima_accel_total = sum(IMA.Accel.Total),
    ima_decel_total = sum(IMA.Decel.Total),
    explosive_effors_total = sum(Explosive.Efforts),
    max_acceleration = max(Max.Acceleration),
    max_deceleration = max(Max.Deceleration),
    player_load_1d_fwd_total = sum(Player.Load..1D.Fwd.),
    player_load_1d_side_total = sum(Player.Load..1D.Side.),
    player_load_1d_up_total = sum(Player.Load..1D.Up.)
  )