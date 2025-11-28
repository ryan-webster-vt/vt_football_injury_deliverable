input_checker <- function(prompt_msg, must_be_nonneg = FALSE, must_be_whole = FALSE) {
  repeat {
    input <- readline(prompt = prompt_msg)
    num <- suppressWarnings(as.numeric(input))
    if (is.na(num)) {
      cat("Invalid input. Please enter a numeric value.\n")
      next
    }
    if (must_be_whole && num %% 1 != 0) {
      cat("Input must be a whole number without decimals.\n")
      next
    }
    if (must_be_nonneg && num < 0) {
      cat("Input must be non-negative.\n")
      next
    }
    return(num)
  }
}

# Function for manual entry of VALD metrics and risk prediction
manual_entry <- function() {
  cat("Enter VALD metrics below:\n")
  avg_force  <- input_checker("Average Force: ", must_be_nonneg = TRUE)
  nordic_avg <- input_checker("Nordic Left Avg: ", must_be_nonneg = TRUE)
  
  newdata <- data.frame(
    Average.Force   = avg_force,
    Nordic.Left.Avg = nordic_avg
  )
  execute_model_vald(newdata, type = "manual")
}

# Function to load metrics from a CSV file and predict risk
load_data <- function() {
  repeat {
    filepath <- readline("Path to CSV file (or 'break' to cancel): ")
    if (tolower(filepath) == 'break') return()
    
    df <- tryCatch(
      read.csv(filepath),
      error = function(e) {
        cat("Cannot read file. Check path and try again.\n")
        return(NULL)
      }
    )
    if (is.null(df)) next
    
    required_cols <- c("About", "Average.Force", "Nordic.Left.Avg")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      cat("CSV is missing columns:", paste(missing_cols, collapse = ", "), "\n")
      next
    }
    
    tryCatch({
      if (
        is.numeric(df$Average.Force) && all(df$Average.Force >= 0) &&
        is.numeric(df$Nordic.Left.Avg) && all(df$Nordic.Left.Avg >= 0)
      ) {
        cat("Data validation passed.\n")
        execute_model_vald(df, type = "load")
        break
      } else {
        stop("Data validation failed: values not meeting constraints.")
      }
    },error = function(e) {
      cat(e$message, "\nPlease check your file and try again.\n\n")
    })
  }
}

# Function to call the saved VALD model and compute survival predictions
execute_model_vald <- function(vald_model, type = "manual") {
  model <- tryCatch(
    readRDS("vald_model_final.RDS"),
    error = function(e) stop("Unable to load 'vald_model_final.RDS'. Make sure it is in the working directory.")
  )
  if (!is.null(model$formula)) {
    environment(model$formula) <- asNamespace("survival")
  }
  
  surv_pred <- survival::survfit(model, newdata = vald_model)
  read_survfit(surv_pred, type, vald_model)
}

# Function to read survival object and display injury risk
read_survfit <- function(surv_fit, type = "manual", vald_final) {
  repeat {
    days <- suppressWarnings(as.numeric(readline("Number of days ahead for risk forecast: ")))
    if (!is.na(days) && days > 0 && days %% 1 == 0) break
    cat("Please enter a positive integer.\n")
  }
  
  summary_obj <- summary(surv_fit, times = days)
  probabilities <- round(1 - summary_obj$surv, 4) * 100
  plot(surv_fit)
  
  if (type == "manual") {
    cat(sprintf("Predicted injury risk in %d days: %.2f%%\n", summary_obj$time, probabilities))
  } else {
    sorted_idx <- order(-probabilities)
    injury_prob_sorted <- probabilities[sorted_idx]
    
    for (i in seq_along(injury_prob_sorted)) {
      cat(sprintf("Probability of non-contact injury for %s in %d days: %.2f%%\n", vald_final[sorted_idx[i], 2], summary_obj$time, injury_prob_sorted[i]))
    }
  }
}

# Main function to start the dashboard
main <- function() {
  rm(list = ls())
  
  if (!require("survival", character.only = TRUE)) install.packages("survival")
  library(survival)
  library(dplyr)
  
  repeat {
    choice <- tolower(readline("Would you like to manually insert data or load data from a .csv? (manual/load): "))
    if (choice == "manual") {
      manual_entry()
      break
    } else if (choice == "load") {
      load_data()
      break
    } else {
      cat("Invalid input. Please enter 'manual' or 'load'.\n")
    }
  }
}

# Execute the dashboard
main()