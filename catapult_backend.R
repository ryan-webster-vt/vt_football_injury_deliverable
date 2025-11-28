manual_entry <- function() {
  total_player_load <- input_checker("Insert player's 'Total Player Load': ")

  ima_decel_high <- input_checker("Insert player's 'IMA Decel High': ", TRUE, TRUE) |>
    sqrt()

  ima_cod_left_high <- input_checker("Insert player's 'IMA COD Left High': ", TRUE, TRUE) |>
    sqrt()

  ima_cod_right_low <- input_checker("Insert player's 'IMA COD Right Low': ", TRUE, TRUE) |>
    sqrt()

  max_deceleration <- input_checker("Insert player's 'Max Deceleration': ") |>
    (\(x) sign(x) * abs(x)^(1 / 3))()

  data <- c(
    total_player_load,
    ima_decel_high,
    ima_cod_left_high,
    ima_cod_right_low,
    max_deceleration
  )

  execute_model_catapult(data)
}

load_data <- function() {
  repeat {
    filename <- readline("Insert File Name: ")
    if (filename == "break") {
      break
    }
    
    # Checks if file is found, skips to next iteration if failed
    df <- tryCatch({
      suppressWarnings(read.csv(filename))
    }, error = function(e) {
      cat("File not found or could not be read. Please check the file name and try again.\n")
      return(NULL)
    })
    if (is.null(df)) next 
    
    tryCatch(
      {
        if (!all(c(
          "About",
          "total_player_load", 
          "ima_decel_high", 
          "ima_cod_left_high", 
          "ima_cod_right_low", 
          "max_deceleration"
        ) %in% names(df))) {
          stop("Missing required columns in the data.")
        }
        
        total_player_load <- df$total_player_load
        ima_decel_high <- df$ima_decel_high
        ima_cod_left_high <- df$ima_cod_left_high
        ima_cod_right_low <- df$ima_cod_right_low
        max_deceleration <- df$max_deceleration
        
        if (
          is.numeric(total_player_load) && all(total_player_load > 0, na.rm = TRUE) &&
          is.numeric(ima_decel_high) && all(ima_decel_high >= 0 & ima_decel_high %% 1 == 0, na.rm = TRUE) &&
          is.numeric(ima_cod_left_high) && all(ima_cod_left_high >= 0 & ima_cod_left_high %% 1 == 0, na.rm = TRUE) &&
          is.numeric(ima_cod_right_low) && all(ima_cod_right_low >= 0 & ima_cod_right_low %% 1 == 0, na.rm = TRUE) &&
          is.numeric(max_deceleration) && all(max_deceleration <= 0, na.rm = TRUE)
        ) {
          cat("Data validation passed.\n")
          execute_model_catapult(df, type = "load")
          break
        } else {
          stop("Data validation failed: values not meeting constraints.")
        }
      },
      error = function(e) {
        cat(e$message, "\nPlease check your file and try again.\n\n")
      }
    )
  }
}
    

execute_model_catapult <- function(data, type = "manual") {
  
  model <- tryCatch(
    {
      suppressWarnings(readRDS("catapult_model.rds"))
    },
    error = function(e) {
      stop("Could not load the model. Make sure 'catapult_model.rds' exists and is a valid model file.")
    }
  )
  
  if (!is.null(model$formula)) {
    environment(model$formula) <- asNamespace("survival")
  }
  
  if (type == "manual") {
    catapult_final <- data.frame(
      About = "Player",
      total_player_load = data[1],
      ima_decel_high_sqrt = data[2],
      ima_cod_left_high_sqrt = data[3],
      ima_cod_right_low_sqrt = data[4],
      max_deceleration_cr = data[5]
    )
    
    surv_pred <- survival::survfit(model, newdata = catapult_final)
    read_survfit(surv_pred)
    
  } else {
    catapult_final <- data.frame(
      About = data$About,
      total_player_load = data$total_player_load,
      ima_decel_high_sqrt = sqrt(data$ima_decel_high),
      ima_cod_left_high_sqrt = sqrt(data$ima_cod_left_high),
      ima_cod_right_low_sqrt = sqrt(data$ima_cod_right_low),
      max_deceleration_cr = sign(data$max_deceleration) * abs(data$max_deceleration)^(1 / 3)
    )
    
    surv_pred <- survival::survfit(model, newdata = catapult_final)
    read_survfit(surv_pred, type = "load", catapult_final)
  }
}


read_survfit <- function(surv_fit, type = "manual", catapult_final) {
  repeat {
    days <- readline("How many days forward would you like to see probability of injury?: ")
    days <- suppressWarnings(as.numeric(days))
    tryCatch(
      {
        if (is.numeric(days) & days > 0 & days %% 1 == 0) {
          break
        }
      },
      error = function(e) {
        cat("Day must numeric, greater than zero, and an integer.\n")
      }
    )
  }
  if (type == "manual") {
    s <- summary(surv_fit, times = days)
    injury_prob <- round(1 - s$surv, 4) * 100
    cat("Probability of non-contact injury for player:", injury_prob, "% in", s$time, "days.\n")
  } else {
    s <- summary(surv_fit, times = days)
    injury_prob <- round(1 - s$surv, 4) * 100
    
    sorted_idx <- order(-injury_prob)
    injury_prob_sorted <- injury_prob[sorted_idx]
    
    for (i in seq_along(injury_prob_sorted)) {
      cat("Probability of non-contact injury for", catapult_final[sorted_idx[i], 1], ":", injury_prob_sorted[i], "% in", s$time, "days.\n")
    }
  }
}

input_checker <- function(prompt_msg, apply_sqrt = FALSE, must_be_whole = FALSE) {
  repeat {
    input <- readline(prompt = prompt_msg)
    num <- suppressWarnings(as.numeric(input))

    if (is.na(num)) {
      cat("Invalid input. Please enter a numeric value.\n")
      next
    }

    if (must_be_whole && num %% 1 != 0) {
      cat("Input must be a whole number (no decimals).\n")
      next
    }

    if (apply_sqrt && num < 0) {
      cat("Input must be non-negative for square root.\n")
      next
    }
    return(num)
  }
}


main <- function() {
  # Empties variable environment
  rm(list = ls())

  # Checks/installs survival package if necessary
  if (!require("survival", character.only = TRUE)) {
    install.packages("survival")
  }

  # Load libraries
  library(survival)

  # Asks user how they want to input data
  repeat {
    answer <- readline("Would you like to manually insert data or load data from a .csv? (manual/load): ") |> tolower()
    if (answer == "manual") {
      manual_entry()
      break
    } else if (answer == "load") {
      cat("Make sure the file you would like to load is in:\n", getwd(), "\n")
      load_data()
      break
    } else {
      cat("Invalid input. Please enter 'manual' or 'load'.\n")
    }
  }
}

main()
