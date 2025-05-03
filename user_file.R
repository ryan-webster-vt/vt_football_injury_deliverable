# To run script, press Ctrl+A+Enter. If first time, libraries 'survival' and
# 'rstudioapi' will be installed if never used previously
# 
# To quit at any time, click the escape key
# 

# Checks/installs rstudioapi package if necessary
if (!require("rstudioapi", character.only = TRUE)) {
  install.packages("rstudioapi")
}
library(rstudioapi)

# Sets path to current file directory
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

repeat {
  cat("\f")
  type <- readline("Would you like to work with Catapult or Vald Performance? (c/v): ") |> tolower()
  if (type == "c") {
    script_name <- "catapult_backend.R"
    full_script_path <- file.path(dirname(current_path), script_name)
    cat("\f")
    source(full_script_path)
    break
  } else if (type == "v") {
    script_name <- "vald_backend.R"
    full_script_path <- file.path(dirname(current_path), script_name)
    cat("\f")
    source(full_script_path)
    break
  } else {
    cat("Please insert a valid input.")
  }
}

