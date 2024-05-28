cat("\014")
rm(list = ls())

library(pacman)
p_load(rvest, dplyr, tidyr, stringr, here)

folder <- getwd()

# =======================================================================

# Load existing data if it exists
output_file <- file.path(folder, "families_full.csv")
if (file.exists(output_file)) {
  families_full <- read.csv(output_file)
  if (nrow(families_full) > 0 && any(grepl("^I\\d+$", families_full$from))) {
    last_iteration <- max(as.numeric(gsub("I", "", families_full$from)), na.rm = TRUE)
  } else {
    last_iteration <- 0
  }
} else {
  families_full <- data.frame(from = character())
  last_iteration <- 0
}

start_time <- Sys.time()

# Function to ensure positive sleep durations
positive_sleep <- function(duration) {
  if (duration < 0) return(0)
  return(duration)
}

# Resume from the last iteration
for (i in (last_iteration + 1):50) {
  #for (i in (last_iteration + 1):146431) {
  cat(":::::::::::::::::::::::::::", i, "::::::::::::::::::::::\n")
  source(here("genealogy.R"))
  data_families$from <- paste0("I", i)  # Ensure each iteration number is saved in the desired format
  families_full <- rbind(families_full, data_families)
  
  # Save the current state of families_full to a CSV file
  write.csv(families_full, file = output_file, row.names = FALSE)
  
  # Add a normally distributed pause every iteration (average of 1 second)
  normal_pause_duration <- positive_sleep(rnorm(1, mean = 1, sd = 0.2))  # Standard deviation of 0.2 seconds for variability
  Sys.sleep(normal_pause_duration)
  
  # Add a normally distributed pause every 20 iterations (average of 3 seconds)
  if (i %% 20 == 0) {
    normal_pause_duration <- positive_sleep(rnorm(1, mean = 3, sd = 1))  # Standard deviation of 1 second
    Sys.sleep(normal_pause_duration)
  }
  
  # Add a normally distributed pause every 50 iterations (average of 5 seconds)
  if (i %% 50 == 0) {
    normal_pause_duration <- positive_sleep(rnorm(1, mean = 5, sd = 1))  # Standard deviation of 1 second
    Sys.sleep(normal_pause_duration)
  }
  
  # Add a normally distributed pause every 100 iterations (average of 7 seconds)
  if (i %% 100 == 0) {
    normal_pause_duration <- positive_sleep(rnorm(1, mean = 7, sd = 1))  # Standard deviation of 1 second
    Sys.sleep(normal_pause_duration)
  }
  
  # Add a normally distributed pause every 500 iterations (average of 10 seconds)
  if (i %% 500 == 0) {
    normal_pause_duration <- positive_sleep(rnorm(1, mean = 10, sd = 2))  # Standard deviation of 2 seconds
    Sys.sleep(normal_pause_duration)
  }
  
  # Add a normally distributed pause every 1000 iterations (average of 30 seconds)
  if (i %% 1000 == 0) {
    normal_pause_duration <- positive_sleep(rnorm(1, mean = 30, sd = 5))  # Standard deviation of 5 seconds
    Sys.sleep(normal_pause_duration)
  }
}

# Record end time
end_time <- Sys.time()

# Calculate elapsed time
elapsed_time <- end_time - start_time
elapsed_time
