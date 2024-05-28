cat("\014")
rm(list = ls())

library(pacman)
p_load(rvest, dplyr, tidyr, stringr, here)
folder <- getwd()
folder <- "/media/rober/4612-9FBE/names"

# =======================================================================

start_time <- Sys.time()

families_full <- NULL

#for (i in 1:50) {
for (i in 1:146431) {
  cat(":::::::::::::::::::::::::::", i, "::::::::::::::::::::::")
  source(here("genealogy.R"))
  families_full <- rbind(families_full, data_families)
  
  # Save the current state of families_full to a CSV file
  write.csv(families_full, file = file.path(folder, "families_full.csv"), row.names = FALSE)
  
  # Add a log-normal distributed pause
  meanlog <- log(1.5) - 0.5  # Adjust mean to 12 seconds
  sdlog <- 1.5  # Standard deviation for the log-normal distribution
  pause_duration <- rlnorm(1, meanlog = meanlog, sdlog = sdlog)
  Sys.sleep(pause_duration)
  
  # Add a normally distributed pause every 20 iterations
  if (i %% 30 == 0) {
    normal_pause_duration <- rnorm(1, mean = 7, sd = 2)  # Standard deviation of 5 seconds
    Sys.sleep(normal_pause_duration)
  }
}

# Record end time
end_time <- Sys.time()

# Calculate elapsed time
elapsed_time <- end_time - start_time
elapsed_time
