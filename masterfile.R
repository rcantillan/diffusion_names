cat("\014") 
rm(list = ls())

library(pacman)
p_load(rvest,dplyr,tidyr,stringr,here)

folder <- getwd()

# =======================================================================

start_time <- Sys.time()

families_full <- NULL

for (i in 1:146431) {
  cat(":::::::::::::::::::::::::::",i,"::::::::::::::::::::::")
  source(here("genealogy.R"))
  families_full <- rbind(families_full,data_families)
}

# Record end time
end_time <- Sys.time()

# Calculate elapsed time
elapsed_time <- end_time - start_time
elapsed_time