cat("\014")
rm(list = ls())
library(pacman)
p_load(rvest, dplyr, tidyr, stringr, here)

folder <- "/home/rober/Desktop"

# Load existing data if it exists
output_file <- file.path(folder, "families_full.csv")
if (file.exists(output_file)) {
  families_full <- read.csv(output_file)
  if (nrow(families_full) > 0 && any(grepl("^I(\\d+)$", families_full$from, perl = TRUE))) {
    last_iteration <- max(as.numeric(gsub("I(\\d+)$", "\\1", families_full$from, perl = TRUE)), na.rm = TRUE)
  } else {
    last_iteration <- 0
  }
} else {
  families_full <- data.frame(from = character())
  last_iteration <- 0
}

start_time <- Sys.time()

positive_sleep <- function(duration) {
  if (duration < 0) return(0)
  return(duration)
}

last_completed_iteration <- last_iteration

while (TRUE) {
  tryCatch({
    for (i in (last_completed_iteration + 1):146431) {
      cat(":::::::::::::::::::::::::::", i, "::::::::::::::::::::::\n")
      
      tryCatch({
        source(here("genealogy.R"))
        data_families$from <- paste0("I", i)
        families_full <- rbind(families_full, data_families)
        
        write.csv(families_full, file = output_file, row.names = FALSE)
        
        last_completed_iteration <- i
        
        normal_pause_duration <- positive_sleep(rnorm(1, mean = 1, sd = 0.2))
        Sys.sleep(normal_pause_duration)
        
      }, error = function(e) {
        if (grepl("HTTP error 500", e$message)) {
          cat("Error HTTP 500 en el registro", i, ". Continuando con el siguiente registro.\n")
        } else if (grepl("Could not resolve host: www.genealogiachilenaenred.cl", e$message)) {
          cat("Error de resolución de host en el registro", i, ". Esperando 15 segundos antes de reintentar.\n")
          Sys.sleep(15)
          # No incrementamos last_completed_iteration aquí para reintentar este registro
        } else {
          stop(e)
        }
      })
    }
    
    break
  }, error = function(e) {
    if (grepl("Timeout was reached", e$message)) {
      cat("Error de timeout. Guardando progreso y reiniciando la descarga en 5 segundos...\n")
      
      write.csv(families_full, file = output_file, row.names = FALSE)
      saveRDS(last_completed_iteration, file = file.path(folder, "last_completed_iteration.rds"))
      
      Sys.sleep(5)
      
      if (file.exists(file.path(folder, "last_completed_iteration.rds"))) {
        last_completed_iteration <- readRDS(file.path(folder, "last_completed_iteration.rds"))
      }
    } else {
      stop(e)
    }
  })
}

write.csv(families_full, file = output_file, row.names = FALSE)

end_time <- Sys.time()
elapsed_time <- end_time - start_time
elapsed_time



