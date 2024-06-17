cat("\014")
rm(list = ls())
library(pacman)
p_load(rvest, dplyr, tidyr, stringr, here)
#folder <- getwd()
folder <- "/home/rober/Desktop"
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
last_completed_iteration <- last_iteration
while (TRUE) {
  tryCatch({
    # Resume from the last iteration
    for (i in (last_completed_iteration + 1):146431) {
      cat(":::::::::::::::::::::::::::", i, "::::::::::::::::::::::\n")
      
      tryCatch({
        source(here("genealogy.R"))
        data_families$from <- paste0("I", i)  # Ensure each iteration number is saved in the desired format
        families_full <- rbind(families_full, data_families)
        
        # Save the current state of families_full to a CSV file
        write.csv(families_full, file = output_file, row.names = FALSE)
        
        last_completed_iteration <- i  # Actualizar la última iteración completada
        
        # Add a normally distributed pause every iteration (average of 1 second)
        normal_pause_duration <- positive_sleep(rnorm(1, mean = 1, sd = 0.2))  # Standard deviation of 0.2 seconds for variability
        Sys.sleep(normal_pause_duration)
        
        # Add pauses as before...
        
      }, error = function(e) {
        if (grepl("HTTP error 500", e$message)) {
          cat("Error HTTP 500 en el registro", i, ". Continuando con el siguiente registro.\n")
          last_completed_iteration <- last_completed_iteration + 1  # Avanzar al siguiente registro
        } else {
          stop(e)
        }
      })
    }
    
    # Si el bucle for se completa sin errores, salir del bucle while
    break
  }, error = function(e) {
    # Maneja el error de timeout
    if (grepl("Timeout was reached", e$message)) {
      cat("Error de timeout. Reiniciando la descarga en 5 segundos...\n")
      Sys.sleep(5)  # Espera 5 segundos antes de reiniciar
    } else {
      # Para cualquier otro error, detén la ejecución y muestra el mensaje de error
      stop(e)
    }
  })
}
# Record end time
end_time <- Sys.time()
# Calculate elapsed time
elapsed_time <- end_time - start_time
elapsed_time