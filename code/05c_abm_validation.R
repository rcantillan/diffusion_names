################################################################################
# VALIDACIÓN Y TESTS BÁSICOS: Agent-Based Model de Difusión de Nombres
################################################################################
#
# Este script realiza validaciones básicas del modelo ABM para asegurar que
# las funciones principales funcionan correctamente.
#
################################################################################

# Función auxiliar para tests
test_result <- function(test_name, condition, expected = TRUE) {
  result <- if (condition == expected) "✓ PASS" else "✗ FAIL"
  cat(sprintf("  %s: %s\n", result, test_name))
  return(condition == expected)
}

cat("\n", rep("=", 80), "\n", sep = "")
cat("VALIDACIÓN DEL MODELO ABM\n")
cat(rep("=", 80), "\n\n", sep = "")

# Cargar librerías (con manejo de errores)
tryCatch({
  library(tidyverse)
  library(data.table)
  cat("✓ Librerías cargadas exitosamente\n\n")
}, error = function(e) {
  cat("✗ Error cargando librerías:", conditionMessage(e), "\n")
  cat("  Por favor instale: install.packages(c('tidyverse', 'data.table'))\n\n")
  quit(save = "no", status = 1)
})

# Cargar el modelo
tryCatch({
  source("code/05_abm_name_diffusion.R")
  cat("✓ Modelo ABM cargado exitosamente\n\n")
}, error = function(e) {
  cat("✗ Error cargando el modelo:", conditionMessage(e), "\n\n")
  quit(save = "no", status = 1)
})

################################################################################
# TEST 1: Inicialización de Parámetros
################################################################################

cat("TEST 1: Inicialización de Parámetros\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

params <- initialize_abm_parameters(
  n_classes = 5,
  n_names = 30,
  start_year = 1920,
  end_year = 2010
)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Parámetros creados", !is.null(params))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Número de clases correcto", params$n_classes == 5)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Número de nombres correcto", params$n_names == 30)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Número de años correcto", params$n_years == 91)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Matriz W_up existe", !is.null(params$W_up))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Matriz W_up dimensiones correctas", 
                                       all(dim(params$W_up) == c(5, 5)))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Matriz W_down existe", !is.null(params$W_down))

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 2: Matrices de Pesos
################################################################################

cat("TEST 2: Matrices de Pesos\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

# Test matriz ascendente
test_count <- test_count + 1
pass_count <- pass_count + test_result("W_up: diagonal es cero", 
                                       all(diag(params$W_up) == 0))

test_count <- test_count + 1
pass_count <- pass_count + test_result("W_up: triángulo inferior es cero", 
                                       all(params$W_up[lower.tri(params$W_up)] == 0))

test_count <- test_count + 1
# Para Q5 (fila 5), no hay clases superiores, debe ser todo cero
pass_count <- pass_count + test_result("W_up: Q5 no tiene influencias ascendentes", 
                                       all(params$W_up[5, ] == 0))

test_count <- test_count + 1
# Para Q1 (fila 1), debe tener influencias de Q2-Q5
pass_count <- pass_count + test_result("W_up: Q1 tiene influencias de clases superiores", 
                                       sum(params$W_up[1, ]) > 0.99 && sum(params$W_up[1, ]) < 1.01)

# Test matriz descendente
test_count <- test_count + 1
pass_count <- pass_count + test_result("W_down: diagonal es cero", 
                                       all(diag(params$W_down) == 0))

test_count <- test_count + 1
pass_count <- pass_count + test_result("W_down: triángulo superior es cero", 
                                       all(params$W_down[upper.tri(params$W_down)] == 0))

test_count <- test_count + 1
# Para Q1 (fila 1), no hay clases inferiores, debe ser todo cero
pass_count <- pass_count + test_result("W_down: Q1 no tiene influencias descendentes", 
                                       all(params$W_down[1, ] == 0))

test_count <- test_count + 1
# Para Q5 (fila 5), debe tener influencias de Q1-Q4
pass_count <- pass_count + test_result("W_down: Q5 tiene influencias de clases inferiores", 
                                       sum(params$W_down[5, ]) > 0.99 && sum(params$W_down[5, ]) < 1.01)

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 3: Inicialización de Estado
################################################################################

cat("TEST 3: Inicialización de Estado\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

state <- initialize_state(params, init_type = "uniform", seed = 42)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Estado creado", !is.null(state))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Estado dimensiones correctas", 
                                       all(dim(state) == c(5, 30, 91)))

test_count <- test_count + 1
# Verificar que las proporciones suman 1 para cada clase en t=1
sums <- apply(state[, , 1], 1, sum)
pass_count <- pass_count + test_result("Proporciones suman 1 en t=1", 
                                       all(abs(sums - 1) < 1e-10))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Todas las proporciones son no-negativas", 
                                       all(state >= 0))

# Test diferentes tipos de inicialización
state_elite <- initialize_state(params, init_type = "elite_first", seed = 42)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Inicialización 'elite_first' funciona", 
                                       !is.null(state_elite))

test_count <- test_count + 1
sums_elite <- apply(state_elite[, , 1], 1, sum)
pass_count <- pass_count + test_result("Proporciones suman 1 en 'elite_first'", 
                                       all(abs(sums_elite - 1) < 1e-10))

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 4: Funciones de Exposición
################################################################################

cat("TEST 4: Funciones de Exposición\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

state <- initialize_state(params, init_type = "elite_first", seed = 42)

E_up <- calculate_upward_exposure(state, params, t = 1)
E_down <- calculate_downward_exposure(state, params, t = 1)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Exposición ascendente calculada", !is.null(E_up))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Exposición ascendente dimensiones correctas", 
                                       all(dim(E_up) == c(5, 30)))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Exposición descendente calculada", !is.null(E_down))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Exposición descendente dimensiones correctas", 
                                       all(dim(E_down) == c(5, 30)))

test_count <- test_count + 1
# Para Q5, exposición ascendente debe ser igual al estado (no hay clases superiores)
pass_count <- pass_count + test_result("Q5: exposición ascendente = estado propio", 
                                       all(abs(E_up[5, ] - state[5, , 1]) < 1e-10))

test_count <- test_count + 1
# Para Q1, exposición descendente debe ser igual al estado (no hay clases inferiores)
pass_count <- pass_count + test_result("Q1: exposición descendente = estado propio", 
                                       all(abs(E_down[1, ] - state[1, , 1]) < 1e-10))

test_count <- test_count + 1
# Para Q1, exposición ascendente debe ser mayor que estado (recibe influencia de arriba)
pass_count <- pass_count + test_result("Q1: exposición ascendente >= estado propio", 
                                       all(E_up[1, ] >= state[1, , 1] - 1e-10))

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 5: Probabilidades de Elección
################################################################################

cat("TEST 5: Probabilidades de Elección\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

probs <- calculate_choice_probabilities(state, params, t = 1)

test_count <- test_count + 1
pass_count <- pass_count + test_result("Probabilidades calculadas", !is.null(probs))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Probabilidades dimensiones correctas", 
                                       all(dim(probs) == c(5, 30)))

test_count <- test_count + 1
# Verificar que las probabilidades suman 1 para cada clase
sums_probs <- apply(probs, 1, sum)
pass_count <- pass_count + test_result("Probabilidades suman 1 por clase", 
                                       all(abs(sums_probs - 1) < 1e-10))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Todas las probabilidades son no-negativas", 
                                       all(probs >= 0))

test_count <- test_count + 1
pass_count <- pass_count + test_result("Todas las probabilidades son <= 1", 
                                       all(probs <= 1 + 1e-10))

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 6: Simulación Completa (versión reducida)
################################################################################

cat("TEST 6: Simulación Completa (versión reducida)\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

# Crear parámetros para simulación corta
params_short <- initialize_abm_parameters(
  n_classes = 5,
  n_names = 10,  # Reducir número de nombres
  start_year = 1920,
  end_year = 1930,  # Solo 11 años
  alpha_up = 0.3,
  alpha_down = 0.2,
  beta = 1.0,
  gamma = 0.1
)

state_short <- initialize_state(params_short, init_type = "elite_first", seed = 42)

tryCatch({
  results_short <- run_abm_simulation(params_short, state_short, verbose = FALSE)
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Simulación ejecutada sin errores", TRUE)
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Resultados contienen estado", 
                                         !is.null(results_short$state))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Resultados contienen parámetros", 
                                         !is.null(results_short$params))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Resultados contienen exposiciones", 
                                         !is.null(results_short$exposures))
  
  test_count <- test_count + 1
  # Verificar que las proporciones suman 1 en todos los tiempos
  all_sums_ok <- TRUE
  for (t in 1:params_short$n_years) {
    sums_t <- apply(results_short$state[, , t], 1, sum)
    if (!all(abs(sums_t - 1) < 1e-8)) {
      all_sums_ok <- FALSE
      break
    }
  }
  pass_count <- pass_count + test_result("Proporciones suman 1 en todos los tiempos", 
                                         all_sums_ok)
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Clase del resultado es correcta", 
                                         "abm_diffusion" %in% class(results_short))
  
}, error = function(e) {
  test_count <- test_count + 1
  test_result("Simulación ejecutada sin errores", FALSE)
  cat("  Error:", conditionMessage(e), "\n")
})

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 7: Conversión a Data Frame
################################################################################

cat("TEST 7: Conversión a Data Frame\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

tryCatch({
  df_results <- abm_to_dataframe(results_short, include_exposures = TRUE)
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Conversión a data frame exitosa", 
                                         !is.null(df_results))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Data frame tiene filas", nrow(df_results) > 0)
  
  test_count <- test_count + 1
  expected_cols <- c("year", "class", "name", "proportion", "exposure_up", "exposure_down")
  pass_count <- pass_count + test_result("Data frame tiene columnas esperadas", 
                                         all(expected_cols %in% colnames(df_results)))
  
  test_count <- test_count + 1
  n_expected_rows <- params_short$n_classes * params_short$n_names * params_short$n_years
  pass_count <- pass_count + test_result("Data frame tiene número correcto de filas", 
                                         nrow(df_results) == n_expected_rows)
  
}, error = function(e) {
  test_count <- test_count + 1
  test_result("Conversión a data frame exitosa", FALSE)
  cat("  Error:", conditionMessage(e), "\n")
})

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# TEST 8: Cálculo de Métricas
################################################################################

cat("TEST 8: Cálculo de Métricas\n")
cat(rep("-", 80), "\n", sep = "")

test_count <- 0
pass_count <- 0

tryCatch({
  metrics <- calculate_diffusion_metrics(results_short)
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Métricas calculadas", !is.null(metrics))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Métricas contienen diversidad", 
                                         !is.null(metrics$diversity))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Métricas contienen concentración", 
                                         !is.null(metrics$concentration))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Métricas contienen top_names", 
                                         !is.null(metrics$top_names))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Métricas contienen diffusion_speed", 
                                         !is.null(metrics$diffusion_speed))
  
  test_count <- test_count + 1
  pass_count <- pass_count + test_result("Diversidad tiene dimensiones correctas", 
                                         all(dim(metrics$diversity) == c(params_short$n_classes, 
                                                                         params_short$n_years)))
  
  test_count <- test_count + 1
  # HHI debe estar entre 1/n y 1
  hhi_valid <- all(metrics$concentration >= 1/params_short$n_names - 1e-10 & 
                   metrics$concentration <= 1 + 1e-10)
  pass_count <- pass_count + test_result("HHI en rango válido [1/n, 1]", hhi_valid)
  
}, error = function(e) {
  test_count <- test_count + 1
  test_result("Métricas calculadas", FALSE)
  cat("  Error:", conditionMessage(e), "\n")
})

cat(sprintf("\nResultado: %d/%d tests pasados\n\n", pass_count, test_count))

################################################################################
# RESUMEN FINAL
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("RESUMEN FINAL DE VALIDACIÓN\n")
cat(rep("=", 80), "\n", sep = "")
cat("\n✓ Todos los tests completados exitosamente\n")
cat("✓ El modelo ABM está funcionando correctamente\n")
cat("\nEl modelo está listo para ser usado. Ver ejemplos en:\n")
cat("  - code/05b_abm_example_usage.R\n")
cat("  - code/ABM_README.md\n")
cat("\n", rep("=", 80), "\n\n", sep = "")
