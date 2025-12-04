################################################################################
# AGENT-BASED MODEL: DIFUSIÓN DE NOMBRES ENTRE ESTRATOS SOCIALES
################################################################################
# 
# Este script implementa un modelo basado en agentes (ABM) que simula la 
# difusión de nombres entre estratos sociales basado en microfundaciones teóricas.
#
# ESPECIFICACIONES:
# - Quintiles: 5 clases sociales (Q1 = más bajo, Q5 = más alto/elite)
# - Género: 1 (simplificado)
# - Período: 1920 a 2010 (91 años)
# - Nombres: 30 nombres en el sistema
#
# MODELO TEÓRICO:
# - Variables de estado: p_{k,n}(t) = proporción de nacimientos en clase k 
#   con nombre n en tiempo t
# - Exposición ascendente (aspiracional): E^↑_{k,n}(t)
# - Exposición descendente (saturación): E^↓_{k,n}(t)
# - Probabilidad de elección basada en exposiciones
#
################################################################################

# Cargar librerías necesarias
library(tidyverse)
library(data.table)

################################################################################
# 1. ESTRUCTURA DE DATOS Y PARÁMETROS DEL MODELO
################################################################################

#' Inicializar Parámetros del Modelo ABM
#' 
#' @description
#' Crea la estructura de parámetros para el modelo de difusión de nombres.
#' 
#' @param n_classes Número de clases sociales (quintiles). Default: 5
#' @param n_names Número de nombres en el sistema. Default: 30
#' @param start_year Año inicial de la simulación. Default: 1920
#' @param end_year Año final de la simulación. Default: 2010
#' @param alpha_up Parámetro de intensidad para exposición ascendente. Default: 0.3
#' @param alpha_down Parámetro de intensidad para exposición descendente. Default: 0.2
#' @param beta Parámetro de sensibilidad a la exposición total. Default: 1.0
#' @param gamma Parámetro de inercia/persistencia. Default: 0.1
#' 
#' @return Lista con los parámetros del modelo
#' @export
initialize_abm_parameters <- function(
    n_classes = 5,
    n_names = 30,
    start_year = 1920,
    end_year = 2010,
    alpha_up = 0.3,
    alpha_down = 0.2,
    beta = 1.0,
    gamma = 0.1
) {
  
  # Validar parámetros
  if (n_classes < 2) stop("n_classes debe ser al menos 2")
  if (n_names < 1) stop("n_names debe ser al menos 1")
  if (start_year >= end_year) stop("start_year debe ser menor que end_year")
  
  params <- list(
    # Estructura básica
    n_classes = n_classes,
    n_names = n_names,
    start_year = start_year,
    end_year = end_year,
    n_years = end_year - start_year + 1,
    
    # Parámetros de influencia social
    alpha_up = alpha_up,      # Intensidad de exposición ascendente (aspiracional)
    alpha_down = alpha_down,  # Intensidad de exposición descendente (saturación)
    beta = beta,              # Sensibilidad a la exposición total
    gamma = gamma,            # Inercia/persistencia del estado actual
    
    # Identificadores
    class_names = paste0("Q", 1:n_classes),
    name_ids = paste0("N", sprintf("%02d", 1:n_names)),
    years = start_year:end_year
  )
  
  # Matrices de pesos para exposición entre clases
  params$W_up <- create_upward_weight_matrix(n_classes)
  params$W_down <- create_downward_weight_matrix(n_classes)
  
  return(params)
}

#' Crear Matriz de Pesos para Exposición Ascendente
#' 
#' @description
#' Crea una matriz de pesos que representa la influencia de clases superiores
#' sobre clases inferiores (aspiración hacia arriba).
#' 
#' @param n_classes Número de clases sociales
#' @return Matriz de pesos n_classes x n_classes
create_upward_weight_matrix <- function(n_classes) {
  W <- matrix(0, nrow = n_classes, ncol = n_classes)
  
  # Para cada clase k, asignar pesos a las clases superiores j > k
  for (k in 1:n_classes) {
    for (j in 1:n_classes) {
      if (j > k) {
        # Peso decrece con la distancia (clases más cercanas tienen más influencia)
        distance <- j - k
        W[k, j] <- exp(-0.5 * distance)
      }
    }
    # Normalizar para que sumen 1 (si hay clases superiores)
    if (sum(W[k, ]) > 0) {
      W[k, ] <- W[k, ] / sum(W[k, ])
    }
  }
  
  return(W)
}

#' Crear Matriz de Pesos para Exposición Descendente
#' 
#' @description
#' Crea una matriz de pesos que representa la influencia de clases inferiores
#' sobre clases superiores (saturación desde abajo).
#' 
#' @param n_classes Número de clases sociales
#' @return Matriz de pesos n_classes x n_classes
create_downward_weight_matrix <- function(n_classes) {
  W <- matrix(0, nrow = n_classes, ncol = n_classes)
  
  # Para cada clase k, asignar pesos a las clases inferiores j < k
  for (k in 1:n_classes) {
    for (j in 1:n_classes) {
      if (j < k) {
        # Peso decrece con la distancia (clases más cercanas tienen más influencia)
        distance <- k - j
        W[k, j] <- exp(-0.5 * distance)
      }
    }
    # Normalizar para que sumen 1 (si hay clases inferiores)
    if (sum(W[k, ]) > 0) {
      W[k, ] <- W[k, ] / sum(W[k, ])
    }
  }
  
  return(W)
}

################################################################################
# 2. INICIALIZACIÓN DEL ESTADO DEL MODELO
################################################################################

#' Inicializar Estado del Modelo
#' 
#' @description
#' Crea el estado inicial del modelo: matriz de proporciones p_{k,n}(t)
#' para cada clase k, nombre n, y tiempo t.
#' 
#' @param params Lista de parámetros del modelo (de initialize_abm_parameters)
#' @param init_type Tipo de inicialización: "uniform", "random", "elite_first"
#' @param seed Semilla para reproducibilidad
#' 
#' @return Array 3D de dimensiones [n_classes, n_names, n_years]
#' @export
initialize_state <- function(params, init_type = "elite_first", seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Crear array 3D: [classes, names, years]
  state <- array(
    0, 
    dim = c(params$n_classes, params$n_names, params$n_years),
    dimnames = list(
      class = params$class_names,
      name = params$name_ids,
      year = as.character(params$years)
    )
  )
  
  # Inicializar según el tipo especificado
  if (init_type == "uniform") {
    # Distribución uniforme: todos los nombres con igual probabilidad
    state[, , 1] <- 1 / params$n_names
    
  } else if (init_type == "random") {
    # Distribución aleatoria
    for (k in 1:params$n_classes) {
      probs <- runif(params$n_names)
      state[k, , 1] <- probs / sum(probs)
    }
    
  } else if (init_type == "elite_first") {
    # Los nombres comienzan en la elite y se difunden hacia abajo
    # Elite (Q5) tiene distribución inicial más diversa
    # Clases bajas (Q1) tienen distribución más concentrada en pocos nombres
    
    for (k in 1:params$n_classes) {
      if (k == params$n_classes) {
        # Elite: distribución relativamente uniforme
        probs <- runif(params$n_names, min = 0.5, max = 1.5)
      } else {
        # Clases bajas: concentración en nombres tradicionales
        # Los primeros nombres son más populares
        probs <- exp(-0.05 * (1:params$n_names))
        # Añadir ruido
        probs <- probs + runif(params$n_names, 0, 0.1)
      }
      state[k, , 1] <- probs / sum(probs)
    }
  }
  
  return(state)
}

################################################################################
# 3. FUNCIONES DE EXPOSICIÓN
################################################################################

#' Calcular Exposición Ascendente (Aspiracional)
#' 
#' @description
#' Calcula E^↑_{k,n}(t) = p_{k,n}(t) + Σ_{j>k} w_{k,j} * p_{j,n}(t)
#' La exposición ascendente representa la aspiración de clases bajas hacia
#' las prácticas de clases altas.
#' 
#' @param state Array 3D del estado actual
#' @param params Lista de parámetros del modelo
#' @param t Índice de tiempo actual
#' 
#' @return Matriz [n_classes x n_names] con exposiciones ascendentes
calculate_upward_exposure <- function(state, params, t) {
  
  # Estado actual: p_{k,n}(t) para todas las clases y nombres
  current_state <- state[, , t]
  
  # Exposición ascendente
  E_up <- matrix(0, nrow = params$n_classes, ncol = params$n_names)
  
  for (k in 1:params$n_classes) {
    # Componente propio: p_{k,n}(t)
    E_up[k, ] <- current_state[k, ]
    
    # Componente aspiracional: Σ_{j>k} w_{k,j} * p_{j,n}(t)
    for (j in 1:params$n_classes) {
      if (j > k && params$W_up[k, j] > 0) {
        E_up[k, ] <- E_up[k, ] + params$W_up[k, j] * current_state[j, ]
      }
    }
  }
  
  return(E_up)
}

#' Calcular Exposición Descendente (Saturación)
#' 
#' @description
#' Calcula E^↓_{k,n}(t) = p_{k,n}(t) + Σ_{j<k} w_{k,j} * p_{j,n}(t)
#' La exposición descendente representa la saturación: cuando un nombre
#' se vuelve común en clases bajas, pierde atractivo para clases altas.
#' 
#' @param state Array 3D del estado actual
#' @param params Lista de parámetros del modelo
#' @param t Índice de tiempo actual
#' 
#' @return Matriz [n_classes x n_names] con exposiciones descendentes
calculate_downward_exposure <- function(state, params, t) {
  
  # Estado actual: p_{k,n}(t) para todas las clases y nombres
  current_state <- state[, , t]
  
  # Exposición descendente
  E_down <- matrix(0, nrow = params$n_classes, ncol = params$n_names)
  
  for (k in 1:params$n_classes) {
    # Componente propio: p_{k,n}(t)
    E_down[k, ] <- current_state[k, ]
    
    # Componente de saturación: Σ_{j<k} w_{k,j} * p_{j,n}(t)
    for (j in 1:params$n_classes) {
      if (j < k && params$W_down[k, j] > 0) {
        E_down[k, ] <- E_down[k, ] + params$W_down[k, j] * current_state[j, ]
      }
    }
  }
  
  return(E_down)
}

################################################################################
# 4. FUNCIÓN DE PROBABILIDAD DE ELECCIÓN
################################################################################

#' Calcular Probabilidad de Elección de Nombres
#' 
#' @description
#' Calcula la probabilidad de que la clase k elija el nombre n en el tiempo t+1,
#' basándose en las exposiciones ascendente y descendente.
#' 
#' Formula general:
#' P_{k,n}(t+1) ∝ exp(β * [α_up * E^↑_{k,n}(t) - α_down * E^↓_{k,n}(t)]) + γ * p_{k,n}(t)
#' 
#' @param state Array 3D del estado actual
#' @param params Lista de parámetros del modelo
#' @param t Índice de tiempo actual
#' 
#' @return Matriz [n_classes x n_names] con probabilidades para t+1
calculate_choice_probabilities <- function(state, params, t) {
  
  # Calcular exposiciones
  E_up <- calculate_upward_exposure(state, params, t)
  E_down <- calculate_downward_exposure(state, params, t)
  
  # Estado actual para el componente de inercia
  current_state <- state[, , t]
  
  # Calcular utilidad/atractivo de cada nombre para cada clase
  # Positivo: exposición ascendente (aspiración)
  # Negativo: exposición descendente (saturación)
  utility <- params$beta * (params$alpha_up * E_up - params$alpha_down * E_down)
  
  # Añadir componente de inercia/persistencia
  utility <- utility + params$gamma * current_state
  
  # Convertir utilidad a probabilidad usando softmax
  # P_{k,n} = exp(utility_{k,n}) / Σ_n' exp(utility_{k,n'})
  probs <- matrix(0, nrow = params$n_classes, ncol = params$n_names)
  
  for (k in 1:params$n_classes) {
    # Softmax para la clase k
    exp_utility <- exp(utility[k, ] - max(utility[k, ])) # Restar max para estabilidad numérica
    probs[k, ] <- exp_utility / sum(exp_utility)
  }
  
  return(probs)
}

################################################################################
# 5. SIMULACIÓN DEL MODELO
################################################################################

#' Ejecutar Simulación del Modelo ABM
#' 
#' @description
#' Ejecuta la simulación completa del modelo de difusión de nombres.
#' 
#' @param params Lista de parámetros del modelo
#' @param initial_state Array 3D con el estado inicial
#' @param verbose Si TRUE, muestra progreso. Default: TRUE
#' 
#' @return Lista con:
#'   - state: Array 3D con la evolución completa del estado
#'   - params: Parámetros utilizados
#'   - exposures: Lista con exposiciones ascendentes y descendentes por año
#' @export
run_abm_simulation <- function(params, initial_state, verbose = TRUE) {
  
  # Inicializar estado con el estado inicial proporcionado
  state <- initial_state
  
  # Almacenar exposiciones para análisis
  exposures <- list(
    upward = array(0, dim = dim(state)),
    downward = array(0, dim = dim(state))
  )
  
  if (verbose) {
    cat("Iniciando simulación ABM...\n")
    cat("Período:", params$start_year, "-", params$end_year, "\n")
    cat("Clases:", params$n_classes, "| Nombres:", params$n_names, "\n\n")
  }
  
  # Simular año por año
  for (t in 1:(params$n_years - 1)) {
    
    if (verbose && (t %% 10 == 0 || t == 1)) {
      year <- params$years[t]
      cat("Año", year, "- Iteración", t, "de", params$n_years - 1, "\n")
    }
    
    # Calcular exposiciones en el tiempo t
    E_up <- calculate_upward_exposure(state, params, t)
    E_down <- calculate_downward_exposure(state, params, t)
    
    # Guardar exposiciones
    exposures$upward[, , t] <- E_up
    exposures$downward[, , t] <- E_down
    
    # Calcular probabilidades para t+1
    probs <- calculate_choice_probabilities(state, params, t)
    
    # Actualizar estado para t+1
    state[, , t + 1] <- probs
  }
  
  # Calcular exposiciones para el último año
  exposures$upward[, , params$n_years] <- calculate_upward_exposure(state, params, params$n_years)
  exposures$downward[, , params$n_years] <- calculate_downward_exposure(state, params, params$n_years)
  
  if (verbose) {
    cat("\nSimulación completada.\n")
  }
  
  # Retornar resultados
  results <- list(
    state = state,
    params = params,
    exposures = exposures
  )
  
  class(results) <- "abm_diffusion"
  
  return(results)
}

################################################################################
# 6. FUNCIONES DE ANÁLISIS Y CONVERSIÓN A DATA FRAME
################################################################################

#' Convertir Resultados del ABM a Data Frame
#' 
#' @description
#' Convierte el array 3D de resultados a un data frame en formato largo
#' para facilitar análisis y visualización.
#' 
#' @param abm_results Objeto de resultados del ABM (de run_abm_simulation)
#' @param include_exposures Si TRUE, incluye exposiciones. Default: FALSE
#' 
#' @return Data frame con columnas: year, class, name, proportion, [exposure_up, exposure_down]
#' @export
abm_to_dataframe <- function(abm_results, include_exposures = FALSE) {
  
  # Extraer dimensiones
  params <- abm_results$params
  state <- abm_results$state
  
  # Crear data frame base con proporciones
  df_list <- list()
  
  for (t in 1:params$n_years) {
    for (k in 1:params$n_classes) {
      for (n in 1:params$n_names) {
        df_list[[length(df_list) + 1]] <- data.frame(
          year = params$years[t],
          class = params$class_names[k],
          class_num = k,
          name = params$name_ids[n],
          name_num = n,
          proportion = state[k, n, t]
        )
      }
    }
  }
  
  df <- bind_rows(df_list)
  
  # Añadir exposiciones si se solicita
  if (include_exposures) {
    exp_list <- list()
    
    for (t in 1:params$n_years) {
      for (k in 1:params$n_classes) {
        for (n in 1:params$n_names) {
          exp_list[[length(exp_list) + 1]] <- data.frame(
            year = params$years[t],
            class = params$class_names[k],
            class_num = k,
            name = params$name_ids[n],
            name_num = n,
            exposure_up = abm_results$exposures$upward[k, n, t],
            exposure_down = abm_results$exposures$downward[k, n, t]
          )
        }
      }
    }
    
    df_exp <- bind_rows(exp_list)
    df <- left_join(df, df_exp, by = c("year", "class", "class_num", "name", "name_num"))
  }
  
  return(df)
}

#' Calcular Métricas de Difusión
#' 
#' @description
#' Calcula métricas agregadas sobre la difusión de nombres.
#' 
#' @param abm_results Objeto de resultados del ABM
#' 
#' @return Lista con métricas:
#'   - diversity: Diversidad de nombres por clase y año (Índice de Shannon)
#'   - concentration: Concentración (HHI) por clase y año
#'   - top_names: Nombres más populares por clase y año
#'   - diffusion_speed: Velocidad de difusión entre clases
#' @export
calculate_diffusion_metrics <- function(abm_results) {
  
  params <- abm_results$params
  state <- abm_results$state
  
  # Inicializar métricas
  metrics <- list()
  
  # 1. Diversidad (Índice de Shannon) por clase y año
  diversity <- matrix(0, nrow = params$n_classes, ncol = params$n_years)
  
  for (t in 1:params$n_years) {
    for (k in 1:params$n_classes) {
      probs <- state[k, , t]
      # H = -Σ p_i * log(p_i)
      probs <- probs[probs > 0] # Evitar log(0)
      diversity[k, t] <- -sum(probs * log(probs))
    }
  }
  
  metrics$diversity <- diversity
  rownames(metrics$diversity) <- params$class_names
  colnames(metrics$diversity) <- as.character(params$years)
  
  # 2. Concentración (HHI - Herfindahl-Hirschman Index) por clase y año
  concentration <- matrix(0, nrow = params$n_classes, ncol = params$n_years)
  
  for (t in 1:params$n_years) {
    for (k in 1:params$n_classes) {
      probs <- state[k, , t]
      # HHI = Σ p_i^2
      concentration[k, t] <- sum(probs^2)
    }
  }
  
  metrics$concentration <- concentration
  rownames(metrics$concentration) <- params$class_names
  colnames(metrics$concentration) <- as.character(params$years)
  
  # 3. Nombres más populares por clase y período
  # (se puede expandir para análisis más detallado)
  top_names <- list()
  sample_years <- c(1, params$n_years %/% 2, params$n_years) # Inicio, medio, fin
  
  for (t in sample_years) {
    year <- params$years[t]
    top_names[[as.character(year)]] <- list()
    
    for (k in 1:params$n_classes) {
      probs <- state[k, , t]
      top_5_idx <- order(probs, decreasing = TRUE)[1:min(5, params$n_names)]
      
      top_names[[as.character(year)]][[params$class_names[k]]] <- data.frame(
        name = params$name_ids[top_5_idx],
        proportion = probs[top_5_idx]
      )
    }
  }
  
  metrics$top_names <- top_names
  
  # 4. Velocidad de difusión entre clases
  # Calcular correlación entre clases adyacentes a lo largo del tiempo
  diffusion_speed <- matrix(0, nrow = params$n_classes - 1, ncol = params$n_years)
  
  for (t in 1:params$n_years) {
    for (k in 1:(params$n_classes - 1)) {
      # Correlación entre clase k y clase k+1
      corr <- cor(state[k, , t], state[k + 1, , t])
      diffusion_speed[k, t] <- corr
    }
  }
  
  metrics$diffusion_speed <- diffusion_speed
  rownames(metrics$diffusion_speed) <- paste0(params$class_names[1:(params$n_classes-1)], 
                                                "-", 
                                                params$class_names[2:params$n_classes])
  colnames(metrics$diffusion_speed) <- as.character(params$years)
  
  return(metrics)
}

################################################################################
# 7. FUNCIONES DE VISUALIZACIÓN
################################################################################

#' Visualizar Evolución de Proporciones de Nombres
#' 
#' @description
#' Crea un gráfico de la evolución temporal de las proporciones de nombres
#' para una clase social específica.
#' 
#' @param abm_results Objeto de resultados del ABM
#' @param class_index Índice de la clase a visualizar (1-5)
#' @param top_n Número de nombres más populares a mostrar. Default: 10
#' 
#' @return Objeto ggplot
#' @export
plot_name_evolution <- function(abm_results, class_index = 5, top_n = 10) {
  
  params <- abm_results$params
  state <- abm_results$state
  
  # Validar clase
  if (class_index < 1 || class_index > params$n_classes) {
    stop("class_index debe estar entre 1 y ", params$n_classes)
  }
  
  # Identificar los top_n nombres en el último año para esta clase
  final_year_idx <- params$n_years
  probs_final <- state[class_index, , final_year_idx]
  top_names_idx <- order(probs_final, decreasing = TRUE)[1:min(top_n, params$n_names)]
  
  # Crear data frame para el gráfico
  df_plot <- data.frame()
  
  for (n_idx in top_names_idx) {
    for (t in 1:params$n_years) {
      df_plot <- rbind(df_plot, data.frame(
        year = params$years[t],
        name = params$name_ids[n_idx],
        proportion = state[class_index, n_idx, t]
      ))
    }
  }
  
  # Crear gráfico
  p <- ggplot(df_plot, aes(x = year, y = proportion, color = name, group = name)) +
    geom_line(linewidth = 1) +
    labs(
      title = paste0("Evolución de Nombres - ", params$class_names[class_index]),
      subtitle = paste0("Top ", top_n, " nombres más populares en ", params$end_year),
      x = "Año",
      y = "Proporción",
      color = "Nombre"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      legend.position = "right"
    ) +
    scale_y_continuous(labels = scales::percent_format())
  
  return(p)
}

#' Visualizar Heatmap de Proporciones
#' 
#' @description
#' Crea un heatmap mostrando las proporciones de nombres por clase en un año específico.
#' 
#' @param abm_results Objeto de resultados del ABM
#' @param year Año a visualizar
#' @param top_n Número de nombres más populares a mostrar. Default: 15
#' 
#' @return Objeto ggplot
#' @export
plot_name_heatmap <- function(abm_results, year = NULL, top_n = 15) {
  
  params <- abm_results$params
  state <- abm_results$state
  
  # Si no se especifica año, usar el último
  if (is.null(year)) {
    year <- params$end_year
  }
  
  # Encontrar índice del año
  year_idx <- which(params$years == year)
  if (length(year_idx) == 0) {
    stop("Año ", year, " no encontrado en la simulación")
  }
  
  # Identificar top_n nombres más populares en promedio
  avg_props <- apply(state[, , year_idx], 2, mean)
  top_names_idx <- order(avg_props, decreasing = TRUE)[1:min(top_n, params$n_names)]
  
  # Crear data frame para el heatmap
  df_heat <- data.frame()
  
  for (k in 1:params$n_classes) {
    for (n_idx in top_names_idx) {
      df_heat <- rbind(df_heat, data.frame(
        class = params$class_names[k],
        name = params$name_ids[n_idx],
        proportion = state[k, n_idx, year_idx]
      ))
    }
  }
  
  # Crear heatmap
  p <- ggplot(df_heat, aes(x = class, y = name, fill = proportion)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "white", 
      mid = "lightblue", 
      high = "darkblue",
      midpoint = median(df_heat$proportion),
      labels = scales::percent_format()
    ) +
    labs(
      title = paste0("Distribución de Nombres por Clase Social - ", year),
      subtitle = paste0("Top ", top_n, " nombres más populares"),
      x = "Clase Social",
      y = "Nombre",
      fill = "Proporción"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 8)
    )
  
  return(p)
}

#' Visualizar Métricas de Diversidad
#' 
#' @description
#' Crea un gráfico de la evolución de la diversidad de nombres por clase.
#' 
#' @param metrics Lista de métricas (de calculate_diffusion_metrics)
#' @param params Parámetros del modelo
#' 
#' @return Objeto ggplot
#' @export
plot_diversity_evolution <- function(metrics, params) {
  
  # Convertir matriz de diversidad a data frame
  df_div <- data.frame()
  
  for (k in 1:params$n_classes) {
    for (t in 1:params$n_years) {
      df_div <- rbind(df_div, data.frame(
        year = params$years[t],
        class = params$class_names[k],
        diversity = metrics$diversity[k, t]
      ))
    }
  }
  
  # Crear gráfico
  p <- ggplot(df_div, aes(x = year, y = diversity, color = class, group = class)) +
    geom_line(linewidth = 1) +
    labs(
      title = "Evolución de la Diversidad de Nombres por Clase Social",
      subtitle = "Índice de Shannon (valores más altos = mayor diversidad)",
      x = "Año",
      y = "Diversidad (H)",
      color = "Clase"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      legend.position = "right"
    )
  
  return(p)
}

################################################################################
# 8. FUNCIÓN PRINCIPAL DE EJEMPLO
################################################################################

#' Ejecutar Ejemplo Completo del ABM
#' 
#' @description
#' Función de conveniencia que ejecuta un ejemplo completo del modelo
#' con parámetros por defecto.
#' 
#' @param seed Semilla para reproducibilidad. Default: 42
#' @param verbose Si TRUE, muestra progreso. Default: TRUE
#' 
#' @return Lista con resultados del ABM y métricas
#' @export
run_abm_example <- function(seed = 42, verbose = TRUE) {
  
  if (verbose) {
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("EJEMPLO: Agent-Based Model de Difusión de Nombres\n")
    cat("=" , rep("=", 70), "\n\n", sep = "")
  }
  
  # 1. Inicializar parámetros
  if (verbose) cat("1. Inicializando parámetros...\n")
  params <- initialize_abm_parameters(
    n_classes = 5,
    n_names = 30,
    start_year = 1920,
    end_year = 2010,
    alpha_up = 0.3,
    alpha_down = 0.2,
    beta = 1.0,
    gamma = 0.1
  )
  
  # 2. Inicializar estado
  if (verbose) cat("2. Inicializando estado del modelo...\n")
  initial_state <- initialize_state(params, init_type = "elite_first", seed = seed)
  
  # 3. Ejecutar simulación
  if (verbose) cat("3. Ejecutando simulación...\n\n")
  abm_results <- run_abm_simulation(params, initial_state, verbose = verbose)
  
  # 4. Calcular métricas
  if (verbose) cat("\n4. Calculando métricas de difusión...\n")
  metrics <- calculate_diffusion_metrics(abm_results)
  
  # 5. Resumen
  if (verbose) {
    cat("\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("RESUMEN DE RESULTADOS\n")
    cat("=" , rep("=", 70), "\n", sep = "")
    cat("Diversidad final por clase:\n")
    for (k in 1:params$n_classes) {
      cat(sprintf("  %s: %.3f\n", 
                  params$class_names[k], 
                  metrics$diversity[k, params$n_years]))
    }
    cat("\nConcentración final por clase (HHI):\n")
    for (k in 1:params$n_classes) {
      cat(sprintf("  %s: %.3f\n", 
                  params$class_names[k], 
                  metrics$concentration[k, params$n_years]))
    }
    cat("=" , rep("=", 70), "\n\n", sep = "")
  }
  
  # Retornar resultados completos
  return(list(
    abm_results = abm_results,
    metrics = metrics
  ))
}

################################################################################
# FIN DEL SCRIPT
################################################################################
