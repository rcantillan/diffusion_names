###############################################################################
# ANÁLISIS DE DIFUSIÓN DE NOMBRES EN CHILE
# 
# Este código analiza cómo los nombres se "difunden" entre diferentes comunas 
# (municipios) de Chile a lo largo del tiempo, usando dos enfoques:
# 1. Análisis secuencial (año a año)
# 2. Análisis por ventanas temporales (períodos de 10 o 20 años)
#
# El objetivo es generar métricas que cuantifiquen la influencia que
# unas comunas ejercen sobre otras en la adopción de nombres.
###############################################################################

# Cargar librerías necesarias
library(data.table)  # Para manipulación eficiente de datos
library(progress)    # Para mostrar barras de progreso

###############################################################################
# Función create_diffusion_dataset
#
# PROPÓSITO:
# Esta función analiza cómo los nombres se difunden año a año entre comunas.
# Si un nombre estaba presente en la comuna A en el año X, y aparece por primera
# vez en la comuna B en el año X+1, consideramos que hubo una "difusión" 
# del nombre desde la comuna A hacia la comuna B.
#
# PARÁMETROS:
# - df: DataFrame con los datos de nombres (debe contener: nombre_final, ano, comuna, cantidad)
# - batch_size: Tamaño de los lotes para procesar los nombres (por eficiencia)
#
# PROCESO GENERAL:
# 1. Preparación de datos y cálculo de proporciones
# 2. Identificación de nombres que muestran patrones de difusión
# 3. Procesamiento por lotes de esos nombres para generar métricas de difusión
# 4. Combinación de resultados
#
# RESULTADO:
# Un data.table con 6 métricas de difusión para cada combinación de 
# (comuna origen, comuna destino, año):
# - sequential_count: Número de conexiones entre comunas
# - sequential_weighted_sum/mean: Suma/promedio ponderado de las conexiones
# - first_rank_count: Conexiones desde la comuna donde el nombre apareció primero
# - first_rank_weighted_sum/mean: Suma/promedio ponderado de las first_rank
###############################################################################
create_diffusion_dataset <- function(df, batch_size = 500) {
  
  message("Preparando datos...")
  
  # Convertir a data.table para optimizar el procesamiento
  dt <- as.data.table(df)
  
  # Eliminar filas con valores NA en columnas clave
  # Esto es crítico para evitar errores durante los cálculos
  dt <- dt[
    !is.na(nombre_final) & !is.na(ano) & 
      !is.na(comuna) & !is.na(cantidad)
  ]
  
  # Definir llave principal para optimizar búsquedas
  setkey(dt, nombre_final, ano, comuna)
  
  #-----------------------------------------------------------------------------
  # 1. CÁLCULO DE PROPORCIONES INICIALES
  #-----------------------------------------------------------------------------
  # Calcular el total de nombres por (ano, comuna)
  # Esto es fundamental para conocer qué proporción representa cada nombre
  # del total de nombres en una comuna específica en un año determinado
  dtTotals <- dt[, .(total_comuna_all = sum(cantidad)), by = .(ano, comuna)]
  
  # Definir rango de años a analizar (se excluye 1955 como año origen)
  anos <- 1956:2024
  
  #-----------------------------------------------------------------------------
  # 2. IDENTIFICACIÓN DE NOMBRES CON DIFUSIÓN
  #-----------------------------------------------------------------------------
  # Esta etapa es crucial para reducir la carga computacional:
  # en lugar de analizar todos los nombres (que serían miles),
  # identificamos solo aquellos que muestran patrones de difusión
  message("\nIdentificando nombres con difusión...")
  
  # Barra de progreso para visualizar el avance
  pb_anos <- progress_bar$new(
    format = "Años [:bar] :percent | :current/:total | ETA: :eta",
    total = length(anos)
  )
  
  # Para cada año, identificamos nombres que muestran difusión
  nombres_difusion <- rbindlist(lapply(anos, function(ano_adopcion) {
    ano_origen <- ano_adopcion - 1  # Año anterior
    
    # Filtrar datos para estos dos años específicos
    result <- dt[ano %in% c(ano_origen, ano_adopcion), {
      # Identificar comunas donde el nombre existía en el año origen
      comunas_origen   <- unique(comuna[ano == ano_origen])
      # Identificar comunas donde el nombre existe en el año adopción
      comunas_destino  <- unique(comuna[ano == ano_adopcion])
      
      # Un nombre muestra difusión si:
      # 1. Existía en al menos una comuna en el año origen
      # 2. Aparece en al menos una comuna nueva en el año adopción
      hay_difusion <- length(comunas_origen) > 0 &&
        !all(comunas_destino %in% comunas_origen)
      
      .(hay_difusion = hay_difusion)
    }, by = nombre_final][hay_difusion == TRUE, .(nombre_final)]
    
    pb_anos$tick()
    result
  }))
  
  # Obtener lista única de nombres que muestran difusión
  nombres_difusion <- unique(nombres_difusion$nombre_final)
  message(sprintf("\nTotal nombres que muestran difusión: %d", length(nombres_difusion)))
  
  # Verificar si hay nombres con difusión
  if (length(nombres_difusion) == 0) {
    message("No hay nombres con difusión.")
    return(NULL)
  }
  
  #-----------------------------------------------------------------------------
  # 3. PROCESAMIENTO POR LOTES (BATCHES)
  #-----------------------------------------------------------------------------
  # Dividir la lista de nombres en lotes para procesamiento eficiente
  # Esto evita problemas de memoria al procesar grandes volúmenes de datos
  batches <- split(nombres_difusion, ceiling(seq_along(nombres_difusion)/batch_size))
  n_batches <- length(batches)
  
  message(sprintf("\nProcesando %d nombres en %d batches", length(nombres_difusion), n_batches))
  
  pb_batches <- progress_bar$new(
    format = "Batches [:bar] :percent | Batch :current/:total | ETA: :eta",
    total = n_batches
  )
  
  # Lista para almacenar resultados de cada lote
  all_results <- vector("list", n_batches)
  
  # Procesamiento de cada lote
  for (i in seq_len(n_batches)) {
    # Filtrar datos para los nombres del lote actual
    dt_batch <- dt[nombre_final %in% batches[[i]]]
    
    # Combinar con totales para calcular proporciones
    dt_batch <- merge(
      dt_batch,
      dtTotals,
      by = c("ano", "comuna"),
      all.x = TRUE
    )
    
    # Calcular proporción real: cantidad de veces que aparece el nombre
    # dividido por el total de nombres en esa comuna y año
    dt_batch[, prop_real := cantidad / total_comuna_all]
    
    # Identificar primera aparición global de cada nombre en este lote
    # Esto es necesario para las métricas first_rank_*
    first_appearances <- dt_batch[
      , .(
        # Año más antiguo en que aparece el nombre
        first_year    = min(ano),
        # Comuna donde apareció primero (si hay empate, toma la primera en orden)
        first_comuna  = comuna[which.min(ano)],
        # Cantidad de veces que apareció en esa primera ocasión
        first_cantidad = cantidad[which.min(ano)]
      ),
      by = nombre_final
    ]
    
    # Barra de progreso para los años dentro del lote
    pb_batch_anos <- progress_bar$new(
      format = "  Años del batch [:bar] :percent | :current/:total",
      total = length(anos),
      clear = FALSE,
      width = 60
    )
    
    # Vector para almacenar resultados por año dentro del lote
    batch_results_year <- vector("list", length(anos))
    
    # Para cada año, analizar la difusión
    for (idx_year in seq_along(anos)) {
      year <- anos[idx_year]       # Año actual
      year_prev <- year - 1        # Año anterior
      
      # Filtrar datos para el año anterior y el actual
      data_prev <- dt_batch[ano == year_prev]
      data_curr <- dt_batch[ano == year]
      
      # Si no hay datos para alguno de los dos años, continuar al siguiente
      if (nrow(data_prev) == 0 || nrow(data_curr) == 0) {
        pb_batch_anos$tick()
        next
      }
      
      # Analizar difusión para cada nombre en data_prev
      diffusion_by_year <- rbindlist(lapply(unique(data_prev$nombre_final), function(nm) {
        
        # Datos del año anterior para este nombre
        datos_prev <- data_prev[nombre_final == nm,
                                .(comuna, prop_source = prop_real, 
                                  total_source = total_comuna_all)]
        
        # Datos del año actual para este nombre
        datos_curr <- data_curr[nombre_final == nm,
                                .(comuna, prop_target = prop_real)]
        
        # Identificar comunas en cada año
        comunas_prev <- unique(datos_prev$comuna)
        comunas_curr <- unique(datos_curr$comuna)
        
        # Encontrar comunas donde el nombre aparece por primera vez
        comunas_nuevas <- setdiff(comunas_curr, comunas_prev)
        if (length(comunas_nuevas) == 0) return(NULL)  # No hay nuevas comunas
        
        # Crear todas las combinaciones posibles de "edges" (conexiones)
        # entre comunas origen (donde ya existía el nombre) y comunas destino (nuevas)
        edges <- CJ(source = comunas_prev, target = comunas_nuevas)
        edges[, `:=`(nombre_final = nm, year = year)]
        
        # Añadir proporciones de comunas origen
        edges <- merge(edges, datos_prev, 
                       by.x = "source", by.y = "comuna")
        # Añadir proporciones de comunas destino
        edges <- merge(edges, datos_curr, 
                       by.x = "target", by.y = "comuna")
        
        # Calcular el peso de cada conexión
        # La fórmula sqrt(prop_source * prop_target) asigna más peso cuando
        # el nombre es popular tanto en la comuna origen como en la destino
        edges[, weighted := sqrt(prop_source * prop_target)]
        
        edges
      }))
      
      pb_batch_anos$tick()
      batch_results_year[[idx_year]] <- diffusion_by_year
    }
    
    # Combinar resultados de todos los años en este lote
    batch_results <- rbindlist(batch_results_year, use.names = TRUE, fill = TRUE)
    if (is.null(batch_results) || nrow(batch_results) == 0) {
      pb_batches$tick()
      all_results[[i]] <- NULL
      next
    }
    
    # Añadir información sobre primera aparición
    combined <- merge(batch_results, first_appearances, 
                      by = "nombre_final", all.x = TRUE)
    # Solo mantener difusiones posteriores a la primera aparición
    combined <- combined[year >= first_year]
    
    #---------------------------------------------------------------------------
    # 4. CÁLCULO DE MÉTRICAS DE DIFUSIÓN
    #---------------------------------------------------------------------------
    # Para cada combinación de (source, target, year), calculamos:
    local_res <- combined[
      , .(
        # 1. Métricas secuenciales: consideran todas las difusiones
        # - sequential_count: número de conexiones
        sequential_count         = .N,
        # - sequential_weighted_sum: suma de pesos de todas las conexiones
        sequential_weighted_sum  = sum(weighted, na.rm = TRUE),
        # - sequential_weighted_mean: promedio de pesos
        sequential_weighted_mean = mean(weighted, na.rm = TRUE),
        
        # 2. Métricas first_rank: consideran solo difusiones desde la comuna
        # donde el nombre apareció por primera vez (first_comuna)
        # - first_rank_count: número de conexiones desde first_comuna
        first_rank_count = sum(first_comuna == source),
        # - first_rank_weighted_sum: suma ponderada desde first_comuna
        first_rank_weighted_sum = sum(
          weighted * (first_comuna == source) * (first_cantidad / total_source),
          na.rm = TRUE
        ),
        # - first_rank_weighted_mean: promedio ponderado desde first_comuna
        first_rank_weighted_mean = mean(
          weighted * (first_comuna == source) * (first_cantidad / total_source),
          na.rm = TRUE
        )
      ),
      by = .(source, target, year)
    ]
    
    all_results[[i]] <- local_res
    pb_batches$tick()
    
    # Limpiar variables para liberar memoria
    rm(dt_batch, batch_results, batch_results_year, local_res)
    gc()
  }
  
  #-----------------------------------------------------------------------------
  # 5. COMBINACIÓN DE RESULTADOS FINALES
  #-----------------------------------------------------------------------------
  message("\nCombinando resultados finales...")
  final_results <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  if (is.null(final_results) || nrow(final_results) == 0) {
    message("No se generaron resultados en la difusión.")
    return(NULL)
  }
  
  # Agregamos por (source, target, year) para consolidar resultados
  # de diferentes lotes que puedan tener las mismas combinaciones
  final_results <- final_results[
    , .(
      # Suma de conteos
      sequential_count         = sum(sequential_count, na.rm = TRUE),
      sequential_weighted_sum  = sum(sequential_weighted_sum, na.rm = TRUE),
      # Promedio ponderado (suma de productos / suma de conteos)
      sequential_weighted_mean = fifelse(
        sum(sequential_count) == 0,
        0,
        sum(sequential_weighted_sum) / sum(sequential_count)
      ),
      
      # Igual lógica para métricas first_rank
      first_rank_count         = sum(first_rank_count, na.rm = TRUE),
      first_rank_weighted_sum  = sum(first_rank_weighted_sum, na.rm = TRUE),
      first_rank_weighted_mean = fifelse(
        sum(first_rank_count) == 0,
        0,
        sum(first_rank_weighted_sum) / sum(first_rank_count)
      )
    ),
    by = .(source, target, year)
  ]
  
  # Reemplazar posibles NA por 0 en columnas numéricas
  num_cols <- c(
    "sequential_count", "sequential_weighted_sum", "sequential_weighted_mean",
    "first_rank_count", "first_rank_weighted_sum", "first_rank_weighted_mean"
  )
  for (col_ in num_cols) {
    final_results[is.na(get(col_)), (col_) := 0]
  }
  
  # Mostrar resumen de resultados
  message("\nResumen del dataset final:")
  print(final_results[
    , .(
      total_edges = .N,  # Total de conexiones (source, target, year)
      mean_sequential = mean(sequential_count),  # Promedio de sequential_count
      mean_first_rank = mean(first_rank_count)   # Promedio de first_rank_count
    )
  ])
  
  return(final_results)
}

###############################################################################
# Ejemplo de uso de create_diffusion_dataset:
###############################################################################
result <- create_diffusion_dataset(
  df = nombres_procesados,  # dataset con nombre_final, ano, comuna, cantidad
  batch_size = 500          # procesar 500 nombres por lote
)

# Guardar resultados
save(result, file = "/home/rober/Escritorio/diffusion_dataset_p.RData")

# Limpiar memoria
gc()

###############################################################################
# Función create_window_dataset
#
# PROPÓSITO:
# A diferencia de la función anterior que analiza la difusión año a año,
# esta función analiza la difusión en ventanas temporales más amplias
# (por ejemplo, 10 o 20 años). Esto permite capturar patrones de difusión
# que ocurren a más largo plazo.
#
# PARÁMETROS:
# - dt: DataFrame con los datos de nombres
# - window_size: Tamaño de la ventana temporal (en años)
# - start_year: Año inicial para análisis
# - end_year: Año final para análisis
#
# PROCESO GENERAL:
# 1. Calcular proporciones de nombres respecto al total
# 2. Identificar primera aparición de cada nombre
# 3. Analizar difusión en cada ventana temporal
# 4. Calcular métricas para cada ventana
#
# RESULTADO:
# Un data.table similar al de la función anterior, pero con nombres de
# columnas que incluyen el tamaño de la ventana como sufijo (ejemplo: "_10")
###############################################################################
create_window_dataset <- function(
    dt,
    window_size = 10,
    start_year  = 1955,
    end_year    = 2014
) {
  library(data.table)
  library(progress)
  
  # Asegurar que dt sea un data.table
  dt <- as.data.table(dt)
  
  #-----------------------------------------------------------------------------
  # 1. CÁLCULO DE PROPORCIONES INICIALES
  #-----------------------------------------------------------------------------
  # Calcular totales globales por (ano, comuna) antes de aplicar filtros
  # Esto garantiza proporciones consistentes
  dtTotals <- dt[
    !is.na(ano) & !is.na(comuna) & !is.na(cantidad),
    .(total_comuna_global = sum(cantidad, na.rm = TRUE)),
    by = .(ano, comuna)
  ]
  
  # Añadir total global a cada fila
  dt <- merge(dt, dtTotals, by = c("ano", "comuna"), all.x = TRUE)
  
  # Calcular proporción real de cada nombre
  dt[, prop_real := cantidad / total_comuna_global]
  
  # Filtrar filas con NA en columnas clave
  dt <- dt[
    !is.na(nombre_final) & !is.na(ano) & !is.na(comuna) & !is.na(cantidad)
  ]
  
  # Crear sufijo para nombres de columnas
  suffix <- paste0("_", window_size)
  
  #-----------------------------------------------------------------------------
  # 2. IDENTIFICACIÓN DE PRIMERA APARICIÓN
  #-----------------------------------------------------------------------------
  # Para cada nombre, identificar cuándo y dónde apareció por primera vez
  first_appearances <- dt[
    , .SD[which.min(ano)], by = nombre_final
  ][
    , .(nombre_final, first_comuna = comuna, first_year = ano, first_cantidad = cantidad)
  ]
  
  #-----------------------------------------------------------------------------
  # 3. PROCESAMIENTO POR VENTANAS TEMPORALES
  #-----------------------------------------------------------------------------
  # Calcular número de ventanas a procesar
  total_diadas <- end_year - start_year - window_size + 1
  if (total_diadas < 1) {
    stop("Rango de años o window_size inválido; no se pueden formar ventanas.")
  }
  
  message(sprintf(
    "Se calcularán %d ventanas (%d años → %d años).",
    total_diadas, start_year, end_year
  ))
  
  # Barra de progreso
  pb <- progress_bar$new(
    format = "Ventana [:bar] :percent | :current/:total | ETA: :eta",
    total = total_diadas,
    clear = FALSE
  )
  
  # Lista para almacenar resultados de cada ventana
  all_results <- vector("list", total_diadas)
  
  # Procesar cada ventana temporal
  for (i in seq_len(total_diadas)) {
    # Definir años en los extremos de la ventana
    year_origin   <- start_year + (i - 1)          # Año inicial
    year_adoption <- year_origin + window_size     # Año final (separado por window_size)
    
    # Filtrar datos para los años en los extremos de la ventana
    data_origin   <- dt[ano == year_origin]        # Datos del año inicial
    data_adoption <- dt[ano == year_adoption]      # Datos del año final
    
    # Calcular totales por comuna en cada año
    # (Necesario para métricas first_rank_*)
    totales_origen   <- data_origin[,   .(total_comuna = sum(cantidad)), by = comuna]
    totales_adopcion <- data_adoption[, .(total_comuna = sum(cantidad)), by = comuna]
    
    # Identificar nombres que aparecen en ambos extremos de la ventana
    nombres_comunes <- intersect(
      unique(data_origin$nombre_final),
      unique(data_adoption$nombre_final)
    )
    
    # Si no hay nombres comunes, pasar a la siguiente ventana
    if (length(nombres_comunes) == 0) {
      pb$tick()
      next
    }
    
    # Lista para almacenar difusiones en esta ventana
    all_diffusions <- vector("list", length(nombres_comunes))
    
    # Analizar cada nombre por separado
    for (j in seq_along(nombres_comunes)) {
      nm <- nombres_comunes[j]
      
      # Datos del año origen para este nombre, con totales
      datos_origen_nm <- merge(
        data_origin[nombre_final == nm],
        totales_origen,
        by = "comuna",
        suffixes = c("", "_tot")
      )[
        # Usar la proporción global ya calculada
        , `:=`(
          prop_source = prop_real,      # Proporción en comuna origen
          total_source = total_comuna   # Total en comuna origen (para first_rank_*)
        )
      ]
      
      # Datos del año adopción para este nombre, con totales
      datos_adop_nm <- merge(
        data_adoption[nombre_final == nm],
        totales_adopcion,
        by = "comuna",
        suffixes = c("", "_tot")
      )[
        , `:=`(
          prop_target = prop_real       # Proporción en comuna destino
          # No necesitamos total_source aquí
        )
      ]
      
      # Identificar comunas en cada extremo de la ventana
      comunas_origen   <- unique(datos_origen_nm$comuna)
      comunas_adopcion <- unique(datos_adop_nm$comuna)
      
      # Encontrar comunas donde el nombre aparece por primera vez en esta ventana
      nuevas_comunas <- setdiff(comunas_adopcion, comunas_origen)
      if (length(nuevas_comunas) == 0) {
        next  # No hay nuevas comunas
      }
      
      # Crear todas las combinaciones de conexiones entre comunas
      edges <- CJ(source = comunas_origen, target = nuevas_comunas)
      edges[, nombre_final := nm]
      
      # Añadir datos de comunas origen
      edges <- merge(
        edges,
        datos_origen_nm[, .(comuna, prop_source, total_source)],
        by.x = "source", by.y = "comuna"
      )
      
      # Añadir datos de comunas destino
      edges <- merge(
        edges,
        datos_adop_nm[, .(comuna, prop_target)],
        by.x = "target", by.y = "comuna"
      )
      
      # Calcular peso de cada conexión
      edges[, weighted := sqrt(prop_source * prop_target)]
      
      # Añadir información de primera aparición
      fa <- first_appearances[nombre_final == nm]
      if (nrow(fa) == 1) {
        edges[, `:=`(
          first_comuna   = fa$first_comuna,
          first_year     = fa$first_year,
          first_cantidad = fa$first_cantidad
        )]
      }
      
      # Solo considerar difusiones donde el año origen es posterior a la primera aparición
      edges <- edges[year_origin >= first_year]
      
      if (nrow(edges) > 0) {
        all_diffusions[[j]] <- edges
      }
    }
    
    # Combinar difusiones de todos los nombres en esta ventana
    window_diffusions <- rbindlist(all_diffusions, use.names = TRUE, fill = TRUE)
    if (!is.null(window_diffusions) && nrow(window_diffusions) > 0) {
      
      #-------------------------------------------------------------------------
      # 4. CÁLCULO DE MÉTRICAS DE DIFUSIÓN
      #-------------------------------------------------------------------------
      # Calcular las 6 métricas de difusión
      res <- window_diffusions[
        , .(
          # Métricas secuenciales
          sequential_count       = .N,
          sequential_weighted_sum   = sum(weighted, na.rm = TRUE),
          sequential_weighted_mean  = mean(weighted, na.rm = TRUE),
          
          # Métricas first_rank
          first_rank_count          = sum(first_comuna == source, na.rm = TRUE),
          first_rank_weighted_sum   = sum(
            weighted * (first_comuna == source) * (first_cantidad / total_source),
            na.rm = TRUE
          ),
          first_rank_weighted_mean  = mean(
            weighted * (first_comuna == source) * (first_cantidad / total_source),
            na.rm = TRUE
          )
        ),
        by = .(source, target)
      ]
      
      # Añadir año de adopción (para identificar la ventana)
      res[, year := year_adoption]
      
      # Renombrar columnas con sufijo que indica tamaño de ventana
      setnames(
        res,
        old = c(
          "sequential_count", 
          "sequential_weighted_sum", 
          "sequential_weighted_mean",
          "first_rank_count", 
          "first_rank_weighted_sum", 
          "first_rank_weighted_mean"
        ),
        new = c(
          paste0("sequential_count", suffix),
          paste0("sequential_weighted_sum", suffix),
          paste0("sequential_weighted_mean", suffix),
          paste0("first_rank_count", suffix),
          paste0("first_rank_weighted_sum", suffix),
          paste0("first_rank_weighted_mean", suffix)
        )
      )
      
      all_results[[i]] <- res
    }
    
    pb$tick()
    
    # Limpiar memoria
    rm(data_origin, data_adoption, all_diffusions)
    gc()
  }
  
  # Combinar resultados de todas las ventanas
  final_result <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  # Ordenar por año, comuna origen y comuna destino
  setorder(final_result, year, source, target)
  
  # Mostrar estadísticas finales
  message(sprintf(
    "\n=== Estadísticas finales (ventana %d años) ===", window_size
  ))
  if (nrow(final_result) > 0) {
    message(sprintf("Total de díadas en el resultado: %d", nrow(final_result)))
  } else {
    message("No se generaron difusiones para esta ventana.")
  }
  
  return(final_result)
}

# Limpiar memoria
gc()

###############################################################################
# EJEMPLO DE USO
###############################################################################

# Versión para ventanas de 10 años
result_10 <- create_window_dataset(
  nombres_procesados,
  window_size = 10,   # Ventana de 10 años
  start_year = 1956,  # Año inicial
  end_year = 2024     # Año final
)

# Guardar resultados
save(result_10, file = "/home/rober/Escritorio/diffusion_dataset_10_p.RData")

# Versión para ventanas de 20 años
gc()  # Limpiar memoria
result_20 <- create_window_dataset(
  nombres_procesados,
  window_size = 20,   # Ventana de 20 años
  start_year = 1956,  # Año inicial
  end_year = 2024     # Año final
)

# Guardar resultados
save(result_20, file = "/home/rober/Escritorio/diffusion_dataset_20_p.RData")

# Combinar todos los resultados en un solo dataset
library(tidyverse)

diffusion_data <- result %>%      # Resultados secuenciales
  full_join(result_10,            # Resultados ventana 10 años
            by = c("source", "target", "year")) %>%
  full_join(result_20,            # Resultados ventana 20 años
            by = c("source", "target", "year"))

# Ver estructura del resultado final
glimpse(diffusion_data)

# Guardar resultados combinados
save(diffusion_data, file = "/home/rober/Escritorio/diffusion_data_r.RData")