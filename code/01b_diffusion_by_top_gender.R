###############################################################################
# Cargar librerías necesarias
###############################################################################
library(data.table)
library(progress)

###############################################################################
# Función create_diffusion_dataset adaptada para top_nombres_fem
# Mantiene la estructura original con cambios mínimos
###############################################################################
create_diffusion_dataset <- function(df, batch_size = 500) {
  
  message("Preparando datos...")
  
  # Asegurar data.table
  dt <- as.data.table(df)
  
  # Renombrar columnas para ajustar al código original
  setnames(dt, "total_cantidad", "cantidad")
  
  # Filtrar filas con NA en columnas clave
  dt <- dt[
    !is.na(nombre_final) & !is.na(decada) & 
      !is.na(comuna) & !is.na(cantidad)
  ]
  
  # Definir llave principal
  setkey(dt, nombre_final, decada, comuna)
  
  # 1) Crear tabla de totales por (decada, comuna) con TODOS los nombres
  dtTotals <- dt[, .(total_comuna_all = sum(cantidad)), by = .(decada, comuna)]
  
  # Rango de décadas 
  decadas <- sort(unique(dt$decada))
  
  message("\nIdentificando nombres con difusión...")
  
  # Barra de progreso para décadas
  pb_decadas <- progress_bar$new(
    format = "Décadas [:bar] :percent | :current/:total | ETA: :eta",
    total = length(decadas) - 1  # -1 porque necesitamos pares de décadas consecutivas
  )
  
  # 2) Encontrar nombres con difusión (en alguna década)
  nombres_difusion <- rbindlist(lapply(seq_len(length(decadas)-1), function(i) {
    decada_origen <- decadas[i]
    decada_adopcion <- decadas[i+1]
    
    # Filtrar dt para esas 2 décadas
    result <- dt[decada %in% c(decada_origen, decada_adopcion), {
      comunas_origen   <- unique(comuna[decada == decada_origen])
      comunas_destino  <- unique(comuna[decada == decada_adopcion])
      hay_difusion <- length(comunas_origen) > 0 &&
        !all(comunas_destino %in% comunas_origen)
      .(hay_difusion = hay_difusion)
    }, by = nombre_final][hay_difusion == TRUE, .(nombre_final)]
    
    pb_decadas$tick()
    result
  }))
  
  # Lista final de nombres difusores
  nombres_difusion <- unique(nombres_difusion$nombre_final)
  message(sprintf("\nTotal nombres que muestran difusión: %d", length(nombres_difusion)))
  
  if (length(nombres_difusion) == 0) {
    message("No hay nombres con difusión.")
    return(NULL)
  }
  
  # Dividir la lista de nombres en batches
  batches <- split(nombres_difusion, ceiling(seq_along(nombres_difusion)/batch_size))
  n_batches <- length(batches)
  
  message(sprintf("\nProcesando %d nombres en %d batches", length(nombres_difusion), n_batches))
  
  pb_batches <- progress_bar$new(
    format = "Batches [:bar] :percent | Batch :current/:total | ETA: :eta",
    total = n_batches
  )
  
  # Lista de resultados
  all_results <- vector("list", n_batches)
  
  for (i in seq_len(n_batches)) {
    # Filtrar dt para los nombres del batch
    dt_batch <- dt[nombre_final %in% batches[[i]]]
    
    # Merge con dtTotals para obtener total_comuna_all y calcular prop_real
    dt_batch <- merge(
      dt_batch,
      dtTotals,
      by = c("decada", "comuna"),
      all.x = TRUE
    )
    
    # prop_real = cantidad / total_comuna_all
    dt_batch[, prop_real := cantidad / total_comuna_all]
    
    # Identificar primera aparición global en este batch
    first_appearances <- dt_batch[
      , .(
        first_decada    = min(decada),
        first_comuna    = comuna[which.min(decada)],
        first_cantidad  = cantidad[which.min(decada)]
      ),
      by = nombre_final
    ]
    
    # Barra de progreso para décadas
    pb_batch_decadas <- progress_bar$new(
      format = "  Décadas del batch [:bar] :percent | :current/:total",
      total = length(decadas) - 1,
      clear = FALSE,
      width = 60
    )
    
    batch_results_decada <- vector("list", length(decadas) - 1)
    
    for (idx_decada in seq_len(length(decadas) - 1)) {
      decada <- decadas[idx_decada + 1]
      decada_prev <- decadas[idx_decada]
      
      data_prev <- dt_batch[decada == decada_prev]
      data_curr <- dt_batch[decada == decada]
      
      if (nrow(data_prev) == 0 || nrow(data_curr) == 0) {
        pb_batch_decadas$tick()
        next
      }
      
      # Para cada nombre en data_prev
      diffusion_by_decada <- rbindlist(lapply(unique(data_prev$nombre_final), function(nm) {
        
        datos_prev <- data_prev[nombre_final == nm,
                                .(comuna, prop_source = prop_real, 
                                  total_source = total_comuna_all)]
        
        datos_curr <- data_curr[nombre_final == nm,
                                .(comuna, prop_target = prop_real)]
        
        comunas_prev <- unique(datos_prev$comuna)
        comunas_curr <- unique(datos_curr$comuna)
        
        comunas_nuevas <- setdiff(comunas_curr, comunas_prev)
        if (length(comunas_nuevas) == 0) return(NULL)
        
        # Habilitamos cartesiano para mantener el código original
        # Esto es equivalente a CJ() pero con allow.cartesian=TRUE
        edges <- data.table::CJ(source = comunas_prev, target = comunas_nuevas, 
                                allow.cartesian=TRUE)
        
        edges[, `:=`(nombre_final = nm, decada = decada)]
        
        # Merge con prop_source
        edges <- merge(edges, datos_prev, 
                       by.x = "source", by.y = "comuna")
        # Merge con prop_target
        edges <- merge(edges, datos_curr, 
                       by.x = "target", by.y = "comuna")
        
        # weighted = sqrt(prop_source * prop_target)
        edges[, weighted := sqrt(prop_source * prop_target)]
        
        edges
      }))
      
      pb_batch_decadas$tick()
      batch_results_decada[[idx_decada]] <- diffusion_by_decada
    }
    
    # Combinar los resultados del batch
    batch_results <- rbindlist(batch_results_decada, use.names = TRUE, fill = TRUE)
    if (is.null(batch_results) || nrow(batch_results) == 0) {
      pb_batches$tick()
      all_results[[i]] <- NULL
      next
    }
    
    # Merge con las primeras apariciones
    combined <- merge(batch_results, first_appearances, 
                      by = "nombre_final", all.x = TRUE)
    combined <- combined[decada >= first_decada]
    
    # Calcular las 6 métricas
    local_res <- combined[
      , .(
        sequential_count         = .N,
        sequential_weighted_sum  = sum(weighted, na.rm = TRUE),
        sequential_weighted_mean = mean(weighted, na.rm = TRUE),
        
        first_rank_count = sum(first_comuna == source),
        first_rank_weighted_sum = sum(
          weighted * (first_comuna == source) * (first_cantidad / total_source),
          na.rm = TRUE
        ),
        first_rank_weighted_mean = mean(
          weighted * (first_comuna == source) * (first_cantidad / total_source),
          na.rm = TRUE
        )
      ),
      by = .(source, target, decada)
    ]
    
    all_results[[i]] <- local_res
    pb_batches$tick()
    
    rm(dt_batch, batch_results, batch_results_decada, local_res)
    gc()
  }
  
  # Unificar todos los resultados
  message("\nCombinando resultados finales...")
  final_results <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  if (is.null(final_results) || nrow(final_results) == 0) {
    message("No se generaron resultados en la difusión.")
    return(NULL)
  }
  
  final_results <- final_results[
    , .(
      sequential_count         = sum(sequential_count, na.rm = TRUE),
      sequential_weighted_sum  = sum(sequential_weighted_sum, na.rm = TRUE),
      sequential_weighted_mean = fifelse(
        sum(sequential_count) == 0,
        0,
        sum(sequential_weighted_sum) / sum(sequential_count)
      ),
      
      first_rank_count         = sum(first_rank_count, na.rm = TRUE),
      first_rank_weighted_sum  = sum(first_rank_weighted_sum, na.rm = TRUE),
      first_rank_weighted_mean = fifelse(
        sum(first_rank_count) == 0,
        0,
        sum(first_rank_weighted_sum) / sum(first_rank_count)
      )
    ),
    by = .(source, target, decada)
  ]
  
  # Reemplazar posibles NA por 0
  num_cols <- c(
    "sequential_count", "sequential_weighted_sum", "sequential_weighted_mean",
    "first_rank_count", "first_rank_weighted_sum", "first_rank_weighted_mean"
  )
  for (col_ in num_cols) {
    final_results[is.na(get(col_)), (col_) := 0]
  }
  
  # Resumen
  message("\nResumen del dataset final:")
  print(final_results[
    , .(
      total_edges = .N,
      mean_sequential = mean(sequential_count),
      mean_first_rank = mean(first_rank_count)
    )
  ])
  
  return(final_results)
}

###############################################################################
# Ejemplo de uso:
###############################################################################

# Habilitar productos cartesianos grandes en configuración global
options(datatable.allow.cartesian=TRUE)
rm(result)
gc()

result_mas <- create_diffusion_dataset(
  df = top_nombres_mas,  # tu dataset
  batch_size = 1000      # ajustado para manejar más nombres por batch
)

result_fem <- create_diffusion_dataset(
  df = top_nombres_fem,  # tu dataset
  batch_size = 1000      # ajustado para manejar más nombres por batch
)

# Guardar resultados
save(result_fem, file = "C:/Users/qramo/Desktop/diffusion_dataset_fem.RData")
save(result_mas, file = "C:/Users/qramo/Desktop/diffusion_dataset_mas.RData")


