#==============================================================================
# Agregar atributos a los datos de difusión de nombres
#==============================================================================
# Este script procesa los resultados del análisis de difusión de nombres 
# entre comunas (municipalidades) de Chile, añadiendo variables contextuales
# sociodemográficas y geográficas.
#
# NOTA IMPORTANTE:
# Este código se ejecuta DESPUÉS de haber construido el dataset de difusión
# mediante las funciones create_diffusion_dataset y create_window_dataset
# definidas en los códigos anteriores. Se asume que ya existen los siguientes 
# objetos:
#   - result: Resultados generales de difusión (todos los nombres)
#   - result_mas: Resultados de difusión para nombres masculinos
#   - result_fem: Resultados de difusión para nombres femeninos
#   - attr_comunas: Dataset con atributos socioeconómicos y geográficos de comunas
#==============================================================================

#------------------------------------------------------------------------------
# 1. CONFIGURACIÓN INICIAL
#------------------------------------------------------------------------------
# Cargar paquetes necesarios
library(tidyverse)      # Para manipulación y visualización de datos
library(geosphere)      # Para cálculo de distancias geográficas
library(stringi)        # Para procesamiento de texto

# Configurar opciones globales
options(scipen = 999)   # Desactivar notación científica

#------------------------------------------------------------------------------
# 2. NORMALIZACIÓN DE NOMBRES DE COMUNAS
#------------------------------------------------------------------------------
# Normalizar caracteres especiales en nombres de comunas para facilitar el join
# entre diferentes conjuntos de datos

# Para nombres femeninos
result_fem$source = stri_trans_general(str = result_fem$source, id = "Latin-ASCII")
result_fem$target = stri_trans_general(str = result_fem$target, id = "Latin-ASCII")

# Para nombres masculinos
result_mas$source = stri_trans_general(str = result_mas$source, id = "Latin-ASCII")
result_mas$target = stri_trans_general(str = result_mas$target, id = "Latin-ASCII")

# También se podría normalizar result si se necesita
# result$source = stri_trans_general(str = result$source, id = "Latin-ASCII")
# result$target = stri_trans_general(str = result$target, id = "Latin-ASCII")

# Verificar estructura de datos
glimpse(result_fem)
glimpse(result_mas)

#------------------------------------------------------------------------------
# 3. FUNCIONES AUXILIARES PARA MAPEO DE AÑOS
#------------------------------------------------------------------------------
# Esta sección contiene funciones para resolver el problema de datos 
# faltantes en ciertos años, encontrando el año más cercano disponible.

#' Encuentra el año más cercano disponible dentro de una distancia máxima
#' 
#' @param target_year Año objetivo que necesitamos
#' @param available_years Vector de años disponibles
#' @param max_distance Distancia máxima permitida en años
#' @return El año disponible más cercano o NA si no hay ninguno dentro de la distancia máxima
find_nearest_year <- function(target_year, available_years, max_distance = 10) {
  differences <- abs(available_years - target_year)  # Calcular distancias absolutas
  min_diff <- min(differences)                       # Encontrar distancia mínima
  
  # Devolver año más cercano si está dentro de la distancia máxima
  if(min_diff <= max_distance) {
    return(available_years[which.min(differences)])
  } else {
    return(NA)  # No hay año cercano dentro del rango aceptable
  }
}

#' Crea un mapeo entre los años necesarios para cada comuna y los años disponibles
#' 
#' @param data_attr Dataset con atributos de comunas (contiene los años disponibles)
#' @param data_diff Dataset de difusión (contiene los años necesarios)
#' @param max_distance Distancia máxima permitida en años
#' @return DataFrame con el mapeo de años por comuna
create_year_mapping <- function(data_attr, data_diff, max_distance = 10) {
  # Obtener todas las comunas únicas del dataset de difusión
  comunas <- unique(c(unique(data_diff$source), unique(data_diff$target)))
  
  # Para cada comuna, crear un mapeo de años
  map_df <- map_dfr(comunas, function(comuna) {
    # Años disponibles para esta comuna en el dataset de atributos
    years_available <- data_attr %>%
      filter(comuna == !!comuna) %>%
      pull(ano)
    
    # Años necesarios para esta comuna en el dataset de difusión
    years_needed <- data_diff %>%
      filter(source == !!comuna | target == !!comuna) %>%
      pull(decada) %>%   # Usamos 'decada' como referencia temporal
      unique()
    
    # Para cada año necesario, encontrar el más cercano disponible
    map_dfr(years_needed, function(year) {
      nearest <- find_nearest_year(year, years_available, max_distance)
      tibble(
        comuna = comuna,
        year_needed = year,
        year_available = nearest
      )
    })
  })
  
  return(map_df)
}

#------------------------------------------------------------------------------
# 4. PROCESAMIENTO DE DATOS 
#------------------------------------------------------------------------------
# A continuación se procesarán los datos para nombres masculinos y femeninos
# En este ejemplo se muestra el procesamiento para nombres masculinos (result_mas)
# El mismo proceso puede aplicarse para nombres femeninos (result_fem)

# 4.1 PROCESAMIENTO PARA NOMBRES MASCULINOS
#-----------------------------------------
# Crear mapeo de años para nombres masculinos
year_mapping_mas <- create_year_mapping(attr_comunas, result_mas, max_distance = 10)

# Construir dataset final con variables diádicas para nombres masculinos
resultado_final_mas <- result_mas %>%
  # Unir con el mapeo para comuna de origen
  left_join(
    year_mapping_mas %>% 
      rename(decada_source = year_needed,
             year_available_source = year_available),
    by = c("source" = "comuna", "decada" = "decada_source")
  ) %>%
  # Unir con el mapeo para comuna de destino
  left_join(
    year_mapping_mas %>% 
      rename(decada_target = year_needed,
             year_available_target = year_available),
    by = c("target" = "comuna", "decada" = "decada_target")
  ) %>%
  # Unir con atributos para comuna de origen
  left_join(
    attr_comunas %>% 
      select(ano, comuna, median_prom_isei, poblacion, latitud, longitud),
    by = c("source" = "comuna", "year_available_source" = "ano")
  ) %>%
  # Unir con atributos para comuna de destino
  left_join(
    attr_comunas %>% 
      select(ano, comuna, median_prom_isei, poblacion, latitud, longitud),
    by = c("target" = "comuna", "year_available_target" = "ano"),
    suffix = c("_source", "_target")
  ) %>%
  # Calcular variables diádicas
  mutate(
    # 1. Diferencia de estatus socioeconómico (ISEI) entre origen y destino
    status_diff = median_prom_isei_source - median_prom_isei_target,
    
    # 2. Proximidad geográfica (inversa de la distancia en km)
    # Se calcula como 1/distancia para que valores más altos indiquen mayor proximidad
    geo_prox = case_when(
      !is.na(latitud_source) & !is.na(latitud_target) ~ 
        1/distGeo(
          cbind(longitud_source, latitud_source),
          cbind(longitud_target, latitud_target)
        ),
      TRUE ~ NA_real_  # Si faltan coordenadas, devolver NA
    ),
    
    # 3. Diferencia poblacional entre origen y destino
    pop_diff = poblacion_source - poblacion_target,
    
    # Variables de control para diagnóstico de calidad del matching temporal
    years_diff_source = decada - year_available_source,  # Diferencia entre año necesario y disponible (origen)
    years_diff_target = decada - year_available_target   # Diferencia entre año necesario y disponible (destino)
  )

# 4.2 PROCESAMIENTO PARA NOMBRES FEMENINOS (similar al masculino)
#--------------------------------------------------------------
# Se puede implementar el mismo proceso para nombres femeninos si es necesario
# year_mapping_fem <- create_year_mapping(attr_comunas, result_fem, max_distance = 10)
# resultado_final_fem <- result_fem %>% ... [código similar al anterior]

# Revisar estructura del resultado
glimpse(resultado_final_mas)
# Si se procesaron datos femeninos: glimpse(resultado_final_fem)

# Limpiar variables temporales para liberar memoria
rm(year_mapping_mas)
# Si existe: rm(year_mapping_fem)
# Si no son necesarios en análisis posteriores:
# rm(diffusion_data, attr_comunas)
gc()

#------------------------------------------------------------------------------
# 5. ANÁLISIS ESTADÍSTICO DESCRIPTIVO
#------------------------------------------------------------------------------
# A continuación se realizará un análisis estadístico para los nombres masculinos
# Un proceso similar podría aplicarse para los nombres femeninos

# Crear una lista para almacenar múltiples resultados estadísticos
summary_stats <- list()

# 5.1 Estadísticas generales de cobertura
summary_stats$cobertura <- resultado_final_mas %>%
  summarize(
    n_total_diadas = n(),                       # Número total de díadas
    n_años_unicos = n_distinct(year),           # Número de años únicos
    n_comunas_source = n_distinct(source),      # Número de comunas origen
    n_comunas_target = n_distinct(target),      # Número de comunas destino
    # Proporción de díadas con datos completos en las variables clave
    prop_completos = mean(!is.na(status_diff) & !is.na(geo_prox) & !is.na(pop_diff))
  )

# 5.2 Estadísticas sobre distancia temporal en el matching
summary_stats$distancia_temporal <- resultado_final_mas %>%
  summarize(
    # Estadísticas para comunas origen
    mean_gap_source = mean(abs(years_diff_source), na.rm = TRUE),    # Media
    median_gap_source = median(abs(years_diff_source), na.rm = TRUE), # Mediana
    sd_gap_source = sd(abs(years_diff_source), na.rm = TRUE),        # Desviación estándar
    max_gap_source = max(abs(years_diff_source), na.rm = TRUE),      # Máximo
    
    # Estadísticas para comunas destino
    mean_gap_target = mean(abs(years_diff_target), na.rm = TRUE),    # Media
    median_gap_target = median(abs(years_diff_target), na.rm = TRUE), # Mediana
    sd_gap_target = sd(abs(years_diff_target), na.rm = TRUE),        # Desviación estándar
    max_gap_target = max(abs(years_diff_target), na.rm = TRUE)       # Máximo
  )

# 5.3 Estadísticas descriptivas de las variables diádicas
summary_stats$variables_diadicas <- resultado_final_mas %>%
  summarize(
    # Diferencia de estatus
    mean_status_diff = mean(status_diff, na.rm = TRUE),          # Media
    median_status_diff = median(status_diff, na.rm = TRUE),      # Mediana
    sd_status_diff = sd(status_diff, na.rm = TRUE),              # Desviación estándar
    min_status_diff = min(status_diff, na.rm = TRUE),            # Mínimo
    max_status_diff = max(status_diff, na.rm = TRUE),            # Máximo
    
    # Proximidad geográfica
    mean_geo_prox = mean(geo_prox, na.rm = TRUE),                # Media
    median_geo_prox = median(geo_prox, na.rm = TRUE),            # Mediana
    sd_geo_prox = sd(geo_prox, na.rm = TRUE),                    # Desviación estándar
    min_geo_prox = min(geo_prox, na.rm = TRUE),                  # Mínimo
    max_geo_prox = max(geo_prox, na.rm = TRUE),                  # Máximo
    
    # Diferencia poblacional
    mean_pop_diff = mean(pop_diff, na.rm = TRUE),                # Media
    median_pop_diff = median(pop_diff, na.rm = TRUE),            # Mediana
    sd_pop_diff = sd(pop_diff, na.rm = TRUE),                    # Desviación estándar
    min_pop_diff = min(pop_diff, na.rm = TRUE),                  # Mínimo
    max_pop_diff = max(pop_diff, na.rm = TRUE)                   # Máximo
  )

# 5.4 Análisis de valores faltantes por década
summary_stats$missing_por_decada <- resultado_final_mas %>%
  mutate(decada = floor(year/10)*10) %>%  # Crear variable de década
  group_by(decada) %>%
  summarize(
    n_diadas = n(),                             # Número de díadas por década
    prop_na_status = mean(is.na(status_diff)),  # Proporción de NAs en status_diff
    prop_na_geo = mean(is.na(geo_prox)),        # Proporción de NAs en geo_prox
    prop_na_pop = mean(is.na(pop_diff))         # Proporción de NAs en pop_diff
  )

# 5.5 Matriz de correlaciones entre variables diádicas
summary_stats$correlaciones <- cor(
  resultado_final_mas %>%
    select(status_diff, geo_prox, pop_diff),
  use = "complete.obs"  # Usar solo casos completos
)

# 5.6 Top 10 díadas más frecuentes
summary_stats$top_diadas <- resultado_final_mas %>%
  count(source, target, sort = TRUE) %>%  # Contar frecuencia de cada par source-target
  head(10)  # Tomar las 10 más frecuentes

# Imprimir resultados
print("1. Estadísticas de Cobertura:")
print(summary_stats$cobertura)
print("\n2. Estadísticas de Distancia Temporal:")
print(summary_stats$distancia_temporal)
print("\n3. Estadísticas de Variables Diádicas:")
print(summary_stats$variables_diadicas)
print("\n4. Valores Faltantes por Década:")
print(summary_stats$missing_por_decada)
print("\n5. Correlaciones entre Variables:")
print(summary_stats$correlaciones)
print("\n6. Top 10 Díadas más Frecuentes:")
print(summary_stats$top_diadas)

#------------------------------------------------------------------------------
# 6. VISUALIZACIONES
#------------------------------------------------------------------------------
# 6.1 Distribución de diferencias temporales
p1 <- ggplot(resultado_final_mas) +
  # Histograma para comunas origen (azul)
  geom_histogram(aes(abs(years_diff_source)), bins=30, fill="blue", alpha=0.5) +
  # Histograma para comunas destino (rojo)
  geom_histogram(aes(abs(years_diff_target)), bins=30, fill="red", alpha=0.5) +
  theme_minimal() +
  labs(title="Distribución de Diferencias Temporales",
       subtitle="Azul: comunas origen | Rojo: comunas destino",
       x="Años de diferencia",
       y="Frecuencia")

# 6.2 Evolución de valores faltantes por década
p2 <- summary_stats$missing_por_decada %>%
  # Convertir datos a formato largo
  pivot_longer(cols=starts_with("prop_na"),
               names_to="variable",
               values_to="proporcion") %>%
  # Crear gráfico de líneas
  ggplot(aes(x=decada, y=proporcion, color=variable)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_color_manual(
    values = c("prop_na_status" = "blue", "prop_na_geo" = "red", "prop_na_pop" = "green"),
    labels = c("prop_na_status" = "Estatus", "prop_na_geo" = "Geografía", "prop_na_pop" = "Población")
  ) +
  labs(title="Proporción de Valores Faltantes por Década",
       x="Década",
       y="Proporción de NA's",
       color="Variable")

# Mostrar visualizaciones
print(p1)
print(p2)

#------------------------------------------------------------------------------
# 7. GUARDAR RESULTADOS
#------------------------------------------------------------------------------
# Renombrar para consistencia y guardar resultado final
# Se puede guardar el dataset masculino, femenino o ambos

# Para dataset masculino
names_diffusion_data <- resultado_final_mas

# Guardar como archivo RData para uso en R
save(names_diffusion_data, file = "/home/rober/Escritorio/names_diffusion_data.RData")

# Guardar como CSV para uso en otros programas
write_csv(resultado_final_mas, "/media/rober/4612-9FBE/names/diffusion_dataset_with_dyadic_vars.csv")

# Si se procesaron datos femeninos, también se podrían guardar:
# save(resultado_final_fem, file = "/home/rober/Escritorio/names_diffusion_data_fem.RData")
# write_csv(resultado_final_fem, "/media/rober/4612-9FBE/names/diffusion_dataset_with_dyadic_vars_fem.csv")

# Limpieza final
if(exists("ipumsi_00005")) rm(ipumsi_00005)
rm(result, result_fem, result_mas)  # Limpiar objetos grandes que ya no se necesitan
gc()