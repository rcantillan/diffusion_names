#==============================================================================
# ANÁLISIS DE DIFUSIÓN DE NOMBRES MEDIANTE MODELOS POISSON
#==============================================================================
# Este script analiza la difusión de nombres entre localizaciones (posiblemente comunas)
# utilizando modelos de regresión Poisson. Se enfoca en el efecto de la
# diferencia de estatus socioeconómico ('status_diff') entre las localizaciones
# de origen (source 's') y destino (target 't'), controlando por proximidad geográfica,
# diferencia poblacional y década.
#
# El script realiza las siguientes tareas:
# 1. Carga y prepara los datos, creando variables categóricas y estandarizadas.
# 2. Ajusta varios modelos Poisson (lineal, categórico, cuadrático).
# 3. Genera predicciones a partir de los modelos.
# 4. Visualiza los efectos marginales predichos, especialmente el de la
#    distancia de estatus, a lo largo de diferentes décadas y para subgrupos
#    (general, femenino, masculino).
#==============================================================================

#------------------------------------------------------------------------------
# 1. CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
#------------------------------------------------------------------------------
# Cargar las librerías necesarias para el análisis
library("tidyverse") 
library("modelr")    
library("broom")    
library("broom.mixed") 

gc()

#------------------------------------------------------------------------------
# 2. PREPARACIÓN DE DATOS
#------------------------------------------------------------------------------
# Esta sección carga los datos preprocesados y realiza transformaciones
# necesarias para el modelado, como la creación de variables categóricas
# basadas en la diferencia de estatus y la estandarización de predictores continuos.

# Liberar memoria antes de cargar datos
gc()

# 2.1 Carga de datos
# Se asume que los objetos 'resultado_final', 'resultado_final_fem' y 'resultado_final_mas'
# están guardados en el archivo .RData especificado.
load("/home/rober/Escritorio/names_diffusion_data.RData")

# 2.2 Vistazo inicial a los datos
glimpse(resultado_final)
glimpse(resultado_final_fem) # Ejemplo con datos femeninos

# 2.3 Exploración de la variable clave: status_diff
# Histograma para visualizar la distribución de la diferencia de estatus.
# Útil para entender el rango y la forma de la distribución antes de categorizar.
# Descomentar la siguiente línea para ejecutarla:
# resultado_final %>% with(hist(status_diff))

# 2.4 Creación de variables derivadas de 'status_diff'
# 'status_diff_cat': Variable categórica que agrupa 'status_diff' en 5 niveles.
#    Representa si el estatus de origen (s) es mucho menor (s<<t), menor (s<t),
#    similar (s≈t), mayor (s>t) o mucho mayor (s>>t) que el de destino (t).
resultado_final <- resultado_final %>%
  mutate(status_diff_cat = case_when(
    status_diff < -5                      ~ "s<<t", # Origen con estatus mucho menor que destino
    status_diff > -5 & status_diff < -0.5 ~ "s<t",  # Origen con estatus menor que destino
    status_diff > 0.5 & status_diff < 5   ~ "s>t",  # Origen con estatus mayor que destino
    status_diff > 5                       ~ "s>>t", # Origen con estatus mucho mayor que destino
    TRUE                                  ~ "s≈t"   # Estatus similar (categoría de referencia)
  )
  )

# 'status_diff_cat2': Variable numérica creada redondeando 'status_diff' a cero decimales.
#    Se usará como predictor numérico en algunos modelos.
resultado_final <- resultado_final %>%
  mutate(status_diff_cat2 = round(status_diff, 0))

# Revisar la estructura del dataframe después de añadir las nuevas variables.
glimpse(resultado_final)

# 2.5 Creación de la variable Offset (Exposición)
# 'log_exposure': Se calcula como el logaritmo del promedio de las poblaciones
#                 de origen y destino. Sirve como medida de la exposición potencial
#                 o tamaño base para la difusión. Podría usarse como offset en
#                 modelos Poisson para modelar tasas en lugar de conteos crudos.
#                 (Nota: Los modelos más adelante NO usan explícitamente este offset).
resultado_final <- resultado_final %>%
  mutate(log_exposure = log(poblacion_source + poblacion_target) / 2) # Potencialmente agregar ratio.

# 2.6 Estandarización de variables predictoras continuas
# Se estandarizan (centrar en 0, desviación estándar 1) las variables de
# proximidad geográfica, diferencia poblacional y exposición (log_exposure).
# Esto ayuda a comparar la magnitud de los coeficientes y puede mejorar la
# convergencia numérica de los modelos.
resultado_final <- resultado_final %>%
  mutate(
    geo_prox_std = as.numeric(scale(geo_prox, center = TRUE, scale = TRUE)),
    pop_diff_std = as.numeric(scale(pop_diff, center = TRUE, scale = TRUE)),
    exposure_std = as.numeric(scale(log_exposure, center = TRUE, scale = TRUE)) # Variable de exposición estandarizada
  )

# 2.7 Ajuste de niveles de la variable Factor 'status_diff_cat'
# Verificar los niveles actuales de la variable categórica creada.
levels(factor(resultado_final$status_diff_cat))

# Reordenar los niveles del factor para que 's≈t' (estatus similar) sea la
# categoría de referencia en los modelos de regresión.
resultado_final$status_diff_cat <- relevel(factor(resultado_final$status_diff_cat), ref = "s≈t")

# Verificar la estructura final del dataframe principal antes del modelado.
glimpse(resultado_final)

# NOTA IMPORTANTE: Se asume que los dataframes 'resultado_final_fem' y
# 'resultado_final_mas' también contienen las variables necesarias
# (status_diff, sequential_count, geo_prox, pop_diff, decada) y que idealmente
# deberían pasar por un preprocesamiento similar (creación de status_diff_cat2,
# estandarización de geo_prox_std, pop_diff_std) si se van a usar en modelos
# con esas variables transformadas. El código más adelante asume que estas
# variables ya existen en esos dataframes.

#------------------------------------------------------------------------------
# 3. AJUSTE DE MODELOS POISSON Y VISUALIZACIÓN DE EFECTOS
#------------------------------------------------------------------------------
# Esta sección ajusta diferentes especificaciones de modelos de regresión Poisson
# para predecir el conteo de difusión ('sequential_count'). Se exploran distintas
# formas de incluir la variable de distancia de estatus y se visualizan las
# predicciones para interpretar los resultados.

# Liberar memoria antes de iniciar el modelado intensivo
gc()

# Resumen de la variable de exposición estandarizada (opcional, para verificar rango)
summary(resultado_final$exposure_std)

#------------------------------------------------------------------------------
# 3.1 Modelo 1: Distancia de Estatus Categórica ('status_diff_cat')
#------------------------------------------------------------------------------
# Primer modelo: Regresión Poisson con 'sequential_count' como variable dependiente.
# Predictores:
# - status_diff_cat: Efecto categórico de la distancia de estatus (5 niveles).
# - geo_prox_std: Control por proximidad geográfica (estandarizada).
# - pop_diff_std: Control por diferencia poblacional (estandarizada).
# - factor(decada): Efectos fijos por década para controlar tendencias temporales.
# Familia: Poisson con enlace logarítmico.

model_01 <- glm(sequential_count ~
                  status_diff_cat +  # Variable categórica principal
                  geo_prox_std +     # Control geográfico
                  pop_diff_std +     # Control demográfico
                  factor(decada),    # Control temporal (efectos fijos)
                data = resultado_final,
                family = poisson(link = "log")) # Modelo Poisson

# Liberar memoria después del ajuste del modelo
gc()

# Mostrar resumen del modelo 1 (coeficientes, significancia, etc.)
summary(model_01)

#--- Predicción y Visualización para Modelo 1 ---

# 1. Crear un data frame (grid) para generar predicciones.
#    Se varían los niveles de 'status_diff_cat'.
#    Las otras variables se fijan en valores representativos:
#    - Variables continuas estandarizadas ('geo_prox_std', 'pop_diff_std') en su media (0 por estandarización).
#    - 'decada' se fija en un año específico (e.g., 2000) para controlar el efecto temporal.
newx_m1 <- resultado_final %>%
  data_grid(
    status_diff_cat,                                  # Variable a variar
    geo_prox_std = mean(geo_prox_std, na.rm = TRUE),  # Fijar en la media (aprox. 0)
    pop_diff_std = mean(pop_diff_std, na.rm = TRUE),  # Fijar en la media (aprox. 0)
    decada = "2000",                                  # Fijar década para la predicción
    .model = model_01                                 # Asociar con el modelo (útil con modelr)
  )

# 2. Generar predicciones usando el modelo ajustado ('model_01').
#    'type = "response"' devuelve las predicciones en la escala original (conteos esperados).
newx_m1 <- newx_m1 %>%
  mutate(y_hat = predict(model_01, newdata = newx_m1, type = "response")) %>%
  drop_na() # Eliminar filas con NA si las hubiera

# 3. Verificar los datos predichos (opcional).
print(newx_m1)

# 4. Crear el gráfico de predicciones para el Modelo 1.
#    Muestra el conteo predicho ('y_hat') para cada categoría de 'status_diff_cat'.
plot_m1 <- newx_m1 %>%
  ggplot(aes(
    # Convertir el factor a numérico para el eje x, manteniendo el orden lógico de las categorías.
    x = as.numeric(factor(status_diff_cat,
                          levels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t"))), # Orden específico
    y = y_hat # Conteo predicho en el eje y
  )) +
  # Configurar eje X: principal numérico (1-5), secundario con etiquetas categóricas.
  scale_x_continuous(
    name   = "Status Distance (Categorical -> Numeric)", # Etiqueta eje principal
    breaks = 1:5, # Marcas en 1, 2, 3, 4, 5
    labels = 1:5, # Etiquetas numéricas
    sec.axis = sec_axis( # Eje secundario superior
      ~., # Misma escala/transformación que el primario
      name   = "Status Distance Category", # Etiqueta eje secundario
      breaks = 1:5,
      labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t") # Etiquetas categóricas correspondientes
    )
  ) +
  geom_point(size = 3) + # Dibujar puntos para cada predicción categórica.
  # Añadir una curva suavizada (LOESS) para visualizar la tendencia general entre categorías.
  geom_smooth(
    aes(group = 1),      # Agrupar todos los puntos para una única curva.
    method = "loess",    # Método de suavizado local.
    se = TRUE,           # Mostrar banda de error estándar/confianza.
    level = 0.68,        # Nivel de confianza (~ +/- 1 Error Estándar).
    span = 1,            # Parámetro de suavidad (1 = bastante suave).
    color = "blue",      # Color de la línea.
    fill = "lightblue",  # Color del relleno de la banda de confianza.
    alpha = 0.3          # Transparencia del relleno.
  ) +
  labs(
    title = "Effect of Status Distance Category on Predicted Count (Model 1)",
    # x = ... (cubierto por scale_x_continuous)
    y = "Predicted Count (y_hat)",
    caption = "Predictions from Poisson model. Controls fixed at mean values (Decade = 2000)."
  ) +
  theme_minimal() + # Tema base limpio.
  theme( # Ajustes adicionales al tema.
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x.top = element_text(size = 10), # Ajustar tamaño etiquetas eje secundario si es necesario
    axis.title.x.top = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic")
  )

# Mostrar el gráfico (descomentar si se ejecuta interactivamente)
# print(plot_m1)

#------------------------------------------------------------------------------
# 3.2 Modelo 2: Distancia de Estatus Numérica Redondeada ('status_diff_cat2')
#------------------------------------------------------------------------------
# Segundo modelo: Similar al anterior, pero usa la variable numérica redondeada
# 'status_diff_cat2' en lugar de la categórica. Esto asume (inicialmente) una
# relación log-lineal entre la distancia de estatus numérica y el conteo.

gc() # Liberar memoria

model_02 <- glm(sequential_count ~
                  status_diff_cat2 + # Variable numérica redondeada
                  geo_prox_std +
                  pop_diff_std +
                  factor(decada),
                data = resultado_final, # Usar el mismo dataset general
                family = poisson(link = "log"))

# Mostrar resumen del modelo 2 (descomentar para ver)
# summary(model_02)

#--- Predicción y Visualización para Modelo 2 ---

# 1. Crear grid de predicción.
#    Se varía 'status_diff_cat2' sobre un rango relevante (e.g., -10 a 10).
#    Los controles se fijan en sus medias y la década en 2000, como antes.
#    Es importante usar el rango y las medias del dataset 'resultado_final' que se usó para ajustar el modelo.
range_status_diff_cat2 <- range(resultado_final$status_diff_cat2, na.rm = TRUE)
newx_m2 <- expand_grid(
  # Crear una secuencia de valores para status_diff_cat2 dentro de su rango observado.
  status_diff_cat2 = seq(range_status_diff_cat2[1], range_status_diff_cat2[2], by = 1),
  geo_prox_std = mean(resultado_final$geo_prox_std, na.rm = TRUE), # Media del dataset del modelo
  pop_diff_std = mean(resultado_final$pop_diff_std, na.rm = TRUE), # Media del dataset del modelo
  decada = 2000 # Fijar década
)

# 2. Hacer las predicciones usando el modelo 2.
newx_m2 <- newx_m2 %>%
  mutate(y_hat = predict(model_02, newdata = newx_m2, type = "response"))

# 3. Ver la estructura de los datos predichos (opcional).
# glimpse(newx_m2)

# 4. Crear el gráfico mejorado para el Modelo 2.
#    Muestra la relación predicha entre la distancia de estatus numérica y el conteo.
plot_m2 <- ggplot(data = newx_m2, aes(x = status_diff_cat2, y = y_hat)) +
  # Añadir una curva suavizada (LOESS) sobre los puntos predichos.
  # Esto ayuda a visualizar posibles no linealidades que el modelo lineal simple no captura.
  geom_smooth(
    se = TRUE,           # Mostrar banda de confianza.
    color = "blue",      # Color de la línea.
    fill = "lightblue",  # Color del relleno.
    method = "loess",    # Método de suavizado.
    level = 0.68,        # Nivel de confianza (+/- 1 SE).
    span = 0.75,         # Sensibilidad de la curva (menor = más sensible).
    alpha = 0.4,         # Transparencia.
    linewidth = 1.2,     # Grosor de la línea (usar linewidth en lugar de size).
    linetype = "solid"   # Estilo de línea.
  ) +
  # Añadir puntos para mostrar los valores predichos individuales.
  geom_point(size = 2, alpha = 0.7, color = "darkblue") +
  # Configurar eje X: numérico principal, categórico secundario (aproximado).
  scale_x_continuous(
    name   = "Status distance (numeric, rounded)",
    breaks = seq(-10, 10, by = 2), # Ajustar marcas según el rango.
    sec.axis = sec_axis(
      trans  = ~., # Misma escala.
      name   = "Status Distance Category (approx.)",
      breaks = c(-10, -5, 0, 5, 10), # Puntos de referencia categóricos.
      labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t")
    )
  ) +
  labs(
    title   = "Status distance vs. Expected Count (Model 2 - Rounded Numeric)",
    y       = "Predicted Count (y_hat)",
    caption = "Note: Predicted values (y_hat) from Poisson regression. Controls held at means (Decade = 2000)."
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    axis.title       = element_text(size = 14),
    axis.text        = element_text(size = 12),
    axis.text.x.top = element_text(size = 10),
    axis.title.x.top = element_text(size = 12),
    plot.caption     = element_text(size = 10, hjust = 0.5, face = "italic")
  )

# Mostrar el gráfico (descomentar si se ejecuta interactivamente)
# print(plot_m2)

#------------------------------------------------------------------------------
# 3.3 Modelo 3: Efecto Cuadrático de la Distancia de Estatus ('status_diff_cat2')
#------------------------------------------------------------------------------
# Tercer modelo: Introduce un término cuadrático (I(status_diff_cat2^2)) para
# permitir una relación no lineal (forma de U o U invertida) entre la distancia
# de estatus numérica y el logaritmo del conteo esperado.

model_quadratic <- glm(sequential_count ~
                         status_diff_cat2 +         # Término lineal
                         I(status_diff_cat2^2) +    # Término cuadrático
                         geo_prox_std +
                         pop_diff_std +
                         factor(decada),
                       data = resultado_final,        # Usar dataset general
                       family = poisson(link = "log"))

# Mostrar resumen del modelo cuadrático (descomentar para ver)
# summary(model_quadratic)

#--- Predicción y Visualización para Modelo Cuadrático ---

# 1. Crear datos para la predicción.
#    Generar una secuencia fina de valores de 'status_diff_cat2' para dibujar una curva suave.
#    Fijar controles en la media y década en 2000.
newdata_quad <- data.frame(
  # Secuencia más densa (100 puntos) en el rango de la variable.
  status_diff_cat2 = seq(range_status_diff_cat2[1], range_status_diff_cat2[2], length.out = 100)
) %>%
  mutate(
    # Usar medias del dataset de ajuste del modelo ('resultado_final').
    geo_prox_std = mean(resultado_final$geo_prox_std, na.rm = TRUE),
    pop_diff_std = mean(resultado_final$pop_diff_std, na.rm = TRUE),
    decada = 2000 # Fijar década.
  )

# 2. Obtener predicciones Y errores estándar ('se.fit = TRUE').
#    Los errores estándar son necesarios para calcular las bandas de confianza.
pred_results_quad <- predict(model_quadratic, newdata_quad, type = "response", se.fit = TRUE)
newdata_quad$pred_quadratic <- pred_results_quad$fit # Predicción puntual
newdata_quad$se_quadratic <- pred_results_quad$se.fit # Error estándar de la predicción

# 3. Crear el gráfico de la curva cuadrática predicha con banda de confianza.
plot_m3_quad <- ggplot(data = newdata_quad, aes(x = status_diff_cat2)) +
  # Dibujar la línea de la predicción cuadrática.
  geom_line(aes(y = pred_quadratic), color = "blue", linewidth = 1) +
  # Añadir la banda de confianza del 95% (predicción +/- 1.96 * SE).
  # Usar pmax(0, ...) para evitar bandas de confianza negativas para conteos.
  geom_ribbon(aes(ymin = pmax(0, pred_quadratic - 1.96 * se_quadratic),
                  ymax = pred_quadratic + 1.96 * se_quadratic),
              alpha = 0.3, fill = "lightblue") + # Transparencia y color.
  labs(
    title = "Quadratic Effect of Status Distance on Predicted Count (Model 3)",
    x = "Status distance (numeric, rounded)",
    y = "Predicted Count (y_hat)",
    caption = "Predictions fixed at mean controls (Decade = 2000). Shaded area = 95% CI."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x.top = element_text(size = 10),
    axis.title.x.top = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic")
  ) +
  # Añadir eje secundario categórico para referencia.
  scale_x_continuous(
    sec.axis = sec_axis(~.,
                        breaks = c(-10, -5, 0, 5, 10),
                        labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t"),
                        name = "Status Distance Category (approx.)")
  )

# Mostrar el gráfico (descomentar si se ejecuta interactivamente)
# print(plot_m3_quad)

#------------------------------------------------------------------------------
# 3.4 Modelo Cuadrático por Década (General - Todos los Nombres)
#------------------------------------------------------------------------------
# Objetivo: Visualizar cómo la relación cuadrática entre distancia de estatus y
# difusión ha cambiado a lo largo de las décadas.
# Se ajusta el modelo cuadrático (puede ser el mismo 'model_quadratic') y se
# generan predicciones separadas para cada década de interés.

# 1. Ajustar/Usar el modelo cuadrático simple.
#    El script original lo llama 'model_quadratic_simple'. Si es idéntico a
#    'model_quadratic' (ajustado con 'resultado_final'), podemos reutilizarlo.
#    Asumiremos que son el mismo modelo aquí.
model_quadratic_simple <- model_quadratic # Reutilizar el modelo si es idéntico.

# 2. Generar predicciones para cada década relevante.
all_decades <- sort(unique(resultado_final$decada))
# Definir las décadas específicas para las que se generarán predicciones.
valid_decades <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
# Inicializar un dataframe vacío para almacenar todas las predicciones.
all_predictions <- data.frame()

# Bucle a través de todas las décadas presentes en los datos.
for(dec in all_decades) {
  # Procesar solo si la década está en la lista de décadas válidas.
  if(dec %in% valid_decades) {
    cat("Generando predicciones (General) para década:", dec, "\n")
    
    # Crear grid de predicción para la década actual.
    newdata_decade <- data.frame(
      status_diff_cat2 = seq(range_status_diff_cat2[1], range_status_diff_cat2[2], length.out = 100), # Rango fino
      geo_prox_std = mean(resultado_final$geo_prox_std, na.rm = TRUE), # Media general
      pop_diff_std = mean(resultado_final$pop_diff_std, na.rm = TRUE), # Media general
      decada = dec # Establecer la década actual
    )
    
    # Obtener predicciones y errores estándar (con manejo de errores tryCatch).
    tryCatch({
      pred_results <- predict(model_quadratic_simple, newdata_decade,
                              type = "response", se.fit = TRUE)
      
      # Añadir resultados al dataframe de la década.
      newdata_decade$pred <- pred_results$fit
      newdata_decade$se <- pred_results$se.fit
      # Calcular Intervalos de Confianza (95%), asegurando que no sean negativos.
      newdata_decade$lower_ci <- pmax(0, newdata_decade$pred - 1.96 * newdata_decade$se)
      newdata_decade$upper_ci <- newdata_decade$pred + 1.96 * newdata_decade$se
      newdata_decade$decada_factor <- as.factor(dec) # Crear factor de década
      
      # Unir las predicciones de esta década al dataframe acumulado.
      all_predictions <- rbind(all_predictions, newdata_decade)
      
    }, error = function(e) {
      # Informar si ocurre un error durante la predicción para una década.
      cat("Error en década (General):", dec, "-", conditionMessage(e), "\n")
    })
  }
}

# 3. Preparación para Graficar por Década

# (Opcional) Normalización: El script original tenía código para normalizar las
# predicciones dentro de cada década (dividiendo por el máximo). Esto puede ser útil
# para comparar la *forma* de las curvas si las escalas varían mucho.
# Sin embargo, el gráfico final presentado parece usar los valores absolutos,
# por lo que se omite la normalización aquí.

# 4. Definir Tema Personalizado de ggplot (Opcional)
#    El script original define 'my_custom_theme'. Si se desea usar un estilo
#    muy específico y consistente, definirlo aquí es una buena práctica.
my_custom_theme <- function() {
  theme_bw() + # Empezar con un tema base bueno (blanco y negro)
    theme(
      plot.background = element_blank(),       # Fondo transparente
      strip.background = element_blank(),      # Fondo de facetas transparente
      strip.placement = "outside",           # Colocación de etiquetas de facetas
      panel.border = element_rect(linewidth = 1, colour = "black"), # Borde del panel
      panel.spacing = unit(0.3, "lines"),    # Espacio entre facetas
      axis.line = element_line(linewidth = 0.1, colour = "black"), # Líneas de ejes
      axis.ticks.y = element_line(linewidth = 0.5, colour = "black"), # Marcas eje Y
      axis.ticks.length = unit(-0.15, "cm"), # Longitud de marcas (hacia adentro)
      axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)), # Texto eje X
      axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)), # Texto eje Y
      text = element_text(size = 18),          # Tamaño de texto general (puede ser grande)
      legend.position = "top",               # Posición de la leyenda
      plot.title = element_text(hjust = 0.5),  # Título centrado
      plot.subtitle = element_text(hjust = 0.5), # Subtítulo centrado
      plot.title.position = "plot"             # Posición del título
    )
}

# Aplicar el tema personalizado globalmente para todos los gráficos ggplot siguientes.
# Descomentar si se quiere usar este tema específico:
# theme_set(my_custom_theme())

# 5. Crear el Gráfico Combinado por Década (General - Todos los Nombres)
#    Muestra una línea de predicción para cada década en el mismo gráfico.

# Asegurar que el factor 'decada_factor' esté ordenado cronológicamente para la leyenda y colores.
all_predictions$decada_factor <- factor(all_predictions$decada_factor,
                                        levels = sort(as.numeric(levels(factor(all_predictions$decada_factor)))))

plot_decades_general <- ggplot(all_predictions, aes(x = status_diff_cat2, y = pred, color = decada_factor, group = decada_factor)) +
  # Dibujar las líneas de predicción para cada década.
  geom_line(linewidth = 1) + # Usar linewidth en lugar de size para versiones recientes de ggplot2.
  # Usar una paleta de colores secuencial (Viridis - plasma) para indicar progresión temporal.
  scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9, name = "Decade") +
  # Etiquetas y Títulos.
  labs(
    title = "Status Distance and Diffusion (All Names) by Decade",
    subtitle = "Poisson Model with Quadratic Term and Decade Fixed Effects",
    x = "Status Distance (numeric, rounded)",
    y = "Predicted Count",
    caption = "Note: Predictions based on quadratic Poisson regression. Controls held at means."
  ) +
  # Aplicar tema: se puede usar el tema personalizado 'my_custom_theme()' o refinar uno base.
  theme_minimal() + # Usar tema minimalista como base.
  theme( # Refinamientos específicos para este gráfico.
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)), # Margen inferior para subtítulo.
    legend.position = "bottom", # Leyenda abajo.
    legend.title = element_text(face = "bold"), # Título de leyenda en negrita.
    panel.grid.minor = element_blank(), # Sin rejilla menor.
    axis.title = element_text(face = "bold"), # Títulos de ejes en negrita.
    plot.caption = element_text(hjust = 0, face = "italic", size = 9) # Nota al pie.
  ) +
  # Configurar eje X con doble etiqueta (numérica y categórica aprox.).
  scale_x_continuous(
    breaks = c(-10, -5, 0, 5, 10),
    sec.axis = sec_axis(~.,
                        breaks = c(-10, -5, 0, 5, 10),
                        labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t"),
                        name = "Status Distance Category (approx.)")
  ) +
  # Añadir línea vertical en x=0 como referencia (diferencia de estatus cero).
  geom_vline(xintercept = 0, linetype = "dotted", color="grey50") +
  # Añadir anotaciones interpretativas (ajustar coordenadas 'y' según la escala del gráfico).
  # Estas etiquetas sugieren mecanismos teóricos (imitación, diferenciación, homofilia).
  annotate("text", x = -5, y = max(all_predictions$pred) * 0.85, label = "Upward Imitation?", hjust = 0.5, fontface = "italic", size = 4, color="grey40") +
  annotate("text", x = 5, y = max(all_predictions$pred) * 0.85, label = "Downward Differentiation?", hjust = 0.5, fontface = "italic", size = 4, color="grey40") +
  annotate("text", x = 0, y = max(all_predictions$pred) * 0.95, label = "Homophily Peak?", hjust = 0.5, fontface = "italic", size = 4, color = "gray40")
# La flecha del gráfico original se puede añadir con annotate("segment", ...) si se desea.

# Mostrar el gráfico (descomentar si se ejecuta interactivamente)
# print(plot_decades_general)


#------------------------------------------------------------------------------
# 3.5 Modelo Cuadrático por Década (Nombres Femeninos)
#------------------------------------------------------------------------------
# Repetir el análisis de la sección 3.4, pero utilizando el conjunto de datos
# específico para nombres femeninos ('resultado_final_fem') y ajustando un modelo
# específico para estos datos.

# PRE-REQUISITO: Asegurarse de que 'resultado_final_fem' contenga las variables
# necesarias preprocesadas (status_diff_cat2, geo_prox_std, pop_diff_std).
# Si no existen, deben crearse como se hizo para 'resultado_final' en la sección 2.
# Ejemplo:
# resultado_final_fem <- resultado_final_fem %>%
#   mutate(status_diff_cat2 = round(status_diff, 0),
#          geo_prox_std = as.numeric(scale(geo_prox, center=TRUE, scale=TRUE)),
#          pop_diff_std = as.numeric(scale(pop_diff, center=TRUE, scale=TRUE)))

# 1. Ajustar el modelo cuadrático para datos femeninos.
model_quadratic_simple_fem <- glm(sequential_count ~
                                    status_diff_cat2 +
                                    I(status_diff_cat2^2) +
                                    geo_prox_std +
                                    pop_diff_std +
                                    factor(decada),
                                  data = resultado_final_fem, # Usar datos femeninos
                                  family = poisson(link = "log"))

# Revisar el resumen del modelo femenino.
summary(model_quadratic_simple_fem)

# 2. Generar predicciones por década (Femenino).
all_decades_fem <- sort(unique(resultado_final_fem$decada))
# Determinar el rango de status_diff_cat2 específico para los datos femeninos.
range_status_diff_cat2_fem <- range(resultado_final_fem$status_diff_cat2, na.rm = TRUE)
all_predictions_fem <- data.frame() # Inicializar dataframe

# Bucle sobre las décadas válidas.
for(dec in all_decades_fem) {
  if(dec %in% valid_decades) { # Usar la misma lista de décadas válidas.
    cat("Generando predicciones (Fem) para década:", dec, "\n")
    newdata_decade_fem <- data.frame(
      status_diff_cat2 = seq(range_status_diff_cat2_fem[1], range_status_diff_cat2_fem[2], length.out = 100),
      # IMPORTANTE: Usar las medias calculadas a partir del dataset femenino.
      geo_prox_std = mean(resultado_final_fem$geo_prox_std, na.rm = TRUE),
      pop_diff_std = mean(resultado_final_fem$pop_diff_std, na.rm = TRUE),
      decada = dec
    )
    tryCatch({
      pred_results_fem <- predict(model_quadratic_simple_fem, newdata_decade_fem,
                                  type = "response", se.fit = TRUE)
      newdata_decade_fem$pred <- pred_results_fem$fit
      newdata_decade_fem$se <- pred_results_fem$se.fit
      newdata_decade_fem$lower_ci <- pmax(0, newdata_decade_fem$pred - 1.96 * newdata_decade_fem$se)
      newdata_decade_fem$upper_ci <- newdata_decade_fem$pred + 1.96 * newdata_decade_fem$se
      newdata_decade_fem$decada_factor <- as.factor(dec)
      
      all_predictions_fem <- rbind(all_predictions_fem, newdata_decade_fem)
    }, error = function(e) {
      cat("Error en década (Fem):", dec, "-", conditionMessage(e), "\n")
    })
  }
}

# 3. Crear el Gráfico Combinado por Década (Femenino).
# Ordenar factor de década.
all_predictions_fem$decada_factor <- factor(all_predictions_fem$decada_factor,
                                            levels = sort(as.numeric(levels(factor(all_predictions_fem$decada_factor)))))

plot_decades_fem <- ggplot(all_predictions_fem, aes(x = status_diff_cat2, y = pred, color = decada_factor, group = decada_factor)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9, name = "Decade") +
  labs(
    title = "Status Distance and Diffusion (Female Names) by Decade", # Título específico.
    subtitle = "Poisson Model with Quadratic Term and Decade Fixed Effects",
    x = "Status Distance (numeric, rounded)",
    y = "Predicted Count",
    caption = "Note: Predictions based on quadratic Poisson regression (Female data). Controls held at means."
  ) +
  # Aplicar tema (puede ser el mismo que el general o adaptado).
  theme_minimal() + # O theme_set(my_custom_theme()) si se aplicó globalmente.
  theme( # Copiar/adaptar refinamientos del gráfico general.
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9)
    # Añadir los elementos del tema personalizado 'my_custom_theme' si no se usó theme_set()
    # panel.border = element_rect(linewidth = 1, colour = "black"), etc.
  ) +
  scale_x_continuous(
    breaks = c(-10, -5, 0, 5, 10),
    sec.axis = sec_axis(~., breaks = c(-10, -5, 0, 5, 10), labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t"), name = "Status Distance Category (approx.)")
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", color="grey50") +
  # Anotaciones (ajustar coordenadas 'y' según la escala del gráfico femenino).
  annotate("text", x = -5, y = max(all_predictions_fem$pred) * 0.85, label = "Upward Imitation?", hjust = 0.5, fontface = "italic", size = 4, color="grey40") +
  annotate("text", x = 5, y = max(all_predictions_fem$pred) * 0.85, label = "Downward Differentiation?", hjust = 0.5, fontface = "italic", size = 4, color="grey40") +
  annotate("text", x = 0, y = max(all_predictions_fem$pred) * 0.95, label = "Homophily Peak?", hjust = 0.5, fontface = "italic", size = 4, color = "gray40")

# Mostrar gráfico femenino (descomentar)
# print(plot_decades_fem)


#------------------------------------------------------------------------------
# 3.6 Modelo Cuadrático por Década (Nombres Masculinos)
#------------------------------------------------------------------------------
# Repetir el análisis de la sección 3.4/3.5, pero utilizando el conjunto de datos
# específico para nombres masculinos ('resultado_final_mas').

# PRE-REQUISITO: Asegurarse de que 'resultado_final_mas' contenga las variables
# necesarias preprocesadas (status_diff_cat2, geo_prox_std, pop_diff_std).

# 1. Ajustar el modelo cuadrático para datos masculinos.
model_quadratic_simple_mas <- glm(sequential_count ~
                                    status_diff_cat2 +
                                    I(status_diff_cat2^2) +
                                    geo_prox_std +
                                    pop_diff_std +
                                    factor(decada),
                                  data = resultado_final_mas, # Usar datos masculinos
                                  family = poisson(link = "log"))

# Revisar el resumen del modelo masculino.
summary(model_quadratic_simple_mas)

# 2. Generar predicciones por década (Masculino).
all_decades_mas <- sort(unique(resultado_final_mas$decada))
# Determinar el rango de status_diff_cat2 específico para los datos masculinos.
range_status_diff_cat2_mas <- range(resultado_final_mas$status_diff_cat2, na.rm = TRUE)
all_predictions_mas <- data.frame() # Inicializar dataframe

# Bucle sobre las décadas válidas.
for(dec in all_decades_mas) {
  if(dec %in% valid_decades) { # Usar la misma lista de décadas válidas.
    cat("Generando predicciones (Mas) para década:", dec, "\n")
    newdata_decade_mas <- data.frame(
      status_diff_cat2 = seq(range_status_diff_cat2_mas[1], range_status_diff_cat2_mas[2], length.out = 100),
      # IMPORTANTE: Usar las medias calculadas a partir del dataset masculino.
      geo_prox_std = mean(resultado_final_mas$geo_prox_std, na.rm = TRUE),
      pop_diff_std = mean(resultado_final_mas$pop_diff_std, na.rm = TRUE),
      decada = dec
    )
    tryCatch({
      pred_results_mas <- predict(model_quadratic_simple_mas, newdata_decade_mas,
                                  type = "response", se.fit = TRUE)
      newdata_decade_mas$pred <- pred_results_mas$fit
      newdata_decade_mas$se <- pred_results_mas$se.fit
      newdata_decade_mas$lower_ci <- pmax(0, newdata_decade_mas$pred - 1.96 * newdata_decade_mas$se)
      newdata_decade_mas$upper_ci <- newdata_decade_mas$pred + 1.96 * newdata_decade_mas$se
      newdata_decade_mas$decada_factor <- as.factor(dec)
      
      all_predictions_mas <- rbind(all_predictions_mas, newdata_decade_mas)
    }, error = function(e) {
      cat("Error en década (Mas):", dec, "-", conditionMessage(e), "\n")
    })
  }
}

# 3. Crear el Gráfico Combinado por Década (Masculino).
# Ordenar factor de década.
all_predictions_mas$decada_factor <- factor(all_predictions_mas$decada_factor,
                                            levels = sort(as.numeric(levels(factor(all_predictions_mas$decada_factor)))))

plot_decades_mas <- ggplot(all_predictions_mas, aes(x = status_diff_cat2, y = pred, color = decada_factor, group = decada_factor)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "plasma", begin = 0.1, end = 0.9, name = "Decade") +
  labs(
    title = "Status Distance and Diffusion (Male Names) by Decade", # Título específico.
    subtitle = "Poisson Model with Quadratic Term and Decade Fixed Effects",
    x = "Status Distance (numeric, rounded)",
    y = "Predicted Count",
    caption = "Note: Predictions based on quadratic Poisson regression (Male data). Controls held at means."
  ) +
  # Aplicar tema (puede ser el mismo que el general o adaptado).
  theme_minimal() + # O theme_set(my_custom_theme()) si se aplicó globalmente.
  theme( # Copiar/adaptar refinamientos del gráfico general.
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic", size = 9)
    # Añadir elementos del tema personalizado si es necesario
  ) +
  scale_x_continuous(
    breaks = c(-10, -5, 0, 5, 10),
    sec.axis = sec_axis(~., breaks = c(-10, -5, 0, 5, 10), labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t"), name = "Status Distance Category (approx.)")
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", color="grey50") +
  # Anotaciones (ajustar coordenadas 'y' según la escala del gráfico masculino).
  annotate("text", x = -5, y = max(all_predictions_mas$pred) * 0.85, label = "Upward Imitation?", hjust = 0.5, fontface = "italic", size = 4, color="grey40") +
  annotate("text", x = 5, y = max(all_predictions_mas$pred) * 0.85, label = "Downward Differentiation?", hjust = 0.5, fontface = "italic", size = 4, color="grey40") +
  annotate("text", x = 0, y = max(all_predictions_mas$pred) * 0.95, label = "Homophily Peak?", hjust = 0.5, fontface = "italic", size = 4, color = "gray40")

# Mostrar gráfico masculino (descomentar)
# print(plot_decades_mas)


#------------------------------------------------------------------------------
# 3.7 Función de Graficación Mejorada y Aplicación (Opcional/Alternativa)
#------------------------------------------------------------------------------
# El script original incluye una función ('create_smooth_plot') que encapsula
# la generación de predicciones y la creación de gráficos por década.
# Esta función utiliza geom_smooth() directamente sobre los datos predichos para
# obtener curvas visualmente más suaves y aplica una estética ligeramente diferente
# (colores manuales, anotaciones distintas).

# Definición de la función create_smooth_plot
create_smooth_plot <- function(model, data_source, title_prefix) {
  # 1. Generar predicciones con MÁS PUNTOS para suavizar visualmente la curva de geom_smooth.
  all_decades_func <- sort(unique(data_source$decada))
  # Calcular rango y medias DESDE el data_source específico pasado a la función.
  range_status_diff_cat2_func <- range(data_source$status_diff_cat2, na.rm = TRUE)
  mean_geo_prox_std <- mean(data_source$geo_prox_std, na.rm = TRUE)
  mean_pop_diff_std <- mean(data_source$pop_diff_std, na.rm = TRUE)
  
  all_predictions_func <- data.frame() # Inicializar
  valid_decades_func <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020) # Décadas válidas
  
  for(dec in all_decades_func) {
    if(dec %in% valid_decades_func) {
      # Crear grid con MÁS puntos (e.g., 300) para que geom_smooth funcione mejor.
      newdata_decade_func <- data.frame(
        status_diff_cat2 = seq(range_status_diff_cat2_func[1], range_status_diff_cat2_func[2], length.out = 300), # Más puntos
        geo_prox_std = mean_geo_prox_std, # Usar media específica del data_source
        pop_diff_std = mean_pop_diff_std, # Usar media específica del data_source
        decada = dec
      )
      tryCatch({
        # Predecir SOLO el valor ajustado (fit), SE no se usa en este gráfico.
        pred_results_func <- predict(model, newdata_decade_func,
                                     type = "response", se.fit = FALSE) # se.fit = FALSE
        
        newdata_decade_func$pred <- pred_results_func # Solo se necesita la predicción
        newdata_decade_func$decada_factor <- as.factor(dec) # Factor de década
        
        all_predictions_func <- rbind(all_predictions_func, newdata_decade_func)
      }, error = function(e) {
        cat("Error en función create_smooth_plot (Década:", dec, "):", conditionMessage(e), "\n")
      })
    }
  }
  
  # Ordenar factor de década.
  all_predictions_func$decada_factor <- factor(all_predictions_func$decada_factor,
                                               levels = sort(as.numeric(levels(factor(all_predictions_func$decada_factor)))))
  
  # Paleta de colores manual (Set1 de RColorBrewer es buena alternativa).
  # decade_colors <- RColorBrewer::brewer.pal(n = length(valid_decades_func), name = "Set1")
  # names(decade_colors) <- as.character(valid_decades_func)
  # O usar la paleta definida en el script original:
  decade_colors <- c(
    "1960" = "#E41A1C", # red
    "1970" = "#377EB8", # blue
    "1980" = "#4DAF4A", # green
    "1990" = "#984EA3", # purple
    "2000" = "#FF7F00", # orange
    "2010" = "#FFFF33", # yellow (difícil de ver sobre fondo blanco)
    "2020" = "#A65628"  # brown
  )
  
  # Crear el gráfico usando geom_smooth en lugar de geom_line.
  p <- ggplot(all_predictions_func, aes(x = status_diff_cat2, y = pred, color = decada_factor, group = decada_factor)) +
    # Usar geom_smooth para dibujar las curvas (se=FALSE ya que no mostramos CIs aquí).
    # 'span' controla la suavidad (más bajo = más wiggly).
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.3, linewidth = 1.2) +
    # Aplicar la paleta de colores manual.
    scale_color_manual(values = decade_colors, name = "Decade") +
    # Etiquetas y Títulos.
    labs(
      title = paste0(title_prefix, " by Decade (Smoothed)"),
      subtitle = "Structural Models with fixed effects by time",
      x = "Status Distance",
      y = "Expected Diffusion Count (Smoothed Prediction)",
      caption = "Note: Smoothed predictions (LOESS on predictions from quadratic Poisson model). Controls held at means."
    ) +
    # Tema base y refinamientos.
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray90"), # Rejilla mayor suave.
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, face = "italic", size = 9),
      text = element_text(size = 12) # Tamaño base de texto.
    ) +
    # Eje X con doble etiqueta.
    scale_x_continuous(
      breaks = c(-10, -5, 0, 5, 10),
      sec.axis = sec_axis(~., breaks = c(-10, -5, 0, 5, 10), labels = c("s<<t", "s<t", "s≈t", "s>t", "s>>t"), name = "Status Category (approx.)")
    ) +
    # Línea vertical de referencia.
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray50") +
    # Anotaciones (ajustar coordenadas 'y' según la escala y densidad del gráfico).
    # Estas anotaciones son más sutiles en el script original.
    annotate("text", x = -5, y = max(all_predictions_func$pred) * 0.6, label = "Upward Imitation", hjust = 0.5, fontface = "italic", size = 3.5, color = "gray40") +
    annotate("text", x = 5, y = max(all_predictions_func$pred) * 0.6, label = "Downward Differentiation", hjust = 0.5, fontface = "italic", size = 3.5, color = "gray40") +
    annotate("text", x = 0, y = max(all_predictions_func$pred) * 0.9, label = "Homophily", hjust = 0.5, fontface = "italic", size = 3.5, color = "gray40")
  # La flecha del script original se puede añadir con:
  # annotate("segment", x = -1, y = max(all_predictions_func$pred) * 0.87,
  #          xend = 0, yend = max(all_predictions_func$pred) * 0.8,
  #          arrow = arrow(length = unit(0.2, "cm")), color = "gray40")
  
  return(p) # Devolver el objeto ggplot.
}

# --- Aplicación de la función create_smooth_plot ---

# Generar el gráfico suavizado para el modelo general.
plot_general_smooth <- create_smooth_plot(
  model = model_quadratic_simple, # Modelo cuadrático general
  data_source = resultado_final,    # Datos generales
  title_prefix = "Status Distance and Diffusion (All Names)"
)

# Generar el gráfico suavizado para el modelo femenino.
plot_female_smooth <- create_smooth_plot(
  model = model_quadratic_simple_fem, # Modelo cuadrático femenino
  data_source = resultado_final_fem,    # Datos femeninos
  title_prefix = "Status Distance and Diffusion (Female Names)"
)

# Generar el gráfico suavizado para el modelo masculino.
plot_male_smooth <- create_smooth_plot(
  model = model_quadratic_simple_mas, # Modelo cuadrático masculino
  data_source = resultado_final_mas,    # Datos masculinos
  title_prefix = "Status Distance and Diffusion (Male Names)"
)

# Mostrar los gráficos generados por la función (descomentar para verlos).
# print(plot_general_smooth)
# print(plot_female_smooth)
# print(plot_male_smooth)


#------------------------------------------------------------------------------
# 4. FIN DEL SCRIPT
#------------------------------------------------------------------------------
# Este script ha cargado y preparado los datos de difusión de nombres, ajustado
# varios modelos Poisson explorando el efecto de la distancia de estatus, y
# generado visualizaciones detalladas de las predicciones.
# Se han creado gráficos para modelos individuales y comparaciones a través de
# las décadas, tanto para el conjunto general de datos como para subgrupos
# femeninos y masculinos.
#
# Los objetos principales generados y disponibles en el entorno son:
# - Modelos ajustados: model_01, model_02, model_quadratic,
#                      model_quadratic_simple_fem, model_quadratic_simple_mas
# - Dataframes con predicciones: newx_m1, newx_m2, newdata_quad,
#                                all_predictions, all_predictions_fem, all_predictions_mas
# - Gráficos (objetos ggplot): plot_m1, plot_m2, plot_m3_quad,
#                             plot_decades_general, plot_decades_fem, plot_decades_mas,
#                             plot_general_smooth, plot_female_smooth, plot_male_smooth
#------------------------------------------------------------------------------