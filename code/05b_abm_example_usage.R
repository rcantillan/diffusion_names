################################################################################
# EJEMPLO DE USO: Agent-Based Model de Difusión de Nombres
################################################################################
# 
# Este script demuestra cómo usar el modelo ABM de difusión de nombres
# entre estratos sociales.
#
################################################################################

# Cargar librerías necesarias
library(tidyverse)
library(data.table)
library(scales)

# Cargar el modelo ABM
source("code/05_abm_name_diffusion.R")

################################################################################
# EJEMPLO 1: Simulación Básica con Parámetros por Defecto
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("EJEMPLO 1: Simulación Básica\n")
cat(rep("=", 80), "\n\n", sep = "")

# Ejecutar ejemplo completo con función de conveniencia
results <- run_abm_example(seed = 42, verbose = TRUE)

# Extraer componentes
abm_results <- results$abm_results
metrics <- results$metrics

################################################################################
# EJEMPLO 2: Visualizaciones
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("EJEMPLO 2: Creando Visualizaciones\n")
cat(rep("=", 80), "\n\n", sep = "")

# 2.1. Evolución de nombres en la elite (Q5)
cat("Creando gráfico de evolución para la elite (Q5)...\n")
plot_elite <- plot_name_evolution(abm_results, class_index = 5, top_n = 10)
print(plot_elite)

# Guardar gráfico
ggsave("plot/abm_evolution_elite.png", plot_elite, width = 10, height = 6, dpi = 300)
cat("Gráfico guardado en: plot/abm_evolution_elite.png\n\n")

# 2.2. Evolución de nombres en la clase baja (Q1)
cat("Creando gráfico de evolución para la clase baja (Q1)...\n")
plot_low <- plot_name_evolution(abm_results, class_index = 1, top_n = 10)
print(plot_low)

# Guardar gráfico
ggsave("plot/abm_evolution_low.png", plot_low, width = 10, height = 6, dpi = 300)
cat("Gráfico guardado en: plot/abm_evolution_low.png\n\n")

# 2.3. Heatmap de distribución de nombres en 1920 (inicio)
cat("Creando heatmap para el año 1920...\n")
plot_heat_1920 <- plot_name_heatmap(abm_results, year = 1920, top_n = 15)
print(plot_heat_1920)

# Guardar gráfico
ggsave("plot/abm_heatmap_1920.png", plot_heat_1920, width = 8, height = 10, dpi = 300)
cat("Gráfico guardado en: plot/abm_heatmap_1920.png\n\n")

# 2.4. Heatmap de distribución de nombres en 2010 (fin)
cat("Creando heatmap para el año 2010...\n")
plot_heat_2010 <- plot_name_heatmap(abm_results, year = 2010, top_n = 15)
print(plot_heat_2010)

# Guardar gráfico
ggsave("plot/abm_heatmap_2010.png", plot_heat_2010, width = 8, height = 10, dpi = 300)
cat("Gráfico guardado en: plot/abm_heatmap_2010.png\n\n")

# 2.5. Evolución de la diversidad
cat("Creando gráfico de evolución de la diversidad...\n")
plot_div <- plot_diversity_evolution(metrics, abm_results$params)
print(plot_div)

# Guardar gráfico
ggsave("plot/abm_diversity_evolution.png", plot_div, width = 10, height = 6, dpi = 300)
cat("Gráfico guardado en: plot/abm_diversity_evolution.png\n\n")

################################################################################
# EJEMPLO 3: Análisis de Datos en Formato Data Frame
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("EJEMPLO 3: Conversión a Data Frame para Análisis\n")
cat(rep("=", 80), "\n\n", sep = "")

# Convertir resultados a data frame
cat("Convirtiendo resultados a data frame...\n")
df_results <- abm_to_dataframe(abm_results, include_exposures = TRUE)

cat("Dimensiones del data frame:", nrow(df_results), "filas x", ncol(df_results), "columnas\n")
cat("\nPrimeras filas:\n")
print(head(df_results, 10))

cat("\nEstadísticas descriptivas de proporciones:\n")
print(summary(df_results$proportion))

# Guardar data frame como CSV
write.csv(df_results, "plot/abm_results.csv", row.names = FALSE)
cat("\nData frame guardado en: plot/abm_results.csv\n")

################################################################################
# EJEMPLO 4: Simulación Personalizada con Diferentes Parámetros
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("EJEMPLO 4: Simulación con Parámetros Personalizados\n")
cat(rep("=", 80), "\n\n", sep = "")

# Crear parámetros personalizados con mayor influencia ascendente
cat("Configurando parámetros con mayor aspiración social (alpha_up = 0.5)...\n")
params_custom <- initialize_abm_parameters(
  n_classes = 5,
  n_names = 30,
  start_year = 1920,
  end_year = 2010,
  alpha_up = 0.5,      # Mayor aspiración hacia clases altas
  alpha_down = 0.15,   # Menor efecto de saturación
  beta = 1.2,          # Mayor sensibilidad general
  gamma = 0.08         # Menor inercia
)

# Inicializar estado
initial_state_custom <- initialize_state(params_custom, init_type = "elite_first", seed = 123)

# Ejecutar simulación
cat("\nEjecutando simulación personalizada...\n")
abm_results_custom <- run_abm_simulation(params_custom, initial_state_custom, verbose = FALSE)

# Calcular métricas
metrics_custom <- calculate_diffusion_metrics(abm_results_custom)

cat("\nComparación de diversidad final:\n")
cat("Simulación estándar:\n")
for (k in 1:5) {
  cat(sprintf("  Q%d: %.3f\n", k, metrics$diversity[k, 91]))
}
cat("\nSimulación con mayor aspiración:\n")
for (k in 1:5) {
  cat(sprintf("  Q%d: %.3f\n", k, metrics_custom$diversity[k, 91]))
}

# Visualizar comparación
plot_div_custom <- plot_diversity_evolution(metrics_custom, params_custom) +
  labs(title = "Evolución de Diversidad - Mayor Aspiración Social (α_up = 0.5)")
print(plot_div_custom)

ggsave("plot/abm_diversity_high_aspiration.png", plot_div_custom, width = 10, height = 6, dpi = 300)
cat("\nGráfico guardado en: plot/abm_diversity_high_aspiration.png\n")

################################################################################
# EJEMPLO 5: Análisis de Exposición
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("EJEMPLO 5: Análisis de Exposición Ascendente y Descendente\n")
cat(rep("=", 80), "\n\n", sep = "")

# Seleccionar un nombre específico y una clase para análisis
target_name_idx <- 1  # Primer nombre
target_class <- 3     # Clase media (Q3)

# Extraer series temporales de exposición para ese nombre y clase
years <- abm_results$params$years
exp_up_series <- abm_results$exposures$upward[target_class, target_name_idx, ]
exp_down_series <- abm_results$exposures$downward[target_class, target_name_idx, ]
prop_series <- abm_results$state[target_class, target_name_idx, ]

# Crear data frame para visualización
df_exposure <- data.frame(
  year = years,
  proportion = prop_series,
  exposure_up = exp_up_series,
  exposure_down = exp_down_series
)

# Convertir a formato largo
df_exposure_long <- df_exposure %>%
  pivot_longer(
    cols = c(proportion, exposure_up, exposure_down),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, 
                   levels = c("proportion", "exposure_up", "exposure_down"),
                   labels = c("Proporción", "Exposición Ascendente", "Exposición Descendente"))
  )

# Crear gráfico
plot_exposure <- ggplot(df_exposure_long, aes(x = year, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  labs(
    title = sprintf("Análisis de Exposición: %s en %s", 
                   abm_results$params$name_ids[target_name_idx],
                   abm_results$params$class_names[target_class]),
    subtitle = "Comparación de proporción real vs. exposiciones",
    x = "Año",
    y = "Valor",
    color = "Métrica"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Proporción" = "black", 
                                "Exposición Ascendente" = "blue",
                                "Exposición Descendente" = "red"))

print(plot_exposure)

ggsave("plot/abm_exposure_analysis.png", plot_exposure, width = 10, height = 6, dpi = 300)
cat("Gráfico guardado en: plot/abm_exposure_analysis.png\n")

################################################################################
# EJEMPLO 6: Análisis de Velocidad de Difusión
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("EJEMPLO 6: Velocidad de Difusión entre Clases\n")
cat(rep("=", 80), "\n\n", sep = "")

# Crear data frame para velocidad de difusión
df_speed <- data.frame()

for (k in 1:4) {  # 4 pares de clases adyacentes
  for (t in 1:91) {
    df_speed <- rbind(df_speed, data.frame(
      year = abm_results$params$years[t],
      class_pair = rownames(metrics$diffusion_speed)[k],
      correlation = metrics$diffusion_speed[k, t]
    ))
  }
}

# Crear gráfico
plot_speed <- ggplot(df_speed, aes(x = year, y = correlation, color = class_pair)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Velocidad de Difusión entre Clases Sociales",
    subtitle = "Correlación de distribuciones de nombres entre clases adyacentes",
    x = "Año",
    y = "Correlación",
    color = "Pares de Clases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50")

print(plot_speed)

ggsave("plot/abm_diffusion_speed.png", plot_speed, width = 10, height = 6, dpi = 300)
cat("Gráfico guardado en: plot/abm_diffusion_speed.png\n")

################################################################################
# RESUMEN FINAL
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("RESUMEN FINAL\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nSe han generado los siguientes archivos:\n\n")
cat("Visualizaciones:\n")
cat("  - plot/abm_evolution_elite.png\n")
cat("  - plot/abm_evolution_low.png\n")
cat("  - plot/abm_heatmap_1920.png\n")
cat("  - plot/abm_heatmap_2010.png\n")
cat("  - plot/abm_diversity_evolution.png\n")
cat("  - plot/abm_diversity_high_aspiration.png\n")
cat("  - plot/abm_exposure_analysis.png\n")
cat("  - plot/abm_diffusion_speed.png\n\n")
cat("Datos:\n")
cat("  - plot/abm_results.csv\n\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Ejemplo completado exitosamente!\n\n")
