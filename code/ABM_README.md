# Agent-Based Model: Difusión de Nombres entre Estratos Sociales

## Descripción General

Este modelo implementa un Agent-Based Model (ABM) que simula la difusión de nombres entre estratos sociales en Chile durante el período 1920-2010. El modelo está basado en microfundaciones teóricas que incorporan mecanismos de aspiración social (imitación ascendente) y saturación (diferenciación descendente).

## Especificaciones del Modelo

### Estructura Básica

- **Quintiles**: 5 clases sociales (Q1 = más bajo, Q5 = más alto/elite)
- **Género**: 1 (simplificado)
- **Período**: 1920 a 2010 (91 años)
- **Nombres**: 30 nombres en el sistema

### Variables de Estado

El estado del modelo en cada momento está definido por:

- `p_{k,n}(t)`: Proporción de nacimientos en la clase `k` con el nombre `n` en el tiempo `t`

Donde:
- `k ∈ {1, 2, 3, 4, 5}` representa las clases sociales (quintiles)
- `n ∈ {1, 2, ..., 30}` representa los nombres
- `t ∈ {1920, 1921, ..., 2010}` representa los años

### Microfundaciones Teóricas

#### 1. Exposición Ascendente (Aspiracional)

Representa la aspiración de las clases bajas hacia las prácticas de las clases altas:

```
E^↑_{k,n}(t) = p_{k,n}(t) + Σ_{j>k} w_{k,j} · p_{j,n}(t)
```

Donde:
- `w_{k,j}` son pesos que representan la influencia de la clase `j` sobre la clase `k`
- Los pesos decaen exponencialmente con la distancia entre clases

#### 2. Exposición Descendente (Saturación)

Representa el efecto de saturación: cuando un nombre se vuelve común en clases bajas, pierde atractivo para clases altas:

```
E^↓_{k,n}(t) = p_{k,n}(t) + Σ_{j<k} w_{k,j} · p_{j,n}(t)
```

#### 3. Función de Probabilidad de Elección

La probabilidad de que la clase `k` elija el nombre `n` en el tiempo `t+1`:

```
U_{k,n}(t) = β · [α_up · E^↑_{k,n}(t) - α_down · E^↓_{k,n}(t)] + γ · p_{k,n}(t)

P_{k,n}(t+1) = exp(U_{k,n}(t)) / Σ_{n'} exp(U_{k,n'}(t))
```

Donde:
- `α_up`: Intensidad de la exposición ascendente (aspiración)
- `α_down`: Intensidad de la exposición descendente (saturación)
- `β`: Sensibilidad general a las exposiciones
- `γ`: Inercia/persistencia del estado actual

### Matrices de Pesos

#### Matriz de Pesos Ascendentes (W_up)

Define la influencia de clases superiores sobre clases inferiores. Para la clase `k`:
- Solo las clases `j > k` tienen influencia positiva
- El peso decrece exponencialmente con la distancia: `w_{k,j} ∝ exp(-0.5 · (j - k))`
- Los pesos se normalizan para que sumen 1

Ejemplo (para 5 clases):
```
        Q1    Q2    Q3    Q4    Q5
Q1    [  0  0.61  0.37  0.22  0.14]  (normalizado)
Q2    [  0    0  0.61  0.37  0.22]
Q3    [  0    0    0  0.61  0.37]
Q4    [  0    0    0    0  0.61]
Q5    [  0    0    0    0    0]
```

#### Matriz de Pesos Descendentes (W_down)

Define la influencia de clases inferiores sobre clases superiores. Para la clase `k`:
- Solo las clases `j < k` tienen influencia positiva
- El peso decrece exponencialmente con la distancia: `w_{k,j} ∝ exp(-0.5 · (k - j))`
- Los pesos se normalizan para que sumen 1

## Instalación y Uso

### Requisitos

```r
# Librerías necesarias
library(tidyverse)
library(data.table)
```

### Uso Básico

```r
# 1. Cargar el modelo
source("code/05_abm_name_diffusion.R")

# 2. Ejecutar ejemplo completo
results <- run_abm_example(seed = 42, verbose = TRUE)

# 3. Acceder a resultados
abm_results <- results$abm_results
metrics <- results$metrics
```

### Uso Avanzado

#### Simulación Personalizada

```r
# Crear parámetros personalizados
params <- initialize_abm_parameters(
  n_classes = 5,
  n_names = 30,
  start_year = 1920,
  end_year = 2010,
  alpha_up = 0.3,      # Aspiración hacia arriba
  alpha_down = 0.2,    # Saturación desde abajo
  beta = 1.0,          # Sensibilidad
  gamma = 0.1          # Inercia
)

# Inicializar estado
initial_state <- initialize_state(
  params, 
  init_type = "elite_first",  # "uniform", "random", o "elite_first"
  seed = 42
)

# Ejecutar simulación
abm_results <- run_abm_simulation(params, initial_state, verbose = TRUE)

# Calcular métricas
metrics <- calculate_diffusion_metrics(abm_results)
```

#### Análisis de Resultados

```r
# Convertir a data frame
df_results <- abm_to_dataframe(abm_results, include_exposures = TRUE)

# Ver estructura
head(df_results)
#   year class class_num name name_num proportion exposure_up exposure_down
# 1 1920    Q1         1  N01        1  0.0234567   0.0234567     0.0234567
# 2 1920    Q1         1  N02        2  0.0345678   0.0345678     0.0345678
# ...

# Filtrar datos específicos
df_elite_2010 <- df_results %>%
  filter(class == "Q5", year == 2010) %>%
  arrange(desc(proportion))
```

## Visualizaciones

### 1. Evolución de Nombres por Clase

```r
# Visualizar evolución para la elite (Q5)
plot <- plot_name_evolution(abm_results, class_index = 5, top_n = 10)
print(plot)
```

### 2. Heatmap de Distribución

```r
# Heatmap para un año específico
plot <- plot_name_heatmap(abm_results, year = 2010, top_n = 15)
print(plot)
```

### 3. Evolución de la Diversidad

```r
# Evolución de la diversidad por clase
plot <- plot_diversity_evolution(metrics, params)
print(plot)
```

## Métricas de Análisis

El modelo calcula las siguientes métricas:

### 1. Diversidad (Índice de Shannon)

Mide la diversidad de nombres en cada clase:

```
H = -Σ p_i · log(p_i)
```

Valores más altos indican mayor diversidad de nombres.

### 2. Concentración (HHI)

Índice de Herfindahl-Hirschman que mide la concentración:

```
HHI = Σ p_i²
```

Valores más altos indican mayor concentración en pocos nombres.

### 3. Nombres Más Populares

Lista de los top 5 nombres más populares por clase en momentos clave (inicio, medio, fin).

### 4. Velocidad de Difusión

Correlación entre las distribuciones de nombres de clases adyacentes, que indica qué tan rápido se difunden los nombres entre clases.

## Estructura de Archivos

```
code/
├── 05_abm_name_diffusion.R      # Implementación principal del ABM
└── 05b_abm_example_usage.R      # Ejemplos de uso

plot/
├── abm_evolution_elite.png      # Evolución de nombres en elite
├── abm_evolution_low.png        # Evolución de nombres en clase baja
├── abm_heatmap_1920.png         # Distribución inicial
├── abm_heatmap_2010.png         # Distribución final
├── abm_diversity_evolution.png  # Evolución de diversidad
├── abm_exposure_analysis.png    # Análisis de exposiciones
├── abm_diffusion_speed.png      # Velocidad de difusión
└── abm_results.csv              # Datos exportados
```

## Interpretación de Resultados

### Patrones Esperados

1. **Imitación Ascendente**: Las clases bajas tienden a adoptar nombres populares en clases altas con un retraso temporal.

2. **Diferenciación Descendente**: Cuando un nombre se vuelve muy común en clases bajas, las clases altas tienden a abandonarlo.

3. **Homofilia**: Dentro de cada clase, existe una tendencia a mantener ciertos nombres característicos.

4. **Gradiente de Diversidad**: Generalmente, las clases altas muestran mayor diversidad de nombres que las clases bajas.

### Parámetros y Su Interpretación

- **α_up alto (> 0.3)**: Fuerte aspiración social, rápida difusión desde elite hacia abajo
- **α_down alto (> 0.2)**: Fuerte efecto de saturación, elite abandona rápidamente nombres comunes
- **β alto (> 1.0)**: Mayor sensibilidad a las exposiciones, cambios más dramáticos
- **γ alto (> 0.15)**: Mayor inercia, cambios más lentos y graduales

## Extensiones Posibles

1. **Género**: Separar el modelo por género (masculino/femenino)
2. **Geografía**: Incorporar dimensión espacial (comunas/regiones)
3. **Redes Sociales**: Añadir estructura de red entre individuos
4. **Heterogeneidad**: Variar parámetros entre clases
5. **Eventos Históricos**: Incorporar shocks exógenos (ej. cambios políticos)
6. **Nombres Nuevos**: Permitir entrada de nuevos nombres en el sistema
7. **Movilidad Social**: Incorporar movilidad entre clases

## Referencias

Este modelo se basa en la literatura sobre difusión de innovaciones y diferenciación social:

- Berger, J., & Heath, C. (2007). "Where Consumers Diverge from Others: Identity Signaling and Product Domains"
- Lieberson, S. (2000). "A Matter of Taste: How Names, Fashions, and Culture Change"
- Simmel, G. (1957). "Fashion" (original 1904)

## Autores y Contacto

- Cantillan, R.
- Proyecto: Difusión de nombres y clase social en Chile

## Licencia

Este código es parte del proyecto de investigación "Diffusion of names and social class" y está disponible para uso académico.
