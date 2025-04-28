# ----------------------------------------------------------------------------
# ANÁLISIS SOCIOECONÓMICO DE NOMBRES EN CHILE (1920-2022)
# DESCRIPTIVOS***
# ----------------------------------------------------------------------------
# Este script analiza la relación entre patrones de nombres y factores 
# socioeconómicos en Chile durante un siglo, explorando mecanismos
# de imitación y distinción entre clases sociales.
# ----------------------------------------------------------------------------

# =============================================================================
# 1. CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
# =============================================================================

# Librerías para manipulación de datos
library(tidyverse)      
library(ipumsr)         
library(readr)          
library(dplyr)          
library(purrr)          
library(tidyr)          
library(stringr)        
library(forcats)        

# Librerías para análisis de redes y visualización
library(igraph)         
library(migraph)        
library(ggraph)         
library(graphlayouts)   
library(bipartite)      

# Librerías para análisis de texto
library(stringi)        
library(tidytext)       
library(stringdist)     

# Librerías para visualización
library(ggplot2)        
library(ggforce)        
library(ggpubr)         
library(ggalluvial)     
library(patchwork)      
library(scales)         

# Librerías para análisis especializados
library(RPostgres)      
library(TraMineR)       
library(tidylog)        
library(entropy)        
library(netmem)         

# =============================================================================
# 2. CONFIGURACIÓN DEL TEMA VISUAL PARA GRÁFICOS
# =============================================================================

# Definición de un tema personalizado para todos los gráficos
my_custom_theme <- function() {
  theme_bw() + 
    theme(
      plot.background = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      panel.border = element_rect(size = 1, colour = "black"),
      panel.spacing = unit(0.3, "lines"),
      axis.line = element_line(size = 0.1, colour = "black"),
      axis.ticks.y = element_line(size = 0.5, colour = "black"),
      axis.ticks.length = unit(-0.15, "cm"),
      axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
      axis.text.y = element_text(size = 12, color = "black", margin = margin(r = 3)),
      text = element_text(size = 18),
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.title.position = "plot"
    )
}

# Establecer tema por defecto para todos los gráficos
theme_set(my_custom_theme())

# Paleta de colores discreta para visualizaciones consistentes
mypal_disc <- c("#007BFF", "#FFA24A", "#E60000","#41D941", "#003366", 
                "#FF8484", "#6C3483", "#607D39", "#FFD966")

# Deshabilitar notación científica para números
options(scipen=999)

# =============================================================================
# 3. CARGA Y PREPARACIÓN INICIAL DE DATOS
# =============================================================================

# Carga de datos desde archivo RData
load("C:/Users/qramo/Documents/names/data/30982/datos_combinados.RData")
names <- datos_combinados
rm(datos_combinados)
gc()  # Limpieza de memoria

# Renombrar columnas para mejor manejo
colnames(names) <- c("comuna", "ano", "sexo", "nombre", "cantidad")

# Normalización de caracteres especiales en texto
names$comuna = stri_trans_general(str = names$comuna, id = "Latin-ASCII")
names$nombre = stri_trans_general(str = names$nombre, id = "Latin-ASCII")

# =============================================================================
# 4. PROCESAMIENTO Y NORMALIZACIÓN DE NOMBRES
# =============================================================================

# Separar los nombres compuestos en primer y segundo nombre
nombres_procesados <- names %>%
  mutate(decada = ano - ano %% 10) %>%  # Crear variable de década
  separate(nombre, into=c("primer_nombre","segundo_nombre"), 
           sep=" ", extra = "merge", remove = FALSE) %>% 
  mutate(segundo_nombre = na_if(segundo_nombre, ""))  # Convertir strings vacíos a NA

# Calcular probabilidades para primer nombre
probs_primernombre <- nombres_procesados %>% 
  group_by(decada, sexo, primer_nombre) %>% 
  summarise(num = n()) %>% 
  group_by(decada, sexo) %>% 
  mutate(denom = sum(num)) %>% 
  mutate(p_primernombre = num/denom) 

# Calcular probabilidades para segundo nombre
probs_segundonombre <- nombres_procesados %>%
  drop_na(segundo_nombre) %>% 
  group_by(decada, sexo, segundo_nombre) %>% 
  summarise(num = n()) %>% 
  group_by(decada, sexo) %>% 
  mutate(denom = sum(num)) %>% 
  mutate(p_segundonombre = num/denom) 

# Calcular probabilidades para nombre completo
probs_nombrecompleto <- nombres_procesados %>% 
  group_by(decada, sexo, nombre) %>% 
  summarise(num = n()) %>% 
  group_by(decada, sexo) %>% 
  mutate(denom = sum(num)) %>% 
  mutate(p_nombrecompleto = num/denom) 

# Unir todas las probabilidades calculadas
x <- probs_nombrecompleto %>% 
  separate(nombre, into=c("primer_nombre","segundo_nombre"), 
           sep=" ", extra = "merge", remove = FALSE) %>% 
  left_join(probs_primernombre, by = c("decada", "sexo", "primer_nombre")) %>% 
  left_join(probs_segundonombre, by = c("decada", "sexo", "segundo_nombre"))

# Calcular ratio logarítmico para identificar asociaciones no aleatorias entre nombres
x <- x %>% 
  mutate(p_segundonombre = if_else(is.na(p_segundonombre) == TRUE, 1, p_segundonombre)) %>% 
  mutate(p_indep = p_segundonombre * p_primernombre) %>% 
  # Si log_ratio > 0.4, indica fuerte asociación entre primer y segundo nombre
  mutate(log_ratio = log(p_nombrecompleto / p_indep)) %>% 
  filter(num > 100)  # Filtrar nombres con suficiente frecuencia

# Simplificar columnas
x <- x %>% 
  filter(sexo %in% c("F", "M")) %>% 
  select(
    decada, sexo, nombre, p_nombrecompleto, 
    p_primernombre, p_segundonombre, p_indep, log_ratio)

# Determinar el nombre final a usar (nombre completo o primer nombre)
nombres_procesados <- left_join(nombres_procesados, x, by = c("nombre", "decada", "sexo"))
rm(x); gc()  # Limpieza de memoria

nombres_procesados <- nombres_procesados %>%
  mutate(nombre_final = case_when(
    # Usar nombre completo si:
    # 1. Segundo nombre es preposición (DE, DEL, etc.)
    # 2. Log_ratio indica fuerte asociación entre primer y segundo nombre
    (!is.na(segundo_nombre) & str_detect(segundo_nombre, "^(DE |DEL |DE LOS |DE LAS)")) | 
      log_ratio > 0.4 ~ nombre,
    # De lo contrario, usar solo el primer nombre
    TRUE ~ primer_nombre
  ))

# Limpieza de variables temporales
rm(probs_nombrecompleto, probs_primernombre, probs_segundonombre, names)
gc()

# =============================================================================
# 5. IDENTIFICACIÓN DE NOMBRES MÁS POPULARES
# =============================================================================

# Calcular popularidad nacional sumando todas las comunas
nombres_nacionales <- nombres_procesados %>%
  mutate(decada = ano - ano %% 10) %>%
  filter(sexo %in% c("F","M")) %>% 
  group_by(decada, sexo, nombre_final) %>%
  summarise(total_nacional = sum(cantidad)) %>%
  # Seleccionar los TOP 20 a nivel país
  group_by(decada, sexo) %>%
  slice_max(order_by = total_nacional, n = 20) %>%
  ungroup()

# Filtrar datos originales para mantener solo los nombres top
top_nombres <- nombres_procesados %>%
  mutate(decada = ano - ano %% 10) %>%
  semi_join(nombres_nacionales, by = c("decada", "sexo", "nombre_final")) %>%
  group_by(decada, comuna, sexo, nombre_final) %>%
  summarise(total_cantidad = sum(cantidad)) %>%
  ungroup()

# Separar datos por género para análisis específicos
top_nombres_fem <- top_nombres %>% filter(sexo == "F")
top_nombres_mas <- top_nombres %>% filter(sexo == "M")

# Guardar datos para análisis posteriores
#save(top_nombres, file = "C:/Users/qramo/Desktop/top_nombres.RData")
#save(top_nombres_fem, file = "C:/Users/qramo/Desktop/top_nombres_fem.RData")
#save(top_nombres_mas, file = "C:/Users/qramo/Desktop/top_nombres_mas.RData")

# =============================================================================
# 6. IDENTIFICACIÓN DE NOMBRES NUEVOS Y RECICLADOS
# =============================================================================

# 6.1 Identificación de nombres nuevos (raros históricos)
# =======================================================
# Un nombre nuevo es aquel que ha aparecido pocas veces en el pasado
raros_hist <- data.frame()

for (i in 1921:2022) {
  cat("========= ", i, " =========")
  hist <- nombres_procesados %>% filter(ano < i) 
  
  # Un nombre es "raro" si su frecuencia acumulada es baja
  # El umbral crece con el tiempo: (i-1920)*10
  raros <- hist %>% group_by(nombre_final) %>%
    summarise(acum = sum(cantidad)) %>%
    mutate(ano=i, raro=1) %>%
    filter(acum <= (i-1920)*10)  
  
  raros_hist <- rbind(raros_hist, raros)
}

# 6.2 Identificación de nombres reciclados 
# ========================================
# Nombres poco frecuentes en los últimos 30 años pero que reaparecen
start_year <- 1955 
end_year <- 2022
gap_years <- 30
raros_recent <- tibble()

for (i in seq(start_year, end_year)) {
  # Período de 30 años anterior al año actual
  hist_until <- i - gap_years
  
  cat("========= ", hist_until, "-", i, " =========")
  
  hist <- nombres_procesados %>% 
    filter(ano >= hist_until)
  
  # Un nombre es "reciclado" si fue poco frecuente en los últimos 30 años
  # pero aparece en el año actual
  raros_ <- hist %>%
    group_by(nombre_final) %>%
    summarise(acum = sum(cantidad)) %>% 
    mutate(ano = i, 
           raro_recent = 1) %>%
    filter(acum <= gap_years*10)  # Umbral basado en duración del periodo
  
  raros_recent <- bind_rows(raros_recent, raros_)
}

# 6.3 Unir información de nombres raros y reciclados al dataset principal
# =======================================================================
nombres_procesados$ano <- as.numeric(nombres_procesados$ano)
raros_hist$ano <- as.numeric(raros_hist$ano)

nombres_procesados <- nombres_procesados %>% 
  left_join(raros_hist %>% select(-acum), by=c("nombre_final", "ano"))

nombres_procesados <- nombres_procesados %>% 
  left_join(raros_recent %>% select(-acum), by=c("nombre_final", "ano"))

nombres_procesados <- nombres_procesados %>% 
  replace_na(list(raro=0, raro_recent=0))  # Reemplazar NA con 0

# Limpieza de variables temporales
rm(raros, raros_, raros_hist, raros_recent, hist)
gc()

# Guardar datos procesados
save(nombres_procesados, file = "nombres_procesados.RData")

# =============================================================================
# 7. INTEGRACIÓN CON DATOS SOCIOECONÓMICOS
# =============================================================================

# Cargar datos socioeconómicos por comuna
load("C:/Users/qramo/Desktop/names_data/gse_comunas.RData")

# CORRECCIÓN: Crear un subset limpio para evitar problemas de columnas
nombres_procesados_subset <- nombres_procesados %>%
  select(comuna, ano, sexo, nombre_final, cantidad, raro, raro_recent)

# Normalizar caracteres especiales
nombres_procesados_subset$comuna <- stri_trans_general(str = nombres_procesados_subset$comuna, id = "Latin-ASCII")

# Asegurar tipos de datos compatibles para el join
nombres_procesados_subset$ano <- as.numeric(nombres_procesados_subset$ano)
gse_comunas$ano <- as.numeric(gse_comunas$ano)

# Realizar el join de forma segura
names_gse <- left_join(nombres_procesados_subset, gse_comunas, by = c("comuna", "ano")) %>%
  filter(ano >= 1955)  # Filtrar años con datos disponibles

# Asignar año de censo más cercano para extender datos socioeconómicos
census_years <- c(1960, 1970, 1982, 1992, 2002, 2017)
names_gse <- names_gse %>% 
  mutate(census_year = case_when(
    ano %in% (1955:1965) ~ 1960,  
    ano %in% (1966:1976) ~ 1970,
    ano %in% (1977:1987) ~ 1982, 
    ano %in% (1988:1998) ~ 1992,
    ano %in% (1999:2009) ~ 2002,
    ano %in% (2010:2022) ~ 2017
  ))

# Imputar datos socioeconómicos faltantes usando la mediana por comuna/censo
names_gse <- names_gse %>%
  group_by(comuna, census_year) %>%
  mutate(
    # ISEI = International Socio-Economic Index (índice socioeconómico ocupacional)
    median_prom_isei = ifelse(is.na(median_prom_isei), 
                              median(median_prom_isei, na.rm = TRUE), 
                              median_prom_isei),
    # Educación promedio
    median_prom_school = ifelse(is.na(median_prom_school),
                                median(median_prom_school, na.rm = TRUE),  
                                median_prom_school)
  ) %>%
  select(-census_year)

# Limpieza de variables
if(exists("ipums")) rm(ipums)
if(exists("id_comunas_ipums")) rm(id_comunas_ipums)
gc()

# =============================================================================
# 8. CÁLCULO DE PUNTAJE SOCIOECONÓMICO PARA NOMBRES
# =============================================================================

# Calcular puntaje socioeconómico ponderado para cada nombre
names_gse <- names_gse %>%
  group_by(nombre_final, ano, sexo) %>%
  # Promedio ponderado por cantidad de registros
  mutate(weighted_isei = weighted.mean(median_prom_isei, cantidad, na.rm = TRUE)) %>% 
  mutate(weighted_school = weighted.mean(median_prom_school, cantidad, na.rm = TRUE)) %>%
  ungroup()

# Clasificar nombres en quintiles socioeconómicos por año
names_gse <- names_gse %>%
  group_by(ano) %>%
  mutate(quintil_weighted_isei = ntile(weighted_isei, 5)) %>%
  mutate(quintil_weighted_school = ntile(weighted_school, 5)) 

# Clasificar cada nombre según su origen
names_gse <- names_gse %>% 
  mutate(tipo_nombre = case_when(
    raro == 1 ~ "nuevo",              # Nombre nuevo/raro
    raro_recent == 1 & raro == 0 ~ "reciclado",  # Nombre que reapareció
    TRUE ~ "otro"                     # Nombre común/establecido
  ))

# Guardar datos procesados
save(names_gse, file = "C:/Users/qramo/Desktop/names_gse.RData")

# =============================================================================
# 9. VISUALIZACIONES DE NOMBRES NUEVOS Y RECICLADOS POR QUINTIL
# =============================================================================

# 9.1 Gráfico de nombres nuevos por año y clase social
# ====================================================
nombres_nuevos_plot <- names_gse %>% 
  filter(!is.na(quintil_weighted_isei)) %>% 
  filter(tipo_nombre == "nuevo") %>%
  ggplot(aes(x = ano, y = cantidad, fill = tipo_nombre)) +
  scale_x_continuous(breaks = c(1955, 1980, 2000, 2020)) + 
  geom_bar(stat = "identity", color = "#FF6666") +
  facet_wrap(~quintil_weighted_isei) +
  scale_fill_manual(values = mypal_disc) +
  labs(title = "New names by year and Social Class (quintile)",
       x = "",
       y = "Freq.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  guides(fill=FALSE)

print(nombres_nuevos_plot)

# 9.2 Gráfico de nombres reciclados por año y clase social
# ========================================================
nombres_reciclados_plot <- names_gse %>% 
  filter(!is.na(quintil_weighted_isei)) %>% 
  filter(tipo_nombre == "reciclado") %>%
  ggplot(aes(x = ano, y = cantidad, fill = tipo_nombre)) +
  geom_bar(stat="identity", color = "#FF6666") + 
  facet_wrap(~quintil_weighted_isei) +
  scale_colour_manual(values = mypal_disc) +
  labs(title = "Recicled names by year and Social Class (quintile)",
       x = "",
       y = "Freq.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  guides(fill=FALSE)

print(nombres_reciclados_plot)

# =============================================================================
# 10. ANÁLISIS DE DIFUSIÓN POR QUINTILES SOCIOECONÓMICOS
# =============================================================================

# 10.1 Preparar datos para análisis de difusión y popularidad
# ===========================================================
g1 <- names_gse %>% 
  group_by(ano, quintil_weighted_isei, nombre_final) %>% 
  mutate(cantidad_pais = sum(cantidad, na.rm=FALSE)) %>%
  filter(cantidad_pais >= 10) %>%  # Filtrar nombres con frecuencia mínima
  ungroup() %>%
  group_by(ano) %>%
  # q = popularidad relativa normalizada (0-1)
  mutate(q = rank(cantidad_pais)/max(rank(cantidad_pais)))

# 10.2 Análisis de correlación entre popularidad y nivel socioeconómico
# =====================================================================
c1 <- g1 %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(cor_qi_isei = cor(q, weighted_isei, method = "spearman"))

# Graficar correlación entre popularidad y NSE a lo largo del tiempo
correlacion_plot <- c1 %>%  
  ggplot(aes(x=ano, y=cor_qi_isei, color=factor(quintil_weighted_isei))) + 
  geom_point() +
  geom_line(aes(ano)) +
  geom_line(group = 1) +
  facet_wrap(~quintil_weighted_isei) +
  scale_colour_manual(values = mypal_disc) +
  labs(title="Correlación entre popularidad y NSE a lo largo del tiempo",  
       x="Año",
       y="Coeficiente de correlación",
       color = "Quintil (ISEI)")

print(correlacion_plot)

# =============================================================================
# 11. ANÁLISIS DE SECUENCIAS Y TRAYECTORIAS DE DIFUSIÓN
# =============================================================================

# 11.1 Analizar nombres top por quintil a lo largo de décadas
# ===========================================================
# Convertir año a década para agregación
g1$decada <- as.integer(g1$ano %/% 10) * 10

# Agrupar datos por década, nombre y quintil
datos_agrupados <- g1 %>%
  group_by(decada, nombre_final, quintil_weighted_isei) %>%
  summarise(cantidad_total = sum(cantidad))

# Ordenar y seleccionar top 10 por quintil/década
datos_ordenados <- datos_agrupados %>%
  arrange(decada, quintil_weighted_isei, desc(cantidad_total))

top_nombres_secuencia <- datos_ordenados %>%
  group_by(decada, quintil_weighted_isei) %>%
  top_n(10, wt = cantidad_total)

# 11.2 Crear dataframes por quintil para análisis de secuencias
# =============================================================
crear_dataframes_por_quintil <- function(datos) {
  lapply(1:5, function(quintil) {
    nombre_data_frame <- paste0("top_nombre_q", quintil)
    df <- datos %>%
      filter(quintil_weighted_isei == quintil) %>%
      select(-cantidad_total)
    assign(nombre_data_frame, df, envir = .GlobalEnv)
  })
}

crear_dataframes_por_quintil(top_nombres_secuencia)

# 11.3 Crear matrices de afiliación para análisis de secuencias
# =============================================================
# Filtrar a décadas específicas para visualización
top_nombre_q1 <- top_nombre_q1 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq1 <- netmem::edgelist_to_matrix(cbind(top_nombre_q1$nombre_final, top_nombre_q1$decada), bipartite = TRUE)

top_nombre_q2 <- top_nombre_q2 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq2 <- netmem::edgelist_to_matrix(cbind(top_nombre_q2$nombre_final, top_nombre_q2$decada), bipartite = TRUE)

top_nombre_q3 <- top_nombre_q3 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq3 <- netmem::edgelist_to_matrix(cbind(top_nombre_q3$nombre_final, top_nombre_q3$decada), bipartite = TRUE)

top_nombre_q4 <- top_nombre_q4 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq4 <- netmem::edgelist_to_matrix(cbind(top_nombre_q4$nombre_final, top_nombre_q4$decada), bipartite = TRUE)

top_nombre_q5 <- top_nombre_q5 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq5 <- netmem::edgelist_to_matrix(cbind(top_nombre_q5$nombre_final, top_nombre_q5$decada), bipartite = TRUE)

# 11.4 Convertir matrices a dataframes
# ===================================
df_seq_1 <- as.data.frame(mq1) %>% rownames_to_column("name")
df_seq_2 <- as.data.frame(mq2) %>% rownames_to_column("name")
df_seq_3 <- as.data.frame(mq3) %>% rownames_to_column("name")
df_seq_4 <- as.data.frame(mq4) %>% rownames_to_column("name")
df_seq_5 <- as.data.frame(mq5) %>% rownames_to_column("name")

# 11.5 Crear secuencias para análisis TraMineR
# ===========================================
seqs_1 <- seqdef(df_seq_1[-1], var = names(df_seq_1[-1]), states = c("0","1"), id = df_seq_1$name)
seqs_2 <- seqdef(df_seq_2[-1], var = names(df_seq_2[-1]), states = c("0","1"), id = df_seq_2$name)
seqs_3 <- seqdef(df_seq_3[-1], var = names(df_seq_3[-1]), states = c("0","1"), id = df_seq_3$name)
seqs_4 <- seqdef(df_seq_4[-1], var = names(df_seq_4[-1]), states = c("0","1"), id = df_seq_4$name)
seqs_5 <- seqdef(df_seq_5[-1], var = names(df_seq_5[-1]), states = c("0","1"), id = df_seq_5$name)

# 11.6 Convertir a formato largo (tidy) para visualización
# =======================================================
df_seqs_q1 <- as.data.frame(seqs_1) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q2 <- as.data.frame(seqs_2) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q3 <- as.data.frame(seqs_3) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q4 <- as.data.frame(seqs_4) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q5 <- as.data.frame(seqs_5) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

# 11.7 Ordenar los dataframes por estado y tiempo
# ==============================================
df_seqs_q1 <- df_seqs_q1 %>% arrange(desc(state), time)
df_seqs_q2 <- df_seqs_q2 %>% arrange(desc(state), time)
df_seqs_q3 <- df_seqs_q3 %>% arrange(desc(state), time)
df_seqs_q4 <- df_seqs_q4 %>% arrange(desc(state), time)
df_seqs_q5 <- df_seqs_q5 %>% arrange(desc(state), time)

# 11.8 Factorizar para preservar el orden en visualizaciones
# =========================================================
df_seqs_q1$name <- factor(df_seqs_q1$name, levels = unique(df_seqs_q1$name))
df_seqs_q2$name <- factor(df_seqs_q2$name, levels = unique(df_seqs_q2$name))
df_seqs_q3$name <- factor(df_seqs_q3$name, levels = unique(df_seqs_q3$name))
df_seqs_q4$name <- factor(df_seqs_q4$name, levels = unique(df_seqs_q4$name))
df_seqs_q5$name <- factor(df_seqs_q5$name, levels = unique(df_seqs_q5$name))

# =============================================================================
# 12. VISUALIZACIONES COMPARATIVAS DE TRAYECTORIAS ENTRE QUINTILES
# =============================================================================

# 12.1 Gráfico de presencia de nombres en quintil 5 a lo largo del tiempo
# ======================================================================
p5 <- ggplot(df_seqs_q5) +
  geom_tile(aes(x = time, y = name, fill = state), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  ylab("") + xlab("") +
  labs(title = "Top nombres por año para Quintil 5") +
  theme(axis.text.y = element_text(angle = 0, size = 7)) +
  scale_colour_manual(values = mypal_disc) +
  guides(fill=FALSE)

print(p5)

# 12.2 Preparar datos para comparación entre quintiles
# ===================================================
df_seqs_q5 <- df_seqs_q5 %>% rename(state_q5 = state)
df_seqs_q4 <- df_seqs_q4 %>% rename(state_q4 = state)
df_seqs_q3 <- df_seqs_q3 %>% rename(state_q3 = state)
df_seqs_q2 <- df_seqs_q2 %>% rename(state_q2 = state)
df_seqs_q1 <- df_seqs_q1 %>% rename(state_q1 = state)

# 12.3 Unir datos de todos los quintiles para comparación (Q5 como base)
# =====================================================================
df_tops_q5_all <- df_seqs_q5 %>%
  left_join(df_seqs_q1, by = c("name", "time")) %>%
  left_join(df_seqs_q2, by = c("name", "time")) %>%
  left_join(df_seqs_q3, by = c("name", "time")) %>%
  left_join(df_seqs_q4, by = c("name", "time"))

# 12.4 Crear dataframe para visualización comparativa
# ==================================================
df_points <- rbind(
  transform(df_tops_q5_all, quintil = "state_q1"),
  transform(df_tops_q5_all, quintil = "state_q2"),
  transform(df_tops_q5_all, quintil = "state_q3"),
  transform(df_tops_q5_all, quintil = "state_q4"),
  transform(df_tops_q5_all, quintil = "state_q5")
)

# Convertir estados a numéricos y reemplazar NA con 0
df_points[, 3:7] <- lapply(df_points[, 3:7], function(x) {
  x[is.na(x)] <- 0
  as.numeric(as.character(x))
})

# 12.5 Definir colores para cada quintil
# =====================================
colors <- c("state_q1" = "#007BFF", "state_q2" = "#FFA24A", 
            "state_q3" = "#E60000", "state_q4" = "#41D941", 
            "state_q5" = "#003366")

# 12.6 Gráfico comparativo Q5 vs otros quintiles
# =============================================
# Este gráfico muestra cómo los nombres originalmente populares en Q5
# se adoptan en otros quintiles con el tiempo
comparativo_quintiles_plot <- ggplot() +
  # Base: nombres del Q5
  geom_tile(data = df_tops_q5_all, aes(x = time, y = name, fill = factor(state_q5)), alpha = 0.8) +
  # Puntos cuando ese nombre aparece en otros quintiles
  geom_point(data = subset(df_points, state_q2 == 1), 
             aes(x = time, y = name, color = "state_q2"), size = 3) +
  geom_point(data = subset(df_points, state_q3 == 1), 
             aes(x = time, y = name, color = "state_q3"), size = 3) +
  geom_point(data = subset(df_points, state_q4 == 1), 
             aes(x = time, y = name, color = "state_q4"), size = 3) +
  geom_point(data = subset(df_points, state_q1 == 1), 
             aes(x = time, y = name, color = "state_q1"), size = 3) +
  # Escalas de colores
  scale_fill_manual(values = c("0" = "white", "1" = "#003366"), name = "State Q5") +
  scale_color_manual(values = colors, name = "Quintil") +
  labs(title = "Q5 / Quintil comparison", y = "", x = "") +
  theme(legend.position = "right",
        axis.text.y = element_text(size = 8)) +
  guides(fill = guide_legend(title = "State Q5"))

print(comparativo_quintiles_plot)

# =============================================================================
# 13. ANÁLISIS DE DIFUSIÓN TEMPORAL ENTRE QUINTILES
# =============================================================================

# 13.1 Análisis de difusión de nombres populares del quintil 5 en 1960
# ====================================================================
# Identificar los nombres más populares en Q5 en 1960
nombres_populares_1960 <- g1 %>%
  filter(ano == 1955 & quintil_weighted_isei == 5) %>%
  group_by(nombre_final) %>%
  summarise(popularidad = sum(q, na.rm = TRUE)) %>%
  arrange(desc(popularidad)) %>%
  top_n(20)

# Seguir estos nombres a lo largo del tiempo en todos los quintiles
datos_interes <- g1 %>%
  semi_join(nombres_populares_1960, by = "nombre_final")

# Calcular el promedio de popularidad de nombres top de Q5-1960 en cada quintil por año
promedio_por_cuartil <- datos_interes %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar la evolución temporal de adopción en cada quintil
difusion_1960_plot <- ggplot(promedio_por_cuartil, 
                             aes(x = ano, y = promedio_popularidad, 
                                 color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei)), linewidth = 0.8) +  
  geom_point(size = 2, stroke = 0.5, shape = 21, fill = "white") +
  labs(title = "Most Popular Names in Quintile 5 in 1960",
       x = "Year",
       y = "Average Popularity",
       color = "Socioeconomic\nQuintile") +
  scale_colour_manual(values = mypal_disc) +
  scale_x_continuous(breaks = seq(1960, 2017, by = 5)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "gray90"),
    legend.margin = margin(5, 5, 5, 5)
  )

print(difusion_1960_plot)

# 13.2 Análisis de popularidad promedio general por quintil
# ========================================================
# Filtrar datos para todos los años y quintiles
datos_interes_general <- g1 %>%
  filter(ano %in% 1955:2022,
         quintil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Calcular la popularidad promedio en cada quintil por año
tendencias_temporales <- datos_interes_general %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar tendencias generales de popularidad por quintil
tendencias_plot <- ggplot(tendencias_temporales, 
                          aes(x = ano, y = promedio_popularidad, 
                              color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei)), linewidth = 0.8) +  
  geom_point(size = 2, stroke = 0.5, shape = 21, fill = "white") +
  labs(title = "Trends of Average Popularity of Names by Socioeconomic Quintile",
       x = "Year",
       y = "Average Popularity",
       color = "Socioeconomic\nQuintile") +
  scale_colour_manual(values = mypal_disc) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "gray90"),
    legend.margin = margin(5, 5, 5, 5)
  )

print(tendencias_plot)

