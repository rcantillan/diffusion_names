#==============================================================================
# CREACIÓN DE BASE DE DATOS DE ATRIBUTOS DE COMUNAS EN CHILE
#==============================================================================
# Este script construye un dataset de atributos para las comunas (municipalidades)
# de Chile, incluyendo variables sociodemográficas, socioeconómicas y geográficas.
#
# El dataset resultante (attr_comunas) es un insumo esencial para el análisis
# de difusión de nombres, ya que aporta las características contextuales 
# necesarias para entender los patrones de difusión entre comunas.

# * IMPORTANTE: Tal cómo está configurado el workflow, este script debe ejcutarse 
# antes de crear los atributos de diadas. 
#
# Variables incluidas:
# - Población comunal estimada (basada en nacimientos)
# - Índice socioeconómico ISEI (International Socio-Economic Index)
# - Años de escolaridad
# - Coordenadas geográficas (latitud y longitud)
#==============================================================================

#------------------------------------------------------------------------------
# 1. CONFIGURACIÓN INICIAL
#------------------------------------------------------------------------------
# Cargar librerías necesarias
library(tidyverse)   # Para manipulación y visualización de datos
library(mgcv)        # Para modelos aditivos generalizados
library(haven)       # Para importar datos de STATA
library(naniar)      # Para manejo de valores NA
library(stringi)     # Para procesamiento de texto

#------------------------------------------------------------------------------
# 2. ESTIMACIÓN DE POBLACIÓN COMUNAL
#------------------------------------------------------------------------------
# Esta sección estima la población de cada comuna a partir de:
# - Número de nacimientos registrados
# - Tasas de natalidad históricas a nivel nacional

# Cargar datos de nacimientos
load("/media/rober/4612-9FBE/names/30982/datos_combinados.RData")

# 2.1 Crear dataframe con tasas de natalidad históricas (Datos del Banco Mundial)
# Tasa de natalidad es el número de nacimientos por cada 1000 habitantes
tasas_natalidad <- data.frame(
  AÑO = 1960:2022,
  tasa = c(35.275, 34.825, 34.277, 33.717, 33.063, 32.357, 31.644, 30.916, 30.257, 29.526, 
           28.76, 28.031, 27.435, 26.797, 26.103, 25.477, 24.931, 24.462, 24.052, 23.645, 
           23.294, 23.009, 22.755, 22.615, 22.573, 22.521, 22.53, 22.527, 22.473, 22.39, 
           22.19, 21.866, 21.048, 20.503, 19.918, 19.155, 18.542, 17.847, 17.195, 16.526, 
           15.963, 15.658, 15.367, 14.802, 14.379, 14.169, 14.139, 14.301, 14.459, 14.464, 
           14.327, 14.109, 13.857, 13.697, 13.545, 13.211, 12.7, 12.13, 11.94, 11.855, 
           11.834, 11.788, 11.755)
)

# 2.2 Calcular total de nacimientos por comuna y año
nacimientos_comuna <- datos_combinados %>%
  group_by(`OF INSC NAC`, AÑO) %>%
  summarize(
    nacimientos = sum(CANTIDAD),  # Suma total de nacimientos registrados
    .groups = 'drop'
  )

# 2.3 Unir con tasas de natalidad y estimar población
# Fórmula: población = (nacimientos * 1000) / tasa de natalidad
poblacion_estimada <- nacimientos_comuna %>%
  left_join(tasas_natalidad, by = "AÑO") %>%
  mutate(
    # Para años anteriores a 1960, usar la tasa de 1960
    tasa = ifelse(is.na(tasa), 35.275, tasa),
    # Calcular población: (nacimientos * 1000) / tasa
    poblacion = round((nacimientos * 1000) / tasa)
  )

# 2.4 Crear resumen estadístico de población para verificar resultados
summary_poblacion <- poblacion_estimada %>%
  group_by(AÑO) %>%
  summarize(
    n_comunas = n(),                    # Número de comunas con datos
    poblacion_total = sum(poblacion),   # Población total estimada
    media_poblacion = mean(poblacion),  # Población promedio
    mediana_poblacion = median(poblacion)  # Mediana de población
  )

# 2.5 Simplificar el dataset final de población
poblacion_estimada <- poblacion_estimada %>% 
  select(comuna = `OF INSC NAC`, ano = AÑO, poblacion)

#------------------------------------------------------------------------------
# 3. CONSTRUCCIÓN DE INDICADORES SOCIOECONÓMICOS COMUNALES
#------------------------------------------------------------------------------
# Esta sección construye indicadores de estatus socioeconómico 
# a nivel comunal utilizando datos censales de IPUMS International

# 3.1 Cargar datos de IPUMS (censos chilenos)
ipums <- read_dta("/media/rober/4612-9FBE/names/proyecto_nombres/ipumsi_00005.dta")
ipums <- ipumsi_00005
gc()  # Liberar memoria

# 3.2 Cargar diccionarios de comunas para cada año censal
# Estos son necesarios para mapear los códigos geográficos de IPUMS
# a nombres de comunas

## 1960
id_comunas_1960 <- read_delim("C:/Users/qramo/Documents/names/data/id_comunas_1960", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1960) <- c("id", "name")
id_comunas_1960$year <- 1960

## 1970
id_comunas_1970 <- read_delim("C:/Users/qramo/Documents/names/data/id_comunas_1970", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1970) <- c("id", "name")
id_comunas_1970$year <- 1970

## 1982
id_comunas_1982 <- read_delim("C:/Users/qramo/Documents/names/data/id_comunas_1982", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1982) <- c("id", "name")
id_comunas_1982$year <- 1982

## 1992
id_comunas_1992 <- read_delim("C:/Users/qramo/Documents/names/data/id_comunas_1992", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1992) <- c("id", "name")
id_comunas_1992$year <- 1992

## 2002
id_comunas_2002 <- read_delim("C:/Users/qramo/Documents/names/data/id_comunas_2002", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2002) <- c("id", "name")
id_comunas_2002$year <- 2002

## 2017
id_comunas_2017 <- read_delim("C:/Users/qramo/Documents/names/data/id_comunas_2017", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2017) <- c("id", "name")
id_comunas_2017$year <- 2017

# 3.3 Unir todos los diccionarios de comunas
id_comunas_ipums <- rbind(
  id_comunas_1960, id_comunas_1970, id_comunas_1982, 
  id_comunas_1992, id_comunas_2002, id_comunas_2017
)

# Limpiar variables temporales
rm(id_comunas_1960, id_comunas_1970, id_comunas_1982, 
   id_comunas_1992, id_comunas_2002, id_comunas_2017)

# 3.4 Procesar diccionario de comunas
# Algunas entradas tienen múltiples nombres de comunas separados por comas
id_comunas_ipums <- id_comunas_ipums %>%            
  separate_rows(name, sep = ",") %>%   # Separar filas con múltiples comunas
  mutate(across(where(is.character), str_trim))  # Eliminar espacios

# Normalizar nombres: eliminar acentos y convertir a mayúsculas
id_comunas_ipums$name <- stri_trans_general(id_comunas_ipums$name, id = "Latin-ASCII")
id_comunas_ipums$name <- toupper(id_comunas_ipums$name)

# 3.5 Procesar datos socioeconómicos de IPUMS
# Filtrar jefes de hogar y sus parejas, y construir índice ISEI
ipums <- ipums %>%
  # relate = 1 (jefe hogar) o 2 (cónyuge)
  filter(relate %in% c(1, 2)) %>%
  # Crear índice ISEI basado en la clasificación ocupacional ISCO
  mutate(isei = case_when(
    occisco == 1 ~ 68,     # Directivos
    occisco == 2 ~ 65,     # Profesionales
    occisco == 3 ~ 51,     # Técnicos
    occisco == 4 ~ 41,     # Empleados administrativos
    occisco == 5 ~ 31,     # Trabajadores de servicios
    occisco == 6 ~ 18,     # Trabajadores agrícolas
    occisco == 7 ~ 35,     # Oficiales y operarios
    occisco == 8 ~ 32,     # Operadores de instalaciones
    occisco == 9 ~ 20,     # Ocupaciones elementales
    occisco == 10 ~ 53,    # Fuerzas armadas
    occisco == 11 ~ 0,     # Otros
    occisco == 97 ~ 0,     # NS/NR
    occisco == 98 ~ 0,     # NA
    occisco == 99 ~ 0      # Missing
  )) %>%
  # Reemplazar códigos de valores faltantes en años de escolaridad
  replace_with_na(replace = list(yrschool = c("90", "91", "92", "93", "94", "95", "96", "98", "99"))) %>%
  # Seleccionar variables relevantes
  select(serial,
         year,
         geo2_cl1970,
         geo2_cl1960,
         geo2_cl1982,
         geo2_cl1992,
         geo2_cl2002,
         geo2_cl2017,
         relate,
         yrschool,
         isei
  )

gc()  # Liberar memoria

# 3.6 Transformar datos de formato largo a ancho para jefes de hogar y cónyuges
ipums <- ipums %>% 
  pivot_wider(
    names_from = relate, 
    values_from = c(yrschool, isei), 
    values_fn = list(isei = mean, yrschool = mean)
  )

# 3.7 Unificar columnas geográficas para obtener identificador único de comuna
# Lista de todas las variables geográficas
geo_vars <- grep("geo2_cl", colnames(ipums), value = TRUE)

# Unir variables geográficas y extraer ID numérico
ipums <- ipums %>%
  unite("id_comuna", all_of(geo_vars), sep = "|", na.rm = TRUE) %>%
  mutate(id_comuna = as.numeric(str_extract(id_comuna, "\\d+"))) 

# 3.8 Calcular promedios de ISEI y años de escolaridad por hogar
# Función para calcular el promedio entre jefe de hogar y cónyuge, manejando NAs
promedio_con_na <- function(x, y) {
  if (is.na(x) | is.na(y)) {
    return(mean(c(x, y), na.rm = TRUE))
  } else {
    return((x + y) / 2)
  }
}

# Calcular promedios por hogar
ipums <- ipums %>%
  mutate(
    prom_isei = pmap_dbl(select(., isei_1, isei_2), ~promedio_con_na(..1, ..2)),
    prom_school = pmap_dbl(select(., yrschool_1, yrschool_2), ~promedio_con_na(..1, ..2))
  )

# 3.9 Unir datos de IPUMS con nombres de comunas
id_comunas_ipums$id <- as.numeric(id_comunas_ipums$id)
id_comunas_ipums <- id_comunas_ipums %>% distinct(.keep_all = TRUE)
ipums <- ipums %>% rename(id = id_comuna)

# Unir por ID y año censal
ipums <- left_join(ipums, id_comunas_ipums, by = c("id", "year")) %>%
  select(serial, id, name, year, prom_school, prom_isei)
# Nota: el "serial" se repite por cada nombre comunal que tenga el ID

# 3.10 Calcular medianas comunales por año
# Esto produce un valor representativo de ISEI y escolaridad para cada comuna y año
gse_comunas <- ipums %>%
  group_by(year, name) %>%
  summarise(
    median_prom_school = median(prom_school, na.rm = TRUE),
    median_prom_isei = median(prom_isei, na.rm = TRUE)
  )

# Renombrar columnas para consistencia
colnames(gse_comunas) <- c("ano", "comuna", "median_prom_school", "median_prom_isei")

#------------------------------------------------------------------------------
# 4. AGREGAR DATOS DE GEOLOCALIZACIÓN
#------------------------------------------------------------------------------
# Esta sección añade coordenadas geográficas (latitud/longitud) para cada comuna

# 4.1 Cargar primera fuente de datos geográficos
c <- read_csv("D:/names/comunas.csv")
c <- c %>% select(comuna = nombre, latitud, longitud)
c$comuna <- toupper(c$comuna)
c$comuna = stri_trans_general(str = c$comuna, id = "Latin-ASCII")

# 4.2 Cargar segunda fuente de datos geográficos (complementaria)
c1 <- read_csv("D:/names/Latitud - Longitud Chile.csv")
c1 <- c1 %>% select(comuna = Comuna, latitud = `Latitud (Decimal)`, longitud = `Longitud (decimal)`)
c1$comuna <- toupper(c1$comuna)
c1$comuna = stri_trans_general(str = c1$comuna, id = "Latin-ASCII")

#------------------------------------------------------------------------------
# 5. INTEGRACIÓN FINAL DE DATOS
#------------------------------------------------------------------------------
# Esta sección combina todos los conjuntos de datos en un único dataset 
# de atributos de comunas

# 5.1 Normalizar caracteres en nombres de comunas para facilitar el join
poblacion_estimada$comuna = stri_trans_general(str = poblacion_estimada$comuna, id = "Latin-ASCII")
gse_comunas$comuna = stri_trans_general(str = gse_comunas$comuna, id = "Latin-ASCII")

# 5.2 Unir datasets: primero GSE con población
attr_comunas <- gse_comunas %>% 
  left_join(poblacion_estimada, by = c("comuna", "ano"))

# 5.3 Añadir coordenadas geográficas
attr_comunas <- attr_comunas %>% 
  left_join(c1, by = "comuna")

# 5.4 Verificar estructura del dataset final
glimpse(attr_comunas)

# 5.5 Limpiar variables temporales para liberar memoria
rm(c, c1, datos_combinados, gse_comunas, id_comunas_ipums, ipums, 
   nacimientos_comuna, poblacion_estimada, tasas_natalidad,
   summary_poblacion)

gc()  # Liberar memoria

#------------------------------------------------------------------------------
# RESULTADO FINAL:
#------------------------------------------------------------------------------
# El objeto 'attr_comunas' contiene ahora los siguientes atributos para cada
# combinación de comuna-año:
# - median_prom_school: Años de escolaridad promedio (mediana comunal)
# - median_prom_isei: Índice socioeconómico ISEI (mediana comunal)
# - poblacion: Población estimada
# - latitud/longitud: Coordenadas geográficas
#
# Este dataset será utilizado en el análisis de difusión de nombres para
# caracterizar las comunas de origen y destino en los procesos de difusión.
#------------------------------------------------------------------------------