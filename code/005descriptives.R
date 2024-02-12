
# Librerías
library(ipumsr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(igraph)
library(migraph)
library(stringi)
library(ggraph)
library(ggforce)
library(tidytext)
library(graphlayouts)
library(stringdist)
library(bipartite)
library(ggpubr)
library(tidyverse)
library(RPostgres)
library(ggalluvial)


## Conectar con servidor

con <- dbConnect(
  Postgres(),
  user = "fondecyt",
  password = "9nvGYZ35nUdBTSbVhpmp", #borrar
  dbname = "fondecyt",
  host = "64.227.106.47"
  #port = 5432 # no es necesario, a menos que sea un puerto no estandar
)

## consulta al servidor
names<-tbl(con, "names_chile") %>% collect() %>% glimpse()

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))
names$comuna = stri_trans_general(str = names$comuna, id = "Latin-ASCII")
names$nombre = stri_trans_general(str = names$nombre, id = "Latin-ASCII")

# agrupar por año. 
names <- names %>%
  group_by(ano, comuna, nombre) %>%
  summarize(cantidad = sum(cantidad)) 
glimpse(names)


# ¿cuales son los nombres más populares (10 más populares por año)?
names %>%
  group_by(ano) %>% 
  count(nombre, sort = TRUE) %>%
  slice_max(n, n = 10)


# ¿cuantos nombres aparecen en más de una comuna por año?
names %>%
  group_by(ano) %>%
  count(nombre) %>% 
  filter(n > 1) %>%
  summarise(n_nombres = n())


# flujos (Identificar primera aparición de cada nombre por comuna y año)
primera_aparicion <- names %>% 
  arrange(nombre, ano, comuna) %>%
  group_by(nombre, ano) %>% 
  slice(1)

# Cruzar tablas y contar flujos entre comunas dentro de cada año:
flujos <- inner_join(names, primera_aparicion, by = c("nombre","ano")) 
flujos <- flujos %>% 
  filter(comuna.x != comuna.y) %>%
  count(ano, comuna_origen = comuna.x, comuna_destino = comuna.y)

# flujos entre comunas. 
# ejemplo. 
flujos_ej <- flujos %>%
  filter(ano %in% c(1920, 1930))%>%
  filter(n>1)

# Diagrama alluvial 
ggplot(flujos_ej, 
       aes(y = n, 
           axis1 = comuna_origen, 
           axis2 = comuna_destino)) +
  geom_alluvium(aes(fill = comuna_origen), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") + 
  facet_wrap(~ ano)

ggplot(flujos_ej,  
       aes(y = n, axis1 = comuna_origen, axis2 = comuna_destino)) +
  geom_alluvium(aes(fill = comuna_origen), width = 1/12, show.legend = FALSE) +
  geom_stratum(width = 1/12, fill="grey", color="black", alpha = 0) + 
  facet_wrap(~ ano)



# modelo de flujo
# edgelist to 

# filtrar para años disponibles en ipums
names<-names%>%filter(ano%in%c(1960, 1970, 1982, 1992, 2002, 2017))

names_1960<-names%>%filter(ano==1960)
names_1970<-names%>%filter(ano==1970)
names_1982<-names%>%filter(ano==1982)
names_1992<-names%>%filter(ano==1992)
names_2002<-names%>%filter(ano==2002)
names_2017<-names%>%filter(ano==2017)

# 1960
# co-currencias
m1960 <- netmem::edgelist_to_matrix(cbind(names_1960$nombre,names_1960$comuna), bipartite = T)
m1960t <-t(m1960)
overlap_1960 <- m1960t%*%t(m1960t)

library(reshape2)
# Convertir la matriz en un edgelist
edgelist_1960 <- melt(overlap_1960, varnames = c("comuna_i", "comuna_j"), value.name = "peso")
edgelist_1960$ano <- 1960
glimpse(edgelist_1960)


# 1970 
# co-currencias
m1970 <- netmem::edgelist_to_matrix(cbind(names_1970$nombre,names_1970$comuna), bipartite = T)
m1970t <-t(m1970)
overlap_1970 <- m1970t%*%t(m1970t)

# Convertir la matriz en un edgelist
edgelist_1970 <- melt(overlap_1970, varnames = c("comuna_i", "comuna_j"), value.name = "peso")
edgelist_1970$ano<-1970
glimpse(edgelist_1970)

# 1982
# co-currencias
m1982 <- netmem::edgelist_to_matrix(cbind(names_1982$nombre,names_1982$comuna), bipartite = T)
m1982t <-t(m1982)
overlap_1982 <- m1982t%*%t(m1982t)

# Convertir la matriz en un edgelist
edgelist_1982 <- melt(overlap_1982, varnames = c("comuna_i", "comuna_j"), value.name = "peso")
edgelist_1982$ano <- 1982
glimpse(edgelist_1982)

# 1992 
# co-currencias
m1992 <- netmem::edgelist_to_matrix(cbind(names_1992$nombre,names_1992$comuna), bipartite = T)
m1992t <-t(m1992)
overlap_1992 <- m1992t%*%t(m1992t)

# Convertir la matriz en un edgelist
edgelist_1992 <- melt(overlap_1992, varnames = c("comuna_i", "comuna_j"), value.name = "peso")
edgelist_1992$ano <- 1992
glimpse(edgelist_1992)

# 2002
# co-currencias
m2002 <- netmem::edgelist_to_matrix(cbind(names_2002$nombre,names_2002$comuna), bipartite = T)
m2002t <-t(m2002)
overlap_2002 <- m2002t%*%t(m2002t)

# Convertir la matriz en un edgelist
edgelist_2002 <- melt(overlap_2002, varnames = c("comuna_i", "comuna_j"), value.name = "peso")
edgelist_2002$ano <- 2002
glimpse(edgelist_2002)

# 2017
# co-currencias
m2017 <- netmem::edgelist_to_matrix(cbind(names_2017$nombre,names_2017$comuna), bipartite = T)
m2017t <-t(m2017)
overlap_2017 <- m2017t%*%t(m2017t)

# Convertir la matriz en un edgelist
edgelist_2017 <- melt(overlap_2017, varnames = c("comuna_i", "comuna_j"), value.name = "peso")
edgelist_2017$ano <- 2017
glimpse(edgelist_2017)


# unir
edgelist_comunas <- rbind(edgelist_1960,
                          edgelist_1970,
                          edgelist_1982,
                          edgelist_1992,
                          edgelist_2002,
                          edgelist_2017) %>% filter(comuna_i != comuna_j)


# Desde acá creamos la puntuación socioeconómica de cada nombres

# agrupar por año. 
names <- names %>%
  group_by(ano, comuna, nombre) %>%
  summarize(cantidad = sum(cantidad)) 
glimpse(names)

names$ano <- as.character(names$ano)
gse_comunas$ano <- as.character(gse_comunas$ano)

# merge
names_gse <- left_join(names, gse_comunas, by = c("comuna", "ano")) %>% 
  ungroup() %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"))
#glimpse(names_gse)

# Ranking quantiles ------------------------------------------------------------

## puntajes socioeconómicos de los nombres. 

names_gse <- names_gse %>%
  group_by(nombre, ano) %>%
  mutate(weighted_isei = weighted.mean(median_prom_isei, cantidad, na.rm = TRUE)) %>% 
  mutate(weighted_school = weighted.mean(median_prom_school, cantidad, na.rm = TRUE)) %>%
  ungroup()

## Socioeconomic quartile 
## Crear cuartiles por año para median_prom_isei
names_gse <- names_gse %>%
  group_by(ano) %>%
  mutate(cuartil_weighted_isei = ntile(weighted_isei, 5)) %>%
  mutate(cuartil_weighted_school = ntile(weighted_school, 5))


## Calcular ranking-quantiles
g <- names_gse %>% 
  group_by(ano,nombre) %>% 
  mutate(cantidad_pais = sum(cantidad, na.rm=FALSE)) %>%
  filter(cantidad_pais >= 20) %>%
  ungroup() %>%
  group_by(ano) %>%
  mutate(q = rank(cantidad_pais)/max(rank(cantidad_pais))) %>% 
  filter(ano == 1970 & q <= 0.1) 
#glimpse(g)


# Calcular quantiles dentro de los quintiles socioeconómicos ------------------
g1 <- names_gse %>% 
  group_by(ano, cuartil_weighted_isei, nombre) %>% 
  mutate(cantidad_pais = sum(cantidad, na.rm=FALSE)) %>%
  filter(cantidad_pais >= 10) %>%
  ungroup() %>%
  group_by(ano) %>%
  mutate(q = rank(cantidad_pais)/max(rank(cantidad_pais))) 

glimpse(g1)


# agrupamos por año y cuartil socioeconómico y calculando la correlación.

library(dplyr)

c1 <- g1 %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(cor_qi_isei = cor(q, weighted_isei, method = "spearman"))


c1 %>%  
  ggplot(aes(x=ano, y=cor_qi_isei, color=factor(cuartil_weighted_isei))) + 
  geom_point() +
  geom_line(aes(ano)) +
  geom_line(group = 1) +
  scale_colour_viridis_d() +
  facet_wrap(~cuartil_weighted_isei) +
  theme_bw() +
  labs(title="Correlacion entre popularidad y NSE a lo largo del tiempo",  
       x="Ano",
       y="Coeficiente de correlacion",
       color = "Quintil (ISEI)")



# Una posible explicación sustantiva sería que dentro de mayor prestigio ocupacional (quintil 5: con mayor capital cultural/socioeconómico), 
# existe una tendencia a preferir y popularizar nombres que se distancian del "mainstream" 
# y que no necesariamente tienen tan alta distinción socioeconómica inherente.
# Es decir, por motivos de originalidad, exclusividad o distinción, en los grupos de élite podríamos estar viendo 
# una especie de "contracultura" en las elecciones de nombres, que no replica las preferencias de otros estratos.
# ojo con el fenómeno de regresión a la media: En nuestro contexto, un nombre que en 1960 aparece en el cuartil 4 por tener un weighted_isei muy alto, 
# para 1970 podría mostrar un weighted_isei menor, más cercano a la media poblacional (ahí se manifiesta el fenómeno estadístico).
# Esto explicaría la correlación negativa, porque se generaría una asociación inversa entre la popularidad relativa creciente (q sube en el tiempo)
# y el weighted_isei que exhibiría cierta tendencia a la baja en el tiempo dentro del cuartil 4, producto de este fenómeno de regresión estadística.
# Mientras que en el cuartil 1 (bajos recursos) la correlación positiva indicaría una mayor conformidad con la preferencia de nombres más populares y con mayor estatus inherente.



#g1 %>%
#  group_by(cuartil_weighted_isei) %>%
#  mutate(q_std = scale(q)) %>%
#  group_by(ano, cuartil_weighted_isei) %>%
#  summarise(cor_est = cor(q_std, weighted_isei)) %>%  
#  ggplot(aes(x = ano, y = cor_est, color=factor(cuartil_weighted_isei))) + 
#  geom_point() +
#  geom_line(group = 1) +
#  facet_wrap(~cuartil_weighted_isei)





# Asegurarse de que 'ano' sea de tipo numérico
g1$ano <- as.numeric(g1$ano)

# Crear un gráfico de barras para contar nombres con proporción baja en q por cuartil socioeconómico
ggplot(g1, aes(x = as.factor(ano), fill = factor(cuartil_weighted_isei))) +
  geom_bar(data = subset(g1, q <= 0.02), stat = "count") +
  labs(title = "Cantidad de Nombres con Proporción Baja en q por Década y Cuartil Socioeconómico",
       x = "Década",
       y = "Cantidad") +
  theme_minimal()


ggplot(subset(g1, q <= 0.1), aes(x = factor(ano), fill=factor(cuartil_weighted_isei))) +
  geom_bar(stat = "count") + 
  scale_fill_viridis_d() +
  facet_wrap(~cuartil_weighted_isei) +
  theme_bw() + 
  labs(title="Nombres poco populares por decada y quintil socioeconomico (q <= 0.1)",
       x="Ano",
       y="Cantidad",
       fill = "Quintil (ISEI)")



## Definición 1 de nombres nuevos. 

# Calcular delta relativo 
g1 <- g1 %>%
  group_by(nombre) %>%
  mutate(delta_q = lead(q) - q,
         delta_relativo = delta_q/q)

# Identificar nombres nuevos
nombres_nuevos <- g1 %>%
  filter(q < 0.8 &     
           delta_relativo > 0.2 &
           lead(cantidad) > 3)

# Plot por año
nombres_nuevos %>%
  count(ano, cuartil_weighted_isei) %>%
  ggplot(aes(x = cuartil_weighted_isei, y = n)) +
  scale_fill_viridis_d(aes(cuartil_weighted_isei)) +
  geom_col() +
  facet_wrap(~ano) +
  labs(title = "Nombres nuevos por ano y cuartil socioeconomico")


nombres_nuevos %>%
  count(ano, cuartil_weighted_isei) %>% 
  ggplot(aes(x = cuartil_weighted_isei, y = n, fill = factor(cuartil_weighted_isei))) +
  geom_col() +
  scale_fill_viridis_d() +
  facet_wrap(~ano) + 
  theme(legend.position = "none") +
  labs(title = "Nombres nuevos por ano y cuartil socioeconomico")



glimpse(g1)


# Difusión ----------------------------------------------------------------------


library(dplyr)
library(ggplot2)

# Paso 1: Filtrar los datos para los nombres más populares en el año 1960 en el quintil 5
nombres_populares_1960 <- g1 %>%
  filter(ano == "1960" & cuartil_weighted_isei == 5) %>%
  group_by(nombre) %>%
  summarise(popularidad = sum(q, na.rm = TRUE)) %>%
  arrange(desc(popularidad)) %>%
  top_n(20)

# Paso 2: Filtrar los datos para estos nombres y los años posteriores en todos los quintiles
datos_interes <- g1 %>%
  semi_join(nombres_populares_1960, by = "nombre") %>%
  filter(ano %in% c("1970", "1982", "1992", "2002", "2017"))

# Visualizar la trayectoria de popularidad dentro de cada quintil para cada año, separado por quintil
ggplot(datos_interes, aes(x = ano, y = q, color = nombre)) +
  geom_line() +
  geom_point() +
  geom_line(aes(group = nombre), color = "black", alpha = 0.5) +  # Línea para cada trayectoria de nombre
  labs(title = "Evolución de la popularidad de los 20 nombres más populares en 1960 en cada quintil",
       x = "Año",
       y = "Popularidad (q)",
       color = "Nombre") +
  facet_wrap(~ cuartil_weighted_isei, scales = "free_y") +  # Separar por quintil
  theme_minimal()



# 2 densidad

# Filtrar los datos para los años de interés y los quintiles
datos_interes <- g1 %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Visualizar la distribución de la popularidad de los nombres en diferentes quintiles a lo largo de los años
ggplot(datos_interes, aes(x = q, fill = factor(cuartil_weighted_isei))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de la popularidad de los nombres en diferentes quintiles",
       x = "Popularidad (q)",
       y = "Densidad",
       fill = "Quintil Socioeconómico") +
  theme_minimal()




library(ggplot2)

# Filtrar los datos para los años de interés y los quintiles
datos_interes <- g1 %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Visualizar la distribución de la popularidad de los nombres en diferentes quintiles para cada año
ggplot(datos_interes, aes(x = q, fill = factor(cuartil_weighted_isei))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ ano, scales = "free_x") +
  labs(title = "Distribución de la popularidad de los nombres en diferentes quintiles por año",
       x = "Popularidad (q)",
       y = "Densidad",
       fill = "Quintil Socioeconómico") +
  theme_minimal()



# 3 

library(dplyr)
library(ggplot2)

# Filtrar los datos para incluir solo los años y quintiles de interés
datos_interes <- g1 %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Calcular la popularidad promedio de los nombres en cada quintil para cada año
tendencias_temporales <- datos_interes %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar las tendencias de popularidad promedio en cada quintil a lo largo del tiempo
ggplot(tendencias_temporales, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line() +
  geom_point() +
  labs(title = "Tendencias de popularidad promedio de nombres por quintil a lo largo del tiempo",
       x = "Año",
       y = "Popularidad promedio",
       color = "Quintil Socioeconómico") +
  theme_minimal()


# 4 difusión (descriptivo) -----------------------------------------------------


# Filtrar los datos para incluir solo el primer año y el quintil más alto
datos_primer_ano <- g1 %>%
  filter(ano == "1960", cuartil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares <- datos_primer_ano %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 20 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes <- g1 %>%
  filter(ano %in% c("1970", "1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5),
         nombre %in% nombres_populares$nombre)


# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil <- datos_interes %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line(aes(group = factor(cuartil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Promedio de popularidad de nombres seleccionados por quintil a lo largo del tiempo \n(Mas populares en el quintil 5 en el ano 1960)",
       x = "Ano",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconomico") +
  theme_minimal()




# Filtrar los datos originales para incluir los nombres seleccionados y los años
datos_interes <- g1 %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5),
         nombre %in% nombres_populares$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil <- datos_interes %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line(aes(group = factor(cuartil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Promedio de popularidad de nombres seleccionados por quintil a lo largo del tiempo \n(Mas populares en el quintil 5 en el ano 1960)",
       x = "Ano",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconomico") +
  theme_minimal()


## from 1970 ------------------------

# Filtrar los datos para incluir solo el año 1970 y el quintil más alto
datos_1970 <- g1 %>%
  filter(ano == "1970", cuartil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_1970 <- datos_1970 %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_1970 <- g1 %>%
  filter(ano %in% c("1970", "1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5),
         nombre %in% nombres_populares_1970$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_1970 <- datos_interes_1970 %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_1970, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line(aes(group = factor(cuartil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Promedio de popularidad de nombres seleccionados por quintil a lo largo del tiempo \n(Más populares en el quintil 5 en el año 1970)",
       x = "Año",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconómico") +
  theme_minimal()


## from 1982 ------------------------------------------------

# Filtrar los datos para incluir solo el año 1982 y el quintil más alto
datos_1982 <- g1 %>%
  filter(ano == "1982", cuartil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_1982 <- datos_1982 %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_1982 <- g1 %>%
  filter(ano %in% c("1982", "1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5),
         nombre %in% nombres_populares_1982$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_1982 <- datos_interes_1982 %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_1982, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line(aes(group = factor(cuartil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Promedio de popularidad de nombres seleccionados por quintil a lo largo del tiempo \n(Mas populares en el quintil 5 en el ano 1982)",
       x = "Ano",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconomico") +
  theme_minimal()

## from 1992 -------------------------------------------------------------

# Filtrar los datos para incluir solo el año 1992 y el quintil más alto
datos_1992 <- g1 %>%
  filter(ano == "1992", cuartil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_1992 <- datos_1992 %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_1992 <- g1 %>%
  filter(ano %in% c("1992", "2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5),
         nombre %in% nombres_populares_1992$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_1992 <- datos_interes_1992 %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_1992, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line(aes(group = factor(cuartil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Promedio de popularidad de nombres seleccionados por quintil a lo largo del tiempo \n(Más populares en el quintil 5 en el año 1992)",
       x = "Año",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconómico") +
  theme_minimal()

## from 2022 ------------------------------------------------------------

# Filtrar los datos para incluir solo el año 2002 y el quintil más alto
datos_2002 <- g1 %>%
  filter(ano == "2002", cuartil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_2002 <- datos_2002 %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_2002 <- g1 %>%
  filter(ano %in% c("2002", "2017"),
         cuartil_weighted_isei %in% c(1, 2, 3, 4, 5),
         nombre %in% nombres_populares_2002$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_2002 <- datos_interes_2002 %>%
  group_by(ano, cuartil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_2002, aes(x = ano, y = promedio_popularidad, color = factor(cuartil_weighted_isei))) +
  geom_line(aes(group = factor(cuartil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Promedio de popularidad de nombres seleccionados por quintil a lo largo del tiempo \n(Más populares en el quintil 5 en el año 2002)",
       x = "Año",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconómico") +
  theme_minimal()



# Data original
glimpse(g1)

# 1. Métricas cambio nombre entre décadas
df_flujos <- g1 %>%
  group_by(nombre, ano) %>% 
  mutate(delta_q = lag(q) - q, 
         q_cuartil = lead(cuartil_weighted_isei))

# Total en Q5
total_q5 <- g1 %>% 
  filter(cuartil_weighted_isei == 5) %>% summarise(total = n())

# Proporción con manejo de NA          
prop_dif_q5q2 <- 
  df_flujos %>%
  filter(cuartil_weighted_isei == 5 & q_cuartil == 2) %>% 
  summarise(prop_diff = ifelse(n() > 0, 
                               n()/total_q5$total, 
                               NA))

# 3. Cambio promedio entre pares
difusion_prom <- df_flujos %>% 
  group_by(origen = cuartil_weighted_isei, 
           destino = q_cuartil) %>%
  summarise(cambio_prom = mean(delta_q, na.rm = TRUE))






















# g quintiles 
g1 %>% filter(cuartil_weighted_isei %in% c(1,4))


# Agrupr los datos por década y nombre. 
# Luego, para cada nombre y década, calcule el cambio en el cuantil "q" respecto a la década anterior. 
# Esto nos dará una idea de qué tan rápido está creciendo cada nombre en popularidad.

x <- g1 %>%
  group_by(ano, nombre) %>% 
  summarise(q = mean(q),  
            cuartil = mean(cuartil_weighted_isei),
            delta_q = diff(q),
            n = n()) %>% 
  arrange(nombre, ano) %>%
  filter(delta_q > 0.2) %>% 
  mutate(cuartil_lag = lag(cuartil)) %>%
  summarise(cuartil_actual = mean(cuartil), 
            cuartil_anterior = mean(cuartil_lag, na.rm = TRUE),
            delta_cuartil = cuartil_actual - cuartil_anterior,
            delta_q = mean(delta_q))












df_top <- names_gse %>%
  group_by(ano, comuna, nombre) %>% 
  summarise(cantidad = sum(cantidad)) %>%
  group_by(ano, comuna) %>%
  slice_max(cantidad, n = 30) %>% 
  ungroup() %>%
  left_join(names_gse, by = c("ano", "comuna", "nombre", "cantidad"))


#funciona bien
df_top <- names_gse %>% 
  group_by(ano, comuna) %>% 
  arrange(ano, comuna, desc(cantidad)) %>% 
  slice(1:20) %>%
  ungroup()

df_top<-df_top%>%filter(ano%in%c(1960, 1970, 1982, 1992, 2002, 2017))

df_top_ano <- names_gse %>% 
  group_by(ano) %>% 
  arrange(ano, desc(cantidad)) %>% 
  slice(1:20) %>%
  ungroup()

df_top_ano<-df_top_ano%>%filter(ano%in%c(1960, 1970, 1982, 1992, 2002, 2017))



df_top_ano %>%
  filter(ano%in%c("1960","1970", "1982", "1992", "2002", "2017")) %>%
  #top_n(200, cantidad) %>%
  ggplot(aes(x=reorder_within(nombre, cantidad, ano), y=cantidad, fill=ano)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "D",direction = 1)+
  facet_wrap(~ano, ncol = 3, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 7))



df_top %>%
  filter(comuna%in%c("LAS CONDES","PROVIDENCIA") & ano=="2017")%>% 
  ggplot(aes(x=reorder_within(nombre, cantidad, comuna), y=cantidad, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "D",direction = 1)+
  facet_wrap(~comuna, ncol = 3, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 7))


df_top %>%
  filter(comuna%in%c("PUENTE ALTO","INDEPENDENCIA") & ano=="2017")%>% #"LA REINA"
  ggplot(aes(x=reorder_within(nombre, cantidad, comuna), y=cantidad, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "D",direction = 1)+
  facet_wrap(~comuna, ncol = 3, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 7))




df_top %>%
  filter(comuna%in%c("ARICA","COPIAPO") & ano=="2017")%>%
  ggplot(aes(x=reorder_within(nombre, cantidad, comuna), y=cantidad, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "E",direction = 1)+
  facet_wrap(~comuna, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 9))


df_top %>%
  filter(comuna%in%c("CONSTITUCION","AISEN") & ano=="1960")%>%
  ggplot(aes(x=reorder_within(nombre, cantidad, comuna), y=cantidad, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "E",direction = 1)+
  facet_wrap(~comuna, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 9))


# crear quintiles 

# -----------------------------------------------------
## Crear cuartiles por año para median_prom_isei
names_gse <- names_gse %>%
  group_by(ano) %>%
  mutate(quintil_isei = ntile(median_prom_isei,5)) %>%
  mutate(quintil_school = ntile(median_prom_school, 5))

glimpse(nombres_nuevos_quintil)

nombres_nuevos_quintil_isei <- names_gse %>%
  group_by(ano, quintil_isei) %>% 
  summarise(nombres_nuevos = n_distinct(nombre)) %>%
  drop_na()

nombres_nuevos_quintil_school <- names_gse %>%
  group_by(ano, quintil_school) %>% 
  summarise(nombres_nuevos = n_distinct(nombre)) %>%
  drop_na()


nombres_nuevos_quintil_isei %>%
  ggplot(aes(x = quintil_isei, y = nombres_nuevos, fill = factor(quintil_isei))) + 
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(direction = -1) + 
  facet_wrap(~ano) +
  labs(title="Nombres nuevos por año y quintil (isei)",
       x="Quintil ISEI", y = "Nombres nuevos")


nombres_nuevos_quintil_school %>%
  ggplot(aes(x = quintil_school, y = nombres_nuevos, fill = factor(quintil_school))) + 
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(direction = -1) + 
  facet_wrap(~ano) +
  labs(title="Nombres nuevos por año y quintil (años educativos)",
       x="Quintil Años Educativos", y = "Nombres nuevos")













