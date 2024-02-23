
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
library(TraMineR)

# Set preferences ggplot theme


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
      legend.position = "bottom"
    )
}


theme_set(my_custom_theme())


# Set palettes
mypal_disc <- c("#007BFF", "#FFA24A", "#E60000","#41D941", "#003366", "#FF8484", "#6C3483", "#607D39", "#FFD966")


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

# filtrar los MARIA y JUAN
names <- names %>%
  filter(!(nombre == "MARIA " | nombre == "JUAN "))


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

# Nombres nuevos ------------------------------------------------
# Un nombre nuevo es uno que en el pasado (años anteriores al año analizado) tenía poca prevalencia en la población en general. 
# (poca prevalencia implica menos de un 5%)
# o nombres que aparezcan 
# nombre nuevo sería uno que en todos los años anteriores su promedio de apariciones es 20% o menos. 

# percentil 10 de las proporciones
names %>% 
  group_by(ano,nombre) %>% 
  summarise(cantidad_pais=sum(cantidad)) %>%
  ungroup() %>%
  group_by(ano) %>% 
  arrange(cantidad_pais) %>% 
  mutate(cdf=cumsum(cantidad_pais)/sum(cantidad_pais))%>%
  arrange(desc(cantidad_pais)) %>%
  filter(ano==1960) %>% 
  ggplot(aes(x=cantidad_pais, y=cdf, colour=ano)) +
  geom_line() +
  theme(legend.position = "none") + xlim(0,20)



names %>%   group_by(ano,nombre) %>% 
  summarise(cantidad_pais=sum(cantidad)) %>%
  filter(ano ==1980 & cantidad_pais >= 15 & cantidad_pais <=30) %>% print(n=100)


#### Nombre nuevo

raros_hist <- data.frame()


for (i in 1921:2022) {
  cat("========= ",i," =========")
  hist <- names %>% filter(ano < i) 
  
  raros <- hist %>% group_by(nombre) %>%
    summarise(acum = sum(cantidad)) %>%
    mutate(ano=i, raro=1) %>%
    filter(acum<=(i-1920)*10) 
  
    raros_hist <- rbind(raros_hist,raros)
}

#raros_hist %>% filter(ano==2020) %>% arrange(desc(acum))
# Antes de 1980 (si estamos en 1980), si ha aparecido menos de 10 veces por año. 
# 


#raros_hist %>% filter(ano==2020) %>% arrange(desc(acum))


#### nombre reciclado 
start_year <- 1955 
end_year <- 2022
gap_years <- 30

raros_recent <- tibble()

for (i in seq(start_year, end_year)) {
  
  hist_until <- i - gap_years
  
  cat("========= ",hist_until,"-",i," =========")
  
  hist <- names %>% 
    filter(ano >= hist_until)
  
  raros_ <- hist %>%
    group_by(nombre) %>%
    summarise(acum = sum(cantidad)) %>% 
    mutate(ano = i, 
           raro_recent = 1) %>%
    filter(acum <= gap_years*10)
  
  raros_recent <- bind_rows(raros_recent, raros_)
  
}


### join 
names$ano <- as.numeric(names$ano)
raros_hist$ano <- as.numeric(raros_hist$ano)
names <- names %>% left_join(raros_hist %>% select(-acum), by=c("nombre", "ano"))
names <- names %>% left_join(raros_recent %>% select(-acum), by=c("nombre", "ano"))
names <- names %>% replace_na(list(raro=0, raro_recent=0))









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




# Puntuación socioeconómica de cada nombres ------------------------------------

# agrupar por año. 
names <- names %>%
  group_by(ano, comuna, nombre) %>%
  summarize(cantidad = sum(cantidad)) 
glimpse(names)

names$ano <- as.numeric(names$ano)
gse_comunas$ano <- as.numeric(gse_comunas$ano)

# merge
names_gse <- left_join(names, gse_comunas, by = c("comuna", "ano"))
names_gse <- names_gse %>% filter(ano >= 1955)

#names_gse <- left_join(names, gse_comunas, by = c("comuna", "ano")) %>% 
#  ungroup() %>%
#  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"))
#glimpse(names_gse)


# extender datos para 5 años anterior y 5 posteriores. 

glimpse(names_gse)

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

names_gse <- names_gse %>%
  group_by(comuna, census_year) %>%
  mutate(
    median_prom_isei = ifelse(is.na(median_prom_isei), 
                              median(median_prom_isei, na.rm = TRUE), 
                              median_prom_isei),
    median_prom_school  = ifelse(is.na(median_prom_school),
                                 median(median_prom_school, na.rm = TRUE),  
                                 median_prom_school)
  ) %>%
  select(-census_year)



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
  mutate(quintil_weighted_isei = ntile(weighted_isei, 5)) %>%
  mutate(quintil_weighted_school = ntile(weighted_school, 5)) 


## visualización --------------------------------------------------
# por cada nivel socioeconómico 
# si cada nombre ses nuevo | reciclado | otro  
# top 50 por nivel socioeconómio y ver la distribución de la nueva variable de nombre. 

names_gse <- names_gse %>% mutate(tipo_nombre = case_when(raro==1 ~ "nuevo", 
                                                          raro_recent==1 & raro == 0 ~ "reciclado", 
                                                          TRUE ~ "otro"))


#glimpse(names_gse)

# nombres nuevos 
names_gse %>% 
  filter(!is.na(quintil_weighted_isei)) %>% 
  filter(tipo_nombre == "nuevo") %>%
  ggplot(aes(x = ano, y = cantidad, fill = tipo_nombre)) +
  scale_x_continuous(breaks = c(1955, 1980, 2000, 2020)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~quintil_weighted_isei)  +
  scale_colour_manual(values = mypal_disc) +
  labs(title = "New names by año and Social Class (quintil)",
       x = "",
       y = "Freq.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  guides(fill=FALSE)



names_gse %>% 
  filter(!is.na(quintil_weighted_isei)) %>% 
  filter(tipo_nombre == "nuevo") %>%
  ggplot(aes(x = ano, y = cantidad, fill = tipo_nombre)) +
  scale_x_continuous(breaks = c(1955, 1980, 2000, 2020)) + 
  geom_bar(stat = "identity", color = "#FF6666", width = 0.1) +  # Ajustamos el color y el tamaño de la línea
  facet_wrap(~quintil_weighted_isei)  +
  scale_fill_manual(values = mypal_disc) +
  labs(title = "New names by year and Social Class (quintile)",
       x = "",
       y = "Freq.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  guides(fill=FALSE)





# nombres reciclados
names_gse %>% 
  filter(!is.na(quintil_weighted_isei)) %>% 
  filter(tipo_nombre == "reciclado") %>%
  ggplot(aes(x = ano, y = cantidad, fill = tipo_nombre)) +
  geom_bar(stat="identity", color = "#FF6666", width = 0.1) + 
  facet_wrap(~quintil_weighted_isei)  +
  scale_colour_manual(values = mypal_disc) +
  labs(title = "Recicled names by year and Social Class (quintile)",
       x = "",
       y = "Freq.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.4)) +
  guides(fill=FALSE)


names_gse %>% 
  filter(!is.na(quintil_weighted_isei)) %>% 
  ggplot(aes(x = ano, y = cantidad, fill = tipo_nombre)) +
  geom_bar(position="fill", stat="identity") + 
  facet_wrap(~quintil_weighted_isei)  +
  scale_colour_manual(values = mypal_disc) 




 top50 <- names_gse %>%  
  filter(!is.na(quintil_weighted_isei)) %>% 
  group_by(ano, quintil_weighted_isei, nombre) %>%
  summarise(cantidad = sum(cantidad)) %>% 
  arrange(desc(cantidad)) %>% 
  group_by(ano, quintil_weighted_isei) %>%
  slice_head(n = 200)
glimpse(top50)

names_gse2 <- names_gse %>% select(ano, nombre, quintil_weighted_isei, tipo_nombre) %>% distinct(.keep_all = TRUE)
top50 <- top50 %>% left_join(names_gse2, by = c("ano", "nombre", "quintil_weighted_isei"))


top50 %>%
  ggplot(aes(x=ano, y=cantidad, fill=tipo_nombre)) +
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~quintil_weighted_isei) +
  scale_colour_manual(values = mypal_disc)




## Calcular ranking-quantiles
g <- names_gse %>% 
  group_by(ano,nombre) %>% 
  mutate(cantidad_pais = sum(cantidad, na.rm=FALSE)) %>%
  filter(cantidad_pais >= 20) %>%
  ungroup() %>%
  group_by(ano) %>%
  mutate(q = rank(cantidad_pais)/max(rank(cantidad_pais))) 
#glimpse(g)


# Calcular quantiles dentro de los deciles socioeconómicos ------------------
g1 <- names_gse %>% 
  group_by(ano, quintil_weighted_isei, nombre) %>% 
  mutate(cantidad_pais = sum(cantidad, na.rm=FALSE)) %>%
  filter(cantidad_pais >= 10) %>%
  ungroup() %>%
  group_by(ano) %>%
  mutate(q = rank(cantidad_pais)/max(rank(cantidad_pais))) 


# agrupamos por año y cuartil socioeconómico y calculando la correlación.

c1 <- g1 %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(cor_qi_isei = cor(q, weighted_isei, method = "spearman"))


c1 %>%  
  ggplot(aes(x=ano, y=cor_qi_isei, color=factor(quintil_weighted_isei))) + 
  geom_point() +
  geom_line(aes(ano)) +
  geom_line(group = 1) +
  scale_colour_viridis_d() +
  facet_wrap(~quintil_weighted_isei) +
  scale_colour_manual(values = mypal_disc)+
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
ggplot(subset(g, q <= 0.1), aes(x = factor(ano), fill=factor(quintil_weighted_isei))) +
  geom_bar(stat = "count") + 
  scale_fill_viridis_d() +
  facet_wrap(~quintil_weighted_isei) +
  labs(title="Nombres poco populares a nivel país por decada y quintil\nsocioeconomico (q >= 0.1)",
       x="Ano",
       y="Cantidad",
       fill = "Quintil (ISEI)") +
  scale_colour_manual(values = mypal_disc)
  




# Difusión ----------------------------------------------------------------------
# Paso 1: Filtrar los datos para los nombres más populares en el año 1960 en el quintil 5
nombres_populares_1960 <- g1 %>%
  filter(ano == 1955 & quintil_weighted_isei == 5) %>%
  group_by(nombre) %>%
  summarise(popularidad = sum(q, na.rm = TRUE)) %>%
  arrange(desc(popularidad)) %>%
  top_n(20)

# Paso 2: Filtrar los datos para estos nombres y los años posteriores en todos los quintiles
datos_interes <- g1 %>%
  semi_join(nombres_populares_1960, by = "nombre") 
  #filter(ano %in% c("1970", "1982", "1992", "2002", "2017"))

# Visualizar la trayectoria de popularidad dentro de cada quintil para cada año, separado por quintil
ggplot(datos_interes, aes(x = ano, y = q, color = nombre)) +
  geom_line() +
  geom_point() +
  geom_line(aes(group = nombre), color = "black", alpha = 0.5) +  # Línea para cada trayectoria de nombre
  labs(title = "Evolución de la popularidad de los 20 nombres más populares en 1960 en cada quintil",
       x = "Año",
       y = "Popularidad (q)",
       color = "Nombre") +
  facet_wrap(~ quintil_weighted_isei, scales = "free_y") +  # Separar por quintil
  theme_minimal()



# 2 densidad
# Filtrar los datos para los años de interés y los quintiles
datos_interes <- g1 %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"),
         quintil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Visualizar la distribución de la popularidad de los nombres en diferentes quintiles a lo largo de los años
ggplot(datos_interes, aes(x = q, fill = factor(quintil_weighted_isei))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de la popularidad de los nombres en diferentes quintiles",
       x = "Popularidad (q)",
       y = "Densidad",
       fill = "Quintil Socioeconómico") +
  theme_minimal()




# Filtrar los datos para los años de interés y los quintiles
datos_interes <- g1 %>%
  filter(ano %in% c("1960", "1970", "1982", "1992", "2002", "2017"),
         quintil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Visualizar la distribución de la popularidad de los nombres en diferentes quintiles para cada año
ggplot(datos_interes, aes(x = q, fill = factor(quintil_weighted_isei))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ ano, scales = "free_x") +
  labs(title = "Distribución de la popularidad de los nombres en diferentes quintiles por año",
       x = "Popularidad (q)",
       y = "Densidad",
       fill = "Quintil Socioeconómico") +
  theme_minimal()



# 3 Tendencias de popularidad promedio de nombres a lo largo del tiempo

# Filtrar los datos para incluir solo los años y quintiles de interés
datos_interes <- g1 %>%
  filter(ano %in% 1955:2022,
         quintil_weighted_isei %in% c(1, 2, 3, 4, 5))

# Calcular la popularidad promedio de los nombres en cada quintil para cada año
tendencias_temporales <- datos_interes %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar las tendencias de popularidad promedio en cada quintil a lo largo del tiempo
ggplot(tendencias_temporales, aes(x = ano, y = promedio_popularidad, color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei))) +
  geom_point() +
  labs(title = "Trends of Average Popularity of Names by Socioeconomic Quintile Over Time",
       x = "Year",
       y = "Average Popularity",
       color = "Socioeconomic\nQuintile") +
  scale_colour_manual(values = mypal_disc)



# Entropía--------------------------------------------

## top 50 por quintil 

# next: compute entropy for names
#install.packages("entropy")
library(entropy)

g1 %>%
  #group_by(quintil_weighted_isei, nombre, ano) %>% 
  mutate(total = sum(cantidad)) %>%
  #group_by(quintil_weighted_isei, ano) %>% 
  top_n(50, total) %>%
  ungroup() %>%
  arrange(ano, desc(total)) %>%
  group_by(ano) %>%
  summarise(entr = entropy(total)) %>% 
  ggplot(aes(x = ano, y = entr)) +
  geom_line()


# Consolidation names --------------------------------

# total names
g1 %>% group_by(ano, quintil_weighted_isei) %>% arrange(desc(n)) %>% mutate(total = n()) %>%
  ggplot(aes(x=ano,y=total, group=quintil_weighted_isei, colour=quintil_weighted_isei)) +
  geom_line() 

library(dplyr)

g1 %>% 
  group_by(ano, quintil_weighted_isei) %>% 
  tally() %>%
  ggplot(aes(x = ano, y = n, color = factor(quintil_weighted_isei))) +
  geom_line()

# concentration of names 

g1 %>% 
  group_by(ano, cuartil_weighted_isei) %>% 
  arrange(desc(q)) %>% 
  summarise(
    prop_1  = q[row_number()==1], 
    prop_10 = q[row_number()<=10],
    prop_30 = q[row_number()<=30],
    prop_50 = q[row_number()<=50]
  ) %>%
  pivot_longer(starts_with("prop_"), names_to = "cum_rank", values_to="cum_prop") %>% 
  ggplot(aes(x=ano, y=cum_prop, color=cum_rank)) +
  geom_line() +
  facet_grid( . ~ cuartil_weighted_isei)
#glimpse(g1)



# Most popular por quintil por año. (10) ---------------------------------------

# Agrupar los datos por año, nombre y quintil, luego sumar las cantidades
datos_agrupados <- g1 %>%
  group_by(ano, nombre, quintil_weighted_isei) %>%
  summarise(cantidad_total = sum(cantidad))

# Ordenar los datos agrupados por año, quintil y cantidad total en orden descendente
datos_ordenados <- datos_agrupados %>%
  arrange(ano, quintil_weighted_isei, desc(cantidad_total))

# Capturar los 10 nombres más comunes en cantidad para cada quintil en cada año
top_nombres <- datos_ordenados %>%
  group_by(ano, quintil_weighted_isei) %>%
  top_n(10, wt = cantidad_total)

# Ver los resultados
print(top_nombres)
glimpse(top_nombres)


## top por quintil
crear_dataframes_por_quintil <- function(datos) {
  lista_dataframes <- lapply(1:5, function(quintil) {
    nombre_data_frame <- paste0("top_nombre_q", quintil)
    df <- datos %>%
      filter(quintil_weighted_isei == quintil) %>%
      select(-cantidad_total)
    assign(nombre_data_frame, df, envir = .GlobalEnv)
  })
  
  invisible(lista_dataframes)
}

crear_dataframes_por_quintil(top_nombres)


# agrupando por década --------------------------------------------------------
# Convertir el año a una variable de década
g1$decada <- as.integer(g1$ano %/% 10) * 10

# Agrupar los datos por década, nombre y quintil, luego sumar las cantidades
datos_agrupados <- g1 %>%
  group_by(decada, nombre, quintil_weighted_isei) %>%
  summarise(cantidad_total = sum(cantidad))

# Ordenar los datos agrupados por década, quintil y cantidad total en orden descendente
datos_ordenados <- datos_agrupados %>%
  arrange(decada, quintil_weighted_isei, desc(cantidad_total))

# Capturar los 10 nombres más comunes en cantidad para cada quintil en cada década
top_nombres <- datos_ordenados %>%
  group_by(decada, quintil_weighted_isei) %>%
  top_n(10, wt = cantidad_total)

# Ver los resultados
print(top_nombres)
glimpse(top_nombres)

## top por quintil
crear_dataframes_por_quintil <- function(datos) {
  lista_dataframes <- lapply(1:5, function(quintil) {
    nombre_data_frame <- paste0("top_nombre_q", quintil)
    df <- datos %>%
      filter(quintil_weighted_isei == quintil) %>%
      select(-cantidad_total)
    assign(nombre_data_frame, df, envir = .GlobalEnv)
  })
  
  invisible(lista_dataframes)
}

crear_dataframes_por_quintil(top_nombres)


# affiliation matrices. 
top_nombre_q1 <- top_nombre_q1 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq1 <- netmem::edgelist_to_matrix(cbind(top_nombre_q1$nombre,top_nombre_q1$decada), bipartite = T)

top_nombre_q2 <- top_nombre_q2 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq2 <- netmem::edgelist_to_matrix(cbind(top_nombre_q2$nombre,top_nombre_q2$decada), bipartite = T)

top_nombre_q3 <- top_nombre_q3 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq3 <- netmem::edgelist_to_matrix(cbind(top_nombre_q3$nombre,top_nombre_q3$decada), bipartite = T)

top_nombre_q4 <- top_nombre_q4 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq4 <- netmem::edgelist_to_matrix(cbind(top_nombre_q4$nombre,top_nombre_q4$decada), bipartite = T)

top_nombre_q5 <- top_nombre_q5 %>% filter(decada %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2020))
mq5 <- netmem::edgelist_to_matrix(cbind(top_nombre_q5$nombre,top_nombre_q5$decada), bipartite = T)
#glimpse(mq5)

# Dataframe de la matriz
df_seq_1 <- as.data.frame(mq5) %>% rownames_to_column("name")
df_seq_2 <- as.data.frame(mq5) %>% rownames_to_column("name")
df_seq_3 <- as.data.frame(mq5) %>% rownames_to_column("name")
df_seq_4 <- as.data.frame(mq5) %>% rownames_to_column("name")
df_seq_5 <- as.data.frame(mq5) %>% rownames_to_column("name")


# Secuencias con name como id
seqs_1 <- seqdef(df_seq_1[-1], var = names(df_seq_1[-1]), states = c("0","1"), id = df_seq_1$name)
seqs_2 <- seqdef(df_seq_2[-1], var = names(df_seq_2[-1]), states = c("0","1"), id = df_seq_2$name)
seqs_3 <- seqdef(df_seq_3[-1], var = names(df_seq_3[-1]), states = c("0","1"), id = df_seq_3$name)
seqs_4 <- seqdef(df_seq_4[-1], var = names(df_seq_4[-1]), states = c("0","1"), id = df_seq_4$name)
seqs_5 <- seqdef(df_seq_5[-1], var = names(df_seq_5[-1]), states = c("0","1"), id = df_seq_5$name)
glimpse(seqs_5)

# Dataframe de la matriz
df_seq <- as.data.frame(mq3) %>%
  rownames_to_column("name")

# Secuencias con name como id
seqs <- seqdef(df_seq[-1],
               var = names(df_seq[-1]),
               states = c("0","1"),
               id = df_seq$name)

# Datos a formato tidy
df_seqs_q1 <- as.data.frame(seqs) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q2 <- as.data.frame(seqs) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q3 <- as.data.frame(seqs) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q4 <- as.data.frame(seqs) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")

df_seqs_q5 <- as.data.frame(seqs) %>%
  rownames_to_column("name") %>%
  pivot_longer(cols = -name, names_to = "time", values_to = "state")


# Ordenar el dataframe según el estado y el tiempo
df_seqs_q1 <- df_seqs_q1 %>% arrange(desc(state), time)
df_seqs_q2 <- df_seqs_q2 %>% arrange(desc(state), time)
df_seqs_q3 <- df_seqs_q3 %>% arrange(desc(state), time)
df_seqs_q4 <- df_seqs_q4 %>% arrange(desc(state), time)
df_seqs_q5 <- df_seqs_q5 %>% arrange(desc(state), time)


# Crear un factor ordenado para la columna 'name' basado en el orden de 'state' y 'time'
df_seqs_q1$name <- factor(df_seqs_q1$name, levels = unique(df_seqs_q1$name))
df_seqs_q2$name <- factor(df_seqs_q2$name, levels = unique(df_seqs_q2$name))
df_seqs_q3$name <- factor(df_seqs_q3$name, levels = unique(df_seqs_q3$name))
df_seqs_q4$name <- factor(df_seqs_q4$name, levels = unique(df_seqs_q4$name))
df_seqs_q5$name <- factor(df_seqs_q5$name, levels = unique(df_seqs_q5$name))

#glimpse(df_seqs_q5)
# Trazar el gráfico

p5 <- ggplot(df_seqs_q4) +
  geom_tile(aes(x = time, y = name, fill = state), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  ylab("") + xlab("") +
  labs(title = "Top nombres por anio para Quintil 5") +
  theme(axis.text.y = element_text(angle = 0, size = 7)) +
  scale_colour_manual(values = mypal_disc) +
  guides(fill = FALSE)

# plot- 
print(p5)



# plot 2 (comparar nombres comparando con el quintil 5)
# Renombrar columnas para evitar conflicto
df_seqs_q5 <- df_seqs_q5 %>%
  rename(name = name, 
         time = time,
         state_q5 = state)

df_seqs_q4 <- df_seqs_q4 %>%  
  rename(name = name, 
         time = time,
         state_q4 = state)

df_seqs_q3 <- df_seqs_q3 %>%  
  rename(name = name, 
         time = time,
         state_q3 = state)

df_seqs_q2 <- df_seqs_q2 %>%  
  rename(name = name, 
         time = time,
         state_q2 = state)

df_seqs_q1 <- df_seqs_q1 %>%  
  rename(name = name, 
         time = time,
         state_q1 = state)

# Unir por name y time  
df_tops_q5_q1 <- left_join(df_seqs_q5, df_seqs_q1, 
                           by = c("name", "time"))

df_tops_q5_q2 <- left_join(df_seqs_q5, df_seqs_q2, 
                           by = c("name", "time"))

df_tops_q5_q3 <- left_join(df_seqs_q5, df_seqs_q3, 
                           by = c("name", "time"))

df_tops_q5_q4 <- left_join(df_seqs_q5, df_seqs_q4, 
                     by = c("name", "time"))



#glimpse(df_tops)

ggplot(df_tops) +
  geom_tile(aes(x = time, y = name, fill = state_q5), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  geom_point(data = subset(df_tops, state_q4 == 1),  
             aes(x = time, y = name, color = state_q4),
             size = 4, shape = 1, stroke = 0.3) +
  scale_color_manual(values=c("1"="blue")) +
  labs(title = "Q5 / Q4 comparison") +
  theme(axis.text.y = element_text(size = 7))


ggplot(df_tops) +
  geom_tile(aes(x = time, y = name, fill = state_q5), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  geom_point(data = subset(df_tops, state_q4 == 1),  
             aes(x = time, y = name, fill = factor(state_q5)),
             size = 3, shape = 21, color = "blue") +
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
  labs(title = "Q5 / Q4 comparison", y="") +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE) +
  scale_color_manual(values = c("1" = "red")) +
  scale_fill_manual(name = "State Q4", values = c("0" = "white", "1" = "blue"))

# plot comparativos -----------------------------------------------------------

# plot Q5-Q4 
ggplot(df_tops_q5_q4) +
  geom_tile(aes(x = time, y = name, fill = state_q5), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  geom_point(data = subset(df_tops_q5_q4, state_q4 == 1),  
             aes(x = time, y = name),
             alpha = 0.8,
             fill = "blue",
             size = 3, shape = 21, color = "blue") +
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
  labs(title = "Q5 / Q4 comparison", y="", x="") +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE) +
  scale_color_manual(values = c("1" = "red")) +
  scale_fill_manual(name = "State Q4", values = c("0" = "white", "1" = "blue")) +
  scale_fill_manual(name = "State Q5", values = c("0" = "white", "1" = "red"))


# plot Q5-Q3 
ggplot(df_tops_q5_q3) +
  geom_tile(aes(x = time, y = name, fill = state_q5), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  geom_point(data = subset(df_tops_q5_q3, state_q3 == 1),  
             aes(x = time, y = name),
             alpha = 0.8,
             fill = "blue",
             size = 3, shape = 21, color = "blue") +
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
  labs(title = "Q5 / Q3 comparison", y="", x="") +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE) +
  scale_color_manual(values = c("1" = "red")) +
  scale_fill_manual(name = "State Q4", values = c("0" = "white", "1" = "blue")) +
  scale_fill_manual(name = "State Q5", values = c("0" = "white", "1" = "red"))

# plot Q5-Q2 
ggplot(df_tops_q5_q2) +
  geom_tile(aes(x = time, y = name, fill = state_q5), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  geom_point(data = subset(df_tops_q5_q2, state_q2 == 1),  
             aes(x = time, y = name),
             alpha = 0.8,
             fill = "blue",
             size = 3, shape = 21, color = "blue") +
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
  labs(title = "Q5 / Q2 comparison", y="", x="") +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE) +
  scale_color_manual(values = c("1" = "red")) +
  scale_fill_manual(name = "State Q4", values = c("0" = "white", "1" = "blue")) +
  scale_fill_manual(name = "State Q5", values = c("0" = "white", "1" = "red"))

# plot Q5-Q1 
ggplot(df_tops_q5_q1) +
  geom_tile(aes(x = time, y = name, fill = state_q5), alpha = 0.5) +
  scale_fill_manual(values = c("0" = "white", "1" = "red")) +
  geom_point(data = subset(df_tops_q5_q1, state_q1 == 1),  
             aes(x = time, y = name),
             alpha = 0.8,
             fill = "blue",
             size = 3, shape = 21, color = "blue") +
  scale_fill_manual(values = c("0" = "white", "1" = "blue")) +
  labs(title = "Q5 / Q1 comparison", y="", x="") +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE) +
  scale_color_manual(values = c("1" = "red")) +
  scale_fill_manual(name = "State Q4", values = c("0" = "white", "1" = "blue")) +
  scale_fill_manual(name = "State Q5", values = c("0" = "white", "1" = "red"))




# 4 difusión (descriptivo) -----------------------------------------------------
# Filtrar los datos para incluir solo el primer año y el quintil más alto
datos_primer_ano <- g1 %>%
  filter(ano == 1960, quintil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares <- datos_primer_ano %>%
  group_by(ano, nombre, quintil_weighted_isei) %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 20 nombres más populares (puedes ajustar este número según tus preferencias)

# from 1960 --------------------------------------------------------------------
## Filtrar los datos originales para incluir los nombres seleccionados y los años
datos_interes <- g1 %>%
  filter(ano %in% 1960:2017,
         quintil_weighted_isei %in% 1:5,
         nombre %in% nombres_populares$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_cuartil <- datos_interes %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_cuartil, aes(x = ano, y = promedio_popularidad, color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei))) +  
  geom_point(size = 2) +
  labs(title = "Most Popular Names in Quintile 5 in 1960",
       x = "Year",
       y = "Average Popularity",
       color = "Socioeconomic\nQuintile") +
  scale_colour_manual(values = mypal_disc)

  


## from 1970 ------------------------
# Filtrar los datos para incluir solo el año 1970 y el quintil más alto
datos_1970 <- g1 %>%
  filter(ano == 1970, quintil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_1970 <- datos_1970 %>%
  group_by(ano, nombre, quintil_weighted_isei) %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

datos_interes_1970 <- g1 %>%
  filter(ano %in% 1970:2017,
         quintil_weighted_isei %in% 1:5,
         nombre %in% nombres_populares_1970$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_1970 <- datos_interes_1970 %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_1970, aes(x = ano, y = promedio_popularidad, color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Nombres mas populares en el quintil 5 en el ano 1970",
       x = "Año",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconómico") +
  scale_colour_manual(values = mypal_disc)

## from 1982 ------------------------------------------------

# Filtrar los datos para incluir solo el año 1982 y el quintil más alto
datos_1982 <- g1 %>%
  filter(ano == 1982, quintil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_1982 <- datos_1982 %>%
  group_by(ano, nombre, quintil_weighted_isei) %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_1982 <- g1 %>%
  filter(ano %in% 1982:2017,
         quintil_weighted_isei %in% 1:5,
         nombre %in% nombres_populares_1982$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_1982 <- datos_interes_1982 %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_1982, aes(x = ano, y = promedio_popularidad, color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Nombres mas populares en el quintil 5 en el ano 1982",
       x = "Ano",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconomico")  +
  scale_colour_manual(values = mypal_disc)


## from 1992 -------------------------------------------------------------

# Filtrar los datos para incluir solo el año 1992 y el quintil más alto
datos_1992 <- g1 %>%
  filter(ano == 1992, quintil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_1992 <- datos_1992 %>%
  group_by(ano, nombre, quintil_weighted_isei) %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_1992 <- g1 %>%
  filter(ano %in% 1992:2017,
         quintil_weighted_isei %in% 1:5,
         nombre %in% nombres_populares_1992$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_1992 <- datos_interes_1992 %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_1992, aes(x = ano, y = promedio_popularidad, color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Nombres mas populares en el quintil 5 en el ano 1992",
       x = "Año",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconómico")  +
  scale_colour_manual(values = mypal_disc)

## from 2002 ------------------------------------------------------------

# Filtrar los datos para incluir solo el año 2002 y el quintil más alto
datos_2002 <- g1 %>%
  filter(ano == 2002, quintil_weighted_isei == 5)

# Ordenar los datos por popularidad en orden descendente y seleccionar los nombres más populares
nombres_populares_2002 <- datos_2002 %>%
  group_by(ano, nombre, quintil_weighted_isei) %>%
  arrange(desc(q)) %>%
  slice_head(n = 50)  # Seleccionar los 50 nombres más populares (puedes ajustar este número según tus preferencias)

# Filtrar los datos originales para incluir los nombres seleccionados y los años posteriores
datos_interes_2002 <- g1 %>%
  filter(ano %in% 2002:2022,
         quintil_weighted_isei %in% 1:5,
         nombre %in% nombres_populares_2002$nombre)

# Calcular el promedio de popularidad de los nombres seleccionados para cada quintil en cada año
promedio_por_quintil_2002 <- datos_interes_2002 %>%
  group_by(ano, quintil_weighted_isei) %>%
  summarise(promedio_popularidad = mean(q, na.rm = TRUE))

# Visualizar el promedio de popularidad de los nombres seleccionados para cada quintil a lo largo del tiempo
ggplot(promedio_por_quintil_2002, aes(x = ano, y = promedio_popularidad, color = factor(quintil_weighted_isei))) +
  geom_line(aes(group = factor(quintil_weighted_isei))) +  # Agregar línea que conecta los puntos por grupo
  geom_point() +
  labs(title = "Nombres mas populares en el quintil 5 en el ano 2002",
       x = "Año",
       y = "Promedio de popularidad",
       color = "Quintil\nSocioeconómico")  +
  scale_colour_manual(values = mypal_disc)


















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













