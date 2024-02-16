
# library
library(ipumsr)
library(haven)
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)
library(naniar)
library(ggsci)
library(stringdist)
library(questionr)
library(networkD3)
library(ggalluvial)


# Names data -------------------------------------------------------------------
## setwd
setwd("/home/rober/Documents/proyecto_nombres/wetransfer_ak002t0025787_2023-04-18_1401/AK002T0025787/Anexo_Respuesta_AK002T0025787")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 


## separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))
names$comuna = stri_trans_general(str = names$comuna, id = "Latin-ASCII")
names$nombre = stri_trans_general(str = names$nombre, id = "Latin-ASCII")

# IPUMS data -------------------------------------------------------------------
ipums <- read_dta("~/Documents/proyecto_nombres/ipumsi_00005.dta")

con <- dbConnect(
  Postgres(),
  user = "fondecyt",
  password = "9nvGYZ35nUdBTSbVhpmp", #borrar
  dbname = "fondecyt",
  host = "64.227.106.47"
  #port = 5432 # no es necesario, a menos que sea un puerto no estandar
)

## Subir datos al servidor 
dbListTables(con)
#dbWriteTable(con, "ipums_chile", ipums, overwrite = T)
ipums <- tbl(con, "ipums_chile") %>% collect() %>% glimpse()
#ipums <- read_dta("~/Documents/proyecto_nombres/ipumsi_00006.dta")

## merge 
## with `GEO2_CL` (1960:1970), `GEO2_CL1960`, `GEO2_CL1970`, `GEO2_CL1982`, `GEO2_CL1992`, `GEO2_CL2002`, `GEO2_CL2017`
## Create a file of equivalences for communes by decadeshttp://127.0.0.1:20211/graphics/ac03a1de-455c-4d8d-b357-1b9e8fbb894c.png
## 1- Create a tab by a decade of communes and codes.
## 2- Combine codes with name data.
## 3- Merge of sociodemographic variables by year 


## obtener labels (nombres) de los códigos comunales para el año 1970. 
#freq(ipums$geo2_cl1970)
#id_freq<-freq(ipums$geo2_cl1970)
#id_freq <- tibble::rownames_to_column(id_freq, "id")

## create label key data. 
## 1960
id_comunas_1960 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1960", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1960)<-c("id","name")
id_comunas_1960$year<-1960

## 1970
id_comunas_1970 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1970", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1970)<-c("id","name")
id_comunas_1970$year<-1970

## 1982
id_comunas_1982 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1982", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1982)<-c("id","name")
id_comunas_1982$year<-1982

## 1992
id_comunas_1992 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1992", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1992)<-c("id","name")
id_comunas_1992$year<-1992

## 2002
id_comunas_2002 <- read_delim("~/Documents/diffusion_names/data/id_comunas_2002", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2002)<-c("id","name")
id_comunas_2002$year<-2002

## 2017
id_comunas_2017 <- read_delim("~/Documents/diffusion_names/data/id_comunas_2017", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2017)<-c("id","name")
id_comunas_2017$year<-2017

## join
id_comunas_ipums<-rbind(id_comunas_1960, id_comunas_1970, id_comunas_1982, id_comunas_1992,
                         id_comunas_2002, id_comunas_2017)
rm(id_comunas_1960, id_comunas_1970, id_comunas_1982, id_comunas_1992, id_comunas_1992, id_comunas_2002, id_comunas_2017)

## separate column character by coma. 
id_comunas_ipums<-id_comunas_ipums %>%            
  separate_rows(name, sep=",") %>% 
  mutate(across(where(is.character), str_trim))

## delete accent and `toupper`
id_comunas_ipums$name<-stri_trans_general(id_comunas_ipums$name,id = "Latin-ASCII")
id_comunas_ipums$name <- toupper(id_comunas_ipums$name)

## save
##save(id_comunas_ipums, file = "id_comunas_ipums.RData")


# La proyección de la red bipartita podrías hacerla para los nombres en lugar de las comunas. 
# Probablemente, la red va a ser muy densa y completamente conectada, por lo que un plot de la red no va a ser informativo. 
# Habría que mirar métodos de backbone extraction para extraer los ties más importantes de la red. 
# Yo aplicaría métodos de community detection solo cuando tengamos esta red menos densa. 
# En relación con esto, sería bueno tener una idea de cómo se distribuyen los nombres en el tiempo (por ej., por década), especialmente la cantidad de nombres nuevos que aparecen en el tiempo.

#glimpse(ipums)


# Socio economic variables -----------------------------------------------------

## select variables
ipums<-ipums%>%
  filter(relate%in%c(1,2))%>%
  mutate(isei=case_when(occisco==1~68, 
                        occisco==2~65,
                        occisco==3~51,
                        occisco==4~41,
                        occisco==5~31,
                        occisco==6~18,
                        occisco==7~35,
                        occisco==8~32,
                        occisco==9~20,
                        occisco==10~53,
                        occisco==11~0,
                        occisco==97~0,
                        occisco==98~0,
                        occisco==99~0))%>%
  replace_with_na(replace = list(yrschool = c("90","91","92","93","94","95","96","98","99")))%>%
  select(serial
         ,year
         ,geo2_cl1970
         ,geo2_cl1960
         ,geo2_cl1982
         ,geo2_cl1992
         ,geo2_cl2002
         ,geo2_cl2017
         #,hhwt
         #,gq
         #,pernum
         ,relate
         #,related
         #,edattain
         #,edattaind
         ,yrschool
         #,educcl
         ,isei
         )


#ipums<-ipums %>%  pivot_wider(names_from = relate, values_from = c(yrschool,isei))
ipums <- ipums %>% pivot_wider(names_from = relate, values_from = c(yrschool, isei), values_fn = list(isei = mean, yrschool = mean))

## Lista de todas las variables que tienen "geo2_cl" en su nombre
geo_vars <- grep("geo2_cl", colnames(ipums), value = TRUE)

## Unir las variables en una columna llamada "id_comuna" y pivot wider para promediar considerando los integrantes. 
ipums <- ipums %>%
  unite("id_comuna", all_of(geo_vars), sep = "|", na.rm = TRUE) %>%
  mutate(id_comuna = as.numeric(str_extract(id_comuna, "\\d+"))) 


## Función para calcular el promedio y manejar NAs
promedio_con_na <- function(x, y) {
  if (is.na(x) | is.na(y)) {
    return(mean(c(x, y), na.rm = TRUE))
  } else {
    return((x + y) / 2)
  }
}

## Calcular los promedios usando pmap
ipums <- ipums %>%
  mutate(
    prom_isei = pmap_dbl(select(., isei_1, isei_2), ~promedio_con_na(..1, ..2)),
    prom_school = pmap_dbl(select(., yrschool_1, yrschool_2), ~promedio_con_na(..1, ..2))
  )


## plot prom_isei
median_isei <- ipums %>% pull(prom_isei) %>% median(na.rm = TRUE) %>% signif(6)
ggplot(ipums, aes(x = prom_isei, y = after_stat(density))) +
  geom_histogram(bins = 50) + 
  geom_density(color = NA,  fill = 'blue', alpha = 0.3) +
  #scale_x_log10()+
  geom_vline(xintercept=median_isei, size=1, color="red") +
  labs(x="Prestigio ocupacional", title="IPUMS Chile 1960-2017")

ggplot(ipums, aes(x = prom_isei, y = after_stat(density))) +
  geom_histogram(bins = 40) +  # Ajustar el número de bins
  geom_density(color = NA, fill = 'blue', alpha = 0.2) +
  scale_x_continuous(trans = "log10") +  # Cambiar la escala a logarítmica
  geom_vline(xintercept = median_isei, size = 1, color = "red") +
  labs(x = "Prestigio ocupacional (log scale)", title = "IPUMS Chile 1970-2017") 


## plot prom_school
median_school <- ipums %>% pull(prom_school) %>% median(na.rm = TRUE) %>% signif(6)
ggplot(ipums, aes(x = prom_school, y = after_stat(density))) +
  geom_histogram(bins = 80) + 
  geom_density(color = NA,  fill = 'blue', alpha = 0.3) +
  #scale_x_log10()+
  geom_vline(xintercept=median_school, size=1, color="red") +
  labs(x="Nivel educativo", title="IPUMS Chile 1960-2017")


# Join names and IPUMS data ----------------------------------------------------
id_comunas_ipums$id<-as.numeric(id_comunas_ipums$id)
id_comunas_ipums<-id_comunas_ipums%>%distinct(.keep_all = TRUE)
ipums<-ipums%>%rename(id=id_comuna)

## Join
ipums<-left_join(ipums, id_comunas_ipums, by=c("id", "year"))%>%
  select(serial, id, name, year, prom_school, prom_isei)
# detalle: el "serial" se va a repetir tantas veces como nombres comunales tenga el id de comuna. 


## mediana anual por comuna de los promedios por hogar. 
gse_comunas<-ipums %>%
  group_by(year, name) %>%
  summarise(median_prom_school = median(prom_school, na.rm = TRUE),
            median_prom_isei = median(prom_isei, na.rm = TRUE))

colnames(gse_comunas)<-c("ano","comuna","median_prom_isei","median_prom_school")
# gse_comunas_xxx se debe pegar a los nombres. 
# el proceso debe repetirse por década IPUMS. 


## Plot
gse_comunas %>% pivot_longer(cols=c('median_prom_isei', 'median_prom_school'),
                      names_to='gse',
                      values_to='value') %>% 
  ggplot(aes(x=value, fill = gse, colour = gse)) + 
  geom_density(alpha = 0.1) +
  labs(title= "SEG distribution", x="") 


## Join with names data
gse_comunas$ano<-as.character(gse_comunas$ano)
names_sample_p <- left_join(names, gse_comunas, by = c("comuna", "ano"))

## Convertir las columnas "comuna" y "ano" a caracteres en ambos data frames
names <- names %>% mutate(comuna = as.character(comuna), ano = as.character(ano))
gse_comunas <- gse_comunas %>% mutate(comuna = as.character(comuna), ano = as.character(ano))

## Repetir valores para "median_prom_isei" y "median_prom_school"
names_sample_p_filled <- names_sample_p %>%
  mutate(ano_prefix = substr(ano, 1, 3)) %>%
  group_by(comuna, ano_prefix) %>%
  mutate(
    median_prom_isei = ifelse(is.na(median_prom_isei), median(median_prom_isei, na.rm = TRUE), median_prom_isei),
    median_prom_school = ifelse(is.na(median_prom_school), median(median_prom_school, na.rm = TRUE), median_prom_school)
  ) %>%
  ungroup() %>%
  select(-ano_prefix)

glimpse(names_sample_p_filled)

## Calcula el promedio ponderado de "median_prom_isei" y "median_prom_school"
names_prom <- names_sample_p_filled %>%
  group_by(nombre, ano) %>%
  summarize(
    weighted_isei = weighted.mean(median_prom_isei, cantidad, na.rm = TRUE),
    weighted_school = weighted.mean(median_prom_school, cantidad, na.rm = TRUE)
  ) %>%
  ungroup()


# Socioeconomic quartile  -----------------------------------------------------
## Crear cuartiles por año para median_prom_isei
names_prom <- names_prom %>%
  group_by(ano) %>%
  mutate(cuartil_weighted_isei = ntile(weighted_isei, 4)) %>%
  mutate(cuartil_weighted_school = ntile(weighted_school, 4))

#glimpse(names_prom)

# plot alluvial names flux between quartile 
names_prom$cuartil_weighted_isei <- as.factor(names_prom$cuartil_weighted_isei)
names_prom$cuartil_weighted_isei <- factor(names_prom$cuartil_weighted_isei, levels=c('4', '3', '2', "1"))
#table(names_prom$cuartil_weighted_isei)

names_prom$cuartil_weighted_school <- as.factor(names_prom$cuartil_weighted_school)
names_prom$cuartil_weighted_school <- factor(names_prom$cuartil_weighted_school, levels=c('4', '3', '2', "1"))
#table(names_prom$cuartil_weighted_isei)

## alluvial with occupational prestige
names_prom %>%
  filter(ano %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2019)) %>% 
  drop_na(cuartil_weighted_isei) %>% 
  filter(cuartil_weighted_isei %in% c(4,1)) %>%
  ggplot(aes(x = ano, stratum = cuartil_weighted_isei, alluvium = nombre,
             fill = cuartil_weighted_isei, label = cuartil_weighted_isei)) +
  scale_fill_jama() +
  #scale_fill_cosmic("hallmarks_light") +
  geom_flow(color = "darkgray") + 
  geom_stratum() +
  guides(fill=guide_legend("Occupational prestige (quartile)")) +
  theme(legend.position = "bottom") +
  ggtitle("The flow of names between socioeconomic groups")


## alluvial with school years
names_prom %>%
  filter(ano %in% c(1960, 1970, 1980, 1990, 2000, 2009)) %>% 
  drop_na(cuartil_weighted_school) %>% 
  filter(cuartil_weighted_school %in% c(4,1)) %>%
  ggplot(aes(x = ano, stratum = cuartil_weighted_school, alluvium = nombre,
             fill = cuartil_weighted_school, label = cuartil_weighted_school)) +
  scale_fill_jama() +
  #scale_fill_cosmic("hallmarks_light") +
  geom_flow(color = "darkgray") + 
  geom_stratum() +
  guides(fill=guide_legend("Years of schooling (quartile)")) +
  theme(legend.position = "bottom") +
  ggtitle("The flow of names between socioeconomic groups")

# difusión de nombres ----------------------------------------------------------
# comuna data panel 















