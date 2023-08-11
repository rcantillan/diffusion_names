
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
library(stringdist)
library(questionr)


# load ipums data
ipums <- read_dta("~/Documents/proyecto_nombres/ipumsi_00005.dta")

# merge 
## with `GEO2_CL` (1960:1970), `GEO2_CL1960`, `GEO2_CL1970`, `GEO2_CL1982`, `GEO2_CL1992`, `GEO2_CL2002`, `GEO2_CL2017`

# Create a file of equivalences for communes by decades
## 1- Create a tab by a decade of communes and codes.
## 2- Combine codes with name data.
## 3- Merge of sociodemographic variables by year 


# obtener labels (nombres) de los códigos comunales para el año 1970. 
freq(ipums$geo2_cl1970)
id_freq<-freq(ipums$geo2_cl1970)
id_freq <- tibble::rownames_to_column(id_freq, "id")

# create label key data. 
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

# separate column character by coma. 
id_comunas_ipums<-id_comunas_ipums %>%            
  separate_rows(name, sep=",") %>% 
  mutate(across(where(is.character), str_trim))

# delete accent and `toupper`
id_comunas_ipums$name<-stri_trans_general(id_comunas_ipums$name,id = "Latin-ASCII")
id_comunas_ipums$name <- toupper(id_comunas_ipums$name)

# save
#save(id_comunas_ipums, file = "id_comunas_ipums.RData")


#La proyección de la red bipartita podrías hacerla para los nombres en lugar de las comunas. 
#Probablemente, la red va a ser muy densa y completamente conectada, por lo que un plot de la red no va a ser informativo. 
#Habría que mirar métodos de backbone extraction para extraer los ties más importantes de la red. 
#Yo aplicaría métodos de community detection solo cuando tengamos esta red menos densa. 
#En relación con esto, sería bueno tener una idea de cómo se distribuyen los nombres en el tiempo (por ej., por década), especialmente la cantidad de nombres nuevos que aparecen en el tiempo.

glimpse(ipums)


# variables socioeconómicas. 
## subset 1970 
#ipums_1970<-ipums%>%filter(year==1970)
#glimpse(ipums_1970)

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


# Lista de todas las variables que tienen "geo2_cl" en su nombre
geo_vars <- grep("geo2_cl", colnames(ipums), value = TRUE)

# Unir las variables en una columna llamada "id_comuna" y pivot wider para promediar considerando los integrantes. 
ipums <- ipums %>%
  unite("id_comuna", all_of(geo_vars), sep = "|", na.rm = TRUE) %>%
  mutate(id_comuna = as.numeric(str_extract(id_comuna, "\\d+"))) 



# imputar valor alto cuando una de las columnas tiene NA (opción 1)

# Función para calcular el promedio y manejar NAs (opción 2 `purrr`)
promedio_con_na <- function(x, y) {
  if (is.na(x) | is.na(y)) {
    return(mean(c(x, y), na.rm = TRUE))
  } else {
    return((x + y) / 2)
  }
}

# Calcular los promedios usando pmap
ipums <- ipums %>%
  mutate(
    prom_isei = pmap_dbl(select(., isei_1, isei_2), ~promedio_con_na(..1, ..2)),
    prom_school = pmap_dbl(select(., yrschool_1, yrschool_2), ~promedio_con_na(..1, ..2))
  )



# plot
median_isei <- ipums %>% pull(prom_isei) %>% median(na.rm = TRUE) %>% signif(6)
ggplot(ipums, aes(x = prom_isei, y = after_stat(density))) +
  geom_histogram(bins = 50) + 
  geom_density(color = NA,  fill = 'blue', alpha = 0.3) +
  #scale_x_log10()+
  geom_vline(xintercept=median_isei, size=1, color="red") +
  labs(x="Prestigio ocupacional", title="IPUMS Chile 1960-2017")


# Crear el gráfico
ggplot(ipums, aes(x = prom_isei, y = after_stat(density))) +
  geom_histogram(bins = 40) +  # Ajustar el número de bins
  geom_density(color = NA, fill = 'blue', alpha = 0.2) +
  scale_x_continuous(trans = "log10") +  # Cambiar la escala a logarítmica
  geom_vline(xintercept = median_isei, size = 1, color = "red") +
  labs(x = "Prestigio ocupacional (log scale)", title = "IPUMS Chile 1970-2017") 



# plot
median_school <- ipums %>% pull(prom_school) %>% median(na.rm = TRUE) %>% signif(6)
ggplot(ipums, aes(x = prom_school, y = after_stat(density))) +
  geom_histogram(bins = 80) + 
  geom_density(color = NA,  fill = 'blue', alpha = 0.3) +
  #scale_x_log10()+
  geom_vline(xintercept=median_school, size=1, color="red") +
  labs(x="Nivel educativo", title="IPUMS Chile 1960-2017")


# unir códigos comunales con datos
id_comunas_ipums$id<-as.numeric(id_comunas_ipums$id)
id_comunas_ipums<-id_comunas_ipums%>%distinct(.keep_all = TRUE)
ipums<-ipums%>%rename(id=id_comuna)

# Join
ipums<-left_join(ipums, id_comunas_ipums, by=c("id", "year"))%>%
  select(serial, id, name, year, prom_school, prom_isei)
# detalle: el "serial" se va a repetir tantas veces como nombres comunales tenga el id de comuna. 


# mediana anual por comuna de los promedios por hogar. 
gse_comunas<-ipums %>%
  group_by(year, name) %>%
  summarise(median_prom_school = median(prom_school, na.rm = TRUE),
            median_prom_isei = median(prom_isei, na.rm = TRUE))

colnames(gse_comunas)<-c("ano","comuna","median_prom_isei","median_prom_school")
# gse_comunas_xxx se debe pegar a los nombres. 
# el proceso debe repetirse por década IPUMS. 


# Plot
gse_comunas %>% pivot_longer(cols=c('median_prom_isei', 'median_prom_school'),
                      names_to='gse',
                      values_to='value') %>% 
  ggplot(aes(x=value, fill = gse, colour = gse)) + 
  geom_density(alpha = 0.1) +
  labs(title= "SEG distribution", x="") 


# Unir con datos de nombres.
gse_comunas$ano<-as.character(gse_comunas$ano)
names_sample_p<-left_join(names_sample, gse_comunas, by=c("comuna", "ano"))

# Convertir las columnas "comuna" y "ano" a caracteres en ambos data frames
names_sample <- names_sample %>% mutate(comuna = as.character(comuna), ano = as.character(ano))
gse_comunas <- gse_comunas %>% mutate(comuna = as.character(comuna), ano = as.character(ano))

# Realizar la unión utilizando left_join()
names_sample_p <- left_join(names_sample, gse_comunas, by = c("comuna", "ano"))
#glimpse(names_sample_p)


# Repetir valores para "median_prom_isei" y "median_prom_school"
names_sample_p_filled <- names_sample_p %>%
  mutate(ano_prefix = substr(ano, 1, 3)) %>%
  group_by(comuna, ano_prefix) %>%
  mutate(
    median_prom_isei = ifelse(is.na(median_prom_isei), median(median_prom_isei, na.rm = TRUE), median_prom_isei),
    median_prom_school = ifelse(is.na(median_prom_school), median(median_prom_school, na.rm = TRUE), median_prom_school)
  ) %>%
  ungroup() %>%
  select(-ano_prefix)




