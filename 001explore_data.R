
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
library(graphlayouts)
library(stringdist)
library(bipartite)

# setwd
setwd("/home/rober/Documents/proyecto_nombres/wetransfer_ak002t0025787_2023-04-18_1401/AK002T0025787/Anexo_Respuesta_AK002T0025787")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))
names$comuna = stri_trans_general(str = names$comuna, id = "Latin-ASCII")
names$nombre = stri_trans_general(str = names$nombre, id = "Latin-ASCII")

# sample subset (stratified by "ano", "comuna")
names_sample <- names %>% group_by(ano,comuna) %>% sample_frac(size=.1)

# find similar string (similar groups = smg)
## option 1
similar_groups_str <- function(x, thresh = 0.8, method = "soundex"){
  grp <- integer(length(x))
  comuna <- x
  x <- tolower(x)
  for(i in seq_along(comuna)){
    if(!is.na(comuna[i])){
      sim <- stringdist::stringsim(x[i], x, method = method)
      k <- which(sim > thresh & !is.na(comuna))
      grp[k] <- i
      is.na(comuna) <- k
    }
  }
  grp
}
comunas_x<-names_sample %>%
  mutate(group = comuna[similar_groups_str(comuna, thresh = 0.7, method = "jw")]) %>% count(group)


# create weighted network 
## group by year
names_sample_1970_79 <- names_sample %>% uncount(cantidad) %>% filter(ano%in%1970:1979) %>%
  ungroup() %>% select(nombre, comuna) %>% as_tibble() 

names_sample_1970_79<-names_sample_1970_79 %>% dplyr::mutate(comuna=recode(comuna, AISEN = "AYSEN")) # check


# merge con atributos de 
names_sample_1970_79<-names_sample_1970_79%>%group_by(nombre, comuna) %>% count()
names_sample_1970_79<-left_join(names_sample_1970_79, gse_comunas_1970, by="comuna")
  


















