
library(ipumsr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringi)
library(stringdist)

# setwd
setwd("/home/rober/Documents/proyecto_nombres/wetransfer_ak002t0025787_2023-04-18_1401/AK002T0025787/Anexo_Respuesta_AK002T0025787")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))

# sample subset (stratified by "ano", "comuna")
names_sample <- names %>% group_by(ano,comuna) %>% sample_frac(size=.01)

# remove tilde and encoding (comuna)
names_sample$comuna = stri_trans_general(str = names_sample$comuna, id = "Latin-ASCII")

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


