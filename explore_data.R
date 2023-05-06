
library(ipumsr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)

# setwd
setwd("")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))

# descr
com<-names$COMUNA
lapply(com,agrep,com,value=TRUE)

