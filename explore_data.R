
library(readr)
setwd("")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# descr
com<-names$COMUNA
lapply(com,agrep,com,value=TRUE)


