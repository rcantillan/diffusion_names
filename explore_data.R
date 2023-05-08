
library(ipumsr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringi)
library(stringdist)

# setwd
setwd("")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))

# sample subset (stratified by "ano", "comuna")
names_sample <- names %>% group_by(ano,comuna) %>% sample_frac(size=.1)

# remove tilde and encoding (comuna)
names_sample$comuna = stri_trans_general(str = names_sample$comuna, id = "Latin-ASCII")

# find ambiguous names 
#names_sample$bins <- sapply(names_sample$comuna, function(n)
#  paste(as.integer(agrepl(n, names_sample$comuna, max.distance = 2)), collapse=""))
#names_sample$group <- as.integer(as.factor(names_sample$bins))

# find similar string (similar groups = smg)
## option 1
smg <- function(x, thresh = 0.9){
  grp <- integer(length(x))
  comuna <- x
  for(i in seq_along(comuna)){
    if(!is.na(comuna[i])){
      sim <- agrepl(x[i], x, ignore.case = TRUE, max.distance = 1 - thresh)
      k <- which(sim & !is.na(comuna))
      grp[k] <- i
      is.na(comuna) <- k
    }
  }
  grp
}

#smg(names_sample[['comuna']])
sp1<-names_sample %>%
  mutate(group = comuna[smg(comuna)]) 
#%>%count(group)

## option 2
smg2 <- function(x, thresh = 0.8, method = "soundex"){
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

sp2<-names_sample %>%
  mutate(group = comuna[smg2(comuna, thresh = 0.3, method = "jw")])
#%>% count(group)
table(sp1$comuna)











