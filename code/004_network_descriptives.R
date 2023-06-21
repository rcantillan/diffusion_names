
# librerías
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
library(tibble)
library(progress)
library(furrr)
library(progressr)
library(tidygraph)

# setwd
setwd("/home/rober/Documents/proyecto_nombres/wetransfer_ak002t0025787_2023-04-18_1401/AK002T0025787/Anexo_Respuesta_AK002T0025787")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))
names$comuna = stri_trans_general(str = names$comuna, id = "Latin-ASCII")
names$nombre = stri_trans_general(str = names$nombre, id = "Latin-ASCII")

# Create a stratified sample of 20% of the total data
names_sample <- names %>% group_by(ano, comuna) %>% sample_frac(size=.2, weight=cantidad) %>% ungroup()
#names_sample

# crear una lista de data frames 
names_sample$ano <- as.numeric(names_sample$ano)
split_by_year <- function(names_sample, start_year, end_year) {
  filter(names_sample, ano >= start_year, ano <= end_year)
}

start_years <- seq(1920, 2021, 10)
end_years <- start_years + 9

# lista 
list_decades <- map2(start_years, end_years, ~ split_by_year(names_sample, .x, .y))


# Create a function to create a list of weighted edges
create_edgelist <- function(df) {
  edgelist <- df %>%
      group_by(nombre, comuna) %>%
      summarize(count = n()) %>%
    select(nombre, comuna, count) %>%
    distinct()
  colnames(edgelist) <- c("from", "to", "n")
  edgelist <- migraph::as_edgelist(edgelist)
  return(edgelist)
}


# Create a function to create an affiliation matrix
create_am <- function(edgelist) {
  am <- edgelist %>%
    pivot_wider(names_from = to, values_from = weight)
  am[is.na(am)] <- 0
  am <- am %>% remove_rownames %>% column_to_rownames(var="from")
  return(am)
}

# Apply the functions to each data frame in the list
edgelists <- map(list_decades, create_edgelist)
ams <- map(edgelists, create_am)

#?as.one.mode
## to one mode matrix 
a1<-ams[[1]]
p<-bipartite::as.one.mode(a1, project="lower")

a2<-ams[[2]]
p2<-bipartite::as.one.mode(a2, project="lower")

a3<-ams[[3]]
p3<-bipartite::as.one.mode(a3, project="lower")

a4<-ams[[4]]
p4<-bipartite::as.one.mode(a4, project="lower")

a5<-ams[[5]]
p5<-bipartite::as.one.mode(a5, project="lower")

a6<-ams[[6]]
p6<-bipartite::as.one.mode(a6, project="lower")

a7<-ams[[7]]
p7<-bipartite::as.one.mode(a7, project="lower")

a8<-ams[[8]]
p8<-bipartite::as.one.mode(a8, project="lower")

a9<-ams[[9]]
p9<-bipartite::as.one.mode(a9, project="lower")

a10<-ams[[10]]
p10<-bipartite::as.one.mode(a10, project="lower")

a11<-ams[[11]]
p11<-bipartite::as.one.mode(a11, project="lower")

one_mode_projections <- list(p,p2,p3,p4,p5,p6,p7)
rm(ams)
#save(one_mode_projections, file = "/home/rober/Documents/proyecto_nombres/one_mode_projections.RData")

#one_mode_projections <- map(ams, ~bipartite::as.one.mode(., project="lower"))
#print(one_mode_projections)


# projection (con  bipartite package)
# If two communes A and B interact with names 1 to 5, then the two interaction vectors for A with 1 to 5 and B with 1 to 5 are placed next to each other, 
# and for each name 1 to 5, it holds the minimum for each of these 5 values for the two vectors (the parallel minimum). 
# The idea is that the similarity between communes A and B is due to their lower commonality in the interactions. 
# The five parallel minimum values are then added to obtain the final weight for this link.

# project with backbone package
bb <- map(one_mode_projections, ~ backbone::disparity(., alpha = 0.01, narrative = TRUE))
#print(bb)
save(bb,file="/home/rober/Documents/proyecto_nombres/bb.RData")

# Create a list to store the edgelists
edgelists <- list()

# Iterate over the arrays in the list (to edgelist)
for (i in 1:length(bb)) {
  bb_tibble <- as_tibble(bb[[i]])
  bb_tibble$from <- row.names(bb[[i]])
  bb_tibble <- bb_tibble %>% relocate(from)
  bb_tibble <- bb_tibble %>% pivot_longer(!from, names_to = "to", values_to = "value") %>% filter(value == 1)
  edgelists[[i]] <- bb_tibble
}

# Convert each edgelist to a network object
network_objects <- list()
for (i in 1:length(edgelists)) {
  network_objects[[i]] <- graph_from_data_frame(edgelists[[i]], directed=FALSE) %>% as_tbl_graph()
}

















