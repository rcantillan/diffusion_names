
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(questionr)
library(bipartite)
library(backbone)
library(tidygraph)
library(netdiffuseR)
library(linkcomm)
library(Clustering)
library(networkflow)

# unir datos 
#a<-left_join(names_sample_1970_79,gse_comunas_1970, by="comuna")


# atributos nombres. 
## Weighted mean by group
nombres_attr <- names_sample_1970_79 %>%                                          
  group_by(nombre) %>% 
  summarise(weighted.mean(prom_isei, n, na.rm = TRUE),
            weighted.mean(prom_school, n, na.rm = TRUE)) 

colnames(nombres_attr)<-c("nombres", "prom_isei", "prom_school")


## to weighted edgelist 
edgelist<-names_sample_1970_79%>%select(nombre, comuna, n)%>%distinct()
colnames(edgelist)<-c("from", "to", "n")
edgelist <- migraph::as_edgelist(edgelist) 


## to affiliation matrix
am<-edgelist %>%
  pivot_wider(names_from = to, values_from = weight)
am[is.na(am)] <- 0
am <- am %>% remove_rownames %>% column_to_rownames(var="from")


#?as.one.mode
## to one mode matrix 
p <- bipartite::as.one.mode(am, project="lower") #project="higher"

# projection (con  bipartite package)
# If two communes A and B interact with names 1 to 5, then the two interaction vectors for A with 1 to 5 and B with 1 to 5 are placed next to each other, 
# and for each name 1 to 5, it holds the minimum for each of these 5 values for the two vectors (the parallel minimum). 
# The idea is that the similarity between communes A and B is due to their lower commonality in the interactions. 
# The five parallel minimum values are then added to obtain the final weight for this link.

# project with backbone package
bb <- disparity(p, alpha = 0.01, narrative = TRUE)

# to edgelist
rmbb<-row.names(bb)
bb_tibble<-as_tibble(bb)
bb_tibble$from<-rmbb
bb_tibble<-bb_tibble %>% relocate(from)
bb_tibble <- bb_tibble %>% pivot_longer(!from, names_to = "to", values_to = "value") %>% filter(value==1)


# tidygraph object
g1 <- igraph::graph_from_data_frame(bb_tibble, vertices = nombres_attr) %>% as_tbl_graph()

# descriptivos red. 
g1<- g1 %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>%
  mutate(degree = centrality_degree()) %>% 
  mutate(pagerank = centrality_pagerank()) %>%
  mutate(closeness = centrality_closeness()) %>% 
  mutate(betweenness = centrality_betweenness()) %>%
  mutate(cluster=group_infomap())%>%
  filter(degree>1) %>%
  mutate(tri = local_triangles()) %>%  
  mutate(localclust = 2*tri/(degree*(degree-1)))%>%
  mutate(comp = group_components())
#g1

# degree distribution
deg<-g1%>%activate(nodes)%>%select(degree,pagerank)%>%as_tibble()
#deg%>%ggplot(aes(x = pagerank))+geom_density()

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

deg$degree<-min_max_norm(deg$degree)
deg$pagerank<-min_max_norm(deg$pagerank)


deg %>% pivot_longer(cols=c('degree', 'pagerank'),
                      names_to='Centralities',
                      values_to='value') %>% 
  ggplot(aes(x=value, fill = Centralities, colour = Centralities)) + 
  geom_density(alpha = 0.1) +
  labs(title= "Centrality distribution", x="") 




# plot I
g1 %>%
  activate(nodes) %>% 
  filter(degree=>400) %>%
  ggraph("graphopt") +
  geom_edge_arc0(aes(color = "red", width = value), 
                 alpha = 0.8, strength = 0.2, show.legend = FALSE) +
  #scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(x=x, y=y, size = degree, fill = "darkblue"), colour = "white", 
                  pch = 21, alpha = 0.7, show.legend = FALSE) +
  scale_size_continuous(range = c(1,8)) +
  scale_fill_identity() +
  ggnewscale::new_scale("size") + 
  geom_node_text(aes(label = name, filter = NULL), colour = "#000000", 
                 size = 3, family = "calibri", repel = T) + 
  scale_size_continuous(range = c(0.5,5)) +
  theme_void()



# cluster
bb_tibble<-bb_tibble%>%select(from,to)
bb_tible<-as.data.frame(bb_tibble)

lc<-getLinkCommunities(bb_tibble, hcmethod = "average")
oc <- getOCG.clusters(bb_tibble)


