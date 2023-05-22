
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(questionr)
library(bipartite)
library(backbone)
library(tidygraph)


# unir datos 
a<-left_join(names_sample_1970_79,gse_comunas_1970, by="comuna")


# atributos nombres. 
## Weighted mean by group
nombres_attr <- a %>%                                          
  group_by(nombre) %>% 
  summarise(weighted.mean(prom_isei, n, na.rm = TRUE),
            weighted.mean(prom_school, n, na.rm = TRUE)) 


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
bb <- disparity(p, alpha = 0.05, narrative = TRUE)

plot(bb)
g1 <- igraph::graph_from_data_frame(bb) %>% as_tbl_graph()


# plot (preparativos)
g<- graph.adjacency(bb, mode="undirected", weighted=TRUE)
E(g)$width <- E(g)$weight + min(E(g)$weight) + 1 # offset=1
#g <- igraph::delete.vertices(g, c("AISEN","COLBUN","CHILE CHICO")) #degree0


g



#plot(g)
#V(g)$cluster_louvain
bb <- layout_as_backbone(g, keep = 0.4)

ggraph(g, layout = "manual", x = x, y = y) + 
  geom_edge_link0(aes(width = weight), edge_colour = "#A8A8A8",
                  edge_alpha = 1) + 
  scale_edge_width(range = c(0.05, 2)) + 
  geom_node_point(aes(fill = cluster_louvain,
                      size = eigen_centrality), colour = "#FFFFFF", shape = 21, stroke = 0.5) + 
  geom_node_text(aes(filter = degree >= 50
                     , label = name, size=0.005), family = "Helvetica-Narrow", repel = F) +
  scale_fill_brewer(palette = "Dark2",
                    na.value = "gray53") + 
  scale_size(range = c(2, 9)) + 
  #geom_node_text(aes(label = name), colour = "#000000",size = 3, family = "Helvetica-Narrow") + 
  theme_graph() + 
  theme(legend.position = "none")



