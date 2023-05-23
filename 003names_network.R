
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
  filter(degree>1) %>%
  mutate(tri = local_triangles()) %>%  
  mutate(localclust = 2*tri/(degree*(degree-1)))%>%
  mutate(comp = group_components())
#g1

# plot I
g1 %>%
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>%
  ggraph("graphopt") +
  geom_edge_arc0(aes(color = "red", width = value), 
                 alpha = 0.8, strength = 0.2, show.legend = FALSE) +
  #scale_edge_width_continuous(range = c(0.1,2)) +
  scale_edge_colour_identity() +
  geom_node_point(aes(filter = degree >= 100, x=x, y=y, size = degree, fill = "darkblue"), colour = "white", 
                  pch = 21, alpha = 0.7, show.legend = FALSE) +
  scale_size_continuous(range = c(1,8)) +
  scale_fill_identity() +
  ggnewscale::new_scale("size") + 
  geom_node_text(aes(label = name, filter = NULL), colour = "#000000", 
                 size = 3, family = "calibri", repel = T) + 
  scale_size_continuous(range = c(0.5,5)) +
  theme_void()



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



