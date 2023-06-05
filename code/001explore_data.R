
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
names_sample <- names %>% group_by(ano,nombre,comuna) %>% sample_frac(size=.1)

# create weighted network 
## group by year
names_sample_1970_79 <- names_sample %>% uncount(cantidad) %>% filter(ano%in%1970:1979) %>%
  ungroup() %>% select(nombre, comuna) %>% as_tibble() 
names_sample_1970_79<-names_sample_1970_79 %>% dplyr::mutate(comuna=recode(comuna, AISEN = "AYSEN")) # check

# merge con atributos de 
names_sample_1970_79<-names_sample_1970_79%>%group_by(nombre, comuna) %>% count()
  


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



#Antes de hacer la proyeccion de la red, seria bueno tener ciertos descriptivos basicos. 
#Por ejemplo, 

# 1. Cuantos nombres nuevos hay por ano? 
# 2. Cuan comun son ciertos nombres, es decir, en que proporcion de comunas aparecen los nombres por comuna? 
# 3. Hay alguna division territorial en como la gente da nombres a sus hijos? Hay alguna diferencia entre el sur y el norte (sur/norte de Stgo)? 

#Este tipo de informacion nos puede ayudar a darnos una idea de como va a ser la red de nombres.



# Counting Unique Values by Group Using (Applying group_by & summarise)
data_count_2 <- names_sample %>%    
  group_by(ano) %>%
  summarise(count = n_distinct(nombre))
data_count_2   

# create list by year
names_sample <- names_sample %>% group_by(ano)
names_samples<-names_sample%>%group_split(ano)

# Count the number of unique values in "name" column of current_df that are not present in previous_df
n<-setdiff(names_samples[[2]]$nombre, names_samples[[1]]$nombre)
#n

new_values_count <- map_int(2:length(names_samples), function(i) {
  current_df <- names_samples[[i]]
  previous_df <- names_samples[[i - 1]]
  # Count the number of unique values in "nombre" column of current_df that are not present in previous_df
  sum(!current_df$nombre %in% previous_df$nombre, na.rm = TRUE)
})

#new_values_count

#In the code above, we use the map_int function from purrr to iterate over the 
#indices of the data frames in the list, starting from the second data frame (index 2). 
#For each iteration, we retrieve the current and previous data frames using the index. 
#Then, we use the %in% operator to check which values in the "name" column of the current 
#data frame are not present in the previous data frame. Finally, we calculate the sum of TRUE 
#values to get the count of new values in the "name" column for that particular data frame.


# Assuming your list of data frames is called "data_frames_list"
# Initialize an empty list to store the results
output_list <- vector("list", length(names_samples))

# Iterate over the list of data frames using purrr's map2 function
# Compare the "name" column of each data frame with the previous data frame
# Count the number of new values using setdiff function
output_list <- map2(names_samples[-1], names_samples[-length(names_samples)],
                    ~ tibble(ano = .x$ano, new_names = length(setdiff(.x$nombre, .y$nombre))))

# Combine the data frames in the output list into a single data frame with distinct values by row. 
result <- result%>%bind_rows(output_list)%>%distinct()
result$ano<-as.numeric(result$ano) # year to numeric

# plot
result %>%
  ggplot(aes(x = ano, y = new_names)) +
  geom_point()+
  geom_smooth(method = "loess") + 
  xlim(1921, 2021) +
  stat_cor(method = "pearson", 
           label.x = 2000, label.y = 1100) +
  labs(y = "New names per year (n)", x = "Year")
  


# Assuming your list of data frames is named "df_list"
# Use map to iterate over each data frame in the list
result_list <- map(names_samples, ~ {
  # Group by "name" and "comuna" and count occurrences
  count_df <- count(.x, nombre, comuna) %>%
    arrange(desc(n))
  # Select the top 100 most common names
  top_600 <- count_df %>%
    group_by(nombre) %>%
    top_n(1, wt = n) %>%
    ungroup() %>%
    top_n(600, wt = n) %>%
    arrange(desc(n))
  # Return the resulting data frame
  top_600
})

# Combine the resulting data frames into one
combined_df <- bind_rows(result_list)

# Rename the count column to "amount"
combined_df <- rename(combined_df, amount = n)


# Combine "name" and "comuna" from all data frames into a single data frame
combined_df <- bind_rows(names_samples, .id = "df_id") %>%
  select(df_id, ano, comuna, nombre) %>%
  count(nombre, comuna, sort = TRUE)

top_names <- combined_df %>%
  top_n(600, n) %>%
  arrange(desc(n))

# Customize the size of the nodes based on the frequency
node_sizes <- top_names %>%
  mutate(size = n/600) # Adjust the scaling factor as needed

# Plot the network graph with differentiated node sizes
graph <- ggplot() +
  geom_node_point(aes(x = comuna, y = nombre, size = size, color = size), data = node_sizes) +
  #scale_fill_viridis_c(guide = "legend") +
  scale_color_viridis(option = "G",   direction = 1) +
  scale_size_continuous(range = c(0, 6)) +
  labs(x = "Commune", y = "Name") +
  #theme_minimal() +
  guides(size = "legend", colour = "legend") +
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 7,angle = 90, hjust = 0.9),
        axis.text.y = element_text(size = 7))

# Display the graph
print(graph)


# network
nodes1<-node_sizes%>%select(nombre)
nodes1$type<-"nombre"
colnames(nodes1)<-c("id","type")
nodes2<-node_sizes%>%select(comuna)
nodes2$type<-"comuna"
colnames(nodes2)<-c("id","type")
attr<-rbind(nodes1,nodes2)
attr<-unique(attr)

g <- graph_from_data_frame(d=node_sizes, vertices = attr)


# Customize the appearance of the nodes and edges
ggraph(g, layout = "kk") +
  geom_edge_link0(aes(alpha = size, width = size, colour = n), 
                  arrow = arrow(angle = 30,length = unit(0.15, "inches"), ends = "last", type = "closed")) + 
  scale_edge_colour_gradient(low = "#87CEFF", high = "#27408B") + 
 # geom_edge_arc2(aes(alpha = size, width = size, colour = n), 
 #               arrow = arrow(angle = 30,length = unit(0.15, "inches"), ends = "last", type = "closed")) +
  scale_edge_width(range = c(0.3, 1.8)) + 
  scale_edge_alpha(range = c(0.1, 1)) + 
  geom_node_point(aes(fill = type, size = all_degree),
                  colour = "#000000", shape = 21, stroke = 0.3) + 
  scale_fill_brewer(palette = "Set1", na.value = "gray53") + 
  scale_size(range = c(2,10)) + 
  #geom_node_text(aes(label = name), colour = "#000000", size = 3.5, family = "sans") + 
  geom_node_label(aes(label = name, filter=all_degree>=10), repel = TRUE, size = 2.5) +
  theme_graph(background="white") + 
  theme(legend.position = "none")
 

# network. 
node_sizes%>%
  filter(comuna%in%c("ANTOFAGASTA","ESTACION CENTRAL","PUERTO MONTT"))%>%
ggplot(aes(x=reorder_within(nombre, n, comuna), y=n, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "G",direction = 1)+
  facet_wrap(~comuna, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 9))


node_sizes%>%
  filter(comuna%in%c("PROVIDENCIA","INDEPENDENCIA"))%>%
  ggplot(aes(x=reorder_within(nombre, n, comuna), y=n, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "G",direction = 1)+
  facet_wrap(~comuna, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 9))



# 50 nombres más comunes por comuna. 
result <- map(names_samples, ~count(.x, comuna, nombre))
result <- bind_rows(result)
result <- result %>% group_by(comuna, nombre) %>% summarize(quantity = sum(n))
result <- result %>% arrange(comuna, desc(quantity))
result <- result %>% group_by(comuna) %>% top_n(30, quantity)

result%>%
  filter(comuna%in%c("LAS CONDES","VITACURA","PUENTE ALTO","SAN RAMON"))%>% #"LA REINA"
  ggplot(aes(x=reorder_within(nombre, quantity, comuna), y=quantity, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "G",direction = 1)+
  facet_wrap(~comuna, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 7))

result%>%
  filter(comuna%in%c("ARICA","COPIAPO","CONSTITUCION","AISEN"))%>%
  ggplot(aes(x=reorder_within(nombre, quantity, comuna), y=quantity, fill=comuna)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis_d(option = "G",direction = 1)+
  facet_wrap(~comuna, ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "", y = "", x=NULL) +
  #theme_gray(base_size = 7)+
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 9, hjust = 0.9),
        axis.text.y = element_text(size = 9))


# Customize the size of the nodes based on the frequency
node_sizes <- result %>%
  mutate(size = n/600) # Adjust the scaling factor as needed

# Plot the network graph with differentiated node sizes
graph <- ggplot() +
  geom_node_point(aes(x = comuna, y = nombre, size = size, color = size), data = node_sizes) +
  #scale_fill_viridis_c(guide = "legend") +
  scale_color_viridis(option = "G",   direction = 1) +
  scale_size_continuous(range = c(0, 6)) +
  labs(x = "Commune", y = "Name") +
  #theme_minimal() +
  guides(size = "legend", colour = "legend") +
  theme(axis.ticks.y=element_blank(),
        #legend.position = "top",
        panel.grid.major.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.title = element_text(size=10),
        axis.text.x = element_text(size = 7,angle = 90, hjust = 1),
        axis.text.y = element_text(size = 7))

# Display the graph
print(graph)





























