
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

# setwd
setwd("/home/rober/Documents/proyecto_nombres/wetransfer_ak002t0025787_2023-04-18_1401/AK002T0025787/Anexo_Respuesta_AK002T0025787")
Sys.setlocale( 'LC_ALL','C' ) 
names<-read_delim("datos_1920a2021.txt", delim = ";", locale=locale(encoding="latin1")) 

# separate year/month column 
colnames(names)<-c("ano","comuna","nombre","cantidad")
names<-separate(names, ano, into = c("ano","mes"), sep = c(4))
names$comuna = stri_trans_general(str = names$comuna, id = "Latin-ASCII")
names$nombre = stri_trans_general(str = names$nombre, id = "Latin-ASCII")

# crear data agrupada por año y quitar 2.5 superior e inferior de acuerdo da la distribución de cantidad agrupada por año. 
## sumar cantidad de nombres por año. 
names <- names %>%
  group_by(ano, comuna, nombre) %>%
  summarize(cantidad = sum(cantidad)) 
glimpse(names)

names_dist <- names %>%
  group_by(nombre) %>%
  summarize(cantidad_total = sum(cantidad))

names_dist %>%
arrange(desc(cantidad_total))




# filtrar datos  ---------------------------------------------------------------
# Crear un nuevo data frame con los percentiles 2.5 y 97.5
#names_filtered <- names_dist %>%
#  #group_by(ano) %>%
#  filter(cantidad_total >= quantile(cantidad_total, 0.015) & cantidad_total <= quantile(cantidad_total, 0.985)) %>%
#  ungroup()

names_filtered <- names %>% filter(!nombre %in% c("MARIA ", "JOSE ", "JUAN ", "LUIS " )) # crterio: eliminar los 4 más comunes, en gral. presentes en todas las comunas. 
#head(names_filtered)  


# sample subset (stratified by "ano", "comuna") 
# names_sample <- names %>% group_by(ano,comuna) %>% sample_frac(size=.1)

# Paso 1: Crear estratos basados en las dos variables
strata <- names %>%
  group_by(ano, comuna) %>%
  summarize(count = n()) %>%
  ungroup()

# Paso 2: Calcular el tamaño de muestra deseado para cada estrato (por ejemplo, el 10% de cada estrato)
strata <- strata %>%
  mutate(sample_size = ceiling(0.1 * count))  # Puedes ajustar el porcentaje de muestra según tus necesidades

# Paso 3: Realizar el muestreo en cada estrato
names_sample <- names %>%
  inner_join(strata, by = c("ano", "comuna")) %>%
  group_by(ano, comuna) %>%
  sample_n(size = first(sample_size))  # Usamos first() para obtener el tamaño de muestra del primer registro en cada estrato

# sumar cantidad de nombres por año. 
names_sample <- names_sample %>%
  group_by(ano, comuna, nombre) %>%
  summarize(cantidad = sum(cantidad))



# create weighted network 
## group by year
#names_sample_1970_79 <- names_sample %>% uncount(cantidad) %>% filter(ano%in%1970:1979) %>%
#  ungroup() %>% select(nombre, comuna) %>% as_tibble() 
#names_sample_1970_79 <- names_sample_1970_79 %>% dplyr::mutate(comuna=recode(comuna, AISEN = "AYSEN")) # check

# merge con atributos de 
#names_sample_1970_79 <- names_sample_1970_79 %>% group_by(nombre, comuna) %>% count()
  

# Estadísticos descriptivos ---------------------------------------------------

# 1. Nombres nuevos por ano 
# 2. Frecuencia de nombres: en que proporcion de comunas aparecen los nombres por comuna? 
# 3. Segregación: ¿Hay alguna division territorial en como la gente da nombres a sus hijos? ¿Hay alguna diferencia entre el sur y el norte (sur/norte de Stgo)? 


# Counting Unique Values by Group Using (Applying group_by & summarise)

# create list by year
names <- names %>% group_by(ano)
names <- names %>% group_split(ano)

# Count the number of unique values in "name" column of current_df that are not present in previous_df
n<-setdiff(names[[5]]$nombre, names[[4]]$nombre)
summary(n)


# Contar nombres nuevos por año
nombres_nuevos_por_ano <- numeric(length(names))

for (i in 2:length(names)) {
  # Obtener los nombres únicos del año actual
  nombres_ano_actual <- names[[i]]$nombre
  # Obtener los nombres únicos del año anterior
  nombres_ano_anterior <- names[[i - 1]]$nombre
  # Calcular la cantidad de nombres nuevos
  nombres_nuevos_por_ano[i] <- length(setdiff(nombres_ano_actual, nombres_ano_anterior))
}

mean(nombres_nuevos_por_ano)
# Calcular las diferencias
diferencias <- diff(nombres_nuevos_por_ano)
# Calcular la diferencia promedio
diferencia_promedio <- mean(diferencias)

#nombres_nuevos_por_ano
# plot
resultados <- data.frame(
  ano = 1920:2021, # Ajusta los años según tus datos
  new_names = nombres_nuevos_por_ano) %>%
  ggplot(aes(x = ano, y = new_names)) +
  geom_point()+
  geom_smooth(method = "loess") + 
  xlim(1920, 2021) +
  stat_cor(method = "pearson", 
           label.x = 2000, label.y = 100) +
  labs(y = "New names per year (n)", x = "Year")

resultados

# usamos names_filtered (sin los nombres más y menos comunes)
# create list by year
names_filtered <- names_filtered %>% group_by(ano)
names_filtered <- names_filtered %>% group_split(ano)
#names_filtered[[1]]

# Supongamos que tienes una lista llamada names_filtered que contiene tus data frames

# Assuming your list of data frames is named "df_list"
# Use map to iterate over each data frame in the list
result_list <- map(names_filtered, ~ {
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
combined_df <- bind_rows(names_filtered, .id = "df_id") %>%
  select(df_id, ano, comuna, nombre) %>%
  count(nombre, comuna, sort = TRUE)

top_names <- combined_df %>%
  top_n(400, n) %>%
  arrange(desc(n))

# Customize the size of the nodes based on the frequency
node_sizes <- top_names %>%
  mutate(size = n/400) # Adjust the scaling factor as needed

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



# block modelling 
library(sbm)
library(kableExtra)
nm<-node_sizes%>%select(nombre, comuna, size)
nm<-nm %>% pivot_wider(names_from = comuna, values_from = size)
nm<-nm %>% remove_rownames %>% column_to_rownames(var="nombre") 
nm[is.na(nm)] <- 0
nm<-as.matrix(nm)

BipartitoSBM <-  nm %>% 
  estimateBipartiteSBM(model = 'poisson', 
                       dimLabels = c('nombre', 'comuna'),
                       estimOptions = list(verbosity = 3, plot = F))


BipartitoSBM$storedModels 
BipartitoSBM$storedModels %>%  ggplot() + aes(x = nbBlocks, y = ICL)  + geom_line() + geom_point(alpha = 0.5)
plot(BipartitoSBM,   type = "data",  plotOptions = c(rowNames=T, colNames=T,layout=2),  vertex.label.cex=0.1)

BipartitoSBM$memberships
BipartitoSBM$rMemberships
BipartitoSBM$nbDyads
BipartitoSBM$nbBlocks
BipartitoSBM$indMemberships
BipartitoSBM$nbBlocks
coef(BipartitoSBM, 'block')
coef(BipartitoSBM, 'connectivity')



colnames<-colnames(nm)
rownames<-rownames(nm)





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
#g

ggraph(g, layout = "kk") + 
	 geom_edge_link0(aes(width = n, colour = n), edge_alpha = 1, ends = "last", type = "closed") + 
	 scale_edge_colour_gradient(low = "#C2C2C2", high = "#333333") + 
   scale_edge_width(range = c(0.3, 1.8)) + 
   scale_edge_alpha(range = c(0.1, 3)) + 
	 geom_node_point(aes(fill = type, size = all_degree), colour = "#FFFFFF", shape = 21, stroke = 0.3) + 
	 scale_fill_brewer(palette = "Set1", na.value = "gray53") + 
   scale_size(range = c(2,10)) +
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
result <- map(names_filtered, ~count(.x, comuna, nombre))
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
#node_sizes <- result %>%
#  mutate(size = quantity/600) # Adjust the scaling factor as needed
#
## Plot the network graph with differentiated node sizes
#graph <- ggplot() +
#  geom_node_point(aes(x = comuna, y = nombre, size = size, color = size), data = node_sizes) +
#  #scale_fill_viridis_c(guide = "legend") +
#  scale_color_viridis(option = "G",   direction = 1) +
#  scale_size_continuous(range = c(0, 6)) +
#  labs(x = "Commune", y = "Name") +
#  #theme_minimal() +
#  guides(size = "legend", colour = "legend") +
#  theme(axis.ticks.y=element_blank(),
#        #legend.position = "top",
#        panel.grid.major.y=element_blank(),
#        plot.title = element_text(hjust = 0.5, size = 8),
#        axis.title = element_text(size=10),
#        axis.text.x = element_text(size = 7,angle = 90, hjust = 1),
#        axis.text.y = element_text(size = 7))
#
## Display the graph
#print(graph)




























