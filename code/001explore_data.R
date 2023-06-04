
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
names_sample <- names %>% group_by(ano,comuna) %>% sample_frac(size=.1)

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
# 2. Cuan comun son ciertos nombres, es decir, en que #proporcion de comunas aparecen los nombres por comuna? 
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
  xlim(1921, 2022) +
  stat_cor(method = "pearson", 
           label.x = 2000, label.y = 1100) 








