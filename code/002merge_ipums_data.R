
# library
library(ipumsr)
library(haven)
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)
library(naniar)
library(stringdist)
library(questionr)


# load ipums data
ipums <- read_dta("~/Documents/proyecto_nombres/ipumsi_00005.dta")

# merge 
## with `GEO2_CL` (1960:1970), `GEO2_CL1960`, `GEO2_CL1970`, `GEO2_CL1982`, `GEO2_CL1992`, `GEO2_CL2002`, `GEO2_CL2017`

# Create a file of equivalences for communes by decades
## 1- Create a tab by a decade of communes and codes.
## 2- Combine codes with name data.
## 3- Merge of sociodemographic variables by year 


# obtener labels (nombres) de los códigos comunales para el año 1970. 
freq(ipums$geo2_cl1970)
id_freq<-freq(ipums$geo2_cl1970)
id_freq <- tibble::rownames_to_column(id_freq, "id")

# create label key data. 
## 1960
id_comunas_1960 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1960", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1960)<-c("id","name")
id_comunas_1960$year<-1960

## 1970
id_comunas_1970 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1970", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1970)<-c("id","name")
id_comunas_1970$year<-1970

## 1982
id_comunas_1982 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1982", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1982)<-c("id","name")
id_comunas_1982$year<-1982

## 1992
id_comunas_1992 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1992", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1992)<-c("id","name")
id_comunas_1992$year<-1992

## 2002
id_comunas_2002 <- read_delim("~/Documents/diffusion_names/data/id_comunas_2002", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2002)<-c("id","name")
id_comunas_2002$year<-2002

## 2017
id_comunas_2017 <- read_delim("~/Documents/diffusion_names/data/id_comunas_2017", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2017)<-c("id","name")
id_comunas_2017$year<-2017

## join
id_comunas_ipums<-rbind(id_comunas_1960, id_comunas_1970, id_comunas_1982, id_comunas_1992,
                         id_comunas_2002, id_comunas_2017)
rm(id_comunas_1960, id_comunas_1970, id_comunas_1982, id_comunas_1992, id_comunas_1992, id_comunas_2002, id_comunas_2017)

# separate column character by coma. 
id_comunas_ipums<-id_comunas_ipums %>%            
  separate_rows(name, sep=",") %>% 
  mutate(across(where(is.character), str_trim))

# delete accent and `toupper`
id_comunas_ipums$name<-stri_trans_general(id_comunas_ipums$name,id = "Latin-ASCII")
id_comunas_ipums$name <- toupper(id_comunas_ipums$name)

# save
#save(id_comunas_ipums, file = "id_comunas_ipums.RData")


#La proyección de la red bipartita podrías hacerla para los nombres en lugar de las comunas. 
#Probablemente, la red va a ser muy densa y completamente conectada, por lo que un plot de la red no va a ser informativo. 
#Habría que mirar métodos de backbone extraction para extraer los ties más importantes de la red. 
#Yo aplicaría métodos de community detection solo cuando tengamos esta red menos densa. 
#En relación con esto, sería bueno tener una idea de cómo se distribuyen los nombres en el tiempo (por ej., por década), especialmente la cantidad de nombres nuevos que aparecen en el tiempo.



# merge variables socioeconómicas. 
## subset 1970 
ipums_1970<-ipums%>%filter(year==1970)
glimpse(ipums_1970)

## select variables
ipums_1970<-ipums_1970%>%
  filter(relate%in%c(1,2))%>%
  mutate(isei=case_when(occisco==1~68, 
                        occisco==2~65,
                        occisco==3~51,
                        occisco==4~41,
                        occisco==5~31,
                        occisco==6~18,
                        occisco==7~35,
                        occisco==8~32,
                        occisco==9~20,
                        occisco==10~53,
                        occisco==11~0,
                        occisco==97~0,
                        occisco==98~0,
                        occisco==99~0))%>%
  replace_with_na(replace = list(yrschool = c("90","91","92","93","94","95","96","98","99")))%>%
  select(serial
         ,id=geo2_cl1970
         #,hhwt
         #,gq
         #,pernum
         ,relate
         #,related
         #,edattain
         #,edattaind
         ,yrschool
         #,educcl
         ,isei
         )

# to wide and create means
ipums_1970<-ipums_1970 %>%
  pivot_wider(names_from = relate, values_from = c(yrschool,isei))%>%
  mutate(prom_school=(yrschool_1+yrschool_2)/2)%>%
  mutate(prom_isei=(isei_1+isei_2)/2) 
  
# imputar valor alto cuando una de las columnas tiene NA
for (i in 1:dim(ipums_1970)[1]) {
  if (is.na(ipums_1970$isei_1)[i] | is.na(ipums_1970$isei_2)[i]) {
    ipums_1970$prom_isei[i] <- pmax(ipums_1970$isei_1[i], ipums_1970$isei_2[i], na.rm = TRUE)
  }
  if (is.na(ipums_1970$yrschool_1)[i] | is.na(ipums_1970$yrschool_2)[i]) { 
    ipums_1970$prom_school[i] <- pmax(ipums_1970$yrschool_1[i], ipums_1970$yrschool_2[i], na.rm = TRUE)
  }
}

# plot
median_isei <- ipums_1970 %>% pull(prom_isei) %>% median() %>% signif(6)
ggplot(ipums_1970, aes(x = prom_isei, y = after_stat(density))) +
  geom_histogram(bins = 80) + 
  geom_density(color = "green", linewidth = .2, fill = 'green', alpha = 0.3) +
  #scale_x_log10()+
  geom_vline(xintercept=median_isei, size=.5, color="red") +
  labs(x="Prestigio ocupacional", title="IPUMS - Chile 1970")


median_school <- ipums_1970 %>% pull(prom_school) %>% median(na.rm=TRUE) %>% signif(6)
ggplot(ipums_1970, aes(x = prom_school, y = after_stat(density))) +
  geom_histogram(bins = 80) + 
  geom_density(color = "green", linewidth = .2, fill = 'green', alpha = 0.3) + 
  geom_vline(xintercept=median_school, size=.5, color="red") +
  labs(x="Años escolaridad", title="IPUMS - Chile 1970")


# unir códigos comunales con datos
id_comunas_1970<-id_comunas_ipums%>%filter(year==1970)
id_comunas_1970$id<-as.numeric(id_comunas_1970$id)

ipums_1970<-left_join(ipums_1970, id_comunas_1970, by="id", relationship = "many-to-many")%>%
  select(serial, id, name, year, yrschool_1, yrschool_2, isei_1, isei_2, prom_school, prom_isei)
# detalle: el "serial" se va a repetir tantas veces como nombres comunales tenga el id de comuna. 


# crear data socioeconómica por comuna (1970)
gse_comunas_1970<-ipums_1970 %>%
  group_by(name)%>%
  summarize(median(prom_isei),
            median(prom_school, na.rm=TRUE))

colnames(gse_comunas_1970)<-c("comuna", "prom_isei", "prom_school")
# gse_comunas_xxx se debe pegar a los nombres. 
# el proceso debe repetirse por década IPUMS. 


# Plot
gse_comunas_1970 %>% pivot_longer(cols=c('prom_isei', 'prom_school'),
                      names_to='gse',
                      values_to='value') %>% 
  ggplot(aes(x=value, fill = gse, colour = gse)) + 
  geom_density(alpha = 0.1) +
  labs(title= "SEG distribution 1970", x="") 


rm(names_sample)
# unir con datos de nombres. 
names_sample_1970_79<-left_join(names_sample_1970_79, gse_comunas_1970, by="comuna")

