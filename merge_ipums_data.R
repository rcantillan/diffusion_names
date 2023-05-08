
# library
library(ipumsr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringi)
library(stringdist)
library(questionr)

# load ipums data
ipums <- read_csv("~/Desktop/ipumsi_00002.csv")

# merge 
## with `GEO2_CL` (1960:1970), `GEO2_CL1960`, `GEO2_CL1970`, `GEO2_CL1982`, `GEO2_CL1992`, `GEO2_CL2002`, `GEO2_CL2017`

# Create a file of equivalences for communes by decades
## 1- Create a tab by a decade of communes and codes.
## 2- Combine codes with name data.
## 3- Merge of sociodemographic variables by year 

id_freq<-freq(ipums$GEO2_CL1960)
id_freq <- tibble::rownames_to_column(id_freq, "id")

# create label key data. 
## 1960
id_comunas_1960 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1960", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1960)<-c("id","name")

## 1970
id_comunas_1970 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1970", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1970)<-c("id","name")

## 1982
id_comunas_1982 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1982", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1982)<-c("id","name")

## 1992
id_comunas_1992 <- read_delim("~/Documents/diffusion_names/data/id_comunas_1992", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_1992)<-c("id","name")

## 2002
id_comunas_2002 <- read_delim("~/Documents/diffusion_names/data/id_comunas_2002", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2002)<-c("id","name")

## 2017
id_comunas_2017 <- read_delim("~/Documents/diffusion_names/data/id_comunas_2017", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
colnames(id_comunas_2017)<-c("id","name")








