
# URL de la página web

url <- paste0("http://www.genealogiachilenaenred.cl/gcr/IndividualPage.aspx?Id=I",i)

# Leer el contenido HTML de la página web
page <- read_html(url)

# Extraer la información del individuo principal (EGO)
individual_id <- url %>%
  str_extract("(?<=Id=).*") %>%
  str_trim()

individual_name <- page %>%
  html_node("h7") %>%
  html_text() %>%
  str_trim()

individual_birth <- page %>%
  html_node("#MainContent_IndividualUC_lblBirth") %>%
  html_text() %>%
  str_trim()

individual_death <- page %>%
  html_node("#MainContent_IndividualUC_lblDeath") %>%
  html_text() %>%
  str_trim()

# Extraer la información del padre
father_id <- page %>%
  html_node("#MainContent_FatherUC_hlnkNames") %>%
  html_attr("href") %>%
  str_extract("(?<=Id=).*")

father_name <- page %>%
  html_node("#MainContent_FatherUC_hlnkNames") %>%
  html_text() %>%
  str_trim()

father_birth <- page %>%
  html_node("#MainContent_FatherUC_lblBirth") %>%
  html_text() %>%
  str_trim()

father_death <- page %>%
  html_node("#MainContent_FatherUC_lblDeath") %>%
  html_text() %>%
  str_trim()

# Extraer la información de la madre
mother_id <- page %>%
  html_node("#MainContent_MotherUC_hlnkNames") %>%
  html_attr("href") %>%
  str_extract("(?<=Id=).*")

mother_name <- page %>%
  html_node("#MainContent_MotherUC_hlnkNames") %>%
  html_text() %>%
  str_trim()

mother_birth <- page %>%
  html_node("#MainContent_MotherUC_lblBirth") %>%
  html_text() %>%
  str_trim()

mother_death <- page %>%
  html_node("#MainContent_MotherUC_lblDeath") %>%
  html_text() %>%
  str_trim()

# Extraer la información del cónyuge
spouse_id <- page %>%
  html_node("#MainContent_SpouseUC_hlnkNames") %>%
  html_attr("href") %>%
  str_extract("(?<=Id=).*")

spouse_name <- page %>%
  html_node("#MainContent_SpouseUC_hlnkNames") %>%
  html_text() %>%
  str_trim()

spouse_birth <- page %>%
  html_node("#MainContent_SpouseUC_lblBirth") %>%
  html_text() %>%
  str_trim()

spouse_death <- page %>%
  html_node("#MainContent_SpouseUC_lblDeath") %>%
  html_text() %>%
  str_trim()



# Extract the children node
# 
children_node <- page %>% html_node("#MainContent_pnlChildrens")

# Check if the node exists and is not NA
if (!is.na(children_node)) {
  
  # Extract table data if the node exists
  children_table <- children_node %>% html_table(fill = TRUE)
  
  children_ids <- page %>%
    html_nodes("#MainContent_pnlChildrens tr td:nth-child(1) a") %>%
    html_attr("href") %>%
    str_extract("(?<=ID=).*")
  
  # Obtener los nombres, fechas de nacimiento y defunción de los hijos
  children_names <- as.character(children_table$Nombre)
  children_births <- as.character(children_table$`Fecha de Nacimiento`)
  children_deaths <- as.character(children_table$`Lugar de Nacimiento`)
  
} else {
  # Assign NA if the node doesn't exist
  children_table <- NA
}

children_data <- cbind(id = children_ids, children_table)

data_families <- tibble(
  from = individual_id,
  from_name = individual_name,
  from_birth = individual_birth,
  from_death = individual_death,
  to = list(c(father_id, mother_id, spouse_id, children_ids)),
  to_name = list(c(father_name, mother_name, spouse_name, children_names)),
  to_birth = list(c(father_birth, mother_birth, spouse_birth, children_births)),
  to_death = list(c(father_death, mother_death, spouse_death, children_deaths)),
  relationship = list(c("father", "mother", "spouse", rep("child", length(children_ids))))
) %>%
  unnest(cols = c(to, to_name, to_birth, to_death, relationship))
