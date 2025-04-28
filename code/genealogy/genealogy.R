# URL de la página web
url <- paste0("http://www.genealogiachilenaenred.cl/gcr/IndividualPage.aspx?ID=I", i)

# Leer el contenido HTML de la página web
page <- read_html(url)

# Extraer la información del individuo principal (EGO)
individual_id <- url %>%
  str_extract("(?<=ID=).*") %>%
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
children_node <- page %>% html_node("#MainContent_pnlChildrens")

# Check if the node exists and is not NA
if (!is.na(children_node)) {
  # Extract table data if the node exists
  children_table <- children_node %>% html_table(fill = TRUE)
  
  children_ids <- page %>%
    html_nodes("#MainContent_pnlChildrens tr td:nth-child(1) a") %>%
    html_attr("href") %>%
    str_extract("(?<=ID=).*")
  
  children_names <- as.character(children_table$Nombre)
  children_births <- as.character(children_table$`Fecha de Nacimiento`)
  children_deaths <- as.character(children_table$`Lugar de Nacimiento`)
} else {
  children_table <- NA
  children_ids <- character(0)
  children_names <- character(0)
  children_births <- character(0)
  children_deaths <- character(0)
}

# Crear un vector para la columna 'relationship'
relationship_vector <- c("ego")

if (length(father_id) > 0) {
  relationship_vector <- c(relationship_vector, rep("father", length(father_id)))
}

if (length(mother_id) > 0) {
  relationship_vector <- c(relationship_vector, rep("mother", length(mother_id)))
}

if (length(spouse_id) > 0) {
  relationship_vector <- c(relationship_vector, rep("spouse", length(spouse_id)))
}

if (length(children_ids) > 0) {
  relationship_vector <- c(relationship_vector, rep("child", length(children_ids)))
}

# Crear un data frame largo con todas las relaciones
data_families <- data.frame(
  from = c(individual_id, rep(NA, length(relationship_vector) - 1)),
  from_name = c(individual_name, rep(NA, length(relationship_vector) - 1)),
  from_birth = c(individual_birth, rep(NA, length(relationship_vector) - 1)),
  from_death = c(individual_death, rep(NA, length(relationship_vector) - 1)),
  to = c(NA, if (length(father_id) > 0) father_id, if (length(mother_id) > 0) mother_id, if (length(spouse_id) > 0) spouse_id, if (length(children_ids) > 0) children_ids),
  to_name = c(NA, if (length(father_name) > 0) father_name, if (length(mother_name) > 0) mother_name, if (length(spouse_name) > 0) spouse_name, if (length(children_names) > 0) children_names),
  to_birth = c(NA, if (length(father_birth) > 0) father_birth, if (length(mother_birth) > 0) mother_birth, if (length(spouse_birth) > 0) spouse_birth, if (length(children_births) > 0) children_births),
  to_death = c(NA, if (length(father_death) > 0) father_death, if (length(mother_death) > 0) mother_death, if (length(spouse_death) > 0) spouse_death, if (length(children_deaths) > 0) children_deaths),
  relationship = relationship_vector
)