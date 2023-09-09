#===================================#
#### BIG DATA Y MACHINE LEARNING #### 
#           PROBLEM SET #1          #
#===================================#
# Creation Date: 02/09/2023
# Mod Date: XX/09/2023
# R version 4.3.1 

# AUTHORS:
# 1. Federico Camacho Amar
# 2. Andrés David Chaparro Florez
# 3. Juan David Perez Velez
# 4. Ricardo Andrés Silva Torres
#===================================#


#==================================#
#### [1.] Paquetes y entorno ####  
#==================================#
# Limpieza del entorno de trabajo e instalación de paquetes
rm(list=ls())
install.packages("pacman")
library(pacman)

p_load(rvest, tidyverse, skimr, jsonlite)



#==================================#
#### [2.] Web Scraping ####  
#==================================#
browseURL("https://ignaciomsarmiento.github.io/GEIH2018_sample/", getOption("browser"))
# PAG WEB: https://ignaciomsarmiento.github.io/GEIH2018_sample/
# PAG de ayuda: https://www.projectpro.io/recipes/append-output-from-for-loop-dataframe-r

# Definición de dataframe vacio
df_final <- data.frame()

# For loop de web scraping
for (i in 1:10) {
  cat("-- Iteración = ", i, " --","\n")
  url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  
  chunk_html <- read_html(url)
  # Mismo xpath para cada uno de los data chunks de PAG WEB. 
  # Clave: geih_html en network.
  # Xpath = "/html/body/table"
  
  # Creando tabla por web scraping
  df <- chunk_html %>% 
          html_nodes(xpath =  "/html/body/table") %>% 
          html_table() %>% 
          as.data.frame()
  
  # Añadir df a dataframe final y remover df del loop
  df_final <- rbind(df_final, df)

  # Finalizada iteración del loop
  cat("-- Completada iteración --", "\n", "\n")
}

format(object.size(df_final), units = "MB")
write_csv(df_final, "/Users/ricardoandressilvatorres/Documents/GitHub/
                      BigData-MachineLearning202320/Taller 1/stores/df_final.csv")
