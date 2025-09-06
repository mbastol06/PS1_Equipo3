
#define el directorio una carpeta arriba del código, esto es exactamente el repositorio
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))) 

require("pacman")
pacman::p_load(rvest,tidyverse)

################################################################################
#Extrayendo los datos###########################################################
################################################################################

# URL base
url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/"

# Generamos las URLs de las páginas (del 1 al 10 por ejemplo)
paginas <- paste0(url_base, "geih_page_", 1:10, ".html")

extraer_tabla <- function(url_tabla){
  pagina <- read_html(url_tabla)
  nodo_tabla <- pagina %>% 
    html_node("table")
  tabla <- nodo_tabla %>% 
    html_table(fill = TRUE)
  
  names(tabla) <- if_else(
    is.na(names(tabla)) | names(tabla) == "",
    paste0("col_", seq_along(tabla)),
    names(tabla)
  )
  
  tabla %>% mutate(origen = url_tabla)
}
data_final <- map_dfr(paginas, extraer_tabla)

# guardando los datos en csv

write.csv(data_final,"stores/data_scrapped.csv")
