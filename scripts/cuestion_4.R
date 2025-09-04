
#define el directorio una carpeta arriba del código, esto es exactamente el repositorio
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))) 


# cargando los paquetes
require("pacman")
pacman::p_load(rvest,tidyverse,stargazer)

################################################################################
################## Respondiendo la cuestión 4 ##################################
################################################################################

data = read.csv("stores/data_scrapped.csv")