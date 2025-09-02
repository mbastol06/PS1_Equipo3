#### Taller 1 - Big Data and Machine Learning

### Punto 2 - Web Scrapping

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  rvest,
  httr,
  fs,
  janitor,
  readr,
  haven,
  readxl,
  glue,
  chromote,
  xml2,
  tibble,
  purrr,
  VIM
)

## ============================================================
## 1) Parámetros

#Se carga la base original y se enumeran, del 1 al 10, las páginas subsiguientes para cada chunk

url_base   <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
paginas    <- tibble(chunk = 1:10,
                     url   = sprintf("%spage%d.html", url_base, 1:10))

dir_proc   <- "data/processed"
fs::dir_create(dir_proc, recurse = TRUE)

#Se dejan doce segundos de espera para cada una de las cargas de las páginas ya que tardan en cargar

espera_js  <- 12   
options(timeout = 180)
