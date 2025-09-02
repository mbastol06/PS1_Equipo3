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
  VIM,
  gtsummary,
  gt,
  scales
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

## 2) Se extrae la primera tabla de cada página (la única con base en el tiempo de espera definido)

extraer_primera_tabla <- function(b, url, espera_js = 12) {
  b$Page$navigate(url)
  b$Page$loadEventFired(wait_ = TRUE)
  Sys.sleep(espera_js)
  
  
## 3) Se utiliza el paquete Chromote para extraer la información renderizada de una fuente tipo Java Script y leerla en HTML
  
  doc      <- b$DOM$getDocument()
  html_txt <- b$DOM$getOuterHTML(nodeId = doc$root$nodeId)[["outerHTML"]]
  html_rnd <- xml2::read_html(html_txt)

  nodos_tablas <- rvest::html_elements(html_rnd, "table")
  message("   · Tablas detectadas: ", length(nodos_tablas))
  if (length(nodos_tablas) == 0) return(NULL)

  tb <- rvest::html_table(nodos_tablas[[1]], fill = TRUE)

  tb <- tb |>
    tibble::as_tibble(.name_repair = "unique") |>
    janitor::clean_names()
  
  return(tb)
}
