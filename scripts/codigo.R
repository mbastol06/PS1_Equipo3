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

## 4) Se corre un loop para cada una de las diez páginas en una sola sesión de Chrome, y se fuerza a cerrar

b <- chromote::ChromoteSession$new()
on.exit(b$close(), add = TRUE)

tablas_por_pagina <- vector("list", length = nrow(paginas))

for (i in seq_len(nrow(paginas))) {
  url_i <- paginas$url[i]
  chunk_i <- paginas$chunk[i]
  
  tb_i <- try(extraer_primera_tabla(b, url_i, espera_js = espera_js), silent = TRUE)
  
  if (inherits(tb_i, "try-error") || is.null(tb_i)) {
    message(sprintf("No se pudo extraer tabla de page%d.", chunk_i))
    next
  }
  
  tb_i <- tb_i |>
    dplyr::mutate(chunk = chunk_i, url = url_i) |>
    dplyr::relocate(chunk, url)
  
  ## 5) Se guardan como csv cada una de las tablas extraídas por medio del web scrapping
  
  ruta_csv <- fs::path(dir_proc, sprintf("tabla_page%d.csv", chunk_i))
  readr::write_csv(tb_i, ruta_csv)
  message("Guardado: ", ruta_csv, "  (", nrow(tb_i), " filas, ", ncol(tb_i), " cols)")
  
  tablas_por_pagina[[i]] <- tb_i
}

## 6) Se combinan todas las tablas extraídas y se guardan, a su vez, como df y csv

tablas_validas <- compact(tablas_por_pagina)

if (length(tablas_validas) > 0) {
  combinado <- dplyr::bind_rows(tablas_validas)
  
  ruta_csv_comb <- fs::path(dir_proc, "tablas_page1_a_page10_combinado.csv")
  ruta_rds_comb <- fs::path(dir_proc, "tablas_page1_a_page10_combinado.rds")
  readr::write_csv(combinado, ruta_csv_comb)
  saveRDS(combinado, ruta_rds_comb)
  
  message("Combinado guardado en:")
  message("   - ", ruta_csv_comb)
  message("   - ", ruta_rds_comb)
  
  resumen <- combinado |>
    dplyr::group_by(chunk) |>
    dplyr::summarise(
      filas = dplyr::n(),
      columnas = ncol(dplyr::cur_data()),
      .groups = "drop"
    )
  print(resumen)
} else {
  message("No se extrajo ninguna tabla de las 10 páginas.")
}
