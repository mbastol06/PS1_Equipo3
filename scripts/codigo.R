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

espera_js  <- 25   
options(timeout = 180)

## 2) Se extrae la primera tabla de cada página (la única con base en el tiempo de espera definido)

extraer_primera_tabla <- function(b, url, espera_js = 25) {
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


#### ============================================================
###Punto 2 - Limpieza de Datos

## 1) Se filtran los individuos por edad mayor a 18 años y empleados (se excluyen los empleados por cuenta propia)

combinado <- combinado %>%
  dplyr::filter(age >= 18)

combinado <- combinado %>%
  dplyr::filter(!is.na(y_ing_lab_m) & y_ing_lab_m > 0)

combinado <- combinado %>%
  dplyr::filter(dsi != 1)

combinado <- combinado %>%
  dplyr::filter(cuenta_propia != 1)

## 2) Se filtran las variables ue no sirvan para el análisis, como missing values en todas las observaciones o valores iguales

combinado <- combinado %>%
  dplyr::select(where(~ !all(is.na(.))))

combinado <- combinado %>%
  dplyr::select(where(~ dplyr::n_distinct(.) > 1))

combinado <- combinado %>%
  dplyr::select(where(~ mean(is.na(.)) <= 0.6))

## 3) Se identifican las variables categóricas que están definidas como integers

cols_con_na <- names(combinado)[colSums(is.na(combinado)) > 0]
sapply(combinado[ , cols_con_na, drop = FALSE], class)

variables_categoricas <- c(
  "p6100", "p6585s2a2", "p7510s1", "p7510s2", "p7510s3",
  "p7510s5", "p7510s6", "p7510s7",
  "reg_salud"
)

variables_categoricas <- intersect(variables_categoricas, names(combinado))

## 4) Se reemplazan los NAs de las variables categóricas por la moda de los individuos que comparten estrato

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

combinado <- combinado %>%
  group_by(estrato1) %>%
  mutate(across(all_of(variables_categoricas),
                ~ ifelse(is.na(.), moda(.), .)
  )) %>%
  ungroup()

colSums(is.na(combinado[ , variables_categoricas]))

combinado <- combinado %>%
  mutate(across(where(is.integer), as.factor))

## 5) Se reemplazan los NAs de las variables numéricas con método KNN

variables_numericas <- c("p6510s1", "p6545s1", "p6580s1", "p6585s1a1", 
                         "p6585s2a1", "p6585s3a1", "p7070", "isa", 
                         "y_auxilio_transp_m", "y_prima_servicios_m")

combinado <- VIM::kNN(
  data    = combinado,
  variable = variables_numericas,
  k       = 5,
  imp_var = FALSE
)

## 6) Se eliminan las observaciones que estén por encima del percentil 97.5 para que no exista una distribución asimétrica

variables_ingresos <- names(combinado)[
  grepl("(^y_|ingtot|p6500)", names(combinado), ignore.case = TRUE)
]

for (v in variables_ingresos) {
  p975 <- quantile(combinado[[v]], probs = 0.975, na.rm = TRUE, names = FALSE)
  combinado[[v]] <- ifelse(combinado[[v]] > p975, p975, combinado[[v]])
}

message("Variables de ingreso tratadas: ", paste(variables_ingresos, collapse = ", "))


#### ============================================================
###Punto 2 - Estadísticas Descriptivas

## 1) Se definen los nombres de las variables de interés

combinado <- combinado %>% mutate(ln_salario = log(y_total_m_ha))

variables_descriptivas <- tibble::tribble(
  ~var,               ~label,
  "y_total_m",       "Salario mensual",
  "y_total_m_ha",    "Salario por hora",
  "ln_salario",       "ln(Salario)",
  "age",              "Edad",
  "sex",              "Sexo (1 = Mujer)",
  "micro_empresa",    "Trabaja en microempresa (1 = Sí)",
  "formal",           "Trabajo formal (1 = Sí)",
  "hours_work_usual", "Horas trabajadas",
  "p7040",       "Segundo trabajo (1 = Sí)"
) %>%
  dplyr::filter(var %in% names(combinado))

to_numeric <- function(x) {
  if (is.factor(x) || is.character(x)) suppressWarnings(as.numeric(as.character(x))) else as.numeric(x)
}

## 2) Se calculan las estadísticas descriptivas para cada variable

tabla_1 <- variables_descriptivas %>%
  mutate(
    x        = map(var, ~ to_numeric(combinado[[.x]])),
    N        = map_int(x, ~ sum(!is.na(.))),
    Media    = map_dbl(x, ~ mean(., na.rm = TRUE)),
    DesvEst  = map_dbl(x, ~ sd(.,   na.rm = TRUE)),
    Min      = map_dbl(x, ~ suppressWarnings(min(., na.rm = TRUE))),
    Max      = map_dbl(x, ~ suppressWarnings(max(., na.rm = TRUE)))
  ) %>%
  transmute(
    Variable = label, N, Media, `Desv. Est.` = DesvEst, Min, Max
  )

## 3) Se usan dos decimales 

tabla_1 <- tabla_1 %>%
  mutate(
    N           = comma(N, accuracy = 1),
    Media       = comma(Media, accuracy = 0.01),
    `Desv. Est.`= comma(`Desv. Est.`, accuracy = 0.01),
    Min         = comma(Min, accuracy = 0.01),
    Max         = comma(Max, accuracy = 0.01)
  )

## 4) Se usa GT para construir la tabla, como se presentó en clase 

gt(tabla_1) |>
  tab_header(title = md("**Tabla 1. Estadísticas descriptivas**")) |>
  cols_label(
    Variable = "Variable", N = "N",
    Media = "Media", `Desv. Est.` = "Desv. Est.", Min = "Min", Max = "Max"
  ) |>
  fmt_missing(everything(), missing_text = "NA")