#### Taller 1 - Big Data and Machine Learning

## Limpieza de Base de Datos

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

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
  scales,
  ggplot2
)


#### ============================================================
###Punto 2 - Limpieza de Datos

## Se carga la tabla extraída por medio del Web Scrapping

ruta_csv <- "stores/tablas_page1_a_page10_combinado.csv"

combinado <- read_csv(ruta_csv)

## 1) Se filtran los individuos por edad mayor a 18 años y empleados (se excluyen los empleados por cuenta propia)

combinado <- combinado %>%
  filter(age >= 18)

combinado <- combinado %>%
  filter(!is.na(y_ing_lab_m) & y_ing_lab_m > 0)

combinado <- combinado %>%
  filter(dsi != 1)

combinado <- combinado %>%
  filter(cuenta_propia != 1)

## 2) Se filtran las variables ue no sirvan para el análisis, como missing values en todas las observaciones o valores iguales

combinado <- combinado %>%
  select(where(~ !all(is.na(.))))

combinado <- combinado %>%
  select(where(~ n_distinct(.) > 1))

combinado <- combinado %>%
  select(where(~ mean(is.na(.)) <= 0.6))

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

## 5) Se reemplazan los NAs de las variables numéricas con método KNN usando la función kNN específicamente del paquete VIM

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

## 7) Se hacen modificaciones finales y se eliminan variables innecesarias 

combinado$female <- ifelse(combinado$sex == 0, 1, 0)

combinado$tipo_ocup <- combinado$relab

combinado <- combinado %>%
  dplyr::select(
    directorio,
    sex,
    female,
    age,
    max_educ_level,
    total_hours_worked,
    estrato1,
    micro_empresa,
    formal,
    tipo_ocup,
    size_firm,
    oficio,
    college,
    ingtot,
    y_ing_lab_m,
    y_ing_lab_m_ha,
    y_salary_m,
    y_salary_m_hu,
    y_total_m,
    y_total_m_ha
  )


## 7) Se guarda la tabla para que se pueda cargar más adelante

write_csv(combinado, "stores/base_final.csv")
