#### Taller 1 - Big Data and Machine Learning

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

#### ============================================================
###Punto 2 - Estadísticas Descriptivas

## 1) Se definen los nombres de las variables de interés

combinado <- combinado %>% mutate(ln_salario = log(y_total_m_ha))

variables_descriptivas <- tribble(
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
  filter(var %in% names(combinado))

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


#### ============================================================
###Punto 2 - Histogramas

## 1) Se asegura que las variables a graficar sean numéricas, para eso se mutan

combinado_grafica <- combinado %>%
  mutate(
    salario_hora = as.numeric(y_total_m_ha),
    ln_salario   = log(salario_hora)
  ) %>%
  filter(is.finite(salario_hora), salario_hora > 0,
         is.finite(ln_salario))

## 2) Se definen las estadísticas para las líneas verticales

media_hora   <- mean(combinado_grafica$salario_hora, na.rm = TRUE)
mediana_hora <- median(combinado_grafica$salario_hora, na.rm = TRUE)

media_ln   <- mean(combinado_grafica$ln_salario, na.rm = TRUE)
mediana_ln <- median(combinado_grafica$ln_salario, na.rm = TRUE)

## 3) Se grafica usando ggplot, como se usó en clase, un histograma para Salario por Hora

p1 <- ggplot(combinado_grafica, aes(x = salario_hora)) +
  geom_histogram(binwidth = diff(range(combinado_grafica$salario_hora, na.rm = TRUE))/60,
                 fill = "steelblue", color = "black", alpha = 0.6) +
  geom_vline(xintercept = media_hora,   linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mediana_hora, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Salario por hora",
    x = "Salario por hora",
    y = "Conteo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p1

## 4) Se grafica usando ggplot, como se usó en clase, un histograma para Logaritmo de Salario

p2 <- ggplot(combinado_grafica, aes(x = ln_salario)) +
  geom_histogram(
    binwidth = diff(range(combinado_grafica$ln_salario, na.rm = TRUE)) / 40,
    fill = "steelblue", color = "black", alpha = 0.6
  ) +
  geom_vline(xintercept = media_ln,   linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = mediana_ln, linetype = "dashed", color = "blue", size = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Logaritmo natural del salario",
    x = "Logaritmo natural del salario",
    y = "Conteo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p2

