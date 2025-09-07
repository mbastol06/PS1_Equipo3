#### ============================================================
###Punto 2 - Estadísticas Descriptivas

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

ruta_csv2 <- "stores/base_final.csv"

combinado <- read_csv(ruta_csv2)

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

ggsave(p1, dpi = 300, filename = "views/1_histo_sal.png" )

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

ggsave(p2, dpi = 300, filename = "views/1_histo_sal_hora.png" )