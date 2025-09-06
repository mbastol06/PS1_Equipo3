###########################################################################
#                        PROBLEM SET 1
#                            Equipo 3       
#  Autores:   Maria Paula Basto - Lucas Daniel Carrillo Aguirre 
#            Catalina Leal      -  Lucas Eduardo Vera Acosta
#                     3. Age-wage profil                                   
############################################################################

rm(list = ls()) # Vacía environment

# Paquetes -----------------------------------------------------------------

library(pacman)

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
  scales, boot
)

# Directorio de trabajo
setwd("C:/Users/mpaul/OneDrive - Universidad de los andes/repositorios/PS1_Equipo3")

# Cargue de los datos 
db <- read_csv("stores/base_final.csv")


#### ============================================================
###Punto 2 - Estadísticas Descriptivas

## 1) Se definen los nombres de las variables de interés

db <- db %>% mutate(ln_salario = log(y_total_m_ha))

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
  dplyr::filter(var %in% names(db))

to_numeric <- function(x) {
  if (is.factor(x) || is.character(x)) suppressWarnings(as.numeric(as.character(x))) else as.numeric(x)
}

## 2) Se calculan las estadísticas descriptivas para cada variable

tabla_1 <- variables_descriptivas %>%
  mutate(
    x        = map(var, ~ to_numeric(db[[.x]])),
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



############### PUNTO 3
db3 <- db%>% mutate(log_w = log(y_ing_lab_m_ha))

# la regresion
reg_p3 <- log_w ~ age + I(age^2)
mod_p3 <- lm(reg_p3, data = db3)

stargazer(mod_p3, type = "text", title = "Logaritmo del salario en funcion de la edad")
stargazer(mod_p3, type = "latex", title = "Logaritmo del salario en funcion de la edad")

# RMSE
rmse_mod3 <- sqrt(mean(residuals(mod_p3)^2))


# plot
log_w_hat <- predict(mod_p3, newdata = db3)

pdf("views/edad pico1.pdf", width = 8, height = 6)  
ggplot(db3, aes(age, log_w_hat)) +
  geom_point(aes(color = "Salario estimado"), alpha = 0.5) +
  geom_vline(aes(xintercept = 44.8, color = "Edad pico", linetype = "Edad pico"),
             linewidth = 1) +
  scale_color_manual(name = NULL,
                     values = c("Salario estimado" = "darkblue", "Edad pico" = "darkorchid")) +
  scale_linetype_manual(values = c("Edad pico" = "dashed"), guide = "none") +  # <- quita duplicado
  labs(x = "Edad", y = "log(Salario)") +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off() 



# Para cada modelo, el pico es -beta2/2beta3

# Funcion para estimar los coeficientes segun la muestra y la edad pico

f_edad_pico <- function(db3, index){
  mod_p3 <- lm(log_w ~ age + I(age^2), data = db3 , subset = index)
  
  b2_hat <- coef(mod_p3)[2] # coefciente age
  b3_hat <- coef(mod_p3)[3] # coeficiente age^2b3
  
  edad_max <- -b2_hat/(2*b3_hat) 
  
  return(edad_max)
  
}

f_edad_pico(db3, 1:nrow(db3))

## El bootstrap -------------------------------------------------------------------
# para identificar intervalo de confianza de la edad máxima segun cambie la muestra

set.seed(10101)
boot_p3 <- boot(data = db3, f_edad_pico, R = 1000) # no paramétrico
boot_p3


ic_p3 <- boot.ci(boot_p3, type = "perc")$percent[4:5] # (percentil 2.5 y 97.5 para un 95% de confianza).

ic_p3

edad_max_boot <- boot_p3$t # edades máximas estimadas por el boostrap


#Guardar a PDF
pdf("views/p3edad_max2.pdf", width = 8, height = 6)  
ggplot(data.frame(edad_max_boot), aes(x = edad_max_boot)) +
  geom_histogram(aes(y =after_stat(density)), bins = 30, fill = "lightblue", color = "lightblue", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +  # Agregar densidad
  geom_vline(aes(xintercept = mean(edad_max_boot)), color = "darkorchid", linetype = "dashed", linewidth = 1) +  # Media
  geom_vline(aes(xintercept = ic_p3[1]), color = "black", linetype = "longdash", linewidth = 0.5) +  # Límite inferior IC
  geom_vline(aes(xintercept = ic_p3[2]), color = "black", linetype = "longdash", linewidth = 0.5) +  # Límite superior IC
  labs(
    x = "Edad máxima estimada",
    y = "Densidad") +
  theme_bw()
dev.off() 


