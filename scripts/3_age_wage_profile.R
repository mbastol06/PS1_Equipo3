###########################################################################
#                        PROBLEM SET 1
#                            Equipo 3       
#  Autores:   Maria Paula Basto - Lucas Daniel Carrillo Aguirre 
#            Catalina Leal      -  Lucas Eduardo Vera Costa
#                     3. Age-wage profil                                   
############################################################################

rm(list = ls()) # Vacía environment

# Paquetes -----------------------------------------------------------------

library(pacman)

pacman::p_load(
  readr,       # leer archivos CSV
  dplyr,       # manipulación de datos 
  tibble,      # crear data frames en tibbles
  purr,        # aplicar funciones a listas o vectores
  scales,      # cambiar escalas
  gt,          # tablas
  stargazer,   # exportar tablas
  ggplot2,     # graficos
  boot,        # bootscraping, 
  xtable,      # tablas en latex
  broom,       # tddy
  purr,        # funciones con listas
  ggeffects,   # ggpredict()
  patchwork,   # gráficos lado a lado
)

# Directorio de trabajo
setwd("C:/Users/mpaul/OneDrive - Universidad de los andes/repositorios/PS1_Equipo3")

# Cargue de los datos 
db <- read_csv("stores/base_final.csv")

############### PUNTO 3

# se crea la variable logaritmo del salario

db <- db%>% mutate(log_w = log(y_ing_lab_m_ha))

# la regresion -----------------------------------------------------------------

# estimamos 3 modelos para las diferentes mediadas del perfis salaria hora-edad

real_ho_usual = lm(log(y_salary_m_hu) ~ age + I(age^2), data = db)

all_formal_h = lm(log(y_total_m_ha) ~ age + I(age^2), data = db)

laboral_hour = lm(log(y_ing_lab_m_ha) ~ age + I(age^2), data = db)

stargazer(real_ho_usual,all_formal_h,laboral_hour, type = "text")

# las variables  y_total_m_ha y y_ing_lab_m_ha parecen ser la misma, verificamos
db <- db %>% mutate(prueba = y_total_m_ha-y_ing_lab_m_ha) 
summary(db$prueba) #efectivamente son ma misma variable

#  limpiamos base
db <- db <- dplyr::select(db ,-prueba , y_ing_lab_m_ha)
stargazer(real_ho_usual,all_formal_h, type = "text", title = "Perfil de salario-edad",
          font.size = "tiny" )
stargazer(real_ho_usual,all_formal_h, type = "latex", title = "Perfil de salario-edad",
          font.size = "tiny" , out = "views/3_regresions")

stargazer(mod_p3, type = "text", title = "Logaritmo del salario en funcion de la edad")
stargazer(mod_p3, type = "latex", title = "Logaritmo del salario en funcion de la edad")

## semielasticidades del salario sobre el salario ----------------------------------------

edades <- c(18, 25, 35, 45, 50)

resultados <- data.frame(edad = edades, sem_elast_real_ho_usual = NA, sem_elast_all_formal_h = NA)
colnames(resultados) <- c("Edad", "Salario Real", "Salario Nominal")

# Calcular las semielasticidades para cada edad
for (i in 1:length(edades)) {
  edad <- edades[i]
  
  # Semielasticidad para el modelo real_ho_usual
  resultados$sem_elast_real_ho_usual[i] <- 100 * (real_ho_usual$coefficients[2] + 2 * real_ho_usual$coefficients[3] * edad)
  
  # Semielasticidad para el modelo all_formal_h
  resultados$sem_elast_all_formal_h[i] <- 100 * (all_formal_h$coefficients[2] + 2 * all_formal_h$coefficients[3] * edad)
}

# Ver los resultados

resultados <- round(resultados, 3)
head(resultados)
print(xtable(resultados, digits = 3), include.rownames = FALSE)


# Estadísticos de ajuste dentro de la muestra --------------------------------------------

modelos <- list("Salario real"= real_ho_usual,
                "Salario nominal" = all_formal_h)

resultados_modelos <- map_dfr(
  modelos,
  ~{
    g <- glance(.x)
    
    # Calcular RMSE manualmente (dividiendo por n)
    residuos <- residuals(.x)
    n <- length(residuos)
    rmse <- sqrt(mean(residuos^2))
    
    tibble(
      RSE = g$sigma,                     # Error estándar residual (por df = n - k)
      RMSE = rmse,                       # Calculado sobre n
      R2_Ajustado = g$adj.r.squared,
      F = g$statistic,
      AIC = AIC(.x),
      BIC = BIC(.x)
    )
  },
  .id = "Modelo"
)

# Redondear a tres decimales
resultados_modelos <- resultados_modelos %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# Exportar a LaTeX
xtable(resultados_modelos,
       caption = "Medidas de ajuste para los modelos de salario por edad",
       label = "tab:ajuste_modelos",
       digits = c(0, 0, rep(3, 6)))


# plot de datos predichos --------------------------------------------------------------

# Edad pico
edad_pico1 <- -coef(real_ho_usual)["age"] / (2 * coef(real_ho_usual)["I(age^2)"])
edad_pico2 <- -coef(all_formal_h)["age"] / (2 * coef(all_formal_h)["I(age^2)"])

# se ordean las edades de menos a mayor de la muestra y se quitan los duplicados
grid_age <- data.frame(age = seq(min(db$age, na.rm=TRUE),
                                 max(db$age, na.rm=TRUE),
                                 by = 1))

# funcion para estimar por modelo 1 y 2 los valores dados en el grid

pred_log <- function(mod, grid){
  pr <- predict(mod, newdata = grid, se.fit = TRUE)
  tibble(
    age = grid$age,
    fit = as.numeric(pr$fit),
    lwr = fit - 1.96 * as.numeric(pr$se.fit),
    upr = fit + 1.96 * as.numeric(pr$se.fit)
  )
}

pred1 <- pred_log(real_ho_usual, grid_age) %>% mutate(modelo = "Salario real")
pred2 <- pred_log(all_formal_h, grid_age) %>% mutate(modelo = "Salario nominal")
pred  <- bind_rows(pred1, pred2)



# Dataframe con edades pico

peak_age <- function(mod){
  b1  <- unname(coef(mod)["age"])
  b2  <- unname(coef(mod)["I(age^2)"])
  -b1 / (2*b2)
}

edad_pico1 <- peak_age(real_ho_usual)
edad_pico2 <- peak_age(all_formal_h)

y_pico1 <- as.numeric(predict(real_ho_usual, newdata = data.frame(age = edad_pico1)))
y_pico2 <- as.numeric(predict(all_formal_h, newdata = data.frame(age = edad_pico2)))


peaks <- tibble(
  modelo = c("Salario real", "Salario nominal"),
  xint   = c(edad_pico1,           edad_pico2),
  y      = c(y_pico1,              y_pico2),
  label  = c(
    paste0("Edad pico: ", sprintf("%.2f", edad_pico1)),
    paste0("Edad pico: ", sprintf("%.2f", edad_pico2))
  )
)

# el grafico final

pdf("views/3_fig_pred_ic.pdf", width = 10, height = 6)
ggplot(pred, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "darkblue", size = 1) +
  # líneas de pico: se dibujan UNA por panel usando 'data = peaks' y 'inherit.aes = FALSE'
  geom_vline(data = peaks, aes(xintercept = xint),
             inherit.aes = FALSE, linetype = "dashed",
             color = "darkorchid", linewidth = 1) +
  # etiqueta en la línea del pico, a la altura predicha del pico
  geom_text(data = peaks, aes(x = xint, y = y, label = label),
            inherit.aes = FALSE, color = "darkorchid",
            vjust = -0.6, hjust = -0.05, size = 3.5) +
  labs(title = "Perfil salario-edad con IC 95% y edad pico",
       x = "Edad", y = "Log(Salario por hora)") +
  facet_wrap(~ modelo, nrow = 1) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +   # <- negrilla en el título
  scale_y_continuous(labels = label_number(accuracy = 0.01))
dev.off() 

# Bootstrap edad pico -----------------------------------------------------------

# Funcion para estimar los coeficientes segun la muestra y la edad pico

f_edad_pico <- function(db, index){
  mod_1 <- lm(log(y_ing_lab_m_ha) ~ age + I(age^2), data = db , subset = index)
  mod_2 <- lm(log(y_salary_m_hu)  ~ age + I(age^2), data = db , subset = index)
  
  b2_hat1 <- coef(mod_1)[2] # coefciente age
  b3_hat1 <- coef(mod_1)[3] # coeficiente age^2b3
  
  b2_hat2 <- coef(mod_2)[2] # coefciente age
  b3_hat2 <- coef(mod_2)[3] # coeficiente age^2b3
  
  edad_max1 <- -b2_hat1/(2*b3_hat1) 
  edad_max2 <- -b2_hat2/(2*b3_hat2) 
  
  return(c(edad_max_nominal = edad_max1,
           edad_max_real    = edad_max2))
  
}

f_edad_pico(db, 1:nrow(db))

## El bootstrap -------------------------------------------------------------------
# para identificar intervalo de confianza de la edad máxima segun cambie la muestra

set.seed(10101)
boot_p3 <- boot(data = db, f_edad_pico, R = 1000) # no paramétrico
boot_p3

# CIs percentil para cada componente (1 = nominal, 2 = real)
ic_nom <- boot.ci(boot_p3, type = "perc", index = 1)$percent[4:5]
ic_real <- boot.ci(boot_p3, type = "perc", index = 2)$percent[4:5]

ic_nom
ic_real




# Plot edades máximas ----------------------------------------------------------

# Las edades pico de bootstrap por modelo
edad_max_boot_nom  <- boot_p3$t[, 1]
edad_max_boot_real <- boot_p3$t[, 2]



boot_long <- data.frame(
  edad   = c(edad_max_boot_nom, edad_max_boot_real),
  modelo = factor(rep(c("Nominal", "Real"),
                      each = nrow(boot_p3$t)))
)

peaks_df <- data.frame(
  modelo = c("Nominal", "Real"),
  mean   = c(mean(edad_max_boot_nom,  na.rm = TRUE),
             mean(edad_max_boot_real, na.rm = TRUE)),
  low    = c(ic_nom[1],  ic_real[1]),
  high   = c(ic_nom[2],  ic_real[2])
)



ggplot(boot_long, aes(x = edad)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "lightblue", color = "lightblue", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(data = peaks_df, aes(xintercept = mean),
             color = "darkorchid", linetype = "dashed", linewidth = 1) +
  geom_vline(data = peaks_df, aes(xintercept = low),
             color = "black", linetype = "longdash", linewidth = 0.5) +
  geom_vline(data = peaks_df, aes(xintercept = high),
             color = "black", linetype = "longdash", linewidth = 0.5) +
  labs(title = "Edad pico estima por bootstrap" ,x = "Edad pico estimada", y = "Densidad") +
  facet_wrap(~ modelo, nrow = 1) +
  theme_bw()



