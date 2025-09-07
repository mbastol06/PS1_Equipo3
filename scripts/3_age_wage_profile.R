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
  boot         # bootscraping
)

# Directorio de trabajo
setwd("C:/Users/mpaul/OneDrive - Universidad de los andes/repositorios/PS1_Equipo3")

# Cargue de los datos 
db <- read_csv("stores/base_final.csv")

############### PUNTO 3

# se crea la variable logaritmo del salario

db <- db%>% mutate(log_w = log(y_ing_lab_m_ha))

# la regresion -----------------------------------------------------------------

# estimamos 2 modelos para las diferentes mediadas del perfis salaria hora-edad

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


