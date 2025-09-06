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
db <- read_csv("stores/tablas_page1_a_page10_combinado.csv")
<<<<<<< Updated upstream
db <- as_tibble(db) %>% 
  dplyr::select(
    directorio,     # Llave de vivienda
    secuencia_p,    # Llave de Hogar
    orden,          # Llave individuo
    y_ing_lab_m_ha, # ingreso laboral por hora
    y_ing_lab_m,
    y_total_m,
    y_total_m_ha,
    total_hours_worked,
    sex,            # =1 male, =0 female
    age,
    p6050,          #parentesco jefe hogar
    max_educ_level,
    estrato1, 
    oficio,
    relab,          # tipo de ocupacion
    formal,
    informal,
    size_firm,
    cot_pension,
    cuenta_propia,  # =1 if self employed; =0 otherwise
    depto, 
    dominio,        
    clase,
  ) %>% # 1. Cabecera, 2. Resto (
  rename( rel_jefe = p6050) %>% 
  rename(gender =sex) %>% 
  rename(ocup_type = relab)
  mutate(ln_salarioh = log(y_total_m_ha))  # 23 variables



# Exploracion de datos ------------------------------------------------------

# Estructura de los datos
str(db)

## Creación de variables de interés ---------------------------------

# Dummy de jefe de hogar
db <- db %>% 
  mutate(bin_head = ifelse(test = rel_jefe ==1, yes = 1, no = 0)) %>% 
  mutate(bin_head_female = bin_head * (1-gender))
table(db$bin_head)
table(db$bin_head_female)

# Calculo de numero total de niños por hogar
# dummy 1 si la persona es menor o 0 si no
db <- db %>%
  mutate(bin_minor = ifelse(test = age <=6, yes = 1, no = 0))

# numero de menores por hogar
db <- db %>% 
  group_by(directorio, secuencia_p) %>% 
  mutate(num_minors = sum(bin_minor, na.rm = TRUE)) %>% 
  dplyr::select(-bin_minor) %>% 
  ungroup()

# Conservamos individuos que son mayores de edad
db <- db %>% filter(age>=18)  # 24.568 obs


# Análisis de los datos ---------------------------------------------------

# Vista básica de la estructura y resumen de la base de datos
skim_data <- skim(db)
View(skim_data) # visor de datos

# Estructura 
glimpse(db)
summary(db)

# Se identifica que se está analizando a las personas de Bogotá, cabecera. 
# Se elimina variables
db <- db %>% 
  dplyr::select(-depto, -dominio, -clase) # 23 var

# visualizar en navegador 
resumen <- dfSummary(db, style = "grid", plain.ascii = FALSE)
print(resumen, method = "browser") 
# se identifican varias variables con NA 

## Tratamiento de los NA -----------------------------------

# Identifiación de los NA por variable
skim_data <- as_tibble(skim(db))  
db_miss <- skim_data %>% dplyr::select(skim_variable, n_missing)
db_miss <- arrange(db_miss, n_missing)

# Visualización 

# % de NA por variable
db_w_na <- db %>% select_if(~ sum(is.na(.)) > 0) # Filtra solo variables con NA
plot_missing(db_w_na)

#Guardar a PDF
pdf("views/2plot_na.pdf", width = 8, height = 6)  
plot_missing(db_w_na)  
dev.off() 

# Limpieza de la base de datos -------------------------------------------------
# de acuerdo con los reusltados anteriores resolvemos por variable

## maxEducLevel #2  missing values

ggplot(db, aes(max_educ_level)) +
  geom_histogram(color = "#0099F8", fill = "#0099F8") +
  ggtitle("Histograma de máximo nivel educativo alcanzado") +
  theme_classic() +
  theme(plot.title = element_text(size = 15))

# Dado que es una variable categórica imputamos por la moda
moda_edu <- as.numeric(names(sort(table(db$max_educ_level), decreasing = TRUE)[1]))
db <- db  %>%
  mutate(max_educ_level = ifelse(is.na(max_educ_level) == TRUE, moda_edu , max_educ_level))
sum(is.na(db$max_educ_level))  #0

## ocup_type

# Se refiere al tipo de ocupación, segun el diccionario
# 1: Obrero o empleado de empresa particular"; 2:"Obrero o empleado del gobierno";
# 3: "Empleado doméstico"; 4: "Trabajador por cuenta propia"; 5:	"Patron o empleador";
# 6: "Trabajador familiar sin remuneracion"; 7:	"Trabajador sin remuneracin en empresas o negocios de otros hogares";
# 8: "Jornalero o peon"; 9: "Otro";

#Primero quitamos las cateogerías 6 y 7 dado que no reciben salario por su trabajo 
db <- db %>% 
  filter(!(ocup_type %in% c(6, 7)))     

ggplot(db, aes(ocup_type)) +
  geom_histogram(color = "#0099F8", fill = "#0099F8") +
  ggtitle("Histograma de tipo de ocupación") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5,7,9 ), labels = c("Empleado priv.", "Empleado púb","Empleado dom.",  "Cuenta prop.", "Empleador","Jornalero", "Otro"))+
  #scale_x_discrete(labels = c("Empleado priv.", "Empleado púb", "Cuenta prop.", "Empleador","Jornalero", "Otro"))+
  theme(plot.title = element_text(size = 15))


is.na(db$y_ing_lab_m_ha) %>% table()


## y_ing_lab_m_ha

# plot 
ggplot(db, aes(y_ing_lab_m_ha)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(db$y_ing_lab_m_ha, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(db$y_ing_lab_m_ha, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle("Salario total nominal mensual por hora") +
  theme_classic() +
  theme(plot.title = element_text(size = 15))

# # Número de casos con NA en ambas variables
sum(is.na(db$y_ing_lab_m_ha) & is.na(db$y_ing_lab_m))
table(
  is.na(db$y_ing_lab_m_ha),
  is.na(db$y_ing_lab_m)
) # na en las dos  14.228


# comola distribucion del ingreso tiene una cola larga a la derecha es adecuado usar la mediana
db <- db  %>%
  mutate(y_ing_lab_m_ha_im = ifelse(is.na(y_ing_lab_m_ha) == TRUE, median(db$y_ing_lab_m_ha, na.rm = TRUE) , y_ing_lab_m_ha))


db<- db %>%
  dplyr::select(where(~ !all(is.na(.))))

db <- db %>%
  dplyr::select(where(~ dplyr::n_distinct(.) > 1))

db <- db %>%
  dplyr::select(where(~ mean(is.na(.)) <= 0.6))


db3 <-  db%>% mutate(log_s2 = log(y_ing_lab_m_ha))


reg_p3 <- log_s2 ~ age + I(age^2)
modelo_p3_mean <- lm(reg_p3, data = db3)

#Generacion de la tabla 
stargazer(modelo_p3_mean, type = "text", title = "Logaritmo del salario en funcion de la edad")


modelo_p3_mean$coefficients










# Age-wage profile ------------------------------------------------------------


# ver si hay individuos con salario cero , no hay
db %>% 
  dplyr::filter(y_ing_lab_m_ha ==0) %>% 
  dplyr::select(directorio, secuencia_p, orden, y_ing_lab_m_ha, total_hours_worked, y_ing_lab_m) %>% 
  head()





des_vars <- c("totalHoursWorked", "num_minors", "y_ing_lab_m_ha", "bin_male",
              "bin_head", "bin_headFemale", "age")
stargazer(as.data.frame(db[,des_vars]), type = "text")

##########################################################
=======
>>>>>>> Stashed changes

#### ============================================================
###Punto 2 - Limpieza de Datos

## 1) Se filtran los individuos por edad mayor a 18 años y empleados (se excluyen los empleados por cuenta propia)

db <- db %>%
  dplyr::filter(age >= 18)

db <- db %>%
  dplyr::filter(!is.na(y_ing_lab_m) & y_ing_lab_m > 0)

db <- db %>%
  dplyr::filter(dsi != 1)

db <- db %>%
  dplyr::filter(cuenta_propia != 1)

## 2) Se filtran las variables ue no sirvan para el análisis, como missing values en todas las observaciones o valores iguales

db <- db %>%
  dplyr::select(where(~ !all(is.na(.))))

db <- db %>%
  dplyr::select(where(~ dplyr::n_distinct(.) > 1))

db <- db %>%
  dplyr::select(where(~ mean(is.na(.)) <= 0.6))

## 3) Se identifican las variables categóricas que están definidas como integers

cols_con_na <- names(db)[colSums(is.na(db)) > 0]
sapply(db[ , cols_con_na, drop = FALSE], class)

variables_categoricas <- c(
  "p6100", "p6585s2a2", "p7510s1", "p7510s2", "p7510s3",
  "p7510s5", "p7510s6", "p7510s7",
  "reg_salud"
)

variables_categoricas <- intersect(variables_categoricas, names(db))

## 4) Se reemplazan los NAs de las variables categóricas por la moda de los individuos que comparten estrato

moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

db <- db %>%
  group_by(estrato1) %>%
  mutate(across(all_of(variables_categoricas),
                ~ ifelse(is.na(.), moda(.), .)
  )) %>%
  ungroup()

colSums(is.na(db[ , variables_categoricas]))

db <- db %>%
  mutate(across(where(is.integer), as.factor))

## 5) Se reemplazan los NAs de las variables numéricas con método KNN

variables_numericas <- c("p6510s1", "p6545s1", "p6580s1", "p6585s1a1", 
                         "p6585s2a1", "p6585s3a1", "p7070", "isa", 
                         "y_auxilio_transp_m", "y_prima_servicios_m")

db <- VIM::kNN(
  data    = db,
  variable = variables_numericas,
  k       = 5,
  imp_var = FALSE
)

## 6) Se eliminan las observaciones que estén por encima del percentil 97.5 para que no exista una distribución asimétrica

variables_ingresos <- names(db)[
  grepl("(^y_|ingtot|p6500)", names(db), ignore.case = TRUE)
]

for (v in variables_ingresos) {
  p975 <- quantile(db[[v]], probs = 0.975, na.rm = TRUE, names = FALSE)
  db[[v]] <- ifelse(db[[v]] > p975, p975, db[[v]])
}

message("Variables de ingreso tratadas: ", paste(variables_ingresos, collapse = ", "))


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

set.seed(123)
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


