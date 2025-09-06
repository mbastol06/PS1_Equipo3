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

p_load(rio, # Import/export data.
       tidyverse, # Tidy-data.
       stargazer, # Descriptive statistics.
       gt, # Descriptive statistics.
       gtsummary,
       caret, # For predictive model assessment.
       gridExtra, # Arrange plots.
       skimr, # Summarize data. ,
       summarytools, # Summarize data. 
       ggplot2,
       DataExplorer
)

# Directorio de trabajo
setwd("C:/Users/mpaul/OneDrive - Universidad de los andes/repositorios/PS1_Equipo3")

# Cargue de los datos 
db <- read_csv("stores/tablas_page1_a_page10_combinado.csv")
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
