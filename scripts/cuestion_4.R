
#define el directorio una carpeta arriba del código, esto es exactamente el repositorio
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))) 


# cargando los paquetes
require("pacman")
pacman::p_load(rvest,tidyverse,stargazer)

################################################################################
################## Respondiendo la cuestión 4 ##################################
################################################################################

data = read.csv("stores/data_scrapped.csv")

# Renaming some variable and filtering for above 18

data <- data %>% filter(age >= 18) %>% mutate(female = 1 - sex) %>% mutate(age_sqr = age^2)

# Now creating data_reg for the regresions we will consider only the occupied ones

data_reg <- data %>% filter(ocu == 1)

# Now doing five regressions for part a

nominal_salary = lm(log(y_salary_m) ~ female, data = data_reg)

princ_occup = lm(log(y_salary_m_hu) ~ female, data = data_reg)

all_occup = lm(log(y_ingLab_m) ~ female, data = data_reg)

all_formal = lm(log(y_total_m) ~ female, data = data_reg)

all_formal_h = lm(log(y_total_m_ha) ~ female, data = data_reg)

stargazer(nominal_salary,princ_occup,all_occup,all_formal,all_formal_h, type = "text")

## Estimando usando Frisch Waugh-Lowell

data_FWL = data %>% select(y_ingLab_m, female, age, age_sqr) %>% drop_na()

reg_age = lm(data = data_reg, log(y_ingLab_m) ~ female + age + age_sqr)

resid_ing = lm(data = data_reg, log(y_ingLab_m) ~ age + age_sqr)$resid
resid_fem = lm(data = data_reg, female ~ age + age_sqr)$resid


reg_age_FWL <- lm(resid_ing ~ resid_fem)

stargazer(reg_age, type = "text")