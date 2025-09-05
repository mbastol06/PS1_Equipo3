
#define el directorio una carpeta arriba del código, esto es exactamente el repositorio
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))) 


# cargando los paquetes
require("pacman")
pacman::p_load(rvest,tidyverse,stargazer)

set.seed(1234)

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
stargazer(nominal_salary,princ_occup,all_occup,all_formal,all_formal_h, type = "latex",
          out = "store/simple_female_reg.tex")

## Estimando usando Frisch Waugh-Lowell

data_FWL = data %>% select(y_ingLab_m, female, age, age_sqr) %>% drop_na()

reg_age = lm(data = data_FWL, log(y_ingLab_m) ~ female + age + age_sqr)

data_FWL <- data_FWL %>% mutate(resid_ing = lm(data = data_FWL, log(y_ingLab_m) ~ age + age_sqr)$resid) %>%
mutate(resid_fem = lm(data = data_FWL, female ~ age + age_sqr)$resid)


reg_age_FWL <- lm(resid_ing ~ resid_fem, data = data_FWL)

stargazer(reg_age, reg_age_FWL, type = "text")

# Bootstrap with B = 5000

B = 5000

coefs_store = rep(NA,B)

for(i in 1:B){
  
  FWL_sample<- sample_frac(data_FWL,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  
  FWL_sample <- FWL_sample %>% mutate(resid_ing = lm(data = FWL_sample, log(y_ingLab_m) ~ age + age_sqr)$resid) %>%
    mutate(resid_fem = lm(data = FWL_sample, female ~ age + age_sqr)$resid)
  reg_age_FWL_sample <- lm(resid_ing ~ resid_fem, data = FWL_sample)
  
  coefs<-reg_age_FWL_sample$coefficients[2] # gets the coefficient of interest that coincides with the elasticity of demand
  
  coefs_store[i]<-coefs #saves it in the above vector
}

sd <- sd(coefs_store)
print(sd)

sd_hist <- ggplot(data = data.frame(sd = coefs_store), aes(x = sd)) + geom_histogram()


write.csv(sd, "stores/sd_FWL.csv")
stargazer(reg_age, reg_age_FWL, type = "latex", out = "stores/reg_FWL.tex")
ggsave(sd_hist, dpi = 300,filename = "stores/sd_fwl_hist.png")
#-------------------------------------------------------------------------------
