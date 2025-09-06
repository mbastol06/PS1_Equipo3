
#define el directorio una carpeta arriba del código, esto es exactamente el repositorio
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))) 


# cargando los paquetes
require("pacman")
pacman::p_load(rvest,tidyverse,stargazer)

set.seed(10101)

################################################################################
################## Respondiendo la cuestión 4 ##################################
################################################################################

data = read.csv("stores/data_scrapped.csv")

# Renaming some variable and filtering for above 18

data <- data %>% filter(age >= 18) %>% mutate(female = 1 - sex) %>% mutate(age_sqr = age^2) %>%
  mutate(rural = 1 - clase)

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
          out = "views/simple_female_reg.tex")
#-------------------------------------------------------------------------------
## Estimando usando Frisch Waugh-Lowell
#-------------------------------------------------------------------------------
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

sd_hist <- ggplot(data = data.frame(sd = coefs_store), aes(x = sd)) + geom_histogram() +
  labs(x = "coef_value")

# buil the formula for Frisch-Waugh-Lowell

variables <- c("age", "age_sqr")
dependent <- "y_ingLab_m"
var_interes <- "female"

FWL <- function(variables,dependent,data,var_interes){
  data_reg <- data %>% select(variables,dependent,var_interes) %>% drop_na()
  formula1 = as.formula(paste(paste("log(",dependent, ") ~ "), paste(variables, collapse = " + ")))
  formula2 = as.formula(paste(paste(var_interes, " ~ "), paste(variables, collapse = " + ")))
  data_reg <- data_reg %>% mutate(resid_1 = lm(data = data_reg, formula1)$resid) %>%
                           mutate(resid_2 =lm(data = data_reg, formula2)$resid)
  
  reg_final <- lm(resid1 ~ resid1, data = data_reg)
}


write.csv(sd, "stores/sd_FWL.csv")
stargazer(reg_age, reg_age_FWL, type = "latex", out = "views/reg_FWL.tex")
ggsave(sd_hist, dpi = 300,filename = "views/sd_fwl_hist.png")
#-------------------------------------------------------------------------------
# Haciendo varias regresiones con los controles
#-------------------------------------------------------------------------------
reg_fem <- lm(data = data_reg, log(y_total_m_ha) ~ female)

reg_fem_age <- lm(data = data_reg, log(y_total_m_ha) ~ female + age + age_sqr)

reg_fem_age_educ <- lm(data = data_reg, log(y_total_m_ha) ~ female + age + age_sqr  + as.factor(maxEducLevel) ) + colleg

reg_fem_age_educ_hwork <- lm(data = data_reg, log(y_total_m_ha) ~ female + age + age_sqr  + as.factor(maxEducLevel) + totalHoursWorked)

reg_fem_age_educ_cwork <-lm(data = data_reg, log(y_total_m_ha) ~ female + age + age_sqr  + as.factor(maxEducLevel) + totalHoursWorked 
                             + microEmpresa + formal + as.factor(sizeFirm) + as.factor(oficio)) 
reg_list <- list(
  reg_fem,reg_fem_age,reg_fem_age_educ, reg_fem_age_educ_hwork,reg_fem_age_educ_cwork)

stargazer(reg_list, type = "latex", omit = c("sizeFirm","oficio"), out = "views/reg_controls.tex")


#-------------------------------------------------------------------------------
# Calculating the peak age for the models through bootstrat
data_chart <- data.frame(coef1 = 0,coef2 = 0, peak_age = 0,cf_0.05 = 0, cf_0.95 = 0)

for (reg in reg_list[-1]){
  
  B = 1000
  
  age_peaks <- rep(NA,B)
  
  for(i in 1:B){
    data_sample = sample_frac(data_reg,size=1,replace=TRUE)
    reg_sample <- lm(data = data_sample,formula(reg))
    age_peaks[i] <- -(reg_sample$coef[3]/(2*reg_sample$coef[4]))
    
  }
  data_coef <- data.frame(coef1 = reg$coef[3],coef2 = reg$coef[4],peak_age = -(reg$coef[3]/(2*reg$coef[4])), cf_0.05 = quantile(age_peaks,0.05),
                          cf_0.95 = quantile(age_peaks,0.95))
  data_chart <- rbind(data_chart,data_coef)
  
}

data_chart <- data_chart[-1,]
data_chart$model <- factor(c("No Controls","educ ctrl","educ/hours ctrl","educ/hours/work ctrl"), levels = c("No Controls","educ ctrl","educ/hours ctrl","educ/hours/work ctrl") )

# doing a bar chart

age_peak_bar = ggplot(data = data_chart, aes(y = peak_age, x = model)) + geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(NA,70))

# doing a whiskey point chart

age_peak_whiskey_point <- ggplot(data_chart, aes(x = model, y = peak_age)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = cf_0.05, ymax = cf_0.95), width = 0.2) +
  theme_minimal() +
  labs(x = "Model", y = "Peak Age", 
       title = "Peak Age with 95% CI across Models")

# doing a line chart for the quadratic age function

grid_age =18:75

age_quad_data  = data.frame(age=0,log_wage=0,model=0)

for (i in 1:4){
  age_quad_mod = data.frame(age = grid_age,
                            log_wage = grid_age*data_chart$coef1[i] + grid_age^2*data_chart$coef2[i],
                            model = data_chart$model[i])
  age_quad_data = rbind(age_quad_data,age_quad_mod)
}

age_quad_data  =age_quad_data[-1,]

age_quad_chart = ggplot(data = age_quad_data, aes(x = age, y = log_wage,colour = model)) + 
  geom_line()

# saving the outputs

stargazer(reg_list,omit = c("oficio","sizeFirm"), type = "latex", out = "stores/reg_pay_fem.tex")

ggsave(age_quad_chart, dpi = 300, filename ="views/age_quad_chart.png")

ggsave(age_peak_bar, dpi = 300, filename ="views/age_peak_bar.png")

ggsave(age_peak_whiskey_point, dpi = 300, filename ="views/age_whiskey_point.png")
