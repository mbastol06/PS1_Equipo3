rm(list = ls())
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  rvest,
  httr,
  fs,
  janitor,
  stargazer,
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
  caret,
  gridExtra, 
  skim
)

db <- readRDS("C:/Users/catal/Documents/PEG-1/Big Data/Taller 1/PS1_Equipo3/stores/combinado.rds")

#Ajuste de variables relevantes
db <- db %>% mutate(female = 1 - sex)

install.packages("parallelly")
install.packages("future")
install.packages("future.apply")
install.packages("recipes")
library(caret)
set.seed(10101) 


#Dividimos la muestra en 70/30
inTrain <- createDataPartition(
  y = db$y_salary_m_hu,  
  p = .70, 
  list = FALSE
)

training <- db |> filter(row_number() %in% inTrain)
testing  <- db |> filter(!(row_number() %in% inTrain))

#Definimos los 8 modelos y sacamos el RMSE

#1. modelo del punto 3
form_1   <- ln(y_salary_m_hu) ~ age  + age^2
modelo1a <- lm(form_1,
               data = training)

predictions <- predict(object = modelo1a, newdata = testing)
score1a<- RMSE(pred = predictions, obs = testing$y_salary_m_hu )
score1a

#2. primer modelo del punto 4
form_2   <- ln(y_salary_m_hu) ~ female
modelo2a <- lm(form_2,
               data = training)

predictions <- predict(object = modelo2a, newdata = testing)
score2a <- RMSE(pred = predictions, obs = testing$y_salary_m_hu )
score2a

#3. segundo modelo del punto 4
form_3   <- ln(y_salary_m_hu) ~ female + age + age_sqr  + as.factor(max_educ_level) + totalHoursWorked 
+ micro_empresa + formal + as.factor(size_firm) + as.factor(oficio)) 
modelo3a <- lm(form_3,  
               data = training)

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, testing$y_salary_m_hu )
score3a

#4. modelo con polinomio 3 en edad e interacción con el resto de regresores
form_4   <- ln(y_salary_m_hu) ~ female + poly(age,3,raw=True) + as.factor(max_educ_level)
  + total_hours_worked + micro_empresa + formal + as.factor(size_firm) + as.factor(oficio))
  + poly(age,3,raw=True):female + poly(age,3,raw=True):as.factor(max_educ_level)
  +poly(age,3,raw=True):total_hours_worked + poly(age,3,raw=True):micro_empresa
  +poly(age,3,raw=True):formal + poly(age,3,raw=True):as.factor(size_firm)
  +poly(age,3,raw=True):as.factor(oficio)
modelo4a <- lm(form_4,
               data = training)

predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$y_salary_m_hu )
score4a

#5. modelo con polinomio 5 en edad e interacción con el resto de regresores
form_5   <- ln(y_salary_m_hu) ~ female + poly(age,5,raw=True) + as.factor(max_educ_level)
+ total_hours_worked + micro_empresa + formal + as.factor(size_firm) + as.factor(oficio))
+ poly(age,5,raw=True):female + poly(age,5,raw=True):as.factor(max_educ_level)
+poly(age,5,raw=True):total_hours_worked + poly(age,5,raw=True):micro_empresa
+poly(age,5,raw=True):formal + poly(age,5,raw=True):as.factor(size_firm)
+poly(age,5,raw=True):as.factor(oficio)
modelo5a <- lm(form_5,
               data = training)

predictions <- predict(modelo5a, testing)
score5a<- RMSE(predictions, testing$y_salary_m_hu )
score5a

#6. modelo con 2 variables independientes adicionales (clase y college)
form_6   <- ln(y_salary_m_hu) ~ female + age + age_sqr  + as.factor(max_educ_level) + totalHoursWorked 
+ micro_empresa + formal + as.factor(size_firm) + as.factor(oficio)) + clase + college
modelo6a <- lm(form_6,  
               data = training)

predictions <- predict(modelo6a, testing)
score6a<- RMSE(predictions, testing$y_salary_m_hu )
score6a

#7. modelo con variables continua como polinomios de grado 5 
form_7   <- ln(y_salary_m_hu) ~ female + poly(age,5,raw=TRUE) + as.factor(max_educ_level) 
+ micro_empresa + formal + as.factor(size_firm) + as.factor(oficio)) + clase + college
+ poly(total_hours_worked,5,raw=TRUE)
modelo7a <- lm(form_7,  
               data = training)

predictions <- predict(modelo7a, testing)
score7a<- RMSE(predictions, testing$y_salary_m_hu )
score7a

#8. modelo 7 con interacciones
form_8 <- ln(y_salary_m_hu) ~ poly(age,5,raw=TRUE)*female 
  + poly(age,5,raw=TRUE)*as.factor(max_educ_level) + poly(age,5,raw=TRUE)*micro_empresa
  +poly(age,5,raw=TRUE)*formal + poly(age,5,raw=TRUE)*as.factor(size_firm)
  +poly(age,5,raw=TRUE)*as.factor(oficio) + poly(age,5,raw=TRUE)*clase
  +poly(age,5,raw=TRUE)*college + poly(total_hours_worked,5,raw=TRUE)* female
  +poly(total_hours_worked,5,raw=TRUE)*as.factor(max_educ_level)
  +poly(total_hours_worked,5,raw=TRUE)*micro_empresa
  +poly(total_hours_worked,5,raw=TRUE)*formal
  +poly(total_hours_worked,5,raw=TRUE)*as.factor(size_firm)
  +poly(total_hours_worked,5,raw=TRUE)*as.factor(oficio)
  +poly(total_hours_worked,5,raw=TRUE)*clase + poly(total_hours_worked,5,raw=TRUE)*college

predictions <- predict(modelo8a, testing)
score8a<- RMSE(predictions, testing$y_salary_m_hu )
score8a