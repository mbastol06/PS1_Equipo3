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
  skim,
  recipes,
  dplyr
)

db <- readRDS("C:/Users/catal/Documents/PEG-1/Big Data/Taller 1/PS1_Equipo3/stores/base_final.csv")

#Ajuste de variables relevantes
#db <- db %>% mutate(female = 1 - sex)
categoricas <- c("oficio", "size_firm", "max_educ_level")
db[categoricas] <- lapply(db[categoricas], factor)

set.seed(10101) 


#Dividimos la muestra en 70/30
inTrain <- createDataPartition(
  y = db$y_ing_lab_m_ha,  
  p = .70, 
  list = FALSE
)

training <- db |> filter(row_number() %in% inTrain)
testing  <- db |> filter(!(row_number() %in% inTrain))

#Definimos los 8 modelos y sacamos el RMSE

#1. modelo del punto 3
form_1   <- log(y_ing_lab_m_ha) ~ age  + I(age^2)
modelo1a <- lm(form_1,
               data = training)

predictions <- predict(object = modelo1a, newdata = testing)
score1a<- RMSE(pred = predictions, obs = log(testing$y_ing_lab_m_ha) )
score1a

#2. primer modelo del punto 4
form_2   <- log(y_ing_lab_m_ha) ~ female
modelo2a <- lm(form_2,
               data = training)

predictions <- predict(object = modelo2a, newdata = testing)
score2a <- RMSE(pred = predictions, obs = testing$y_ing_lab_m_ha )
score2a

#3. segundo modelo del punto 4

#para segurar que en ambos grupos estén los mismos factores en
#variables categóricas
rec <- recipe(y_ing_lab_m_ha ~ ., data = training) %>%
  step_other(oficio, size_firm, max_educ_level, threshold = 0.05)

rec_prep <- prep(rec, training = training)
training <- bake(rec_prep, new_data = training)
testing <- bake(rec_prep, new_data = testing)

form_3 <- log(y_ing_lab_m_ha) ~ female + age + I(age^2) + 
  max_educ_level + total_hours_worked + 
  micro_empresa + formal + 
  size_firm + oficio
modelo3a <- lm(form_3,  
               data = training)

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, testing$y_ing_lab_m_ha )
score3a

#4. modelo con polinomio 3 en edad e interacción con el resto de regresores
form_4 <- log(y_ing_lab_m_ha) ~ 
  female + poly(age, 3, raw = TRUE) + max_educ_level +
  total_hours_worked + micro_empresa + formal + size_firm + oficio +
  poly(age, 3, raw = TRUE):female +
  poly(age, 3, raw = TRUE):max_educ_level +
  poly(age, 3, raw = TRUE):total_hours_worked +
  poly(age, 3, raw = TRUE):micro_empresa +
  poly(age, 3, raw = TRUE):formal +
  poly(age, 3, raw = TRUE):size_firm +
  poly(age, 3, raw = TRUE):oficio
modelo4a <- lm(form_4,
               data = training)

predictions <- predict(modelo4a, testing)
score4a<- RMSE(predictions, testing$y_ing_lab_m_ha )
score4a

#5. modelo con polinomio 5 en edad e interacción con el resto de regresores
form_5 <- log(y_ing_lab_m_ha) ~ 
  female + poly(age, 5, raw = TRUE) + max_educ_level +
  total_hours_worked + micro_empresa + formal + size_firm + oficio +
  poly(age, 5, raw = TRUE):female +
  poly(age, 5, raw = TRUE):max_educ_level +
  poly(age, 5, raw = TRUE):total_hours_worked +
  poly(age, 5, raw = TRUE):micro_empresa +
  poly(age, 5, raw = TRUE):formal +
  poly(age, 5, raw = TRUE):size_firm +
  poly(age, 5, raw = TRUE):oficio
modelo5a <- lm(form_5,
               data = training)

predictions <- predict(modelo5a, testing)
score5a<- RMSE(predictions, testing$y_ing_lab_m_ha )
score5a

#6. modelo con 1 variables independientes adicional (college)
form_6 <- log(y_ing_lab_m_ha) ~ 
  female + age + I(age^2) + max_educ_level +
  total_hours_worked + micro_empresa + formal + size_firm + oficio + college
modelo6a <- lm(form_6,  
               data = training)

predictions <- predict(modelo6a, testing)
score6a<- RMSE(predictions, testing$y_ing_lab_m_ha )
score6a

#7. modelo con variables continua como polinomios de grado 5 
form_7 <- log(y_ing_lab_m_ha) ~ 
  female + poly(age, 5, raw = TRUE) + max_educ_level +
  micro_empresa + formal + size_firm + oficio + college +
  poly(total_hours_worked, 5, raw = TRUE)
modelo7a <- lm(form_7,  
               data = training)

predictions <- predict(modelo7a, testing)
score7a<- RMSE(predictions, testing$y_ing_lab_m_ha )
score7a

#8. modelo 7 con interacciones
form_8 <- log(y_ing_lab_m_ha) ~ 
  poly(age, 3, raw = TRUE) * female +
  poly(age, 3, raw = TRUE) * max_educ_level +
  poly(age, 3, raw = TRUE) * micro_empresa +
  poly(age, 3, raw = TRUE) * formal +
  poly(age, 3, raw = TRUE) * size_firm +
  poly(age, 3, raw = TRUE) * oficio +
  poly(age, 3, raw = TRUE) * college +
  poly(total_hours_worked, 3, raw = TRUE) * female +
  poly(total_hours_worked, 3, raw = TRUE) * max_educ_level +
  poly(total_hours_worked, 3, raw = TRUE) * micro_empresa +
  poly(total_hours_worked, 3, raw = TRUE) * formal +
  poly(total_hours_worked, 3, raw = TRUE) * size_firm +
  poly(total_hours_worked, 3, raw = TRUE) * oficio +
  poly(total_hours_worked, 3, raw = TRUE) * college

modelo8a <- lm(form_8, data = training)

predictions <- predict(modelo8a, testing)
score8a<- RMSE(predictions, testing$y_ing_lab_m_ha )
score8a

#2 mejores con LOOCV
## RUN THE MODEL WITH ALL OBS

full_model <- lm(form_1,
                 data = db )

X<- model.matrix(full_model)
y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients

## Calculate the inverse of  (X'X), call it G_inv
G_inv<- solve(t(X)%*%X)

## and 1/1-hi
vec<- 1/(1-hatvalues(full_model))

N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation
for (i in 1:N) {
  # get the new beta
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  ## get the new error
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  LOO[i]<-  new_error
}

looCV_error <- mean(LOO)
sqrt(looCV_error)

#INTENTO CHATG
# Asumo que ya tienes: full_model <- lm(form_1, data = db)

fit <- full_model

# Extrae X e y CONSISTENTES con el modelo (maneja na.omit, factores, etc.)
mf <- model.frame(fit)
X  <- model.matrix(fit)
y  <- model.response(mf)

# Coefs y diagnósticos
beta_hat <- coef(fit)
e <- resid(fit)
h <- hatvalues(fit)

# (X'X)^{-1} estable (mejor que solve(t(X)%*%X))
qrX   <- qr(X)
G_inv <- chol2inv(qr.R(qrX))

N <- nrow(X)
LOO <- numeric(N)

for (i in 1:N) {
  x_i   <- drop(X[i, ])                # vector columna "plano"
  scale <- e[i] / (1 - h[i])           # escalar e_i / (1 - h_ii)
  b_new <- beta_hat - (G_inv %*% x_i) * scale
  yhat_new_i <- drop(x_i %*% b_new)    # predicción de i con el modelo -i
  LOO[i] <- (y[i] - yhat_new_i)^2
}

rmse_loop    <- sqrt(mean(LOO))
rmse_formula <- sqrt(mean( (e/(1 - h))^2 ))

c(loop = rmse_loop, formula = rmse_formula, diff = rmse_loop - rmse_formula)

#idea 2
rmse_loocv <- sqrt(mean( (resid(full_model) / (1 - hatvalues(full_model)))^2 ))
rmse_loocv

#IDEA CON NIVELES
fit <- full_model

mf   <- model.frame(fit)
ylog <- model.response(mf)          # y en log (lo que usa el modelo)
yhat <- fitted(fit)                 # predicciones en log in-sample
e    <- resid(fit)
h    <- hatvalues(fit)

# Predicción LOO en log (vectorizado)
yhat_loo_log <- yhat + e/(1 - h)

# Volver a niveles
y_level        <- exp(ylog)         # y original en niveles
yhat_loo_level <- exp(yhat_loo_log)

rmse_loo_level <- sqrt(mean( (y_level - yhat_loo_level)^2 ))
rmse_loo_level

