#PREGUNTA 5#
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
  dplyr,
  forcats
)

  db <- read_csv("C:/Users/catal/Documents/PEG-1/Big Data/Taller 1/PS1_Equipo3/stores/base_final.csv")


#Ajuste de variables relevantes
  
  #pasar categóricas a factor
  categoricas <- c("oficio", "size_firm", "max_educ_level")
  db[categoricas] <- lapply(db[categoricas], factor)
  
  #crear variable para el logaritmo del salario
  db$ln_w <- log(db$y_ing_lab_m_ha)
  
  #Para oficio, varias categorías corresponden a una única observación en db
  #Para solucionar esto, se agruparan como "otros" las categorías con frecuencia
  #menor a 20. Con esto se espera disminuir el número de dummies con muy pocos
  #valores=1, así como que algunas categorías salgan en el testing set pero no en
  #el training set
  db <- db %>%
    mutate(oficio = fct_lump_min(factor(oficio), min = 20, other_level = "OTROS") |> fct_drop())
  
set.seed(10101) 


#Dividimos la muestra en 70/30
inTrain <- createDataPartition(
  y = db$ln_w,  
  p = .70, 
  list = FALSE
)

training <- db |> filter(row_number() %in% inTrain)
testing  <- db |> filter(!(row_number() %in% inTrain))

#Definimos los 8 modelos y sacamos el RMSE

#1. modelo del punto 3
form_1   <- ln_w ~ age  + I(age^2)
modelo1a <- lm(form_1,
               data = training)

predictions <- predict(object = modelo1a, newdata = testing)
score1a<- RMSE(pred = predictions, obs = testing$ln_w )
score1a

#2. primer modelo del punto 4
form_2   <- ln_w ~ female
modelo2a <- lm(form_2,
               data = training)

predictions <- predict(object = modelo2a, newdata = testing)
score2a <- RMSE(pred = predictions, obs = testing$ln_w )
score2a

#3. segundo modelo del punto 4

form_3 <- ln_w ~ female + age + I(age^2) + 
  max_educ_level + total_hours_worked + 
  micro_empresa + formal + 
  size_firm + oficio
modelo3a <- lm(form_3,  
               data = training)

predictions <- predict(modelo3a, testing)
score3a<- RMSE(predictions, testing$ln_w )
score3a

#4. modelo con polinomio 3 en edad e interacción con el resto de regresores
form_4 <- ln_w ~ 
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
score4a<- RMSE(predictions, testing$ln_w )
score4a

#5. modelo con polinomio 5 en edad e interacción con el resto de regresores
form_5 <- ln_w ~ 
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
score5a<- RMSE(predictions, testing$ln_w )
score5a

#6. modelo con 1 variables independientes adicional (college)
form_6 <- ln_w ~ 
  female + age + I(age^2) + max_educ_level +
  total_hours_worked + micro_empresa + formal + size_firm + oficio + college
modelo6a <- lm(form_6,  
               data = training)

predictions <- predict(modelo6a, testing)
score6a<- RMSE(predictions, testing$ln_w )
score6a

#7. modelo con variables continua como polinomios de grado 5 
form_7 <- ln_w ~ 
  female + poly(age, 5, raw = TRUE) + max_educ_level +
  micro_empresa + formal + size_firm + oficio + college +
  poly(total_hours_worked, 5, raw = TRUE)
modelo7a <- lm(form_7,  
               data = training)

predictions <- predict(modelo7a, testing)
score7a<- RMSE(predictions, testing$ln_w )
score7a

#8. modelo 7 con interacciones
form_8 <- ln_w ~ 
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
score8a<- RMSE(predictions, testing$ln_w )
score8a

#Tabla de resultados
scores<- data.frame( Modelo_1= c(score1a),
                     Modelo_2= c(score2a),
                     Modelo_3= c(score3a),
                     Modelo_4= c(score4a),
                     Modelo_5= c(score5a),
                     Modelo_6= c(score6a),
                     Modelo_7= c(score7a),
                     Modelo_8= c(score8a)
)

head(scores)

#Observaciones inusuales en el mejor modelo en terminos de RMSE
best_fit <- modelo7a       
dtest    <- testing  

prediction <- predict(best_fit, newdata = dtest)  
observed  <- dtest$ln_w                          
error  <- observed - prediction 

summary(error)

p_lo <- 0.01; p_hi <- 0.99
qs   <- quantile(error, c(p_lo, p_hi), na.rm = TRUE)
tails_idx <- which(error <= qs[1] | error >= qs[2])

n_tail <- length(tails_idx)  #60 observaciones en las colas
p_tail <- n_tail / nrow(dtest) #2% de la muestra

ggplot(data.frame(error = error), aes(x = error)) +
  geom_histogram(bins = 30, color = "black", fill = "steelblue", alpha = 0.6) +
  labs(x = "Error en la muestra de prueba", y = "Conteo") +
  theme_gray()

#Revisar si se debe a agrupar algunos oficios como otros
in_otros <- if ("oficio" %in% names(dtest)) dtest$oficio == "OTROS" else rep(NA, nrow(dtest))
tails_in_otros <- table(in_otros[tails_idx], useNA = "ifany")
#-----solo 3 de las 60 corresponden a "otros"

#Leverege
best_fit_test <- lm(form_7,  
                   data = testing)
testing_h <- testing %>% mutate(leverage = hatvalues(best_fit_test))

#mean leverage
p <- mean(testing_h$leverage)
p
cutt <- 3*p
cutt

testing_h_2 <- testing_h %>% 
  dplyr::filter(leverage <= cutt)    #elimina 132 observaciones

influential <- testing_h %>% 
  filter(leverage > cutt) %>%
  dplyr::select(directorio, age, total_hours_worked, max_educ_level, female,  micro_empresa, formal, size_firm,
                college)

##Descriptivas para obs. en las colas

des_vars <- c("age", "total_hours_worked", "max_educ_level", "female",  "micro_empresa",
              "formal", "size_firm", "college")

testing_h_tails |> 
  select(all_of(des_vars)) |>
  tbl_summary( statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n}/{N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  label = list(total_hours_worked = "Horas trabajadas",
               age = "Age",
               max_educ_level = "Educación",
               female = "Mujer",
               micro_empresa = "Micro empresa",
               formal = "Formal",
               size_firm = "Tamaño firma",
               college = "Educación Terciaria"),
  missing_text = "(Missing)") |> 
  modify_header(label ~ "**Variable**") |>
  modify_footnote_header("For continuos variables, mean (sd). For categorical, frequency (percentage).",
                         columns = all_stat_cols()) |>
  bold_labels() |> 
  italicize_levels() |> 
  modify_caption("Descriptivas — Observaciones en las colas")

 
#2 mejores con LOOCV (modelos 4 y 7)
#Como estamos en OLS se tiene que MSE_LOOCV es la media de (residuales_i/1-h_i)^2

full_model_1 <- lm(form_4,
                 data = db )
m <- full_model_1

r <- resid(m)
h <- hatvalues(m)

eps <- .Machine$double.eps # Evita divisiones por ~0 si hubiera h muy cercano a 1
h <- pmin(h, 1 - eps) # Garantiza 1-h>0 evitando inf al dividir

loo_residuals_1 <- r / (1 - h)
rmse_loo_1 <- sqrt(mean(loo_residuals_1^2, na.rm = TRUE))
rmse_loo_1

#modelo 7

full_model_2 <- lm(form_7,
                   data = db )
m <- full_model_2

r <- resid(m)
h <- hatvalues(m)

eps <- .Machine$double.eps # Evita divisiones por ~0 si hubiera h muy cercano a 1
h <- pmin(h, 1 - eps) # Garantiza 1-h>0 evitando inf al dividir

loo_residuals_2 <- r / (1 - h)
rmse_loo_2 <- sqrt(mean(loo_residuals_2^2, na.rm = TRUE))
rmse_loo_2

