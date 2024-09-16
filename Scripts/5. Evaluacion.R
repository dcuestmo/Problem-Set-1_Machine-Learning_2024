
#------------------------------------------------------------------------------#
#--------------Script 5.Prediccion  -------------------------------------------#
#------------------------------------------------------------------------------#

# Inicializacion de la base ---------------------------------
setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-1_Machine-Learning_2024-main/Problem-Set-1_Machine-Learning_2024-main")
source("Scripts/0. Script_Base.R")

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-1_Machine-Learning_2024-main/Problem-Set-1_Machine-Learning_2024-main")
nlsy <- readRDS("Base_Datos/base_final.rds")
# Verificar que se cargó correctamente
summary(nlsy)

# -------------------------------------------------------------

# Realizamos una particion de la muestra

set.seed(40503)
sample <- sample(c(TRUE, FALSE), nrow(nlsy), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- nlsy[sample, ] #train sample those that are TRUE in the sample index
test   <- nlsy[!sample, ] #test sample those that are FALSE in the sample index
dim(train)

glimpse(nlsy)

# Definimos los modelos a utilizar ----------------------------
# Utilizando la variable sin valores atipicos.

m1<-lm(log_ing_h_win ~ Edad + Edad2, data = train)
m2<-lm(log_ing_h_win ~ Sexo, data = train)
m3<-lm(log_ing_h_win ~ Sexo + Edad + Edad2, data = train)
m3_1 <- lm(log_ing_h_win ~ Edad_win + Edad2 + Mujer + estrato_factor + dummy_jefe + edu_factor + Trabajo_informal + Independiente  + Horas_trabajadas_win + Experiencia_win, data = train)


# Otros modelos poly(educ,8,raw=TRUE
m4<-lm(log_ing_h_win ~ poly(Edad,8,raw=TRUE)*Sexo, data = train)
m5<-lm(log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ Sexo, data = train)
m6<-lm(log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ Nivel_educ, data = train)
m7<-lm(log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE):poly(Nivel_educ,raw=TRUE), data = train)
m8<-lm(log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE):poly(Nivel_educ,raw=TRUE)+Independiente+Trabajo_informal,data = train)

summary(m8)

# --------------------------------------------
# Realizar predicciones
test$mp1<-predict(m1,newdata = test)
test$mp2<-predict(m2,newdata = test)
test$mp3<-predict(m3,newdata = test)
test$mp3_1<-predict(m3_1,newdata = test)
test$mp4<-predict(m4,newdata = test)
test$mp5<-predict(m5,newdata = test)
test$mp6<-predict(m6,newdata = test)
test$mp7<-predict(m7,newdata = test)
test$mp8<-predict(m8,newdata = test)


with(test,mean((log_ing_h_win-mp1)^2))
with(test,mean((log_ing_h_win-mp2)^2))
with(test,mean((log_ing_h_win-mp3)^2))
with(test,mean((log_ing_h_win-mp3_1)^2))
with(test,mean((log_ing_h_win-mp4)^2))
with(test,mean((log_ing_h_win-mp5)^2))
with(test,mean((log_ing_h_win-mp6)^2))
with(test,mean((log_ing_h_win-mp7)^2))
with(test,mean((log_ing_h_win-mp8)^2))

# Obtener en una tabla

# Calcular el RMSE para cada modelo
rmse_m1 <- sqrt(mean((test$log_ing_h_win - test$mp1)^2))
rmse_m2 <- sqrt(mean((test$log_ing_h_win - test$mp2)^2))
rmse_m3 <- sqrt(mean((test$log_ing_h_win - test$mp3)^2))
rmse_m3_1 <- sqrt(mean((test$log_ing_h_win - test$mp3_1)^2))
rmse_m4 <- sqrt(mean((test$log_ing_h_win - test$mp4)^2))
rmse_m5 <- sqrt(mean((test$log_ing_h_win - test$mp5)^2))
rmse_m6 <- sqrt(mean((test$log_ing_h_win - test$mp6)^2))
rmse_m7 <- sqrt(mean((test$log_ing_h_win - test$mp7)^2))
rmse_m8 <- sqrt(mean((test$log_ing_h_win - test$mp8)^2))

# Número de predictores para cada modelo (excluyendo el intercepto)
num_pred_m1 <- length(coef(m1)) - 1
num_pred_m2 <- length(coef(m2)) - 1
num_pred_m3 <- length(coef(m3)) - 1
num_pred_m3_1 <- length(coef(m3_1)) - 1
num_pred_m4 <- length(coef(m4)) - 1
num_pred_m5 <- length(coef(m5)) - 1
num_pred_m6 <- length(coef(m6)) - 1
num_pred_m7 <- length(coef(m7)) - 1
num_pred_m8 <- length(coef(m8)) - 1

# Crear la tabla de resultados
resultados <- data.frame(
  Modelo = c("m1", "m2", "m3", "m3.1", "m4", "m5", "m6", "m7", "m8"),
  RMSE = c(rmse_m1, rmse_m2, rmse_m3, rmse_m3_1, rmse_m4, rmse_m5, rmse_m6, rmse_m7, rmse_m8),
  Num_Predictores = c(num_pred_m1, num_pred_m2, num_pred_m3, num_pred_m3_1, num_pred_m4, num_pred_m5, num_pred_m6, num_pred_m7, num_pred_m8)
)

# Mostrar la tabla de resultados
print(resultados)

install.packages("xtable")
library(xtable)

# Convertir el data frame a una tabla de LaTeX
tabla_latex <- xtable(resultados)
print(tabla_latex, type = "latex", file = "tabla_resultados.tex", include.rownames = FALSE)

# ----------------------------

# Especificacion con el menor error de prediccion
m6<-lm(log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ Nivel_educ, data = nlsy)

# Calcular los errores de predicción para el modelo m6
test$residuals_m6 <- test$log_ing_h_win - test$mp6

ggplot(data= test, 
       mapping = aes(x=residuals_m6)) +
  theme_bw() + 
  geom_density() +
  xlab("Residuales del modelo 6")

nlsy <- nlsy %>%
  ungroup() %>%
  mutate(leverage = hatvalues(m6))

# leverage 
nlsy<- nlsy %>% mutate(leverage = hatvalues(m6))

# residuals
nlsy<- nlsy %>% mutate(residuals = m6$residuals)

ggplot(nlsy , aes(y = leverage , x = residuals  )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Residuals",  
       y = "Leverage",
       title = "") # labels

p <- mean(nlsy$leverage)
p

cutt <- 3*p
cutt


# LOOCV -------------------------------------------

m6f<-log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ Nivel_educ
m5f<-log_ing_h_win ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ Sexo

install.packages("snow")
library(parallel)
library(progress)

# Limpiar el entorno de clústeres
rm(cluster)
gc() 

#-------------------------------------------------------

# Seleccionar una muestra más pequeña de observaciones para reducir el tiempo de ejecución
set.seed(123)  # Fijar una semilla para reproducibilidad
nlsy_sample <- nlsy[sample(nrow(nlsy), 16000), ]  # Usar una muestra de 100 observaciones

# Crear y registrar el clúster de paralelismo solo una vez
cluster <- parallel::makeCluster(parallel::detectCores() - 1, type = "SOCK")
doParallel::registerDoParallel(cluster)

# Configuración del control del modelo con Leave-One-Out Cross Validation (LOOCV)
ctrl <- trainControl(
  method = "LOOCV", 
  allowParallel = TRUE  # Permitir paralelismo
)

# Entrenar el modelo utilizando el método de regresión lineal (lm)
modelo1 <- caret::train(
  m6f,
  data = nlsy_sample,  # Utilizar la muestra de 100 observaciones
  method = 'lm', 
  trControl = ctrl
)

# Detener clúster paralelo y desregistrar el paralelismo solo una vez
parallel::stopCluster(cluster)
doParallel::registerDoSEQ()  # Regresar a la ejecución secuencial

# Limpiar el entorno de clústeres
rm(cluster)
gc()  # Llamar a la recolección de basura para liberar memoria

modelo1
score1<-RMSE(modelo1$pred$pred, nlsy_sample$log_ing_h_win)
score1

# -----------------------------------------------------------------------------

# Crear y registrar el clúster de paralelismo solo una vez
cluster <- parallel::makeCluster(parallel::detectCores() - 1, type = "SOCK")
doParallel::registerDoParallel(cluster)

# Configuración del control del modelo con Leave-One-Out Cross Validation (LOOCV)
ctrl <- trainControl(
  method = "LOOCV", 
  allowParallel = TRUE  # Permitir paralelismo
)

# Entrenar el modelo utilizando el método de regresión lineal (lm)
modelo2 <- caret::train(
  m5f,
  data = nlsy_sample,  # Utilizar la muestra de 100 observaciones
  method = 'lm', 
  trControl = ctrl
)

# Detener clúster paralelo y desregistrar el paralelismo solo una vez
parallel::stopCluster(cluster)
doParallel::registerDoSEQ()  # Regresar a la ejecución secuencial

# Limpiar el entorno de clústeres
rm(cluster)
gc()  # Llamar a la recolección de basura para liberar memoria

modelo2
score2<-RMSE(modelo2$pred$pred, nlsy_sample$log_ing_h_win)
score2


