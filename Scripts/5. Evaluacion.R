# Inicializacion de la base ---------------------------------
setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-1_Machine-Learning_2024-main/Problem-Set-1_Machine-Learning_2024-main")
source("Scripts/0. Script_Base.R")
source("Scripts/2. Filtro Base y seleccion de variables.R")

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

m1<-lm(log_ing_h_imp ~ Edad + Edad2, data = nlsy)
m2<-lm(log_ing_h_imp ~ Sexo, data = nlsy)
m3<-lm(log_ing_h_imp ~ Sexo + Edad + Edad2, data = nlsy)

# Otros modelos poly(educ,8,raw=TRUE
m4<-lm(log_ing_h_imp ~ poly(Edad,8,raw=TRUE)*Sexo, data = nlsy)
m5<-lm(log_ing_h_imp ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ Sexo, data = nlsy)
m6<-lm(log_ing_h_imp ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ Nivel_educ, data = nlsy)
m7<-lm(log_ing_h_imp ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE):poly(Nivel_educ,raw=TRUE), data = nlsy)

summary(m7)

# --------------------------------------------
# Realizar predicciones
test$mp1<-predict(m1,newdata = test)
test$mp2<-predict(m2,newdata = test)
test$mp3<-predict(m3,newdata = test)
test$mp4<-predict(m4,newdata = test)
test$mp5<-predict(m5,newdata = test)
test$mp6<-predict(m6,newdata = test)
test$mp7<-predict(m7,newdata = test)


with(test,mean((log_ing_h_imp-mp1)^2))
with(test,mean((log_ing_h_imp-mp2)^2))
with(test,mean((log_ing_h_imp-mp3)^2))
with(test,mean((log_ing_h_imp-mp4)^2))
with(test,mean((log_ing_h_imp-mp5)^2))
with(test,mean((log_ing_h_imp-mp6)^2))
with(test,mean((log_ing_h_imp-mp7)^2))

# Obtener en una tabla

# Calcular el RMSE para cada modelo
rmse_m1 <- sqrt(mean((test$log_ing_h_imp - test$mp1)^2))
rmse_m2 <- sqrt(mean((test$log_ing_h_imp - test$mp2)^2))
rmse_m3 <- sqrt(mean((test$log_ing_h_imp - test$mp3)^2))
rmse_m4 <- sqrt(mean((test$log_ing_h_imp - test$mp4)^2))
rmse_m5 <- sqrt(mean((test$log_ing_h_imp - test$mp5)^2))
rmse_m6 <- sqrt(mean((test$log_ing_h_imp - test$mp6)^2))
rmse_m7 <- sqrt(mean((test$log_ing_h_imp - test$mp7)^2))

# Número de predictores para cada modelo (excluyendo el intercepto)
num_pred_m1 <- length(coef(m1)) - 1
num_pred_m2 <- length(coef(m2)) - 1
num_pred_m3 <- length(coef(m3)) - 1
num_pred_m4 <- length(coef(m4)) - 1
num_pred_m5 <- length(coef(m5)) - 1
num_pred_m6 <- length(coef(m6)) - 1
num_pred_m7 <- length(coef(m7)) - 1

# Crear la tabla de resultados
resultados <- data.frame(
  Modelo = c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
  RMSE = c(rmse_m1, rmse_m2, rmse_m3, rmse_m4, rmse_m5, rmse_m6, rmse_m7),
  Num_Predictores = c(num_pred_m1, num_pred_m2, num_pred_m3, num_pred_m4, num_pred_m5, num_pred_m6, num_pred_m7)
)

# Mostrar la tabla de resultados
print(resultados)

# Especificacion con el menor error de prediccion
m6<-lm(log_ing_h_imp ~ poly(Edad,8,raw=TRUE):poly(Experiencia,8,raw=TRUE)+ poly(Experiencia,8,raw=TRUE)+ Nivel_educ, data = nlsy)

# ----------------------------

# Calcular los errores de predicción para el modelo m6
test$error_m6 <- test$log_ing_h_imp - test$mp6

# Examinar la distribución de los errores de predicción
hist(test$error_m6, main = "Distribución de Errores de Predicción para el Modelo m6", xlab = "Error de Predicción", breaks = 30, col = "skyblue")

# Calcular los cuartiles y los límites para identificar outliers
Q1 <- quantile(test$error_m6, 0.25)
Q3 <- quantile(test$error_m6, 0.75)
IQR <- Q3 - Q1

# Definir los límites de Tukey para los outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Filtrar observaciones que son outliers
outliers <- test[test$error_m6 < limite_inferior | test$error_m6 > limite_superior, ]

# Mostrar los outliers
print(outliers)

# Graficar predicciones vs. valores reales
plot(test$log_ing_h_imp, test$mp6, main = "Predicciones vs. Valores Reales (Modelo m6)",
     xlab = "Valores Reales", ylab = "Predicciones", pch = 19, col = "blue")
abline(0, 1, col = "red")  # Línea de identidad

# Resaltar los outliers en el gráfico
points(outliers$log_ing_h_imp, outliers$mp6, pch = 19, col = "red")
