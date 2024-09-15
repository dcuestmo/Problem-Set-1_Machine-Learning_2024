# Autor: Diego Cuesta
#------------------------------------------------------------------------------#
#----------------------- Punto 4 - Gender earning gap -------------------------#
#------------------------------------------------------------------------------#

# El presente script desarrolla el punto #4 del Problem Set 1. Para ello, es
# necesario cargar la información de la GEIH ajustada en el script 
# "2. Filtro Base y seleccion de variables".

# 0. Se carga la base de datos ajustada ----------------------------------------
setwd(paste0(wd,"/Base_Datos"))
data_webs <- import(file = "base_final.rds")

# 1. Se hacen ajustes generales ------------------------------------------------
Data_P4 <- copy(data_webs)
Data_P4 <- Data_P4%>%mutate(Female = 1-Sexo)    # Se crea la variable Female. Para las mujeres es =1 y para los hombres =0

# 2. Literal A: Unconditional wage gap -----------------------------------------
gender_earnings_gap_1 <- lm(log_ing_h_win ~ Female, data = Data_P4)
summary(gender_earnings_gap_1)  # Resumen de resultados
Coeficiente_interes_1_OLS <-gender_earnings_gap_1$coefficients[2]

# 3. Literal B: Conditional wage gap -------------------------------------------
# Esta seccion busca estimar si se cumple el slogan "Equal pay for equal work". 

# Se verifica inicialmente el tipo de variables que contiene la base  
str(Data_P4)
# Se verifica que las variables de oficio y edicacion sean entendidas como factores. 
# Esto debido a que son categoricas en la GEIH

# 3.1 Se estima la wage gap condicionaal a variables de control por OLS --------
gender_earnings_gap_2_OLS <- lm(log_ing_h_win ~ Female + Edad_win + Edad2+ tfirma_factor + Trabajo_informal + Independiente + 
                                  oficio_factor + edu_factor + Horas_trabajadas_win + Experiencia_win + estrato_factor+ dummy_jefe,data = Data_P4)
Coeficiente_interes_2_OLS <-gender_earnings_gap_2_OLS$coefficients[2]
stargazer(gender_earnings_gap_2_OLS, type = "text")  # Para visualizar en la consola
stargazer(gender_earnings_gap_2_OLS)                 # Para llevar a LATEX

# 3.2 Se estima la wage gap condicional a variables de control por FWL ---------

# La primera etapa consiste en estimar la regresion sin la variable de interes y obtener los residuos
FWL_resid_1 <- residuals(lm(log_ing_h_win ~Edad_win + Edad2+ tfirma_factor + Trabajo_informal + Independiente + 
                              oficio_factor + edu_factor + Horas_trabajadas_win + Experiencia_win + estrato_factor+dummy_jefe, data = Data_P4)) 
  
# La segunda etapa consiste en regresar la variable de interes con respecto al resto de variables de control

FWL_resid_2 <- residuals(lm(Female ~ Edad_win + Edad2+ tfirma_factor + Trabajo_informal + Independiente + 
                              oficio_factor + edu_factor + Horas_trabajadas_win + Experiencia_win + estrato_factor+ dummy_jefe, data = Data_P4)) 
Female      <- FWL_resid_2   # Cambiar el nombre para facilitar la presetnacion de resultados

# Finalmente, se regresan ambos residuos encontrados. Se debería encontrar el mismo coeficiente estimado en OLS
gender_earnings_gap_2_FWL <- lm(FWL_resid_1 ~ Female)
Coeficiente_interes_2_FWL <- gender_earnings_gap_2_FWL$coefficients[2]

# 3.3 Resultados conjuntos -----------------------------------------------------

# Se muestran los resultados con stargazer
# Guardar resultados en Latex
setwd(paste0(wd,"/Latex"))
stargazer(gender_earnings_gap_1,gender_earnings_gap_2_FWL,type = "text",out = "Punto_4_Brecha_Salarial_Genero.tex",
          omit = "Constant",                             # Quitar intercepto              
          omit.stat = c("f", "ser", "aic", "bic", "ll","rsq"), # Omitir estadísticas no deseadas
          dep.var.labels = c("Ln(Salario por hora) sin controles","Ln(Salario por hora) con controles"),
          covariate.labels = "Female",
          notes = "Errores estándar en paréntesis") # Añadir la nota

# 3.4 FWL usando boopstrap -----------------------------------------------------
# Se crea una funcion para estimar la regresion usando FWL

Female_funcion<-function(Data,Indice){
   res_1 <- residuals(lm(log_ing_h_win ~ Edad_win + Edad2+ tfirma_factor + Trabajo_informal + Independiente + 
                           oficio_factor + edu_factor + Horas_trabajadas_win + Experiencia_win + estrato_factor+ dummy_jefe, data = Data, subset=Indice)) 
   
   res_2 <- residuals(lm(Female ~ Edad_win + Edad2+ tfirma_factor + Trabajo_informal + Independiente + 
                           oficio_factor + edu_factor + Horas_trabajadas_win + Experiencia_win + estrato_factor+ dummy_jefe, data = Data, subset=Indice))
   
   coef(lm(res_1 ~ res_2, subset=Indice))[2] # Se retorna el coeficiente de interes
}

# Se verifica que la función funciona de acuerdo con la base original 
Female_funcion(Data_P4,1:nrow(Data_P4)) # Efectivamente genera el mismo coeficiente
  
#Se utiliza la funcion boot para estimar la regresion con bootstrap

set.seed(20242) # Se fija como semilla el año 2024 y el segundo semestre
Bootstrap_FWL <- boot(Data_P4, Female_funcion, R = 1000)
Bootstrap_FWL
Bootstrap_FWL_IC <- boot.ci(Bootstrap_FWL, type = c("perc"), conf = 0.99)

# 4. Literal C: Perfil salario vs edad por genero ------------------------------

# Se lleva cabo el perfil salario - edad diferenciando por género 

# 4.1  Submuestra de mujeres ---------------------------------------------------
Female_Data         <- subset(Data_P4, Female == 1)
Salario_Edad_Female <- lm(log_ing_h_win ~ Edad_win + Edad2, data = Female_Data) 
stargazer(Salario_Edad_Female, type = "text",omit = "Constant")
  
# Se realizan predicciones con el modelo para observar el comportamiento del perfil salario - edad diferenciando por género
Salario_Edad_Female_P <-  predict(Salario_Edad_Female, newdata = Female_Data)
Female_Data$Prediccion <- predict(Salario_Edad_Female, newdata = Female_Data)
  
# Se calcula la edad maxima 

Female_funcion_2 <-function(Data,Indice){
    Reg_1 <- lm(log_ing_h_win ~ Edad_win + Edad2, data = Data, subset= Indice)
    b_1 <- Reg_1$coefficients[2]
    b_2 <- Reg_1$coefficients[3]
    max_edad <- -(b_1)/(2*b_2)
    return(max_edad)
}
  
# Se verifica que la función funciona de acuerdo con la base original 
Max_Female =  Female_funcion_2(Female_Data,1:nrow(Female_Data))
  
#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(20242) # Se fija como semilla el año 2024 y el segundo semestre
Bootstrap_Salario_Edad_Female <- boot(Female_Data, Female_funcion_2, R = 1000)

# Se extran los resultados de cada iteración y se calcula el intervalo de confianza
Bootstrap_Salario_Edad_Female_results <- as.data.frame(Bootstrap_Salario_Edad_Female$t)
setnames(Bootstrap_Salario_Edad_Female_results,"V1","Max_Edad_Female")
hist(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female) # Se obtiene la distribucion del valor maximo de la edad 
Lim_inf_Female = quantile(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female,0.025) #percentil 2.5 (42.53374)
Lim_sup_Female = quantile(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female,0.975) #percentil 97.5 (44.71811)

# 4.2  Submuestra de hombres ---------------------------------------------------
Male_Data         <- subset(Data_P4, Female == 0)
Salario_Edad_Male <- lm(log_ing_h_win ~ Edad_win + Edad2, data = Male_Data) 
stargazer(Salario_Edad_Male, type = "text",omit = "Constant")

# Se realizan predicciones con el modelo para observar el comportamiento de la brecha salarial
Salario_Edad_Male_P <-  predict(Salario_Edad_Male, newdata = Male_Data)
Male_Data$Prediccion <- predict(Salario_Edad_Male, newdata = Male_Data)

# Se calculan los intervalos de confianza a través de bootstrap

Male_funcion_2 <-function(Data,Indice){
  Reg_1 <- lm(log_ing_h_win ~ Edad_win + Edad2, data = Data, subset= Indice)
  b_1 <- Reg_1$coefficients[2]
  b_2 <- Reg_1$coefficients[3]
  max_edad <- -(b_1)/(2*b_2)
  return(max_edad)
}

# Se verifica que la función funciona de acuerdo con la base original 
Max_Male = Male_funcion_2(Male_Data,1:nrow(Male_Data))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(20242) # Se fija como semilla el año 2024 y el segundo semestre
Bootstrap_Salario_Edad_Male <- boot(Male_Data, Male_funcion_2, R = 1000)

# Se extran los resultados de cada iteración y se calcula el intervalo de confianza
Bootstrap_Salario_Edad_Male_results <- as.data.frame(Bootstrap_Salario_Edad_Male$t)
setnames(Bootstrap_Salario_Edad_Male_results,"V1","Max_Edad_Male")
hist(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male) # Se obtiene la distribucion del valor maximo de la edad 
Lim_inf_Male = quantile(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male,0.025) #percentil 2.5 (42.53374)
Lim_sup_Male = quantile(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male,0.975) #percentil 97.5 (44.71811)

# 4.3 Resultados conjuntos -----------------------------------------------------
# Se muestran los resultados con stargazer
setwd(paste0(wd,"/Latex"))
stargazer(Salario_Edad_Female,Salario_Edad_Male,type = "text",out = "Punto_4_Perfil_Salario_Edad_Genero.tex",
          omit = "Constant",                             # Quitar intercepto              
          omit.stat = c("f", "ser", "aic", "bic", "ll","rsq"), # Omitir estadísticas no deseadas
          dep.var.labels = c("Ln(Salario por hora)"),
          covariate.labels = c("Edad","Edad2"),
          column.labels = c("Mujeres", "Hombres"),
          notes = "Errores estándar en paréntesis") # Añadir la nota

# 4.4 Perfil salario vs edad por genero -----------------------------------------------------
age_seq <- seq(min(Data_P4$Edad_win), max(Data_P4$Edad_win), by = 1)  # edades
predic_Male <- predict(Salario_Edad_Male, newdata = data.frame(Edad_win=age_seq,Edad2=age_seq*age_seq))  # Predicciones
predic_Female <- predict(Salario_Edad_Female, newdata = data.frame(Edad_win=age_seq,Edad2=age_seq*age_seq))  # Predicciones

setwd(paste0(wd,"/Graficas"))
png(filename = "Perfil_Salario_Genero.png",width = 1200, height = 600, res = 150)  #

plot(Data_P4$Edad_win, (Data_P4$log_ing_h_win), 
     xlab = "Edad", ylab = "Ln(Salario por hora)", 
     col = alpha("grey", 0.8), main = "Perfil Salario-Edad por género", 
     xlim = c(15, 75), ylim = c(5, 11),  # Establecer rango y frecuencia en ejes X e Y
     xaxp = c(20, 80, 6), yaxp = c(5, 11, 6))  # Establecer frecuencia en ejes X e Y
lines(age_seq, predic_Male, col = "red", lwd = 1)  # Línea de predicciones para Hombres
lines(age_seq, predic_Female, col = "blue", lwd = 1)  # Línea de predicciones para Mujeres

# Colocamos líneas verticales
abline(v = Max_Male, col = "red", lwd = 2, lty = 1)  # Línea discontinua para el valor central Hombres
abline(v = Lim_inf_Male, col = "red", lwd = 2, lty = 3) 
abline(v = Lim_sup_Male, col = "red", lwd = 2, lty = 3) 
abline(v = Max_Female, col = "blue", lwd = 2, lty = 1)  # Línea discontinua para el valor central Mujeres
abline(v = Lim_inf_Female, col = "blue", lwd = 2, lty = 3) 
abline(v = Lim_sup_Female, col = "blue", lwd = 2, lty = 3) 

# Colocamos leyenda
legend("topright",                    # posición de la leyenda
       legend = c("Mujer", "Hombre"), # etiquetas
       col = c("blue", "red"),        # colores de las líneas en la leyenda
       lwd = 1,                       # grosor de las líneas en la leyenda
       bg = "white")                  # color de fondo de la leyenda

# Agregar pie de página
#mtext("Lineas verticales continuas: Peak ages para cada sexo. Lineas verticales punteadas: Intervalos de confianza.", side = 1, line = 3.8, adj = 0, cex = 0.6)

# Agregar etiquetas para las líneas rojas y azules
text(Max_Male+2, 5, "45,2", col = "red", pos = 4)  # Etiqueta para línea roja
text(Max_Female-2, 5, "39,6", col = "blue", pos = 2)  # Etiqueta para línea azul

# cerrar el pdf
dev.off()
dev.off()




















