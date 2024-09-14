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

stargazer(gender_earnings_gap_1, type = "text")  # Para visualizar en la consola
stargazer(gender_earnings_gap_1)                 # Para llevar a LATEX

# 3. Literal B: Conditional wage gap -------------------------------------------
# Esta seccion busca estimar si se cumple el slogan "Equal pay for equal work". 

# Se verifica inicialmente el tipo de variables que contiene la base  
str(Data_P4)
# Se verifica que las variables de oficio y edicacion sean entendidas como factores. 
# Esto debido a que son categoricas en la GEIH

# 3.1 Se estima la wage gap condicionaal a variables de control por OLS --------
gender_earnings_gap_2_OLS <- lm(log_ing_h_win ~ Female + Edad + Edad2+ Tamanio_empresa + Trabajo_formal + Independiente + 
                                  oficio_factor + edu_factor + Horas_trabajadas + Experiencia_anios,data = Data_P4)
Coeficiente_interes_2_OLS <-gender_earnings_gap_2_OLS$coefficients[2]
stargazer(gender_earnings_gap_2_OLS, type = "text")  # Para visualizar en la consola
stargazer(gender_earnings_gap_2_OLS)                 # Para llevar a LATEX

# 3.2 Se estima la wage gap condicional a variables de control por FWL ---------

# La primera etapa consiste en estimar la regresion sin la variable de interes y obtener los residuos
FWL_resid_1 <- residuals(lm(log_ing_h_win ~ Edad + Edad2 +Tamanio_empresa + Trabajo_formal + Independiente + oficio_factor + 
                       edu_factor + Horas_trabajadas + Experiencia_anios, data = Data_P4)) 
  
# La segunda etapa consiste en regresar la variable de interes con respecto al resto de variables de control

FWL_resid_2 <- residuals(lm(Female ~ Edad + Edad2 + Tamanio_empresa + Trabajo_formal + Independiente + oficio_factor + 
                              edu_factor + Horas_trabajadas + Experiencia_anios, data = Data_P4)) 
Female      <- FWL_resid_2   # Cambiar el nombre para facilitar la presetnacion de resultados

# Finalmente, se regresan ambos residuos encontrados. Se debería encontrar el mismo coeficiente estimado en OLS
gender_earnings_gap_2_FWL <- lm(FWL_resid_1 ~ Female)
Coeficiente_interes_2_FWL <- gender_earnings_gap_2_FWL$coefficients[2]

stargazer(gender_earnings_gap_2_FWL, type = "text")  # Para visualizar en la consola
stargazer(gender_earnings_gap_2_FWL)                 # Para llevar a LATEX
  
# 3.3 Resultados conjuntos -----------------------------------------------------

# Se muestran los resultados con stargazer
stargazer(gender_earnings_gap_1,gender_earnings_gap_2_FWL,type = "text",
          omit = "Constant",                             # Quitar intercepto              
          omit.stat = c("f", "ser", "aic", "bic", "ll","rsq"), # Omitir estadísticas no deseadas
          dep.var.labels = c("Ln(Salario por hora) sin controles","Ln(Salario por hora) con controles"),
          covariate.labels = "Female",
          notes = "Errores estándar en paréntesis") # Añadir la nota
  
# 3.4 FWL usando boopstrap -----------------------------------------------------
# Se crea una funcion para estimar la regresion usando FWL

Female_funcion<-function(Data,Indice){
   res_1 <- residuals(lm(log_ing_h_win ~ Edad + Edad2+ Tamanio_empresa + Trabajo_formal + Independiente + oficio_factor + 
                           edu_factor + Horas_trabajadas + Experiencia_anios, data = Data, subset=Indice)) 
   
   res_2 <- residuals(lm(Female ~ Edad + Edad2 + Tamanio_empresa + Trabajo_formal + Independiente + oficio_factor + 
                           edu_factor + Horas_trabajadas + Experiencia_anios, data = Data, subset=Indice))
   
   coef(lm(res_1 ~ res_2, subset=Indice))[2] # Se retorna el coeficiente de interes
}

# Se verifica que la función funciona de acuerdo con la base original 
Female_funcion(Data_P4,1:nrow(Data_P4)) # Efectivamente genera el mismo coeficiente
  
#Se utiliza la funcion boot para estimar la regresion con bootstrap

set.seed(20242) # Se fija como semilla el año 2024 y el segundo semestre
Bootstrap_FWL <- boot(Data_P4, Female_funcion, R = 1000)
Bootstrap_FWL

# 4. Literal C: Perfil salario vs edad por genero ------------------------------

# Se lleva cabo el perfil salario - edad diferenciando por género 

# 4.1  Submuestra de mujeres ---------------------------------------------------
Female_Data         <- subset(Data_P4, Female == 1)
Salario_Edad_Female <- lm(log_ing_h_win ~ Edad + Edad2, data = Female_Data) 
stargazer(Salario_Edad_Female, type = "text",omit = "Constant")
  
# Se realizan predicciones con el modelo para observar el comportamiento de la brecha salarial
Salario_Edad_Female_P <-  predict(Salario_Edad_Female, newdata = Female_Data)
Female_Data$Prediccion <- predict(Salario_Edad_Female, newdata = Female_Data)
  
# Se calculan los intervalos de confianza a través de bootstrap

Female_funcion_2 <-function(Data,Indice){
    Reg_1 <- lm(log_ing_h_win ~ Edad + Edad2, data = Data, subset= Indice)
    b_1 <- Reg_1$coefficients[2]
    b_2 <- Reg_1$coefficients[3]
    max_edad <- -(b_1)/(2*b_2)
    return(max_edad)
}
  
# Se verifica que la función funciona de acuerdo con la base original 
Female_funcion_2(Female_Data,1:nrow(Female_Data))
  
#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(20242) # Se fija como semilla el año 2024 y el segundo semestre
Bootstrap_Salario_Edad_Female <- boot(Female_Data, Female_funcion_2, R = 1000)

# Se extran los resultados de cada iteración y se calcula el intervalo de confianza
Bootstrap_Salario_Edad_Female_results <- as.data.frame(Bootstrap_Salario_Edad_Female$t)
setnames(Bootstrap_Salario_Edad_Female_results,"V1","Max_Edad_Female")
hist(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female) # Se obtiene la distribucion del valor maximo de la edad 
Lim_inf_Female = quantile(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female,0.025) #percentil 2.5 (42.53374)
Lim_sup_Female = quantile(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female,0.975) #percentil 97.5 (44.71811)
  
# Se gráfican los resultados observados

Female_plot <- ggplot(Female_Data, aes(x = Edad, y = log_ing_h_win)) +
    # Puntos para valores observados
    geom_point(aes(color = "Valor_observado"), alpha = 0.05,size=2) +  
    # Línea para valores predichos
    geom_line(aes(y = Salario_Edad_Female_P, color = "Valor_Predicho"), size = 1.5) +  
    # Escala de colores personalizada
    scale_color_manual(values = c("Valor_observado" = "Red", "Valor_Predicho" = "Black"),name="") +  # Colores de puntos y líneas
    labs(title = "Panel B: Mujeres",
         x = "Edad",
         y = "Ln(Salario por hora)") +
    theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centrar y aumentar el tamaño del título
    axis.title.x = element_text(size = 12), # Tamaño de las etiquetas de los ejes
    axis.title.y = element_text(size = 12),
    legend.position = "top", # Colocar la leyenda en la parte superior
    legend.text = element_text(size = 10)
  ) +
  xlim(20, 60) +  # Rango de edades (ejemplo: entre 0 y 80 años)
  ylim(4, 12)      # Rango de salarios logarítmicos (ejemplo)
  print(Female_plot)
  
  
# 4.2  Submuestra de hombres ---------------------------------------------------
Male_Data         <- subset(Data_P4, Female == 0)
Salario_Edad_Male <- lm(log_ing_h_win ~ Edad + Edad2, data = Male_Data) 
stargazer(Salario_Edad_Male, type = "text",omit = "Constant")

# Se realizan predicciones con el modelo para observar el comportamiento de la brecha salarial
Salario_Edad_Male_P <-  predict(Salario_Edad_Male, newdata = Male_Data)
Male_Data$Prediccion <- predict(Salario_Edad_Male, newdata = Male_Data)

# Se calculan los intervalos de confianza a través de bootstrap

Male_funcion_2 <-function(Data,Indice){
  Reg_1 <- lm(log_ing_h_win ~ Edad + Edad2, data = Data, subset= Indice)
  b_1 <- Reg_1$coefficients[2]
  b_2 <- Reg_1$coefficients[3]
  max_edad <- -(b_1)/(2*b_2)
  return(max_edad)
}

# Se verifica que la función funciona de acuerdo con la base original 
Male_funcion_2(Male_Data,1:nrow(Male_Data))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(20242) # Se fija como semilla el año 2024 y el segundo semestre
Bootstrap_Salario_Edad_Male <- boot(Male_Data, Male_funcion_2, R = 1000)

# Se extran los resultados de cada iteración y se calcula el intervalo de confianza
Bootstrap_Salario_Edad_Male_results <- as.data.frame(Bootstrap_Salario_Edad_Male$t)
setnames(Bootstrap_Salario_Edad_Male_results,"V1","Max_Edad_Male")
hist(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male) # Se obtiene la distribucion del valor maximo de la edad 
Lim_inf_Male = quantile(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male,0.025) #percentil 2.5 (42.53374)
Lim_sup_Male = quantile(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male,0.975) #percentil 97.5 (44.71811)

# Se gráfican los resultados observados
Male_plot <- ggplot(Male_Data, aes(x = Edad, y = log_ing_h_win)) +
  # Puntos para valores observados
  geom_point(aes(color = "Valor_observado"), alpha = 0.05,size=2) +  
  # Línea para valores predichos
  geom_line(aes(y = Salario_Edad_Male_P, color = "Valor_Predicho"), size = 1.5) +  
  # Escala de colores personalizada
  scale_color_manual(values = c("Valor_observado" = "Blue", "Valor_Predicho" = "Black"),name="") +  # Colores de puntos y líneas
  labs(title = "Panel B: Mujeres",
       x = "Edad",
       y = "Ln(Salario por hora)") +
  scale_color_manual(values = c("Mujeres" = "Red", "Hombres" = "Blue"), name = "") + # Cambiar colores de las líneas
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centrar y aumentar el tamaño del título
    axis.title.x = element_text(size = 12), # Tamaño de las etiquetas de los ejes
    axis.title.y = element_text(size = 12),
    legend.position = "top", # Colocar la leyenda en la parte superior
    legend.text = element_text(size = 10)
  ) +
  xlim(20, 60) +  # Rango de edades (ejemplo: entre 0 y 80 años)
  ylim(4, 12)      # Rango de salarios logarítmicos (ejemplo)
print(Male_plot)

# 4.3 Resultados conjuntos -----------------------------------------------------

# Se muestran los resultados con stargazer
stargazer(Salario_Edad_Female,Salario_Edad_Male,type = "text",
          omit = "Constant",                             # Quitar intercepto              
          omit.stat = c("f", "ser", "aic", "bic", "ll","rsq"), # Omitir estadísticas no deseadas
          dep.var.labels = c("Ln(Salario por hora)"),
          covariate.labels = c("Edad","Edad2"),
          column.labels = c("Mujeres", "Hombres"),
          notes = "Errores estándar en paréntesis") # Añadir la nota


# 4.4. Se realiza un plot perfil edad - salario diferenciando por sexo ---------

Data_Observada <- rbind(Female_Data,Male_Data)
Data_Observada$Sexo <- factor(Data_Observada$Sexo, levels = c(0, 1), labels = c("Mujeres", "Hombres"))

# Plot de prediccion por edad y sexo
Plot_Genero <- ggplot(Data_Observada, aes(x = Edad, y = Prediccion, color = Sexo)) +
  geom_line(size = 1)+
  labs(x = "Edad",
       y = "Ln(Salario por hora)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centrar y aumentar el tamaño del título
      axis.title.x = element_text(size = 12), # Tamaño de las etiquetas de los ejes
      axis.title.y = element_text(size = 12),
      legend.position = "top", # Colocar la leyenda en la parte superior
      legend.text = element_text(size = 10))
print(Plot_Genero)

# 4.5. Se realiza un plot de la distribucuón de edad vs el ingreso maximo ---------

Histograma_Female <- ggplot(Bootstrap_Salario_Edad_Female_results, aes(x = Max_Edad_Female)) +
  geom_histogram(bins = 50, color = "White", fill = "Red") +
  labs(x ='Edad', y='Frecuencia', title = "Panel A: Mujeres")+
  geom_vline(aes(xintercept = mean(Bootstrap_Salario_Edad_Female_results$Max_Edad_Female)), color = "Black", size = 1)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centrar y aumentar el tamaño del título
      axis.title.x = element_text(size = 12), # Tamaño de las etiquetas de los ejes
      axis.title.y = element_text(size = 12),
      legend.position = "top", # Colocar la leyenda en la parte superior
      legend.text = element_text(size = 10))
Histograma_Female

Histograma_Male <- ggplot(Bootstrap_Salario_Edad_Male_results, aes(x = Max_Edad_Male)) +
  geom_histogram(bins = 50, color = "White", fill = "Blue") +
  labs(x ='Edad', y='Frecuencia', title = "Panel B: Hombres")+
  geom_vline(aes(xintercept = mean(Bootstrap_Salario_Edad_Male_results$Max_Edad_Male)), color = "Black", size = 1)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centrar y aumentar el tamaño del título
        axis.title.x = element_text(size = 12), # Tamaño de las etiquetas de los ejes
        axis.title.y = element_text(size = 12),
        legend.position = "top", # Colocar la leyenda en la parte superior
        legend.text = element_text(size = 10))
Histograma_Male


  
  
  