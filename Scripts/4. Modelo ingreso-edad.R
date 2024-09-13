# Author: Juliet Molano

#------------------------------------------------------------------------------#
#--------------Script 4. MODELO DE INGRESO - EDAD  ----------------------------#
#------------------------------------------------------------------------------#

setwd(paste0(wd,"/Base_Datos"))
data_webs <- import(file = "base_final.rds")

# 1. Grafica de correlacion entre ingreso y edad ------------------------------
  
scatter_plot <- ggplot(data = data_webs, aes(x = Edad, y = log_ing_h_imp2)) +
  geom_point(color = "grey") +  # Puntos en azul
  labs(title = "", 
       x = "Edad", 
       y = "Logaritmo del ingreso por hora") +
  theme_minimal()
png("Caja_sexo.png") # Formato grafica
scatter_plot
dev.off() # Cierra la grafica


 #2. Definir los posibles predictores de la base de datos: 

 # Regresión: Log(wage)=b1 + b2(age) + b3(age)^2 + u (sin controles)
  model_Age_wage1 <- lm(log_ing_h_imp ~ Edad + Edad2, data = data) #Realizamos la regresión
  summary(model_Age_wage1)
  stargazer(model_Age_wage1, type = "text") # Modelo simple
  
  
  #---
  modelo1 <- lm(log_ing_h_imp ~ Edad + Edad2 + Sexo + dummy_jefe + formal + Tamaño_empresa + Estrato + Independiente + oficio_factor + edu_factor + Horas_trabajadas , data = data) #Realizamos la regresión
  summary(modelo1)
  stargazer(modelo1, type = "text") # Modelo simple

  
  # Regresión: Log(wage)=b1 + b2(age) + b3(age)^2 + u (con controles)
  model_Age_wage_cont3 <- lm(log_ing_h_imp ~ Edad + Edad2 + Sexo + Tamaño_empresa + formal + Independiente + oficio_factor + edu_factor + Horas_trabajadas + Experiencia_años, data = data) #Realizamos la regresión
  summary(model_Age_wage_cont3)
  stargazer(model_Age_wage_cont3, type = "text") # Modelo con controles
  
  # Generar tablas de regresion
  stargazer(model_Age_wage_cont3, type = "text", keep = c("Edad", "Edad2") ) #Modelo completo
  stargazer(model_Age_wage1, model_Age_wage_cont3,
            keep = c("Edad", "Edad2"),
            dep.var.caption  = "Logaritmo del salario",
            column.labels   = c("Sin controles", "Con controles"),
            covariate.labels = c("Edad", "Edad al cuadrado"),
            type="latex",out = "C:/Users/JULIETH/Desktop/Big data/latex/modelos.tex")
  
  # Leer el contenido del archivo
  regression_table <- readLines("C:/Users/JULIETH/Desktop/Big data/latex/modelos.tex")
  
  # Reemplazar "Observations" con "Observaciones"
  regression_table <- gsub("Observations", "Observaciones", regression_table)
  regression_table <- gsub("Adjusted R$^{2}$ ", "R$^{2}$ ajustado", regression_table)
  
  # Escribir el contenido modificado de nuevo al archivo
  writeLines(regression_table, "C:/Users/JULIETH/Desktop/Big data/latex/modelos.tex")
  
  
  #---------------------------------------------------------------------------#
  #                      Observaciones influyentes
  #---------------------------------------------------------------------------#
  
  
  ## leverage 
  data <- data %>% 
    ungroup() %>%
    mutate(leverage = hatvalues(model_Age_wage_cont3))  # Calcula los valores leverage
  length(hatvalues(model_Age_wage_cont3))
  
  ## residuals
  data <- data %>%
    ungroup() %>%
    mutate(residuals = residuals(model_Age_wage_cont3))  
  length(residuals(model_Age_wage_cont3))
  
  
  {
  N <- nrow(data)
  
  data$Secuencia <- seq(1 , N)
  a<- ggplot(data , aes(y = leverage , x = id , color= profesion, shape= as.factor(sexo) )) +
    geom_point() + # add points
    theme_bw() + #black and white theme
    labs(x = "Observations",  
         y = "Leverage",
         title = "") # labels
  
  
  b<- ggplot(data , aes(y = leverage , x = residuals  )) +
    geom_point() + # add points
    theme_bw() + #black and white theme
    labs(x = "Residuals",  
         y = "Leverage",
         title = "") # labels
  b
  
  # Arrange the ggplot2 plots side by side using grid.arrange()
  grid.arrange(a, b, ncol = 2)
  }

  #Calcula el apalancamiento medio
  p <- mean(data$leverage)
  p
  
  #punto de corte
  cutt <- 3*p
  cutt
  
  #dejar las observaciones que se consideran influyentes
  data2 <- data %>%
    filter(leverage <= cutt)
  
  #---- Correr nuevamente el modelo -------#

  #Modelo de prueba 2
  n_model_Age_wage_cont3 <- lm(log_ing_h_imp ~ Edad + Edad2 + Sexo + formal + Independiente + oficio_factor + edu_factor + Horas_trabajadas, data = data2) #Realizamos la regresión
  summary(n_model_Age_wage_cont3)
  stargazer(n_model_Age_wage_cont3, type = "text") # Modelo con controles
  
  data2 <- data2 %>%
    ungroup() %>%
    mutate(residuals2 = residuals(n_model_Age_wage_cont3))  
  length(residuals(n_model_Age_wage_cont3))
  
#---------------------------------------------------------------------------#
#                      VALORES ATIPICOS
#---------------------------------------------------------------------------#
  
#Grafica residuos del modelo
  ggplot(data= data2, 
         mapping = aes(x=residuals2)) +
    theme_bw() + 
    geom_density()
  
  #ID
  #data <- data %>%
   # mutate(id = str_c(Direccion, Secuencia, Orden, sep = "_"))
  

  ##Residuo estudentizado
  data2 <-data2 %>% mutate(m1_std_residuals2= studres(n_model_Age_wage_cont3))

  ggplot(data , aes(y = m1_std_residuals , x = id , color= Edad, shape= as.factor(gender) )) +
    geom_point() + # add points
    theme_bw() + #black and white theme
    labs(x = "Observations",  
         y = "Residuals",
         title = "") # labels
  
  #Eliminacion de valores atipicos
  data2 <- data2 %>% filter(m1_std_residuals2<2 & m1_std_residuals2>-2 )
  
  # Correr nuevo the model
  n2_model_Age_wage_cont3 <- lm(log_ing_h_imp ~ Edad + Edad2 + Sexo + formal + Independiente + oficio_factor + edu_factor + Horas_trabajadas, data = data2) #Realizamos la regresión
  summary(n2_model_Age_wage_cont3)
  stargazer(n2_model_Age_wage_cont3, type = "text") # Modelo con controles
  
  
  
  #--------------------------------------------------------------------------#
  # --------------------VALOR MAXIMO - PICO----------------------------------#
  #--------------------------------------------------------------------------#
  
  #1. MODELO SIN CONTROLES
  
  ## FORMA 1##
  
  #Obtener la edad máxima en la que el incremento en el salario empieza a disminuir(SC)
  matrix_coef <- summary(model_Age_wage1)$coefficients
  matrix_coef
  coeficiente_edad<-my_estimates <- matrix_coef[2, 1] 
  coeficiente_edad2<-my_estimates <- matrix_coef[3, 1] 
  coeficiente_edad2

  #Aplico la fórmula para obtener el "peak" de la edad 
  edad_peak<-(-coeficiente_edad/(2*coeficiente_edad2))
  edad_peak
  
  # # FORMA 12##
  Peak_age_fun <- function(age_1, age_2) {
    Edad_P<- -(age_1/(2*age_2))
    return (Edad_P)
  }
  
  Peak_age_mod_simple <- Peak_age_fun(age_1 = model_Age_wage1$coefficients[2],age_2 = model_Age_wage1$coefficients[3])
  Peak_age_mod_simple
  
  #2. MODELO CON CONTROLES
  
  #  FORMA 1
  
  #Obtener la edad máxima en la que el incremento en el salario empieza a disminuir(C)
  matrix_coef <- summary(n2_model_Age_wage_cont3)$coefficients
  matrix_coef
  coeficiente_edad_c<-my_estimates <- matrix_coef[2, 1] 
  coeficiente_edad_c2<-my_estimates <- matrix_coef[3, 1] 
  coeficiente_edad_c2
  
  #Aplico la fórmula para obtener el "peak" de la edad 
  edad_peak_C<-(-coeficiente_edad_c/(2*coeficiente_edad_c2))
  edad_peak_C
  
  #  FORMA 2
  Peak_age_fun <- function(age_1, age_2) {
    Edad_P<- -(age_1/(2*age_2))
    return (Edad_P)
  }
  
  Peak_age_mod_controles <- Peak_age_fun(age_1 = n2_model_Age_wage_cont3$coefficients[2],age_2 = n2_model_Age_wage_cont3$coefficients[3])
  Peak_age_mod_controles
  
  
#-------------------------------------BOOTSTRAP ----------------------------#

#---------- Mirar prediccion -------------#
  
  # Realiza predicciones con el modelo
  data2$predicted <- predict(model_Age_wage1, newdata = data2)
  data2$predicted_cont <- predict(n2_model_Age_wage_cont3, newdata = data2)
  
  #Modelo predicted vs salario 
  ggplot(data2, aes(x = predicted_cont, y = log_ing_h_imp))+
    geom_point(color = "grey")+
    geom_abline(color ="darkblue")+
    xlab("Predicción")+
    ylab("Log(Salario)")+
    theme_minimal()
  
  
  #---------- MODELO simple------------#
  
  
  #Intervalos de confianza con bootstrap
  Age_Wage_Profile_fn<-function(data2,index){
    f <- lm(log_ing_h_imp ~ Edad + Edad2, data = data2, subset= index)
    b1 <- f$coefficients[2]
    b2 <- f$coefficients[3]
    max_edad <- -(b1)/(2*b2)
    return(max_edad)
  }
  
  #Verifiquemos que la función... funciona!
  Age_Wage_Profile_fn(data2,1:nrow(data2))
  
  #Se utiliza la funcion boot para estimar la regresion con bootstrap
  set.seed(2365)
  boot_results_mod_simple <- boot(data2, Age_Wage_Profile_fn, R = 1000)
  Dist_Peak_age_mod_simple <- as.data.frame(boot_results_mod_simple$t)
  hist(Dist_Peak_age_mod_simple$V1) #distribucion del valor maximo de la edad con bootstrap
  
  ## Histograma - Modelo Simple
  Hist_mod_simp <- ggplot(Dist_Peak_age_mod_simple, aes(x = V1)) +
    geom_histogram(bins = 50, color = "white", fill = "#528B8B") +
    labs(x ='Edad', y='Frecuencia', title = "Panel A: Modelo simple")+
    geom_vline(aes(xintercept = Peak_age_mod_simple), color = "mistyrose", linewidth = 1)+
    theme_minimal()
  Hist_mod_simp
  
  
  #---------- MODELO con controles------------#
  
  #Intervalos de confianza con bootstrap
  Age_Wage_Profile_mod_cont<-function(data2,index){
    f <- lm(log_ing_h_imp ~ Edad + Edad2 + Sexo + Tamaño_empresa + formal + Independiente + oficio_factor + edu_factor + Horas_trabajadas + Experiencia_años, data = data2, subset = index)
    b1 <- f$coefficients[2]
    b2 <- f$coefficients[3]
    max_edad <- -(b1)/(2*b2)
    return(max_edad)
  }
  
  #Verifiquemos que la función... funciona!
  Age_Wage_Profile_mod_cont(data2,1:nrow(data2))
  
  #Se utiliza la funcion boot para estimar la regresion con bootstrap
  set.seed(2365)
  boot_results_mod_cont <- boot(data2, Age_Wage_Profile_mod_cont, R = 1000)
  Dist_Peak_age_mod_cont <- as.data.frame(boot_results_mod_cont$t)
  hist(Dist_Peak_age_mod_cont$V1) #distribucion del valor maximo de la edad con bootstrap
  
  ## Histograma - Modelo Simple
  Hist_mod_cont <- ggplot(Dist_Peak_age_mod_cont, aes(x = V1)) +
    geom_histogram(bins = 50, color = "white", fill = "steelblue") +
    labs(x ='Edad', y='Frecuencia', title = "Panel B: Modelo con controles")+
    geom_vline(aes(xintercept = Peak_age_mod_cont), color = "seashell3", linewidth = 1)+
    theme_minimal()
  Hist_mod_cont
  