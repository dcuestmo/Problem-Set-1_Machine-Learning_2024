# Author: Juliet Molano

#------------------------------------------------------------------------------#
#--------------Script 4. MODELO DE INGRESO - EDAD  ----------------------------#
#------------------------------------------------------------------------------#

setwd(paste0(wd,"/Base_Datos"))
data_webs <- import(file = "base_final.rds")

#1. Definir los posibles predictores de la base de datos---------------------- 

   #i. Regresión: Log(wage)=b1 + b2(age) + b3(age)^2 + u (sin controles)
    model_Age_wage <- lm(log_ing_h_win ~ Edad_win + Edad2, data = data_webs) #Realizamos la regresión
    summary(model_Age_wage)
    stargazer(model_Age_wage, type = "text") # Modelo simple
    
  #ii. Regresión: Log(wage)=b1 + b2(age) + b3(age)^2 + u (con controles)
    model_Age_wage_cont1 <- lm(log_ing_h_win ~ Edad_win + Edad2 + Mujer + estrato_factor + dummy_jefe + edu_factor + tfirma_factor + Trabajo_informal + Independiente + oficio_factor  + Horas_trabajadas_win , data = data_webs) #Realizamos la regresión
    summary(model_Age_wage_cont1)
    stargazer(model_Age_wage_cont1, type = "text") # Modelo con controles

    
        # Generar tablas de regresion - modelos indiciales con todas las observaciones
        setwd(paste0(wd,"/Latex"))
        stargazer(model_Age_wage, type = "text", keep = c("Edad", "Edad2") ) #Modelo completo
        stargazer(model_Age_wage, model_Age_wage_cont1,
                  keep = c("Edad", "Edad2"),
                  align = TRUE, 
                  dep.var.labels.include = FALSE,
                  dep.var.caption  = "Log(Ingresos por hora)",            
                  column.labels   = c("Sin controles", "Con controles"),
                  covariate.labels = c("Edad", "Edad al cuadrado"),
                  type="latex",out = "modelos_edad_ingresos.tex")
        
        # Leer el contenido del archivo y modificar
        regression_table <- readLines("modelos_edad_ingresos.tex")
        regression_table <- gsub("Edad_win", "Edad", regression_table) #Reemplazar "Observations" con "Observaciones"
        regression_table <- gsub("Observations", "Observaciones", regression_table) #Reemplazar "Observations" con "Observaciones"
        regression_table <- gsub("Adjusted R$^{2}$ ", "R$^{2}$ ajustado", regression_table)
        writeLines(regression_table, "modelos_edad_ingresos.tex") #Escribir el contenido modificado de nuevo al archivo
        
  #2. Analisis de observaciones influyentes ------------------------------------
  
    #i. Eliminar observaciones con alto apalancamiento
              
        ## leverage 
        data_webs <- data_webs %>% 
          ungroup() %>%
          mutate(leverage = hatvalues(model_Age_wage_cont1))  # Calcula los valores leverage
        length(hatvalues(model_Age_wage_cont1))
        
        ## residuals
        data_webs <- data_webs %>%
          ungroup() %>%
          mutate(residuals = residuals(model_Age_wage_cont1))  
        length(residuals(model_Age_wage_cont1))
        
        #Calcula el apalancamiento medio
        p <- mean(data_webs$leverage)
        p
        
        #punto de corte
        cutt <- 3*p
        cutt
        
        #dejar las observaciones que se consideran influyentes
        data_webs2 <- data_webs %>%
          filter(leverage <= cutt)
  

    #ii .Correr nodelo sin observaciones influyentes
    n_model_Age_wage_cont1 <- lm(log_ing_h_win ~ Edad_win + Edad2 + Mujer + estrato_factor + dummy_jefe + edu_factor + tfirma_factor + Trabajo_informal + Independiente + oficio_factor  + Horas_trabajadas_win + Experiencia_win, data = data_webs2) #Realizamos la regresión
    summary(n_model_Age_wage_cont1)
    stargazer(n_model_Age_wage_cont1, type = "text") # Modelo con controles
    
    data_webs2 <- data_webs2 %>%
      ungroup() %>%
      mutate(residuals2 = residuals(n_model_Age_wage_cont1))  
    length(residuals(n_model_Age_wage_cont1))
    

  
  #3. Analisis de valores atipicos del modelo-----------------------------------
    
    #i. Grafucas de los residuos 
    residuos_c <- ggplot(data = data_webs, 
                       mapping = aes(x=residuals)) +
      theme_bw() + 
      geom_density() +
      labs(title = "",
           x = "Resuduos",
           y = "Densidad") 
    png("residuos_sin_var_influy") # Formato grafica
    residuos_c
    dev.off() # Cierra la grafica
  

    #ii. Residuo estudentizado
    data_webs <-data_webs %>% mutate(m1_std_residuals_c = studres(model_Age_wage_cont1))

    #iii. Eliminacion de valores atipicos
    data_webs3 <- data_webs %>% filter(m1_std_residuals_c<2 & m1_std_residuals_c>-2 )
  
    #iv.  Correr nuevo modelo
    model_Age_wage_cont1_vt <- lm(log_ing_h_win ~ Edad_win + Edad2 + Mujer + estrato_factor + dummy_jefe + edu_factor + tfirma_factor + Trabajo_informal + Independiente + oficio_factor  + Horas_trabajadas_win + Experiencia_win, data = data_webs3) #Realizamos la regresión
    summary(model_Age_wage_cont1_vt)
    stargazer(model_Age_wage_cont1_vt, type = "text") # Modelo con controles
    
  #4. Comparacion modelos con controles analisis-----------------------------------
    

  #Tabla de modelo con controles
  stargazer(model_Age_wage_cont1,n_model_Age_wage_cont1,model_Age_wage_cont1_vt,
            keep = c("Edad_win", "Edad2"),
            align = TRUE, 
            dep.var.labels.include = FALSE,
            dep.var.caption  = "Log(Ingresos por hora)",        
            column.labels   = c("Con controles", "Sin obs. influyentes",  "Sin Valores atípicos"),
            covariate.labels = c("Edad", "Edad al cuadrado"),
            title = "", # Eliminar el título de la tabla
            float = FALSE, # Evitar que stargazer genere el entorno "table"
            type="latex",
            out = "modelos_edad_ingresos_controles.tex")  
  
  # Leer el contenido del archivo
  regression_table <- readLines("modelos_edad_ingresos_controles.tex")
  regression_table <- gsub("Observations", "Observaciones", regression_table) #Reemplazar "Observations" con "Observaciones"
  regression_table <- gsub("Adjusted R$^{2}$ ", "R$^{2}$ ajustado", regression_table)
  writeLines(regression_table, "modelos_edad_ingresos_controles.tex") #Escribir el contenido modificado de nuevo al archivo
  
 
  #5. Modelos para colocar en el documento ----------------------------------------
  
  stargazer(model_Age_wage, model_Age_wage_cont1_vt,
            keep = c("Edad_win", "Edad2"),
            align = TRUE, 
            dep.var.labels.include = FALSE,
            dep.var.caption = "Log(Ingresos por hora)",              
            column.labels = c("Sin Controles", "Con controles Sin valores atípicos"),
            covariate.labels = c("Edad", "Edad al cuadrado"),
            title = "", # Eliminar el título de la tabla
            float = FALSE, # Evitar que stargazer genere el entorno "table"
            type = "latex",
            out = "tabla_final.tex")
  
  # Leer el contenido del archivo
  regression_table <- readLines("tabla_final.tex")
  regression_table <- gsub("Observations", "Observaciones", regression_table) #Reemplazar "Observations" con "Observaciones"
  regression_table <- gsub("Adjusted R$^{2}$ ", "R$^{2}$ ajustado", regression_table)
  writeLines(regression_table, "tabla_final.tex") #Escribir el contenido modificado de nuevo al archivo


  #6. Valor maximo -----------------------------------------------------------
  
  #i. Modelo sin controles
  
    # Forma 1 - edad maxima 
  
      #Obtener la edad máxima en la que el incremento en el salario empieza a disminuir(SC)
      matrix_coef <- summary(model_Age_wage)$coefficients
      matrix_coef
      coeficiente_edad<-my_estimates <- matrix_coef[2, 1] 
      coeficiente_edad2<-my_estimates <- matrix_coef[3, 1] 
      coeficiente_edad2
    
      #Aplico la fórmula para obtener el "peak" de la edad 
      edad_peak<-(-coeficiente_edad/(2*coeficiente_edad2))
      edad_peak
    
    # Forma 2- edad maxima 
      
      Peak_age_fun <- function(age_1, age_2) {
        Edad_P<- -(age_1/(2*age_2))       
        return (Edad_P)
      }
      
      Peak_age_mod_simple <- Peak_age_fun(age_1 = model_Age_wage$coefficients[2],age_2 = model_Age_wage$coefficients[3])
      Peak_age_mod_simple
  
  #ii. Modelo con controles 
  
   # Forma 1 - edad maxima 
      
      #Obtener la edad máxima en la que el incremento en el salario empieza a disminuir(C)
      matrix_coef <- summary(model_Age_wage_cont1_vt)$coefficients
      matrix_coef
      coeficiente_edad_c<-my_estimates <- matrix_coef[2, 1] 
      coeficiente_edad_c2<-my_estimates <- matrix_coef[3, 1] 
      coeficiente_edad_c2
      #Aplico la fórmula para obtener el "peak" de la edad 
      edad_peak_C<-(-coeficiente_edad_c/(2*coeficiente_edad_c2))
      edad_peak_C
  
  #  Forma 2
      Peak_age_fun <- function(age_1, age_2) {
        Edad_P<- -(age_1/(2*age_2))
        return (Edad_P)  }
      
      Peak_age_mod_controles <- Peak_age_fun(age_1 = model_Age_wage_cont1_vt$coefficients[2],age_2 = model_Age_wage_cont1_vt$coefficients[3])
      Peak_age_mod_controles
      
  
  # 7. Calculo de los errores de los modelos ----------------------------------
  
    # Realiza predicciones con el modelo
    data_webs$predicted <- predict(model_Age_wage, newdata = data_webs) #modelo sencillo
    data_webs3$predicted_cont <- predict(model_Age_wage_cont1_vt, newdata = data_webs3)
      
    #Modelo predicho vs salario- sin controles
    graf1 <- ggplot(data_webs, aes(x = predicted, y = log_ing_h_win))+
      geom_point(color = "grey")+
      geom_abline(color ="red")+
      xlab("Predicción")+
      ylab("Log(ingresos por hora)")+
      theme_minimal()
    setwd(paste0(wd,"/Graficas"))
    png("model_simple.png") # Formato grafica
    graf1
    dev.off() # Cierra la grafica
    
  
    #Modelo predicho vs salario- con controles
    graf2 <- ggplot(data_webs3, aes(x = predicted_cont, y = log_ing_h_win))+
      geom_point(color = "grey")+
      geom_abline(color ="red")+
      xlab("Predicción")+
      ylab("Log(ingresos por hora)")+
      theme_minimal()
    setwd(paste0(wd,"/Graficas"))
    png("model_completo.png") # Formato grafica
    graf2
    dev.off() # Cierra la grafica
    
    
    # Calcular error Modelo simple
    err <- data_webs$log_ing_h_win - data_webs$predicted
    err2 <- err^2
    (rmse <-sqrt(mean(err2))) # 0.79
    
    # Calcular error Modelo Completo
    err_cont <- data_webs3$predicted_cont - data_webs3$log_ing_h_win
    err2_cont <- err_cont^2
    (rmse_cont <-sqrt(mean(err2_cont, na.rm = TRUE))) # 0.40
    
  
  # 8. BOOTSTRAP --------------------------------------------------------------


  #i. Modelo simple
  
    #Intervalos de confianza con bootstrap
    Age_Wage_Profile_fn<-function(data_webs,index){
      f <- lm(log_ing_h_win ~ Edad_win + Edad2, data = data_webs, subset= index)
      b1 <- f$coefficients[2]
      b2 <- f$coefficients[3]
      max_edad <- -(b1)/(2*b2)
      return(max_edad)  }
    
    #Verifiquemos que la función... funciona!
    Age_Wage_Profile_fn(data_webs,1:nrow(data_webs))
    
    #Se utiliza la funcion boot para estimar la regresion con bootstrap
    set.seed(2365)
    boot_results_mod_simple <- boot(data_webs, Age_Wage_Profile_fn, R = 1000)
    Dist_Peak_age_mod_simple <- as.data.frame(boot_results_mod_simple$t)
    hist(Dist_Peak_age_mod_simple$V1) #distribucion del valor maximo de la edad con bootstrap
    
    ## Histograma - Modelo Simple
    Hist_mod_simp <- ggplot(Dist_Peak_age_mod_simple, aes(x = V1)) +
      geom_histogram(bins = 50, color = "white", fill = "grey") +
      labs(x ='Edad', y='Frecuencia', title = "Modelo simple")+
      geom_vline(aes(xintercept = Peak_age_mod_simple), color = "red", linewidth = 1)+
      labs(title = "Panel A: Modelo sin controles",
           x = "Edad",
           y = "Ln(Ingreso por hora)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),      # Aumenta el tamaño del título
        axis.title.x = element_text(size = 20),                   # Aumenta el tamaño del label del eje X
        axis.title.y = element_text(size = 20)                    # Aumenta el tamaño del label del eje Y
      )  
    Hist_mod_simp
  
  #Grafica
  Age_wage_P_plot <- ggplot(data_webs, aes(x = Edad_win, y = log_ing_h_win)) +
    geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
    geom_line(aes(y = predicted, color = "Predicho"), linewidth = 1) +  # Línea para valores predichos
    scale_color_manual(values = c("Real" = "grey", "Predicho" = "red"),name="") +  # Colores de puntos y líneas
    labs(title = "Panel A:Modelo simple",
         x = "Edad",
         y = "Log(Ingreso por hora)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),      # Aumenta el tamaño del título
      axis.title.x = element_text(size = 20),                   # Aumenta el tamaño del label del eje X
      axis.title.y = element_text(size = 20)                    # Aumenta el tamaño del label del eje Y
    )
  print(Age_wage_P_plot)
  
  quantile(Dist_Peak_age_mod_simple$V1,0.025) #percentil 2.5 (47.16848)
  quantile(Dist_Peak_age_mod_simple$V1,0.975) #percentil 97.5 (49.35956)
  

  
  #ii. Modelo con controles
  
    #Intervalos de confianza con bootstrap
    Age_Wage_Profile_mod_cont<-function(data_webs3,index){
      f <- lm(log_ing_h_win ~ Edad_win + Edad2 + Mujer + estrato_factor + dummy_jefe + edu_factor + Tamanio_empresa + Trabajo_informal + Independiente + oficio_factor  + Horas_trabajadas + Experiencia_win, data = data_webs3, subset = index)
      b1 <- f$coefficients[2]
      b2 <- f$coefficients[3]
      max_edad <- -(b1)/(2*b2)
      return(max_edad)  }
    
    #Verifiquemos que la función... funciona!
    Age_Wage_Profile_mod_cont(data_webs3,1:nrow(data_webs3))
    
    #Se utiliza la funcion boot para estimar la regresion con bootstrap
    set.seed(2365)
    boot_results_mod_cont <- boot(data_webs3, Age_Wage_Profile_mod_cont, R = 1000)
    Dist_Peak_age_mod_cont <- as.data.frame(boot_results_mod_cont$t)
    hist(Dist_Peak_age_mod_cont$V1) #distribucion del valor maximo de la edad con bootstrap
    
    ## Histograma - Modelo con controles
    Hist_mod_cont <- ggplot(Dist_Peak_age_mod_cont, aes(x = V1)) +
      geom_histogram(bins = 50, color = "white", fill = "grey") +
      labs(x = 'Edad', y = 'Frecuencia', title = "Modelo con controles") +
      geom_vline(aes(xintercept = Peak_age_mod_cont), color = "red", linewidth = 1) +
      labs(title = "Panel B: Modelo con controles",
           x = "Edad",
           y = "Ln(Ingreso por hora)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),      # Aumenta el tamaño del título
        axis.title.x = element_text(size = 20),                   # Aumenta el tamaño del label del eje X
        axis.title.y = element_text(size = 20)                    # Aumenta el tamaño del label del eje Y
      )
    Hist_mod_cont
    
    
    quantile(Dist_Peak_age_mod_cont$V1,0.025) #percentil 2.5 (47.16848)
    quantile(Dist_Peak_age_mod_cont$V1,0.975) #percentil 97.5 (49.35956)
    
    Age_wage_P_plot_cont <- ggplot(data_webs3, aes(x = Edad_win, y = log_ing_h_win)) +
      geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
      geom_line(aes(y = predicted_cont, color = "Predicho"), linewidth = 0.8) +  # Línea para valores predichos
      scale_color_manual(values = c("Real" = "grey", "Predicho" = "red"),name="") +  # Colores de puntos y líneas
      labs(title = "Panel B: Modelo con controles",
           x = "Edad",
           y = "Ln(Ingreso por hora)") +
      theme_minimal()   +
      theme(
        plot.title = element_text(size = 20, face = "bold"),      # Aumenta el tamaño del título
        axis.title.x = element_text(size = 20),                   # Aumenta el tamaño del label del eje X
        axis.title.y = element_text(size = 20)                    # Aumenta el tamaño del label del eje Y
      )
    print(Age_wage_P_plot_cont)
    
    
    ### Exportar histogramas
    hist <-  Hist_mod_simp + Hist_mod_cont
    setwd(paste0(wd,"/Graficas"))
    png("hist_boot.png", width=1200, height=600) # Formato grafica
    hist
    dev.off() # Cierra la grafica
    
    ### Exportar preducciones
    plot_Age <- Age_wage_P_plot + Age_wage_P_plot_cont
    setwd(paste0(wd,"/Graficas"))
    png("Dispersion_boot.png",width=1200, height=600) # Formato grafica
    plot_Age
    dev.off() # Cierra la grafica