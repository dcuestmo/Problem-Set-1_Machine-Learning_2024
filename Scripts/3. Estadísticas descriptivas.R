#------------------------------------------------------------------------------#
#--------------Script 3. Estadisticas descriptivas ----------------------------#
#------------------------------------------------------------------------------#

#1. Cargar base de datos -------------------------------------------------------

setwd(paste0(wd,"/Base_Datos"))
data_webs <- import(file = "base_final.rds")
data_webs <- as.data.frame(data_webs)
names(data_webs)

#2. Grafico de correlacion de las variables continuas -------------------------

subset_data <- data_webs[, c("log_ing_h_win", "Edad_win", 
                             "Mujer", "dummy_jefe", 
                             "Trabajo_informal", "Horas_trabajadas_win", 
                             "Experiencia_win", "Independiente")]

#3. Calcular la matriz de correlación para esas variables
setwd(paste0(wd,"/Graficas"))
png("graf_corr.png") # Formato grafica
cor_matrix <- cor(subset_data, use="complete.obs")
print(cor_matrix)
corrplot(cor_matrix,
         tl.cex = 0.8,               # Tamaño de los labels
         tl.col = "black",           # Color de los labels (negro en este caso)
         tl.srt = 90,                # Rotar las etiquetas 45 grados
         addCoef.col = "black",      # Color de los coeficientes numéricos
         number.cex = 0.7)           # Tamaño de los números que muestran los coeficientes
cor_matrix
dev.off() # Cierra la grafica


#4. Tabla de variables principales
des_vars= c("Ingreso_hora_imp_win", "Edad_win", 
            "Mujer", "Trabajo_informal",
            "Horas_trabajadas_win", "Experiencia_win", 
            "Independiente", "dummy_jefe")
stargazer(data_webs[des_vars], type = "text") 
stargazer(data_webs[des_vars], type = "text", title="Estadísticas Descriptivas", digits=1, out="Tabla_Est_descriptivas.txt")
stargazer(data_webs[des_vars], digits=1)


#5. Graficas de distribucion generales ----------------------------------------

#i. Ingreso
media_ingreso <- mean(data_webs$Ingreso_hora_imp_win, na.rm = TRUE)
density_plot_ing <- ggplot(data = data_webs, aes(x = Ingreso_hora_imp_win)) +
  geom_density(fill = "grey", alpha = 0.5) +  # Rellena la curva de densidad
  labs(title = "Gráfico de Densidad Ingreso por hora", x = "Ingreso", y = "Densidad") +
    geom_vline(aes(xintercept = media_ingreso), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
  )
density_plot_ing


# ii. Edad
media_edad <- mean(data_webs$Edad_win, na.rm = TRUE)
density_plot_edad <- ggplot(data = data_webs, aes(x = Edad_win)) +
  geom_density(fill = "grey", alpha = 0.5) +  # Rellena la curva de densidad
  labs(title = "Gráfico de Densidad Edad", x = "Edad", y = "Densidad") +
  geom_vline(aes(xintercept = media_edad), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
  )
density_plot_edad

#iii. Horas trabajadas
media_h <- mean(data_webs$Horas_trabajadas_win, na.rm = TRUE)
density_plot_h <- ggplot(data = data_webs, aes(x = Horas_trabajadas_win)) +
  geom_density(fill = "grey", alpha = 0.5) +  # Rellena la curva de densidad
  labs(title = "Gráfico de Densidad Horas trabajadas", x = "Horas trabajadas", y = "Densidad") +
  geom_vline(aes(xintercept = media_h), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
  )
density_plot_h

#iv. Experiencia
media_exp <- mean(data_webs$Experiencia_win, na.rm = TRUE)
density_plot_exp <- ggplot(data = data_webs, aes(x = Experiencia_win)) +
  geom_density(fill = "grey", alpha = 0.5) +  # Rellena la curva de densidad
  labs(title = "Gráfico de Densidad Experiencia", x = "Experiencia", y = "Densidad") +
  geom_vline(aes(xintercept = media_exp), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
  )
density_plot_exp

##Combinar graficas
densidad <- (density_plot_ing + density_plot_edad)/(density_plot_h + density_plot_exp)
setwd(paste0(wd,"/Graficas"))
png("Densidad.png") # Formato grafica
densidad
dev.off() # Cierra la grafica

#6. Graficas de distribucion por sexo ----------------------------------------

data_webs$sex <- factor(data_webs$Mujer, levels = c(1, 0), labels = c("Mujer", "Hombre"))

## Graficas por Genero - entrepuestas
box_plot <- ggplot(data = data_webs, aes(x = sex, y = Ingreso_hora_imp_win, fill = sex)) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("Mujer" = "red", "Hombre" = "blue"), labels = c("Mujer", "Hombre")) +
  labs(title = "", x = "Género", y = "Ingreso por hora (Miles de pesos)", fill = "sex") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10)  # Cambia el tamaño y estilo del título
  )
png("Caja_sexo.png") # Formato grafica
box_plot
dev.off() # Cierra la grafica


#Graficas de distribucion del ingreso 
den_plot <- ggplot(data_webs, aes(Ingreso_hora_imp_win, fill=sex)) +
  geom_density(alpha = 0.4) +  # Densidad superpuesta con transparencia
  theme_minimal() +  # Tema minimalista
  labs(title = "Gráfico de densidad del ingreso por hora según género",
       x = "Ingreso por hora",
       y = "Densidad",
       fill = "Sexo") +
  geom_vline(aes(xintercept = mean(Ingreso_hora_imp_win[sex == "Hombre"])), color = "blue", linetype = "dashed") +  # Línea punteada para la media de hombres
  geom_vline(aes(xintercept = mean(Ingreso_hora_imp_win[sex == "Mujer"])), color = "red", linetype = "dashed") +   # Línea punteada para la media de mujeres
  guides(fill = guide_legend(title = NULL)) +  # Elimina el título de la leyenda
  theme(
    plot.title = element_text(size = 10)  # Cambia el tamaño y estilo del título
  )
png("Caja_sexo.png") # Formato grafica
den_plot
dev.off() # Cierra la grafica


