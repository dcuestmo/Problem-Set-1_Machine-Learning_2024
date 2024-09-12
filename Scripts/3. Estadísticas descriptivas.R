# Author: Juliet Molano

#------------------------------------------------------------------------------#
#--------------Script 3. Estadisticas descriptivas ----------------------------#
#------------------------------------------------------------------------------#

#1. Cargar base de datos -------------------------------------------------------

setwd(paste0(wd,"/Base_Datos"))
data_webs <- import(file = "base_final.rds")
data_webs <- as.data.frame(data_webs)
names(data_webs)

#2. Grafico de correlacion de las variables continuas -------------------------

subset_data <- data_webs[, c("log_ing_h_imp", "Edad", 
                             "Sexo", "dummy_jefe", 
                             "formal", "Horas_trabajadas", 
                             "Experiencia_años", "Independiente")]

#3. Calcular la matriz de correlación para esas variables
cor_matrix <- cor(subset_data, use="complete.obs")
print(cor_matrix)
corrplot(cor_matrix, 
         method = "color",           # Método "color" para un heatmap
         type = "upper",             # Solo mostrar la mitad superior del gráfico
         tl.cex = 0.8,               # Tamaño de los labels
         tl.col = "black",           # Color de los labels (negro en este caso)
         tl.srt = 45,                # Rotar las etiquetas 45 grados
         addCoef.col = "black",      # Color de los coeficientes numéricos
         number.cex = 0.7)           # Tamaño de los números que muestran los coeficientes
cor_matrix


#4. Tabla de variables principales
des_vars= c("Ingreso_hora_imp", "Edad", 
            "Sexo", "formal",
            "Horas_trabajadas", "Experiencia_años", 
            "Independiente", "dummy_jefe")
stargazer(data_webs[des_vars], type = "text") 
stargazer(data_webs[des_vars], type = "text", title="Estadísticas Descriptivas", digits=1, out="Tabla_Est_descriptivas.txt")
stargazer(data_webs[des_vars], digits=1)


#5. Graficas de distribucion

#i. Ingreso
media_ingreso <- mean(data_webs$Ingreso_hora_imp, na.rm = TRUE)
density_plot_ing <- ggplot(data = data_webs, aes(x = Ingreso_hora_imp)) +
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
media_edad <- mean(data_webs$Edad, na.rm = TRUE)
density_plot_edad <- ggplot(data = data_webs, aes(x = Edad)) +
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
media_h <- mean(data_webs$Horas_trabajadas, na.rm = TRUE)
density_plot_h <- ggplot(data = data_webs, aes(x = Horas_trabajadas)) +
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
media_exp <- mean(data_webs$Experiencia_años, na.rm = TRUE)
density_plot_exp <- ggplot(data = data_webs, aes(x = Experiencia_años)) +
  geom_density(fill = "grey", alpha = 0.5) +  # Rellena la curva de densidad
  labs(title = "Gráfico de Densidad Experiencia", x = "Experiencia", y = "Densidad") +
  geom_vline(aes(xintercept = media_exp), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
  )
density_plot_exp

## Graficas por sexo - entrepuestas
box_plot <- ggplot(data = data_webs, aes(x = factor(Sexo), y = log_ing_h_imp, fill = as.factor(Sexo))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), labels = c("Mujer", "Hombre")) +
  labs(title = "Distribución del Ingreso por Hora según Sexo", x = "Sexo", y = "Ingreso por hora", fill = "Sexo") +
  scale_x_discrete(labels = c("0" = "Mujer", "1" = "Hombre")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10)  # Cambia el tamaño y estilo del título
  )
box_plot