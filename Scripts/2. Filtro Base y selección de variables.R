# Author: Juliet Molano

#main script
#este código contiene todos los scripts utilizados para el problem set 1

# Prepare workspace


#se borra la memoria
rm(list = ls())
#se cargan los paquetes
library(pacman)
p_load(tidyverse , # tidy-data
       rio, # read/write data from almost any file format
       dtplyr,
       rvest, 
       readr,
       rio, # import/export data
       skimr, # summary data
       visdat, ## visualizing missing data
       corrplot, ## Correlation Plots
       stargazer,
       ggpubr,
       naniar) ## tables/output to TEX.



#Cargar base de datosl
data_webs = import(file = "C:/Users/JULIETH/Desktop/Big data/Bases taller 1/Tabla_webs.rds")
names(data_webs)
str(data_webs)
summary(data_webs$depto) ## corroborando que sea Bogotá
table(data_webs$ocu) ##La base tiene ocupados y no ocupados

############################################################################
#                 FILTRO DE LA BASE
############################################################################

#Filtrar la base para mayores de 18 años
data_webs <- data_webs %>%
  filter(age>=18 & ## Mayores de edad
           ocu==1) ## Empleados
nrow(data_webs)#16542 observaciones que cumplen con estas caracteristicas

############################################################################
#                 Seleccionar variables
############################################################################

#Seleccion de las variables de interes
data_webs <- data_webs %>%
  select(directorio, secuencia_p, orden,  # variables de referenciación
         age, sex, oficio, estrato1, p6050, # variables caracteristicas
         regSalud, cotPension, # Seguridad social
         relab, cuentaPropia, totalHoursWorked, sizeFirm, # caracteristicas empleo
         ingtot, ingtotob, y_salary_m, y_total_m, y_otros_m,  #ingresos
         fex_c, # factor de expansión
         hoursWorkUsual, formal, informal, y_ingLab_m_ha, y_total_m_ha,
         Experiencia=p6426,ingtot, ingtotes,maxEducLevel #Adicionales
  )%>%
  rename(
    Direccion = directorio,
    Secuencia = secuencia_p,
    Orden = orden,
    Edad = age,
    Sexo = sex,
    Profesion = oficio,
    Estrato = estrato1,
    Posicion_hogar = p6050,
    Regimen_salud = regSalud,
    Cotiza_pension = cotPension,
    Tipo_ocupacion = relab,
    Independiente = cuentaPropia,
    Horas_trabajadas = totalHoursWorked,
    Horas_trabajadas_sem =hoursWorkUsual,
    Tamaño_empresa = sizeFirm,
    Ingreso_Total = ingtot,
    Ingreso_observado = ingtotob,
    Salario_mensual = y_salary_m,
    Ingreso_total = y_total_m,
    Ingreso_hora = y_total_m_ha,
    Otros_ingresos = y_otros_m,
    Nivel_educ = maxEducLevel,
    Factor_expansion = fex_c)

############################################################################
#                 MISSINGS
############################################################################

#Missings variables de interes
data_table_missing <- data_webs %>% 
  select(Direccion, Secuencia, Orden, Edad, Sexo, Profesion, formal,
         Estrato, Independiente, Horas_trabajadas, Tamaño_empresa, 
         Ingreso_total, Ingreso_hora, Otros_ingresos)

# i. Grafica general
png("grafica_missing.png") # Formato grafica
vis_miss(data_table_missing) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 90, hjust = 1), # Coloca las etiquetas del eje Y en vertical
        plot.title = element_text(hjust = 0.5), # Centra el título
        plot.subtitle = element_text(hjust = 0.5)) # Centra el subtítulo
dev.off() # Cierra la grafica
#vis_dat(data_table_missing) # Opcion 2
#vis_miss(data_table_missing ,sort_miss = TRUE, cluster = TRUE) # Opcion 3


#ii. Tabla porcentaje de Missing values
db_miss <- skim(data_table_missing) %>% select(skim_variable, n_missing)
Nobs= nrow(data_webs) 
db_miss<- db_miss %>% filter(n_missing!= 0)
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
db_miss

## Visualizar las 40 variables con mas missings
ggplot(head(db_miss, 5), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 5))  # Set size for axis labels
#Variables que no so nde interes

#iii. Analizar nuevamente los missings de las variables seleccionadas
db_miss2 <- skim(data_webs) %>% select(skim_variable, n_missing)
Nobs= nrow(data_webs) 
db_miss2<- db_miss2 %>% filter(n_missing!= 0)
db_miss2<- db_miss2 %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
db_miss2

#Eliminar variable 
data_webs <- data_webs %>%
  select(-Otros_ingresos)

############################################################################
#             MANIPULACION VALORES FALTANTES
############################################################################

#i. Salario por hora

#Imputar el ingreso total por hora teniendo en cuenta el tipo de trabajador (asalariado e independiente) y por oficio
data_webs <- data_webs %>%
  group_by(Profesion, Independiente) %>%
  mutate(Ingreso_hora_imp = ifelse(is.na(Ingreso_hora), mean(Ingreso_hora, na.rm = TRUE), Ingreso_hora))
summary(data_webs$Ingreso_hora_imp)

#no existe valor promedio para imputar, por tanto se decide eliminar las 2 observaciones.
data_webs <- data_webs %>%
  filter(!is.na(Ingreso_hora_imp))
summary(data_webs$Ingreso_hora_imp)


#ii. Imputacion educacion

# calculating the most commun value of maxEducLevel. 
mode_edu <- as.numeric(names(sort(table(data_webs$Nivel_educ), decreasing = TRUE)[1]))

### Imputing the missing value. 
data_webs <- data_webs  %>%
  mutate(Nivel_educ = ifelse(is.na(Nivel_educ) == TRUE, mode_edu , Nivel_educ))
summary(data_webs$Nivel_educ)

#Crear la variable dummy
data_webs$dummy_jefe <- ifelse(data_webs$Posicion_hogar == 1, 1, 0)


#Valores atipicos de la var dep
up <- quantile(data_webs$Ingreso_hora_imp, 0.975, na.rm=T)

data_webs <- data_webs %>% mutate(Ingreso_hora_imp2=  ifelse( test=( Ingreso_hora_imp>= up), 
                                                yes= up,
                                                no= Ingreso_hora_imp ))

#################################
# CREACION DE VARIABLES NUEVAS
################################
data_webs$log_ing_h_imp=log(data_webs$Ingreso_hora_imp)
data_webs$log_ing_h_imp2=log(data_webs$Ingreso_hora_imp2)

data_webs <- data_webs %>%
  mutate( #logaritmo del salario por hora imputado
         Edad2 = Edad^2, #Edad al cuadrado 
         Experiencia_años = Experiencia/12)

#Descargar base de datos final
export(data_webs, "C:/Users/JULIETH/Desktop/Big data/Bases taller 1/base_final.rds")


#################################
#   VALOR ATIPICOS
################################


#ii. Graficas de cajas variables continuas

  ## Salarios por hora imputado - en logaritmo
  summary(data_webs$Ingreso_hora_imp) #Mirar minimos y maximos de edad
  box_ingr_h <- boxplot(data_webs$log_ing_h_imp,
                        main = "",      # Título
                        ylab = "log(ingreso por hora)",                  # Etiqueta eje Y
                        col = "gray",                          # Color de la caja gris
                        border = "black",                      # Color del borde negro
                        notch = TRUE,                          # Agregar muescas para intervalos de confianza
                        boxwex = 0.5,                          # Ancho de la caja
                        whisklty = 1,                          # Líneas sólidas en los bigotes
                        whiskcol = "darkgray",                 # Color de los bigotes
                        staplewex = 0.6,                       # Ancho de las grapas
                        outpch = 19,                           # Tipo de símbolo para outliers
                        outcol = "red",                        # Color de los outliers
                        cex.axis = 0.8,                        # Tamaño de la fuente para los números del eje
                        cex.lab = 0.8,                         # Tamaño de la fuente para etiquetas
                        cex.main = 0.8)                        # Tamaño de la fuente para el título

  summary(data_webs$Ingreso_hora_imp) #Mirar minimos y maximos de edad
  box_ingr_h <- boxplot(data_webs$log_ing_h_imp,
                        main = "",      # Título
                        ylab = "log(ingreso por hora)",                  # Etiqueta eje Y
                        col = "gray",                          # Color de la caja gris
                        border = "black",                      # Color del borde negro
                        notch = TRUE,                          # Agregar muescas para intervalos de confianza
                        boxwex = 0.5,                          # Ancho de la caja
                        whisklty = 1,                          # Líneas sólidas en los bigotes
                        whiskcol = "darkgray",                 # Color de los bigotes
                        staplewex = 0.6,                       # Ancho de las grapas
                        outpch = 19,                           # Tipo de símbolo para outliers
                        outcol = "red",                        # Color de los outliers
                        cex.axis = 0.8,                        # Tamaño de la fuente para los números del eje
                        cex.lab = 0.8,                         # Tamaño de la fuente para etiquetas
                        cex.main = 0.8)                        # Tamaño de la fuente para el título
  
  ## Edad
  summary(data_webs$Edad) #Mirar minimos y maximos de edad
  box_edad <- boxplot(data_webs$Edad,
                      main = "",      # Título
                      ylab = "Edad (años)",                  # Etiqueta eje Y
                      col = "gray",                          # Color de la caja gris
                      border = "black",                      # Color del borde negro
                      notch = TRUE,                          # Agregar muescas para intervalos de confianza
                      boxwex = 0.5,                          # Ancho de la caja
                      whisklty = 1,                          # Líneas sólidas en los bigotes
                      whiskcol = "darkgray",                 # Color de los bigotes
                      staplewex = 0.6,                       # Ancho de las grapas
                      outpch = 19,                           # Tipo de símbolo para outliers
                      outcol = "red",                        # Color de los outliers
                      ylim = c(0, 100),                      # Escala del eje Y de 0 a 100
                      cex.axis = 0.8,                        # Tamaño de la fuente para los números del eje
                      cex.lab = 0.8,                         # Tamaño de la fuente para etiquetas
                      cex.main = 0.8)                        # Tamaño de la fuente para el título
  
  ## Horas trabajadas
  summary(data_webs$Experiencia_años) #Mirar minimos y maximos de edad
  box_edad <- boxplot(data_webs$Experiencia_años,
                      main = "",      # Título
                      ylab = "Años experiencia",                  # Etiqueta eje Y
                      col = "gray",                          # Color de la caja gris
                      border = "black",                      # Color del borde negro
                      notch = TRUE,                          # Agregar muescas para intervalos de confianza
                      boxwex = 0.5,                          # Ancho de la caja
                      whisklty = 1,                          # Líneas sólidas en los bigotes
                      whiskcol = "darkgray",                 # Color de los bigotes
                      staplewex = 0.6,                       # Ancho de las grapas
                      outpch = 19,                           # Tipo de símbolo para outliers
                      outcol = "red",                        # Color de los outliers
                      cex.axis = 0.8,                        # Tamaño de la fuente para los números del eje
                      cex.lab = 0.8,                         # Tamaño de la fuente para etiquetas
                      cex.main = 0.8)                        # Tamaño de la fuente para el título

  ## Esperiencia
  summary(data_webs$ingreso_hora_imp) #Mirar minimos y maximos de edad
  box_ingr_h <- boxplot(data_webs$ingreso_hora_imp,
                      main = "",      # Título
                      ylab = "log(ingreso por hora)",                  # Etiqueta eje Y
                      col = "gray",                          # Color de la caja gris
                      border = "black",                      # Color del borde negro
                      notch = TRUE,                          # Agregar muescas para intervalos de confianza
                      boxwex = 0.5,                          # Ancho de la caja
                      whisklty = 1,                          # Líneas sólidas en los bigotes
                      whiskcol = "darkgray",                 # Color de los bigotes
                      staplewex = 0.6,                       # Ancho de las grapas
                      outpch = 19,                           # Tipo de símbolo para outliers
                      outcol = "red",                        # Color de los outliers
                      cex.axis = 0.8,                        # Tamaño de la fuente para los números del eje
                      cex.lab = 0.8,                         # Tamaño de la fuente para etiquetas
                      cex.main = 0.8)                        # Tamaño de la fuente para el título
  
  ## Horas trabajadas
  summary(data_webs$Horas_trabajadas) #Mirar minimos y maximos de edad
  box_ingr_h <- boxplot(data_webs$Horas_trabajadas,
                        main = "",      # Título
                        ylab = "Horas trabajadas",                  # Etiqueta eje Y
                        col = "gray",                          # Color de la caja gris
                        border = "black",                      # Color del borde negro
                        notch = TRUE,                          # Agregar muescas para intervalos de confianza
                        boxwex = 0.5,                          # Ancho de la caja
                        whisklty = 1,                          # Líneas sólidas en los bigotes
                        whiskcol = "darkgray",                 # Color de los bigotes
                        staplewex = 0.6,                       # Ancho de las grapas
                        outpch = 19,                           # Tipo de símbolo para outliers
                        outcol = "red",                        # Color de los outliers
                        cex.axis = 0.8,                        # Tamaño de la fuente para los números del eje
                        cex.lab = 0.8,                         # Tamaño de la fuente para etiquetas
                        cex.main = 0.8)                        # Tamaño de la fuente para el título
  
 

# Tratar valor atipicos identificados - criterios
  
  #Ingreso 
  

############################################################################
#    Estadisticas descriptivas
############################################################################

## Pasar los datos a dataframe
data_webs <- as.data.frame(data_webs)

## --- Variables cuantitativas importantes - tablas descriptivas ---- ##
des_vars= c("Edad","Horas_trabajadas", "log_ing_h_imp")
stargazer(data_webs[des_vars], type = "text", title="Estadísticas Descriptivas", digits=1, out="Tabla_Est_descriptivas.txt")
stargazer(data_webs[des_vars], digits=1)


## b. Variables Cuantitativas importantes

# i. Edad y sexo
# Igual promedio de edad en hombres y mujeres - 39 años 
data_webs %>% 
  group_by(Sexo) %>% 
  summarise(mean(Edad, na.rm = TRUE))

# ii. Horas trabajadas y sexo
# Las mujeres trabajan menos tiempo 
data_webs %>% #semana
  group_by(Sexo) %>% 
  summarise(mean(Horas_trabajadas, na.rm = TRUE))

data_webs %>% #mes
  group_by(Sexo) %>% 
  summarise(mean(Horas_trabajadas*4, na.rm = TRUE))


# iii. Max educación 
# No es claro las categorias de educación
Matrix_summary <- summary(as.factor(data_webs$Nivel_educ))
#dotchart(Matrix_summary)

max_educ <- as.data.frame(Matrix_summary)
max_educ$Educ <- c("A", "B", "C", "D", "E", "F", "G")

ggdotchart(max_educ, x = "Educ", y = "Matrix_summary",
  add = "segments",
  ylab = "Número de individuos",
  xlab = "Nivel de educación",
  rotate = TRUE)

#####################################
#    GRAFICOS DE DISPERSION
#####################################

#i. Ingreso
  density_plot_ing <- ggplot(data = data_webs, aes(x = log_ing_h_imp)) +
  geom_density(fill = "grey", alpha = 0.5) +  # Rellena la curva de densidad
  labs(title = "Gráfico de Densidad para el Ingreso", x = "Ingreso", y = "Densidad") +
  theme_minimal()
density_plot_ing

# Supongamos que tu dataframe se llama "data" y tienes una columna llamada "Ingreso"

# Crear un histograma básico de la distribución del Ingreso
hist(data$Ingreso, 
     main = "Distribución del Ingreso",  # Título de la gráfica
     xlab = "Ingreso",                   # Etiqueta del eje X
     ylab = "Frecuencia",                # Etiqueta del eje Y
     col = "lightblue",                  # Color de las barras
     border = "black",                   # Color del borde de las barras
     breaks = 30)                        # Número de barras (ajustable)
Código para un gráfico de densidad:
  r
Copiar código
# Gráfico de densidad de la variable Ingreso
plot(density(data$Ingreso, na.rm = TRUE),  # Calcula la densidad, omitiendo valores NA
     main = "Densidad de la Distribución del Ingreso",  # Título del gráfico
     xlab = "Ingreso",                     # Etiqueta del eje X
     ylab = "Densidad",                    # Etiqueta del eje Y
     col = "blue",                         # Color de la línea
     lwd = 2)                              # Grosor de la línea
Código para combinar ambos (histograma y densidad):
  Si quieres una gráfica más completa que combine tanto el histograma como la curva de densidad, puedes hacer algo como esto:
  
  r
Copiar código
# Crear un histograma con una curva de densidad superpuesta
hist(data$Ingreso, 
     prob = TRUE,                         # Esto ajusta la escala del histograma para que se superponga la densidad
     main = "Distribución del Ingreso con Curva de Densidad",  
     xlab = "Ingreso",                   
     ylab = "Densidad",                  
     col = "lightblue",                  
     border = "black",                   
     breaks = 30)                       

# Añadir la curva de densidad al histograma
lines(density(data$Ingreso, na.rm = TRUE),   # Superponer la curva de densidad
      col = "red",                           # Color de la línea de densidad
      lwd = 2)                               # Grosor de la línea

#i. ingresos y edad - Grafico de dispersion


#i. ingresos por sexo - Grafico de cajas




