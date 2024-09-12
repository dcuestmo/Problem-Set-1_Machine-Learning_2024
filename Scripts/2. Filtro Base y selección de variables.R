# Author: Juliet Molano


#------------------------------------------------------------------------------#
#---------Script 2. Filtro y seleccion de variables----------------------------#
#------------------------------------------------------------------------------#

# 1. Cargar base de datos  -----------------------------------------------------

setwd(paste0(wd,"/Base_Datos")) #DirectorioS
data_webs = import(file = "Tabla_Final_GEIH.csv")

names(data_webs) #Mirar nombres de las variables
str(data_webs) #Mirar bases de datos
summary(data_webs$depto) ## corroborando que sea Bogotá
table(data_webs$ocu) ##La base tiene ocupados y no ocupados
skim(data_webs) %>% head()

# 2. Filtrar la base de datos ---------------------------------------------------

#Filtrar la base para mayores de 18 años
data_webs <- data_webs %>%
  filter(age>=18 & ## Mayores de edad
           ocu==1) ## Empleados
nrow(data_webs) #16542 observaciones que cumplen con estas caracteristicas

# 3. Seleccion de las variables de interes --------------------------------------

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
    Salario_mensual = y_salary_m,
    Ingreso_total = y_total_m,
    Ingreso_hora = y_total_m_ha,
    Otros_ingresos = y_otros_m,
    Nivel_educ = maxEducLevel,
    Factor_expansion = fex_c)

                
# 4. Analisis de missings ------------------------------------------------------

# i. Missings variables de interes
data_table_missing <- data_webs %>% 
  select(Direccion, Secuencia, Orden, Edad, Sexo, Profesion, formal,
         Estrato, Independiente, Horas_trabajadas, Tamaño_empresa, 
         Ingreso_total, Ingreso_hora, Otros_ingresos)

## Grafica general
setwd(paste0(wd,"/Graficas"))
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

## Visualizar las 5 variables con mas missings
ggplot(head(db_miss, 5), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 5))  # Set size for axis labels

#Eliminar variable otros ingresos - muchos missings
data_webs <- data_webs %>%
  select(-Otros_ingresos)


# 5. Analisis de missings ------------------------------------------------------

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

# calcular la educacion de moda. 
mode_edu <- as.numeric(names(sort(table(data_webs$Nivel_educ), decreasing = TRUE)[1]))

### Imputar el valor missing. 
data_webs <- data_webs  %>%
  mutate(Nivel_educ = ifelse(is.na(Nivel_educ) == TRUE, mode_edu , Nivel_educ))
summary(data_webs$Nivel_educ)

#Crear la variable dummy jefe de hogar
data_webs$dummy_jefe <- ifelse(data_webs$Posicion_hogar == 1, 1, 0)
data_webs <- data_webs %>% 
  mutate(Experiencia_años = Experiencia/12)

# 5. Analisis de valores atipicos -----------------------------------------------


#i. Graficas de cajas variables continuas

  ## Salarios por hora imputado - en logaritmo
  summary(data_webs$Ingreso_hora_imp) #Mirar minimos y maximos de edad
  box_ingr_h <- boxplot(data_webs$Ingreso_hora_imp,
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
  box_ingr_h
  setwd(paste0(wd,"/Graficas"))
  ggsave("box_ingr_h", plot = box_ingr_h)
  
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
  box_edad
  setwd(paste0(wd,"/Graficas"))
  ggsave("box_edad", plot = box_edad)
  
  ## Experiencia
  summary(data_webs$Experiencia_años) #Mirar minimos y maximos de edad
  box_exp <- boxplot(data_webs$Experiencia_años,
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
  box_exp
  setwd(paste0(wd,"/Graficas"))
  ggsave("box_exp.png", plot = box_edad)
  

  ## Horas trabajadas
  summary(data_webs$Horas_trabajadas) #Mirar minimos y maximos de edad
  box_horas <- boxplot(data_webs$Horas_trabajadas,
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
  box_horas
  setwd(paste0(wd,"/Graficas"))
  ggsave("box_exp.png", plot = box_horas)
  
  #Unir las gráficas en sola
  (box_ingr_h | box_edad)/(box_exp | box_horas)
  
  
 
  #ii. Tramiento valores atipicos
  
  #. Ingreso por hora
  up <- quantile(data_webs$Ingreso_hora_imp, 0.975, na.rm=T)
  data_webs <- data_webs %>% mutate(Ingreso_hora_imp2=  ifelse( test=( Ingreso_hora_imp>= up), 
                                                                yes= up,
                                                                no= Ingreso_hora_imp ))
  #. Edad
  
  
  #. Experiencia
  
  
  
  # 6. Creacion variables nuevas -----------------------------------------------
  
  data_webs$log_ing_h_imp=log(data_webs$Ingreso_hora_imp) # con valores atipicos
  data_webs$log_ing_h_imp2=log(data_webs$Ingreso_hora_imp2) #sin valores atipicos
  
  data_webs <- data_webs %>%
    mutate( #logaritmo del salario por hora imputado
      Edad2 = Edad^2, #Edad al cuadrado 
      Experiencia_años = Experiencia/12)
  
  #Descargar base de datos final
  setwd(paste0(wd,"/Base_Datos"))
  export(data_webs, "base_final.rds")
  
  
