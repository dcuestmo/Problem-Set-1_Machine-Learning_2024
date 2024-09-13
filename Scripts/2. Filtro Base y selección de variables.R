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
  select(directorio, secuencia_p, orden,  #variables de identificacion
         age, sex, oficio, estrato1, p6050, maxEducLevel,  #caracteristicas socioeconomicas
         relab, cuentaPropia, totalHoursWorked, sizeFirm, hoursWorkUsual, 
         y_otros_m, y_total_m, y_total_m_ha, fex_c, 
         formal, informal, p6426
  ) %>%
  rename(
    Direccion = directorio,
    Secuencia = secuencia_p,
    Orden = orden,
    Edad = age,
    Sexo = sex,
    Profesion = oficio,
    Estrato = estrato1,
    Posicion_hogar = p6050,
    Tipo_ocupacion = relab,
    Independiente = cuentaPropia,
    Horas_trabajadas = totalHoursWorked,
    Horas_trabajadas_sem = hoursWorkUsual,
    Tamaño_empresa = sizeFirm,
    Ingreso_total = y_total_m,
    Ingreso_hora = y_total_m_ha,
    Otros_ingresos = y_otros_m,
    Nivel_educ = maxEducLevel,
    Experiencia = p6426,
    Factor_expansion = fex_c
  )

#Crear la variable dummy jefe de hogar y experiencia en años
data_webs$dummy_jefe <- ifelse(data_webs$Posicion_hogar == 1, 1, 0)
data_webs <- data_webs %>% 
  mutate(Experiencia_años = Experiencia/12)
                
# 4. Analisis de missings ------------------------------------------------------

# i. Missings variables de interes
data_table_missing <- data_webs %>% 
  select(Direccion, Secuencia, Orden, Edad, Sexo, Profesion, formal,
         Estrato, Independiente, Horas_trabajadas, Tamaño_empresa, 
         Ingreso_total, Ingreso_hora, Otros_ingresos, Experiencia_años, dummy_jefe)

## Grafica general
setwd(paste0(wd,"/Graficas"))
png("grafica_missing.png") # Formato grafica
m1 <-vis_miss(data_table_missing) +
  theme(axis.text.y = element_text(angle = 90), # Coloca las etiquetas del eje Y en vertical
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5), # Centra el título
        plot.subtitle = element_text(hjust = 0.5)) # Centra el subtítulo
m1
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
setwd(paste0(wd,"/Graficas"))
png("graf_missing_var_prin.png") # Formato grafica
m2 <- ggplot(head(db_miss, 5), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "grey", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Missings de variables", x = "Variables", y = "Missings") +
  theme(axis.text = element_text(size = 8)) + 
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
  )
m2
dev.off() # Cierra la grafica

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


# 5. Analisis de valores atipicos -----------------------------------------------


#i. Graficas de cajas variables continuas

  ## Salarios por hora imputado - en logaritmo
  summary(data_webs$Ingreso_hora_imp) #Mirar minimos y maximos de edad

    #Ingreso laboral
    b1 <- ggplot(data_webs, aes(x = "", y = Ingreso_hora_imp)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Ingreso laboral por hora", x = "", y = "Miles de pesos") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
      )
    b1

  
    #Ingreso laboral
    b2 <- ggplot(data_webs, aes(x = "", y = Edad)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Edad", x = "", y = "años") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
      )
    b2
    

  ## Experiencia
    b3 <- ggplot(data_webs, aes(x = "", y = Experiencia_años)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Experiencia", x = "", y = "años") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
      )
    b3
    

  ## Horas trabajadas
  summary(data_webs$Horas_trabajadas) #Mirar minimos y maximos de edad
    b4 <- ggplot(data_webs, aes(x = "", y = Horas_trabajadas)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Horas trabajadas", x = "", y = "Horas") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamaño y estilo del título
      )
    b4
    
  
  # Combinar los gráficos en una cuadrícula
    setwd(paste0(wd,"/Graficas"))
    png("graf_cajas") # Formato grafica
    box <- (b1+b2)/(b3+b4)
    box
    dev.off() # Cierra la grafica
    

  #ii. Tramiento valores atipicos
  
  #. Ingreso por hora
  up <- quantile(data_webs$Ingreso_hora_imp, 0.975, na.rm=T)
  data_webs <- data_webs %>% mutate(Ingreso_hora_imp2=  ifelse( test=( Ingreso_hora_imp>= up), 
                                                                yes= up,
                                                                no= Ingreso_hora_imp ))
  #. Edad
  
  
  #. Experiencia
  
  #.Horas
  
  
  # 6. Creacion variables nuevas -----------------------------------------------
  
  data_webs$log_ing_h_imp=log(data_webs$Ingreso_hora_imp) # con valores atipicos
  data_webs$log_ing_h_imp2=log(data_webs$Ingreso_hora_imp2) #sin valores atipicos
  data_webs <- data_webs %>% mutate (oficio_factor= as.factor(Profesion))
  data_webs <- data_webs %>% mutate (edu_factor= as.factor(Nivel_educ))

  data_webs <- data_webs %>%
    mutate( #logaritmo del salario por hora imputado
      Edad2 = Edad^2, #Edad al cuadrado 
      Experiencia_años = Experiencia/12)
  
  #Descargar base de datos final
  setwd(paste0(wd,"/Base_Datos"))
  export(data_webs, "base_final.rds")
  
  
