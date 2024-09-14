#------------------------------------------------------------------------------#
#-------------------- Script 2. Filtro y seleccion de variables----------------#
#------------------------------------------------------------------------------#

# 1. Cargar base de datos  -----------------------------------------------------

setwd(paste0(wd,"/Base_Datos")) #DirectorioS
data_webs = import(file = "Tabla_Final_GEIH.csv")

names(data_webs)                     # Mirar nombres de las variables
str(data_webs)                       # Mirar tipo de variables
unique(data_webs$dominio)            # corroborando que sea Bogota
table(data_webs$ocu)                 # La base tiene ocupados (1) y no ocupados (0)
skim(data_webs) %>% head()           # Se observa que desde un principio existen variables, como p550
                                     # y y_gananciaNetaAgro_m, contienen muchos missing y se pueden omitir

# 2. Filtrar la base de datos ---------------------------------------------------

# Como menciona el enunciado, filtrar la base para utilizar solo los ocupados mayores de 18 anios
data_webs <- data_webs %>%
  filter(age>=18 & ## Mayores de edad
           ocu==1) ## Empleados
nrow(data_webs) #16542 observaciones que cumplen con estas caracteristicas

# 3. Seleccion de las variables de interes --------------------------------------

data_webs <- data_webs %>%
  dplyr::select(
    directorio,   # Llave de vivienda
    secuencia_p,  # Llave de hogar
    orden,        # Llave de persona
    age,          # Edad
    sex,          # Sexo  
    oficio,       # Ocupacion 
    estrato1,     # Estrato
    p6050,        # Parentezco con jefe de hogar
    maxEducLevel, # Maximo nivel de eduacion  
    relab,        # Tipo de ocupacion
    cuentaPropia, # 1 si trabaja por  cuenta propia; 0 otro caso
    totalHoursWorked, # Horas trabajadas la semana pasada
    sizeFirm,         # Tam?o de la firma
    hoursWorkUsual,   # Horas semanales trabajadas usualmente en ocuapcion principal  
    y_otros_m,        # Ingreso laboral en especie (otros) - nominal mensual - ocupaci?n principal
    y_total_m,        # Ingreso total de asalariados + independientes - nominal mensual
    y_total_m_ha,     # Ingreso total de asalariados + independientes - nominal por hora
    fex_c,            # Factor de expansi?n anualizado
    formal,           # Trabajo formal de acuerdo a seguridad social (1), otro caso (0)
    informal,         # Trabajo informal de acuerdo a seguridad social (1), otro caso (0)
    p6426,            # Tiempo trabajando en la empresa
    ingtot,           # Ingreso total 
    ingtotes,         # Ingreso total imputado  
    ingtotob,         # Ingreso total observado  
    y_salary_m,       # Salario - nominal mensual - ocupaci?n principal (incluye propinas y comisiones)
    regSalud,         # Regimen de salud
    cotPension        # El trabajador cotiza a pension
    
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
    Tipo_ocupacion = relab,
    Independiente = cuentaPropia,
    Horas_trabajadas = totalHoursWorked,
    Horas_trabajadas_sem = hoursWorkUsual,
    Tamanio_empresa = sizeFirm,
    Ingreso_total = y_total_m,
    Ingreso_hora = y_total_m_ha,
    Otros_ingresos = y_otros_m,
    Nivel_educ = maxEducLevel,
    Experiencia = p6426,
    Factor_expansion = fex_c,
    Trabajo_formal = formal,
    Trabajo_informal = informal,
    Ingreso_total_2 = ingtot,
    Ingreso_total_imputado = ingtotes,
    Ingreso_total_observado = ingtotob,
    Salario_ocupacion_principal_mensual = y_salary_m,
    Regimen_salud = regSalud,
    Cotiza_pension = cotPension,
  )

#Crear la variable dummy jefe de hogar y experiencia en anios

data_webs$dummy_jefe <- ifelse(data_webs$Posicion_hogar == 1, 1, 0)
data_webs <- data_webs %>% 
  mutate(Experiencia_anios = Experiencia/12)
                
# 4. Analisis de missings values -----------------------------------------------

# 4.1 . Missings variables de interes ------------------------------------------
data_table_missing <- data_webs %>% 
  dplyr::select(Direccion, Secuencia, Orden, Edad, Sexo, Profesion, Trabajo_formal,
         Estrato, Independiente, Horas_trabajadas, Tamanio_empresa, Nivel_educ, 
         Ingreso_total, Ingreso_hora, Otros_ingresos, Experiencia_anios,
         dummy_jefe,Trabajo_informal)

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


# 4.2. Tabla porcentaje de Missing values --------------------------------------
db_miss <- skim(data_table_missing) %>% dplyr::select(skim_variable, n_missing)
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
  labs(title = "", x = "Variables", y = "Missings") +
  theme(axis.text = element_text(size = 10)) + 
  theme(
    plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
  )
m2
dev.off() # Cierra la grafica

# Eliminar variable "otros ingresos" dado a que la mayor proporcion de sus valores son missings
data_webs <- data_webs %>%
  dplyr::select(-Otros_ingresos)


# 5. Analisis de missings ------------------------------------------------------

# 5.1 . Salario por hora -------------------------------------------------------

#Imputar el ingreso total por hora promedio teniendo en cuenta el tipo de trabajador (asalariado e independiente) y por oficio

data_webs <- data_webs %>%
  group_by(Profesion, Independiente) %>%
  mutate(Ingreso_hora_imp = ifelse(is.na(Ingreso_hora), mean(Ingreso_hora, na.rm = TRUE), Ingreso_hora))
summary(data_webs$Ingreso_hora_imp)

# No existe valor promedio para imputar, por tanto se decide eliminar las 2 observaciones.

data_webs <- data_webs %>%
  filter(!is.na(Ingreso_hora_imp))
summary(data_webs$Ingreso_hora_imp)

# 5.2. Imputacion educacion ----------------------------------------------------

# Calcular la educacion de moda. 
mode_edu <- as.numeric(names(sort(table(data_webs$Nivel_educ), decreasing = TRUE)[1]))

### Imputar el valor missing. 
data_webs <- data_webs  %>%
  mutate(Nivel_educ = ifelse(is.na(Nivel_educ) == TRUE, mode_edu , Nivel_educ))
summary(data_webs$Nivel_educ)

# 6. Analisis de valores atipicos -----------------------------------------------

# 6.1. Graficas de cajas variables continuas

  ## Salarios por hora imputado 
  summary(data_webs$Ingreso_hora_imp) 

    #Ingreso laboral
    b1 <- ggplot(data_webs, aes(x = "", y = Ingreso_hora_imp)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Ingreso laboral por hora", x = "", y = "Miles de pesos") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
      )
    b1

    # Edad
    summary(data_webs$Edad)
    b2 <- ggplot(data_webs, aes(x = "", y = Edad)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Edad", x = "", y = "anios") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
      )
    b2
    
   # Experiencia
    summary(data_webs$Experiencia_anios)
        b3 <- ggplot(data_webs, aes(x = "", y = Experiencia_anios)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Experiencia", x = "", y = "anios") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
      )
    b3
    
  # Horas trabajadas
  summary(data_webs$Horas_trabajadas) 
    b4 <- ggplot(data_webs, aes(x = "", y = Horas_trabajadas)) +
      geom_boxplot() +
      theme_gray() +
      labs(title = "Horas trabajadas", x = "", y = "Horas") +
      theme(axis.text = element_text(size = 8)) + 
      theme(
        plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
      )
    b4
    
  # Combinar los graficos en una cuadricula
    setwd(paste0(wd,"/Graficas"))
    png("graf_cajas") # Formato grafica
    box <- (b1+b2)/(b3+b4)
    box
    dev.off() # Cierra la grafica
    
# 6.2. Tramiento valores atipicos ----------------------------------------------

  # Ingreso por hora
  up <- quantile(data_webs$Ingreso_hora_imp, 0.99, na.rm=T)
  data_webs <- data_webs %>% mutate(Ingreso_hora_imp_win=  ifelse( test=( Ingreso_hora_imp>= up), 
                                                                yes= up,
                                                                no= Ingreso_hora_imp))
  # Edad 
  up <- quantile(data_webs$Edad, 0.99, na.rm=T)
  data_webs <- data_webs %>% mutate(Edad_win =  ifelse( test=( Edad>= up), 
                                                                yes= up,
                                                                no= Edad))
  # Experiencia 
  up <- quantile(data_webs$Experiencia, 0.99, na.rm=T)
  data_webs <- data_webs %>% mutate(Experiencia_win =  ifelse( test=( Experiencia>= up), 
                                                   yes= up,
                                                   no= Experiencia_anios))
  # Horas Trabajadas
  up <- quantile(data_webs$Horas_trabajadas, 0.99, na.rm=T)
  data_webs <- data_webs %>% mutate(Horas_trabajadas_win =  ifelse( test=( Horas_trabajadas>= up), 
                                                   yes= up,
                                                   no= Horas_trabajadas))

#Mirar nuevamente las graficas de dispersion
  
  #Ingreso laboral por hora winsorizado
  b5 <- ggplot(data_webs, aes(x = "", y = Horas_trabajadas_win)) +
    geom_boxplot() +
    theme_gray() +
    labs(title = "Ingreso laboral por hora", x = "", y = "Miles de pesos") +
    theme(axis.text = element_text(size = 8)) + 
    theme(
      plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
    )
  b5
  
  #Edad winsorizada
  b6 <- ggplot(data_webs, aes(x = "", y = Edad_win)) +
    geom_boxplot() +
    theme_gray() +
    labs(title = "Edad", x = "", y = "Años") +
    theme(axis.text = element_text(size = 8)) + 
    theme(
      plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
    )
  b6
  
  #Experiencia winsorizada
  b7 <- ggplot(data_webs, aes(x = "", y = Experiencia_win)) +
    geom_boxplot() +
    theme_gray() +
    labs(title = "Experiencia", x = "", y = "Años") +
    theme(axis.text = element_text(size = 8)) + 
    theme(
      plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
    )
  b7
  
  #Horas winsorizada
  b8 <- ggplot(data_webs, aes(x = "", y = Horas_trabajadas_win)) +
    geom_boxplot() +
    theme_gray() +
    labs(title = "Horas trabajadas", x = "", y = "Horas") +
    theme(axis.text = element_text(size = 8)) + 
    theme(
      plot.title = element_text(size = 10, face = "bold")  # Cambia el tamanio y estilo del título
    )
  b8  
  
  #Combinar graficas winsorizadas
  setwd(paste0(wd,"/Graficas"))
  png("graf_cajas_win.png") # Formato grafica
  box_win <- (b5+b6)/(b7+b8)
  box_win
  dev.off() # Cierra la grafica
  
  
# 7. Creacion variables nuevas -----------------------------------------------
  
  data_webs$log_ing_h_imp=log(data_webs$Ingreso_hora_imp) # con valores atipicos
  data_webs$log_ing_h_win=log(data_webs$Ingreso_hora_imp_win) #sin valores atipicos
  data_webs <- data_webs %>% mutate (oficio_factor= as.factor(Profesion))
  data_webs <- data_webs %>% mutate (edu_factor= as.factor(Nivel_educ))
  data_webs <- data_webs %>% mutate (estrato_factor= as.factor(Estrato))
  data_webs$Mujer <- ifelse(data_webs$Sexo == 0, 1, 0)

  data_webs <- data_webs %>%
    mutate( #logaritmo del salario por hora imputado
      Edad2 = Edad_win^2, #Edad al cuadrado 
      Experiencia_anios = Experiencia/12)
  
  #Descargar base de datos final
  setwd(paste0(wd,"/Base_Datos"))
  export(data_webs, "base_final.rds")

  
