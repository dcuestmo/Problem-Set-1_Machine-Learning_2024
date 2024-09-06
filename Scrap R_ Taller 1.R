install.packages("pacman")


require(pacman)
p_load(tidyverse,rvest)

vignette("rvest")


url <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/'
# Podemos ingresar a la pagina y observar como se encuentra la pagina
browseURL(url)


# Lectura del enlace
my_html = read_html(url)
my_html


# Extraemos una lista que contenga el atributo href de cada uno de los nodos <a> de en los elementos <li>
links <- my_html%>%
  html_elements("li")%>%
  html_nodes("a")%>%
  html_attr("href")

print(links)

# Observamos que esta lista contiene solo las extensiones finales y no el link completo, ademas es necesario
# omitir el primer elemento de la lista correpondiente al index. Por tanto es necesario completar el link
links <- paste0('https://ignaciomsarmiento.github.io/GEIH2018_sample/', links[-1])



tabla_final <- data.frame()

for (link in links) {
  
  #Ahora se extrae la tabla de datos para cada link utilizando el tml referenciado por el div especifico de la tabla
  page <- read_html(link)
  referencia <- page %>%
    html_element("div.col-md-9") %>%
    html_element("div") %>%
    html_attr("w3-include-html")
  
  url_tabla <- paste0(url, referencia)
  
  chunk <- read_html(url_tabla)%>%
    html_elements("table") %>%
    html_table()
  
  # Convertimos la tabla a un DataFrame
  tabla_chunk <- as.data.frame(chunk)
  print(paste("Total de columnas en este chunk:", ncol(tabla_chunk)))
  
  tabla_final <- bind_rows(tabla_final, tabla_chunk)
}



# Observamos que el total de columnas en cada chunk es la misma que la tabla final, por lo cual
# podemos asumir que no hay variables unicas en algun chunk o duplicados debido a la extracción.
ncol(tabla_final)


#Visualizamos las primeras observaciones
print(head(tabla_final))



# Guardamos la tabal consolidada
ruta_archivo <- file.path(getwd(), "tabla_final.csv")
write.csv(tabla_final, ruta_archivo, row.names = FALSE)





#Cargue de librerias para manejo de los datos

p_load(rio, # import/export data
       skimr, # summary data
       visdat, ## visualizing missing data
       corrplot, ## Correlation Plots
       stargazer) ## tables/output to TEX.



# Cambiamos el formato de la tabla
db <- as_tibble(tabla_final)


#Missing values
db_miss <- skim(db) %>% select( skim_variable, n_missing)
Nobs= nrow(db) # 32177 observaciones
db_miss<- db_miss %>% filter(n_missing!= 0)
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
db_miss




## visualize the 40 variables with less missing values

ggplot(tail(db_miss, 40), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 8))  # Set size for axis labels



# Como menciona el enunciado del problema nos centraremos solo en personas empleadas mayores de dieciocho (18) años.
db <- db %>%
  filter(age>=18 & ## Mayores de edad
           ocu==1) ## Empleados
nrow(db)#16542 observaciones que cumplen con estas caracteristicas




#Selección de variables

db <- db %>%
  select(directorio, secuencia_p, orden, mes, # variables de referenciación
         age, sex, oficio, estrato1, p6050, # variables caracteristicas
         regSalud, cotPension, # Seguridad social
         relab, cuentaPropia, totalHoursWorked, sizeFirm, # caracteristicas empleo
         ingtot, ingtotob, y_salary_m, y_total_m, y_otros_m, #ingresos
         fex_c # factor de expansión
  )%>%
  rename(
    Direccion = directorio,
    Secuencia = secuencia_p,
    Orden = orden,
    Mes = mes,
    Edad = age,
    Sexo = sex,
    Profesion = oficio,
    Estrato = estrato1,
    Posicion_hogar = p6050,
    Regimen_salud = regSalud,
    Cotiza_pension = cotPension,
    Tipo_trabajador = relab,
    Tipo_empleo = cuentaPropia,
    Horas_trabajadas = totalHoursWorked,
    Tamaño_empresa = sizeFirm,
    Ingreso_Total = ingtot,
    Ingreso_observado = ingtotob,
    Salario_mensual = y_salary_m,
    Ingreso_mas_independiente = y_total_m,
    Otros_ingresos = y_otros_m,
    Factor_expansion = fex_c
  )

missing <- skim(db) %>% select( skim_variable, n_missing)
db_miss<- missing %>% filter(n_missing!= 0)
db_miss<- db_miss %>% mutate(p_missing= n_missing/nrow(db)) %>% arrange(-n_missing)
db_miss



#  Como casi la totalidad de la variables Otros_ingresos es nula, la retiramos del set de datos
# Por otro lado pasamos a ver la distribucion del resto de variables con missing
db <- db %>%
  select(-Otros_ingresos)



#Primero observamos la variable categoria Regimen_salud
ggplot(db, aes(Regimen_salud)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Regimen_salud  Distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))


# Como el regimen contributivo es el más comun, y en el pais es obligatorio eque todos los trabajadores
# independientes o dependientes esten afiliados, remplazamos los nulos por este valor

mode_salud <- as.numeric(names(sort(table(db$Regimen_salud), decreasing = TRUE)[1]))

db <- db  %>%
  mutate(Regimen_salud = ifelse(is.na(Regimen_salud) == TRUE, mode_salud , Regimen_salud))


# Ahora  observamos la distribucion de las otras variables
ggplot(db, aes(Salario_mensual)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Salario_mensual  Distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))
ggplot(db, aes(Ingreso_mas_independiente)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Ingreso_mas_independiente  Distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))


#Para imputar las otras dos variables, al tener una distribucion asimetrica a la derecha, utilizamos la mediana de la respectiva variable.
# Además dado que los salarios entre diferentes profesiones puede variar, agrupamos por esta variable
medianas_por_grupo <- db %>%
  group_by(Profesion) %>%
  summarise(
    mediana_Salario_mensual = median(Salario_mensual, na.rm = TRUE),
    mediana_Ingreso_mas_independiente = median(Ingreso_mas_independiente, na.rm = TRUE)
  )%>%
  arrange(desc(mediana_Salario_mensual), desc(mediana_Ingreso_mas_independiente))

head(medianas_por_grupo, 10)
# Observamos que aquellos trabajadores dedicados a:
# 1) "Pilotos, Ingeniero de vuelo,   oficiales de cubierta, maquinistas – Navegación marítima y fluvial"
# 2) "Operador de instalaciones de   producción de energía eléctrica, de máquinas fijas"
# 3) "Agentes   administrativos"

# Son aquellos que tienen una mediana de ingresos más alta.


#Remplazamos y volvemos a buscar valores nulos

db <- db %>%
  left_join(medianas_por_grupo, by = c("Profesion")) %>%
  mutate(
    Salario_mensual = ifelse(is.na(Salario_mensual), mediana_Salario_mensual, Salario_mensual),
    Ingreso_mas_independiente = ifelse(is.na(Ingreso_mas_independiente), mediana_Ingreso_mas_independiente, Ingreso_mas_independiente)
  ) %>%
  select(-mediana_Salario_mensual, -mediana_Ingreso_mas_independiente)  # Eliminar columnas auxiliares

missing <- skim(db) %>% select( skim_variable, n_missing)
db_miss<- missing %>% filter(n_missing!= 0)
db_miss<- db_miss %>% mutate(p_missing= n_missing/nrow(db)) %>% arrange(-n_missing)
db_miss


# Al revisar la tabla luego de la imputacion se observa que aun hay valores nulos, por lo cual revisamos la tabla de medianas para verificar
# si hay profesiones con valores nulos que no se remplazaran en nuestro data set

medianas_por_grupo %>%filter(is.na(mediana_Salario_mensual))





# Para manejar estos dos casos optamos por realizar una nueva imputacion utilizando la variable de Tipo_trabajador
medianas_por_tipo <- db %>%
  group_by(Tipo_trabajador) %>%
  summarise(
    mediana_Salario_mensual = median(Salario_mensual, na.rm = TRUE),
    mediana_Ingreso_mas_independiente = median(Ingreso_mas_independiente, na.rm = TRUE)
  )%>%
  arrange(desc(mediana_Salario_mensual), desc(mediana_Ingreso_mas_independiente))

medianas_por_tipo
# Observamos que los empleados de gobierno, los empleadores, y los empleados de empresas particulares
# son los individuos con mayor nivel de ingresos. Mientras que aquellos que traban por jornal y como
# empleados domesticos tienden a tener una mediana de ingresos menor.


db <- db %>%
  left_join(medianas_por_tipo, by = c("Tipo_trabajador")) %>%
  mutate(
    Salario_mensual = ifelse(is.na(Salario_mensual), mediana_Salario_mensual, Salario_mensual),
    Ingreso_mas_independiente = ifelse(is.na(Ingreso_mas_independiente), mediana_Ingreso_mas_independiente, Ingreso_mas_independiente)
  ) %>%
  select(-mediana_Salario_mensual, -mediana_Ingreso_mas_independiente)  # Eliminar columnas auxiliares








