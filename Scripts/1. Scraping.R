
#------------------------------------------------------------------------------#
#----------------------------- Script 1. Scraping -----------------------------#
#------------------------------------------------------------------------------#

# El presente codigo carga la informacion de la Gran Encuesta Integrada de Hogares 
# (GEIH) del repositorio del docente de la materia Ignacio Sarmiento 


# 0. Se define el directorio de extraccion de informacion ----------------------
url <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/'
browseURL(url)     # Podemos ingresar a la pagina y observar como se encuentra la pagina
vignette("rvest")  # Se carga la explicaci?n de "rvest"

# 1. Extraccion y manejo de la base de datos -----------------------------------
my_html = read_html(url)  # Lectura del enlace

# 1.1. Se extrae la informacion -------------------------------------------------
# Extraemos una lista que contenga el atributo href de cada uno de los nodos <a> de en los elementos <li>
links <- my_html%>%
  html_elements("li")%>%
  html_nodes("a")%>%
  html_attr("href")
print(links)

# Observamos que esta lista contiene solo las extensiones finales y no el link completo. Ademas es necesario
# omitir el primer elemento de la lista correpondiente al index. Por tanto es necesario completar el link

links <- paste0('https://ignaciomsarmiento.github.io/GEIH2018_sample/', links[-1])

# 1.2 Se organiza la informacion en data.frame ---------------------------------

tabla_final <- data.frame()
# link = links[1]
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
# podemos asumir que no hay variables unicas en algun chunk o duplicados debido a la extraccion.
ncol(tabla_final)

#Visualizamos las primeras observaciones
print(head(tabla_final))

# 2. Se guarda la informacion --------------------------------------------------

# Guardamos la tabla consolidada
setwd(paste0(wd,"/Base_Datos"))
fwrite(tabla_final,"Tabla_Final_GEIH.csv",row.names = FALSE)
export(tabla_final, "Tabla_webs.rds")

# 3. Script de Filtro de la base y seleccion de variables ----------------------

# El script: "01_web_scraping.R". Realiza el proceso de web scraping para conseguir los datos
setwd(paste0(wd,"/scripts"))
source("2. Filtro Base y selección de variables.R")







