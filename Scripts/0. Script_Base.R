#------------------------------------------------------------------------------#
#------------------------ CODIGO BASE - PROBLEM SET 1 -------------------------#
#------------------------------------------------------------------------------#

# El presente codigo permite:
# 1: Cambiar el directorio entre cada uno de los colaboradores del proyecto
# 2: Correr cada uno de los scripts utilizados en la resoluci?n del problem set 1.

# 0. Se borra la memoria y se cargan los paquetes ------------------------------
rm(list = ls())   # Borra la memoria

# Se cargan los paquetes de inter?s
library(pacman)
p_load(rio,              # Importacion y exportacion sencilla de datos
       tidyverse,        # Coleccion de paquetes para datos ordenados y graficos (incluye ggplot2).
       skimr,            # Resumen compacto y descriptivo de datos
       visdat,           # Visualizacion de datos faltantes
       corrplot,         # Graficos de matrices de correlacion
       stargazer,        # Generacion de tablas en formatos de salida como LaTeX, HTML o texto
       rvest,            # Herramientas para web scraping
       readxl,           # Importar archivos Excel
       writexl,          # Exportar archivos Excel
       boot,             # Aplicacion de m?todos de remuestreo (bootstrapping)
       patchwork,        # Combinacion y organizacion de graficos
       gridExtra,        # Disposicion de graficos en cuadr?cula
       ggplot2,          # Creacion de graficos mediante gramatica de graficos
       caret,            # Evaluacion y entrenamiento de modelos predictivos
       visdat,           # Visualizar missings
       patchwork,        # Dise?o de graficos
       data.table,       # Manipulacion eficiente de grandes conjuntos de datos
       MASS,             # For post regression calculations
       dplyr)            # For post regression calculations      

getwd() #Mirar directorio

# 1. Definicion del directorio -------------------------------------------------

ifelse(grepl("HP", getwd()), # Diego
       wd <- "C:/Users/HP/OneDrive - Universidad Nacional de Colombia/Documentos/Diego/PEG/2024-2/Machine learning/Problem-Set-1_Machine-Learning_2024",
       ifelse(grepl("JULIETH", getwd()), # Julieth1
              wd <- "C:/Users/JULIETH/Documents/GitHub/Problem-Set-1_Machine-Learning_2024",
              ifelse(grepl("Usuario", getwd()), # Julieth2
                            wd <- "C:/Users/hncar/Documents/GitHub/Problem-Set-1_Machine-Learning_2024",
                            ifelse(grepl("C:/Users/User", getwd()),  # Henry
                                   wd <- "C:/Users/User/OneDrive - Universidad de los Andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1",
                                   ifelse(grepl("/Users/aleja/", getwd()), 
                                          wd <- "Directorio",  # Jorge
                                          ifelse(grepl("Steven Ramirez", getwd()), 
                                                 wd <- "C:/Users/Steven Ramirez/OneDrive - Universidad de los Andes/Grupo - EconometrÃ­a 1/Escritorio/Big Data/Problem-Set-1_Machine-Learning_2024",
                                                 wd <- "otro_directorio")))))))
       

# 2. Script de Web-scraping ----------------------------------------------------

# El script: "1. Scraping.R" realiza el proceso de web scraping para conseguir los datos
setwd(paste0(wd,"/scripts"))
source("1. Scraping.R")

# El script: "2. Filtro Base y seleccion de variables.R" realiza  limpieza de la base de datos, mantiene las variables de interés y hace imputación de datos en missing values. 
# Además, guarda la base de datos de interés (base_final.rds) que sera usada en los siguientes códigos.
source("2. Filtro Base y seleccion de variables.R")

# El script: "3. Estadísticas descriptivas" realiza el análisis descriptivo de los datos. 
setwd(paste0(wd,"/scripts"))
source("3. Estadísticas descriptivas.R")

# El script: "4. Modelo ingreso-edad" realiza las estimaciones y análisis de datos de la sección 3 del Problem Set (Teoría edad-salario) 
setwd(paste0(wd,"/scripts"))
source("4. Modelo ingreso-edad.R")

# El script: "5. Evaluacion" realiza las estimaciones y análisis de datos de la sección 5 del Problem Set (Predicciones del ingreso) 
setwd(paste0(wd,"/scripts"))
source("5. Evaluacion.R")

# El script: "6. Modelo Gender_Earnings_Gap" realiza las estimaciones y análisis de datos de la sección 4 del Problem Set (Brecha salarial diferenciando por género) 
setwd(paste0(wd,"/scripts"))
source("6. Modelo Gender_Earnings_Gap.R")
