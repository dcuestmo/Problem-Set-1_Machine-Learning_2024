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
p_load(rio,              # Importaci?n y exportaci?n sencilla de datos
       tidyverse,        # Colecci?n de paquetes para datos ordenados y gr?ficos (incluye ggplot2).
       skimr,            # Resumen compacto y descriptivo de datos
       visdat,           # Visualizaci?n de datos faltantes
       corrplot,         # Gr?ficos de matrices de correlaci?n
       stargazer,        # Generaci?n de tablas en formatos de salida como LaTeX, HTML o texto
       rvest,            # Herramientas para web scraping
       readxl,           # Importar archivos Excel
       writexl,          # Exportar archivos Excel
       boot,             # Aplicaci?n de m?todos de remuestreo (bootstrapping)
       ggpubr,           # Extensiones para facilitar la creaci?n de gr?ficos en ggplot2
       WVPlots,          # Gr?ficos para an?lisis de variables ponderadas
       patchwork,        # Combinaci?n y organizaci?n de gr?ficos
       gridExtra,        # Disposici?n de gr?ficos en cuadr?cula
       ggplot2,          # Creaci?n de gr?ficos mediante gram?tica de gr?ficos
       caret,            # Evaluaci?n y entrenamiento de modelos predictivos
       data.table)       # Manipulaci?n eficiente de grandes conjuntos de datos

getwd()

# 1. Definicion del directorio -------------------------------------------------

ifelse(grepl("HP", getwd()),
       wd <- "C:/Users/HP/OneDrive - Universidad Nacional de Colombia/Documentos/Diego/PEG/2024-2/Machine learning/Problem-Set-1_Machine-Learning_2024",
       ifelse(grepl("JULIETH", getwd()),
              wd <- "C:/Users/JULIETH/Documents/GitHub/Problem-Set-1_Machine-Learning_2024",
              ifelse(grepl("Jorge", getwd()),
                     wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                     ifelse(grepl("C:/Users/User", getwd()),
                            wd <- "C:/Users/User/OneDrive - Universidad de los Andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1",
                            ifelse(grepl("/Users/aleja/", getwd()),
                                   wd <- "Directorio",
                                   wd <- "otro_directorio")))))

# 2. Script de Web-scraping ----------------------------------------------------
# El script: "01_web_scraping.R". Realiza el proceso de web scraping para conseguir los datos
setwd(paste0(wd,"/scripts"))
source("1. Scraping.R")


