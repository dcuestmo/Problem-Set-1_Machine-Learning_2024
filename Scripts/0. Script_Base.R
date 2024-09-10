#------------------------------------------------------------------------------#
#------------------------ CODIGO BASE - PROBLEM SET 1 -------------------------#
#------------------------------------------------------------------------------#

# El presente codigo permite:
# 1: Cambiar el directorio entre cada uno de los colaboradores del proyecto
# 2: Correr cada uno de los scripts utilizados en la resolución del problem set 1.

# 0. Se borra la memoria y se cargan los paquetes ------------------------------
rm(list = ls())   # Borra la memoria

# Se cargan los paquetes de interés
library(pacman)
p_load(rio,              # Importación y exportación sencilla de datos
       tidyverse,        # Colección de paquetes para datos ordenados y gráficos (incluye ggplot2).
       skimr,            # Resumen compacto y descriptivo de datos
       visdat,           # Visualización de datos faltantes
       corrplot,         # Gráficos de matrices de correlación
       stargazer,        # Generación de tablas en formatos de salida como LaTeX, HTML o texto
       rvest,            # Herramientas para web scraping
       readxl,           # Importar archivos Excel
       writexl,          # Exportar archivos Excel
       boot,             # Aplicación de métodos de remuestreo (bootstrapping)
       ggpubr,           # Extensiones para facilitar la creación de gráficos en ggplot2
       WVPlots,          # Gráficos para análisis de variables ponderadas
       patchwork,        # Combinación y organización de gráficos
       gridExtra,        # Disposición de gráficos en cuadrícula
       ggplot2,          # Creación de gráficos mediante gramática de gráficos
       caret,            # Evaluación y entrenamiento de modelos predictivos
       data.table)       # Manipulación eficiente de grandes conjuntos de datos

# 1. Definicion del directorio -------------------------------------------------
ifelse(grepl("Diego", getwd()),
       wd <- "C:/Users/HP/OneDrive - Universidad Nacional de Colombia/Documentos/Diego/PEG/2024-2/Machine learning/Problem-Set-1_Machine-Learning_2024/Problem-Set-1_Machine-Learning_2024",
       ifelse(grepl("Juan", getwd()),
              wd <- "C:/Users/Juan/Documents/Problem_set_1",
              ifelse(grepl("juanp.rodriguez", getwd()),
                     wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                     ifelse(grepl("C:/Users/User", getwd()),
                            wd <- "C:/Users/User/OneDrive - Universidad de los Andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1",
                            ifelse(grepl("/Users/aleja/", getwd()),
                                   wd <- "Directorio",
                                   wd <- "otro_directorio")))))

# 2. Script de Web-scraping ----------------------------------------------------
# El script: "01_web_scraping.R". Realiza el proceso de web scraping para conseguir los datos
setwd(paste0(wd,"/scripts"))
source("1. Web_Scraping.R")

