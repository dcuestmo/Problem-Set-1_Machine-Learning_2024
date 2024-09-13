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
       patchwork,        # Diseño de graficos
       naniar) ## tables/output to TEX.

getwd() #Mirar directorio

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
                                   ifelse(grepl("Steven Ramirez", getwd()),
                                   wd <- "C:/Users/Steven Ramirez/OneDrive - Universidad de los Andes/Grupo - Econometría 1/Escritorio/Big Data/Problem-Set-1_Machine-Learning_2024",
                                   wd <- "otro_directorio"))))))

# 2. Script de Web-scraping ----------------------------------------------------

# El script: "01_web_scraping.R". Realiza el proceso de web scraping para conseguir los datos
setwd(paste0(wd,"/scripts"))
source("1. Scraping.R")


