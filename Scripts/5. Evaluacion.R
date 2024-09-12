
source("Scripts/0. Script_Base.R")
setwd<-getwd()

nlsy <- read.csv("Base_Datos/Tabla_Final_GEIH.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# source("Scripts/1. Scraping.R")

# Realizamos una particion de la muestra

set.seed(40503)
sample <- sample(c(TRUE, FALSE), nrow(nlsy), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- nlsy[sample, ] #train sample those that are TRUE in the sample index
test   <- nlsy[!sample, ] #test sample those that are FALSE in the sample index
dim(train)



