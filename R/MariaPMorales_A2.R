
library(tidyverse)
library(dplyr)
library(caret)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

DIABETES <-
  read_csv(paste0(parentFolder
                  ,"/data/diabetes_012_health_indicators_BRFSS2015.csv"))

DIABETES_DF <- DIABETES

## 1 = tiene diabetes, 0 = no tiene diabetes

DIABETES_DF$Diabetes_012 <- ifelse(DIABETES_DF$Diabetes_012 >= 1,1,0)

## Muestreo estratificado aleatorio
set.seed(1)
DIAB_DF <- DIABETES_DF %>%
  group_by(Diabetes_012) %>%
  sample_n(1268, replace = FALSE) %>%
  ungroup()

## análisis exploratorio de datos

hist(DIABETES_DF$Diabetes_012
     ,main = "Histograma presencia de la enfermedad",
     xlab = "Presencia de la enfermedad", ylab = "Frecuencia"
     ,col= "red")
hist(DIAB_DF$Diabetes_012
     ,main = "Histograma presencia de la enfermedad",
     xlab = "Presencia de la enfermedad", ylab = "Frecuencia"
     ,col= "red")

plot(DIABETES_DF[4:6])
plot(DIABETES_DF[4:6], pch=21
     , bg= c("red", "green")[unclass(DIABETES_DF$Diabetes_012)])


plot(DIAB_DF[4:6])
plot(DIAB_DF[4:6], pch=21
     , bg= c("red", "green")[unclass(DIAB_DF$Diabetes_012)])

hist(DIABETES_DF$BIM)

summarise (DIAB_DF)

##

