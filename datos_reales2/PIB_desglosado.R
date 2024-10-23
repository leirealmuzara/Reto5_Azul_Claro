# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)
library(readxl)

# Cargar datos
getwd()
setwd("C:/Users/pieza/OneDrive/Escritorio/Bdata2/RETOS/Reto5/Reto5_Azul_Claro/datos_reales2")
PIB_desglosado <- read_xlsx("PIB_desglosado.xlsx", sheet = 3)


colnames(PIB_desglosado) <- PIB_desglosado[7,]
colnames(PIB_desglosado)[1] <- "Time"
colnames(PIB_desglosado)[2] <- "GEO (labels)"
PIB_desglosado <- PIB_desglosado[-c(1:8),]

PIB_Germany <- PIB_desglosado[PIB_desglosado$`GEO (labels)` == "Germany", ]



colnames(PIB_desglosado) <- PIB_desglosado[7,]
PIB_desglosado <- PIB_desglosado[-c(1:8),]
colnames(PIB_desglosado)[1] <- "Time"
colnames(PIB_desglosado)[2] <- "GEO (labels)"

PIB_Germany <- PIB_desglosado[PIB_desglosado$`GEO (labels)` == "Germany", ]
PIB_Germany <- PIB_Germany[-c(1:16),]
PIB_Germany <- PIB_Germany[-c(34:36),]
PIB_Germany <- PIB_Germany[, !is.na(colnames(PIB_Germany)) & colnames(PIB_Germany) != "NA"]

PIB_Germany_desglosado <- PIB_Germany[,c(1,2,3,6,9,14,15,24)]


#PRODECIMIENTO DONDE HAY UN ERROR
colnames(PIB_desglosado) <- PIB_desglosado[7,]
PIB_desglosado <- PIB_desglosado[-c(1:8),]
colnames(PIB_desglosado)[1] <- "Time"
colnames(PIB_desglosado)[2] <- "GEO (labels)"

PIB_Germany <- PIB_desglosado[PIB_desglosado$`GEO (labels)` == "Germany", ]
PIB_Germany <- PIB_Germany[-c(1:16),]
PIB_Germany <- PIB_Germany[-c(34:36),]
PIB_Germany <- PIB_Germany[, !is.na(colnames(PIB_Germany)) & colnames(PIB_Germany) != "NA"]

PIB_Germany_desglosado <- PIB_Germany[,c(1,2,3,6,9,14,15,24)]





