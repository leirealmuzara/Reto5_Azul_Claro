# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)
library(readxl)
#library('readr') ipc <- readr::read_tsv("prc_hicp_manr_tabular.tsv")

#mirar si los datos son los mismo, unidades de medidas
#mirar si son series omogeneas
setwd("C:/Users/pieza/OneDrive/Escritorio/Bdata2/RETOS/Reto5/Reto5_Azul_Claro/datos_reales2")
# Cargar datos
pib_ipc <- read.csv("pib_ipc_paises_punto2.csv")
pib <- read.csv("pib_datos_reales.csv")
ipc <- read.csv("ipc_datos_reales.csv")

#IPC --> No nos da el crecimiento interanual, por lo que vamos a calcularlo.
#limpiamos df
ipc <- ipc[,1:2]
ipc <- ipc[-c(1:9),]
colnames(ipc) <- c("fecha", "Crecimiento_Interanual")
str(ipc)
ipc$Crecimiento_Interanual <- as.numeric(ipc$Crecimiento_Interanual)
ipc$Crecimiento_Interanual <- (ipc$Crecimiento_Interanual / lag(ipc$Crecimiento_Interanual, 12) - 1) * 100

#PIB --> No nos da el crecimiento interanual, por lo que vamos a calcularlo.
# Calcular el crecimiento interanual respecto al año anterior
colnames(pib) <- c("fecha", "Crecimiento_Interanual")
pib$Crecimiento_Interanual <- (pib$Crecimiento_Interanual / lag(pib$Crecimiento_Interanual, 4) - 1) * 100


########################### SERIES TEMPORALES ###########################

#Mirar las fechas de inicio y final
ipc_primer_fecha <- min(ipc$fecha, na.rm = TRUE)
ipc_ultima_fecha <- max(ipc$fecha, na.rm = TRUE)
pib_primer_fecha <- min(ipc$fecha, na.rm = TRUE)
pib_ultima_fecha <- max(ipc$fecha, na.rm = TRUE)

# Crear series temporales
ts_ipc <- ts(ipc$Crecimiento_Interanual, start = c(1996,1), end = c(2024,9), frequency = 12) 
ts_pib <- ts(pib$Crecimiento_Interanual, start = c(1996,1), end = c(2024,9), frequency = 4)

# Graficar series temporales
plot(ts_ipc, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
abline(h = 0, col = "red")
plot(ts_pib, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")
abline(h = 0, col = "red")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)

# Comprobar si hay valores faltantes
sum(is.na(ts_pib))
sum(is.na(ts_ipc))
# los que hay son los valores a predecir

























