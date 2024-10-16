# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)
library(readxl)


#mirar si los datos son los mismo, unidades de medidas
#mirar si son series omogeneas

# Cargar datos
pib_ipc <- read.csv("pib_ipc_paises_punto2.csv")
pib <- read.csv("Gross_domestic_product_for_germany.csv")
ipc <- read.csv("holahola.csv")


#IPC --> Nos da el crecimiento interanual
#limpiamos df
ipc <- ipc[,7:8]
colnames(ipc) <- c("fecha", "Crecimiento_Interanual")

#PIB --> No nos da el crecimiento interanual, por lo que vamos a calcularlo.
# Calcular el crecimiento interanual respecto al año anterior
colnames(pib) <- c("fecha", "Crecimiento_Interanual")
pib$Crecimiento_Interanual <- (pib$Crecimiento_Interanual / lag(pib$Crecimiento_Interanual, 4) - 1) * 100


########################### SERIES TEMPORALES ###########################

# Crear series temporales
ts_ipc <- ts(ipc$Crecimiento_Interanual, start = c(1996,1), end = c(2022,7), frequency = 12) #asi no coge los dos ultimos trimestres
ts_pib <- ts(pib$Crecimiento_Interanual, start = c(1996,1), end = c(2022,2), frequency = 4)

# Graficar series temporales
plot(ts_ipc, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
plot(ts_pib, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)

# Comprobar si hay valores faltantes
sum(is.na(ts_pib))
sum(is.na(ts_ipc))
# los que hay son los valores a predecir

























