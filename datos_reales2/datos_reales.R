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
dir()
# Cargar datos
pib_ipc <- read.csv("datos_reales2/pib_ipc_paises_punto2.csv")
pib <- read.csv("datos_reales2/pib_datos_reales.csv")
ipc <- read.csv("datos_reales2/ipc_datos_reales.csv")


#IPC --> Nos da el crecimiento interanual
#limpiamos df
ipc <- ipc[-c(1:9),]
ipc <- ipc[,-3]
colnames(ipc) <- c("fecha", "Crecimiento_Interanual")
str(ipc)
ipc$Crecimiento_Interanual <- as.numeric(ipc$Crecimiento_Interanual)
ipc$Crecimiento_Interanual <- (ipc$Crecimiento_Interanual / lag(ipc$Crecimiento_Interanual, 12) - 1) * 100

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


########################### ANÁLISIS DE AUTOCORRELACIÓN ###########################

# ACF y PACF para IPC
ts_ipc <- na.omit(ts_ipc)
ts_pib <- na.omit(ts_pib)

acf(ts_ipc, main = "ACF del IPC") #analiza la estacionalidad
pacf(ts_ipc, main = "PACF del IPC")

# ACF y PACF para PIB
acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")


########################### DESCOMPOSICIÓN DE SERIES ###########################

# Descomposición de la serie de IPC
descomposicion_ipc <- decompose(ts_ipc)
plot(descomposicion_ipc)
descomposicion_ipc$seasonal #estacionalidad
descomposicion_ipc$trend #tendencia
random_ipc<-descomposicion_ipc$random #residuo para predecir

# Descomposición de la serie de PIB
descomposicion_pib <- decompose(ts_pib)
plot(descomposicion_pib)
descomposicion_pib$seasonal #estacionalidad
descomposicion_pib$trend #tendencia
random_pib<-descomposicion_pib$random #residuo para predecir

########################### PRUEBAS DE ESTACIONARIEDAD ###########################

# Prueba ADF para verificar estacionariedad de IPC y PIB
adf.test(ts_ipc)
adf.test(ts_pib)

# Diferenciar las series para eliminar tendencia
ts_ipc <- diff(ts_ipc)
ts_pib <- diff(ts_pib)

# Verificar nuevamente la estacionariedad
adf.test(ts_ipc)
adf.test(ts_pib)

# Graficar series diferenciadas
plot(ts_ipc, main="IPC Diferenciado")
plot(ts_pib, main="PIB Diferenciado")

# ACF y PACF para IPC y PIB diferenciados
acf(ts_ipc, main="ACF IPC Diferenciado")
pacf(ts_ipc, main="PACF IPC Diferenciado")
acf(ts_pib, main="ACF PIB Diferenciado")
pacf(ts_pib, main="PACF PIB Diferenciado")

