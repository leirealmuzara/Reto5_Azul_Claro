# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

# Establecer directorio de trabajo
setwd("C:/Users/leire/OneDrive/Escritorio/Bda2/reto5/Nueva carpeta/Reto5_Azul_Claro/Datos-20240913")
# Cargar datos
df1 <- read.csv("pib_ipc_paises_punto2.csv")

########################### FILTRAR Y LIMPIAR DATOS ###########################

# Filtrar Alemania en df1 y df2
df1 <- df1 %>% filter(Code == "DEU")

# Revisar la estructura de los datos filtrados
head(df1)
str(df1)
sum(is.na(df1))

# Separar datos de IPC y PIB
ipc <- df1[,-c(1,3,6)] # Eliminar columnas irrelevantes para IPC
pib <- df1[,-c(1,3,7)] # Eliminar columnas irrelevantes para PIB

# Filtrar datos trimestrales para PIB
pib <- pib %>% filter(Month %in% c(3,6,9,12))

########################### SERIES TEMPORALES ###########################

# Crear series temporales
ts_ipc <- ts(ipc$Consumer.Price.Index..CPI., start = c(1996,1), end = c(2022,7), frequency = 12)#asi no coge los dos ultimos trimestres
ts_pib <- ts(pib$GDP.billion.currency.units, start = c(1996,1), end = c(2022,2), frequency = 4)

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
diff_ipc <- diff(ts_ipc)
diff_pib <- diff(ts_pib)

# Verificar nuevamente la estacionariedad
adf.test(diff_ipc)
adf.test(diff_pib)

# Graficar series diferenciadas
plot(diff_ipc, main="IPC Diferenciado")
plot(diff_pib, main="PIB Diferenciado")

# ACF y PACF para IPC y PIB diferenciados
acf(diff_ipc, main="ACF IPC Diferenciado")
pacf(diff_ipc, main="PACF IPC Diferenciado")
acf(diff_pib, main="ACF PIB Diferenciado")
pacf(diff_pib, main="PACF PIB Diferenciado")

########################### MIRAR ACCURACY MODELOS ###########################
#queremos predecir datos que ya tenemos para evaluar los modelos
#separar datos en train y test
train_ipc<-window(random_ipc,start=c(1996,7),end=c(2021,1))

test_ipc<-window(random_ipc,start=c(2021,2),end=c(2022,1))

# Ajustar modelos para IPC y PIB
#naive
naive_ipc<-naive(train_ipc,h=length(test_ipc))

#seasonal naive
snaive_ipc<-snaive(train_ipc,h=length(test_ipc))

#drift
drift_ipc<-rwf(train_ipc,drift=TRUE,h=length(test_ipc))

#promedio movil (MA)
ma_ipc<-meanf(train_ipc,h=length(test_ipc))

#modelo autoregresivo (AR)
ar_ipc<-Arima(train_ipc,order = c(1,0,0))
ar_ipc_forecast<-forecast(ar_ipc,h=length(test_ipc))

#ARMA
arma_ipc<-Arima(train_ipc,order = c(1,0,1))
arma_ipc_forecast<-forecast(arma_ipc,h=length(test_ipc))

#arima
modelo_arima_ipc <- auto.arima(train_ipc)
summary(modelo_arima_ipc)
arima_forecast_ipc<-forecast(modelo_arima_ipc,h=length(test_ipc))

#calcular precisión de modelos de predicción

accuracy_drift_ipc<-accuracy(drift_ipc,test_ipc) 
accuracy_snaive_ipc<-accuracy(snaive_ipc,test_ipc) 
accuracy_naive_ipc<-accuracy(naive_ipc,test_ipc) 
accuracy_ma_ipc<-accuracy(ma_ipc,test_ipc) 
accuracy_ar_ipc<-accuracy(ar_ipc_forecast,test_ipc) 
accuracy_arma_ipc<-accuracy(arma_ipc_forecast,test_ipc) 
accuracy_arima_ipc<-accuracy(arima_forecast_ipc,test_ipc) #mejor 

######################################################

#mirar arimax con variables exogenas
#plotear resultados con autoplot

#parametros y modelos de ajuste

# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_ipc)
checkresiduals(modelo_arima_pib)

#añadir estacionalidad y tendencia a la serie

# Predicción de IPC y PIB con ARIMA a 12 meses
forecast_ipc <- forecast(modelo_arima_ipc, h = 12)
plot(forecast_ipc, main="Predicción del IPC con ARIMA")
forecast_ipc$
forecast_pib <- forecast(modelo_arima_pib, h = 12)
plot(forecast_pib, main="Predicción del PIB con ARIMA")
