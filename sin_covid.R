#limpieza datos reto 5
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

dir()
source("predicciones del crecimiento IPC y PIB.R")


# Filtrar datos fuera del periodo de COVID-19
# Supongamos que el periodo de COVID-19 es de 2020 a 2022
pib <- pib %>%
  filter(!(Year >= 2020))
ipc <- ipc %>%
  filter(!(Year >= 2020))

# Visualizar los datos filtrados
head(pib)
str(pib)
sum(is.na(pib))

head(ipc)
str(ipc)
sum(is.na(ipc))


#####SERIES TEMPORALES####

#crear serie temporal
ts_ipc <- ts(ipc$Crecimiento_Interanual, start = c(1996, 1), end = c(2019, 12), frequency = 12)
ts_pib <- ts(pib$Crecimiento_Interanual, start = c(1996, 3), end = c(2019, 4), frequency = 4)

# Graficar las series temporales
plot(ts_ipc, main = "Serie Temporal del IPC en Alemania", ylab = "IPC", xlab = "Tiempo")
plot(ts_pib, main = "Serie Temporal del PIB en Alemania", ylab = "PIB", xlab = "Tiempo")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)

#Comprobar si hay valores faltantes
sum(is.na(ts_pib))
sum(is.na(ts_ipc))

####ANALISIS DE AUTOCORRELACION####

#ACF y PACF
ts_ipc <- na.omit(ts_ipc)
ts_pib <- na.omit(ts_pib)

acf(ts_ipc, main = "ACF del IPC")
pacf(ts_ipc, main = "PACF del IPC")

acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")

####DESCOMPOSICION DE SERIES TEMPORALES####

# Descomposición de las serie de IPC
descomposicion_ipc <- decompose(ts_ipc)
plot(descomposicion_ipc)
random_ipc <- descomposicion_ipc$random
descomposicion_ipc$seasonal
descomposicion_ipc$trend


# Descomposición de las serie de PIB
descomposicion_pib <- decompose(ts_pib)
plot(descomposicion_pib)
random_pib<-descomposicion_pib$random
descomposicion_pib$seasonal
descomposicion_pib$trend
plot(descomposicion_pib)

####PRUEBA DE ESTACIOANRIEDAD####

# Prueba ADF para verificar estacionariedad de IPC y PIB
adf.test(ts_ipc)
adf.test(ts_pib)

# Diferenciar las series para eliminar tendencia
ts_ipc<-diff(ts_ipc)
ts_pib<-diff(ts_pib)

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
pacf(ts2_pib, main="PACF PIB Diferenciado")


#####COMPROBAR ACCURACY#####
#IPC
train_ipc<-window(random_ipc, start = c(1996, 6), end = c(2018, 1))
test_ipc <- window(random_ipc, start = c(2019,1), end = c(2019,12))

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
accuracy_drift_ipc<-accuracy(drift_ipc,test_ipc) #0.33044675 
accuracy_snaive_ipc<-accuracy(snaive_ipc,test_ipc)#0.5250969 
accuracy_naive_ipc<-accuracy(naive_ipc,test_ipc)  #0.3304480    
accuracy_ma_ipc<-accuracy(ma_ipc,test_ipc) #0.32001457 
accuracy_ar_ipc<-accuracy(ar_ipc_forecast,test_ipc) #0.28267833 
accuracy_arma_ipc<-accuracy(arma_ipc_forecast,test_ipc) #0.28267206 
accuracy_arima_ipc<-accuracy(arima_forecast_ipc,test_ipc) #0.2067668 --> ES EL MEJOR  


#PIB
train_pib<-window(random_pib, start = c(1996, 3), end = c(2018, 1))
test_pib<- window(random_pib, start = c(2019,1), end = c(2019,4))

#naive
naive_pib<-naive(train_pib,h=length(test_pib))

#seasonal naive
snaive_pib<-snaive(train_pib,h=length(test_pib))

#drift
drift_pib<-rwf(train_pib,drift=TRUE,h=length(test_pib))

#promedio movil (MA)
ma_pib<-meanf(train_pib,h=length(test_pib))

#modelo autoregresivo (AR)
ar_pib<-Arima(train_pib,order = c(1,0,0))
ar_pib_forecast<-forecast(ar_pib,h=length(test_pib))

#ARMA
arma_pib<-Arima(train_pib,order = c(1,0,1))
arma_pib_forecast<-forecast(arma_pib,h=length(test_pib))

#arima
modelo_arima_pib <- auto.arima(train_pib)
summary(modelo_arima_pib)
arima_forecast_pib<-forecast(modelo_arima_pib,h=36)
summary(arima_forecast_pib)

#calcular precisión de modelos de predicción
accuracy_drift_pib<-accuracy(drift_pib,test_pib) #1.265945 
accuracy_snaive_pib<-accuracy(snaive_pib,test_pib) #1.344134  
accuracy_naive_pib<-accuracy(naive_pib,test_pib)  #1.266053  
accuracy_ma_pib<-accuracy(ma_pib,test_pib) #0.8241532  
accuracy_ar_pib<-accuracy(ar_pib_forecast,test_pib) #0.8113892  
accuracy_arma_pib<-accuracy(arma_pib_forecast,test_pib) #0.7092525 
accuracy_arima_pib<-accuracy(arima_forecast_pib,test_pib) #0.6248854  --> ES LO MEJOR 

################PREDICCIONES######################################
#IPC
# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_ipc)
#añadir estacionalidad y tendencia a la serie

# Predicción de IPC con ARIMA a 12 meses
modelo_arima_ipc <- auto.arima(ts_ipc)
forecast_ipc <- forecast(modelo_arima_ipc, h = 4 )
summary(forecast_ipc)
plot(forecast_ipc, main="Predicción del IPC con ARIMA")
abline(h = 0, col = "red", lty = 2)

#PIB
# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_pib)
#añadir estacionalidad y tendencia a la serie

# Predicción de PIB con ARIMA a 12 meses
modelo_arima_pib <- auto.arima(ts_pib)
forecast_pib <- forecast(modelo_arima_pib, h = 4 )
summary(forecast_pib)
plot(forecast_pib, main="Predicción del PIB con ARIMA")
abline(h = 0, col = "red", lty = 2)




## CAMBIAR 2020 por 2022
forecast_pib_table <- data.frame(forecast_pib)
row.names(forecast_pib_table) <- gsub("2020", "2022", row.names(forecast_pib_table))

forecast_pib_table


forecast_ipc_table <- data.frame(forecast_ipc)
row.names(forecast_ipc_table) <- gsub("2020", "2022", row.names(forecast_ipc_table))

forecast_ipc_table
