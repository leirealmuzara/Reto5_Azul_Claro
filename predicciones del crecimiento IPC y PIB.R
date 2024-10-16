# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

# Cargar datos
df1 <- read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")

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

# Calcular el crecimiento interanual respecto al año anterior
pib$Crecimiento_Interanual <- (pib$GDP.billion.currency.units / lag(pib$GDP.billion.currency.units, 4) - 1) * 100
ipc$Crecimiento_Interanual <- (ipc$Consumer.Price.Index..CPI. / lag(ipc$Consumer.Price.Index..CPI., 12) - 1) * 100

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

########################### MIRAR ACCURACY PIB ###########################
#queremos predecir datos que ya tenemos para evaluar los modelos
#separar datos en train y test
train_ipc<-window(random_ipc,start=c(1996,7),end=c(2021,1))
test_ipc<-window(random_ipc,start=c(2021,2),end=c(2022,1))

train_pib<-window(random_pib,start=c(1996,7),end=c(2021,1))
test_pib<-window(random_pib,start=c(2021,2),end=c(2022,1))

# Ajustar modelos para IPC y PIB
#naive
naive_ipc<-naive(train_ipc,h=length(test_ipc))
naive_pib<-naive(train_pib,h=length(test_pib))

#seasonal naive
snaive_ipc<-snaive(train_ipc,h=length(test_ipc))
snaive_pib<-snaive(train_pib,h=length(test_pib))

#drift
drift_ipc<-rwf(train_ipc,drift=TRUE,h=length(test_ipc))
drift_pib<-rwf(train_pib,drift=TRUE,h=length(test_pib))

#promedio movil (MA)
ma_ipc<-meanf(train_ipc,h=length(test_ipc))
ma_pib<-meanf(train_pib,h=length(test_pib))

#modelo autoregresivo (AR)
ar_ipc<-Arima(train_ipc,order = c(1,0,0))
ar_ipc_forecast<-forecast(ar_ipc,h=length(test_ipc))
ar_pib<-Arima(train_ipc,order = c(1,0,0))
ar_pib_forecast<-forecast(ar_pib,h=length(test_pib))

#ARMA
arma_ipc<-Arima(train_ipc,order = c(1,0,1))
arma_ipc_forecast<-forecast(arma_ipc,h=length(test_ipc))
arma_pib<-Arima(train_pib,order = c(1,0,1))
arma_pib_forecast<-forecast(arma_pib,h=length(test_pib))

#arima
modelo_arima_ipc <- auto.arima(train_ipc)
summary(modelo_arima_ipc)
arima_forecast_ipc<-forecast(modelo_arima_ipc,h=length(test_ipc))
modelo_arima_pib <- auto.arima(train_pib)
summary(modelo_arima_pib)
arima_forecast_pib<-forecast(modelo_arima_pib,h=length(test_pib))

#calcular precisión de modelos de predicción

accuracy_drift_pib<-accuracy(drift_pib,test_pib) 
accuracy_snaive_pib<-accuracy(snaive_pib,test_pib) 
accuracy_naive_pib<-accuracy(naive_pib,test_pib) 
accuracy_ma_pib<-accuracy(ma_pib,test_pib) 
accuracy_ar_pib<-accuracy(ar_pib_forecast,test_pib) 
accuracy_arma_pib<-accuracy(arma_pib_forecast,test_pib) 
accuracy_arima_pib<-accuracy(arima_forecast_pib,test_pib) #mejor RMSE de test más bajo

accuracy_drift_ipc <- accuracy(drift_ipc, test_ipc) 
accuracy_snaive_ipc <- accuracy(snaive_ipc, test_ipc) 
accuracy_naive_ipc <- accuracy(naive_ipc, test_ipc) 
accuracy_ma_ipc <- accuracy(ma_ipc, test_ipc) 
accuracy_ar_ipc <- accuracy(ar_ipc_forecast, test_ipc) 
accuracy_arma_ipc <- accuracy(arma_ipc_forecast, test_ipc) 
accuracy_arima_ipc <- accuracy(arima_forecast_ipc, test_ipc) #mejor


######################################################

# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_pib)
checkresiduals(modelo_arima_ipc)
#añadir estacionalidad y tendencia a la serie

# Predicción de IPC y PIB con ARIMA a 12 meses
modelo_arima_ipc <-auto.arima(ts_ipc)
forecast_ipc <- forecast(modelo_arima_ipc, h = 4)
summary(forecast_ipc)
plot(forecast_ipc, main="Predicción del IPC con ARIMA")
abline(h = 0, col = "red", lty = 2)

modelo_arima_pib <- auto.arima(ts_pib)
forecast_pib <- forecast(modelo_arima_pib, h = 1)
summary(forecast_pib)
plot(forecast_pib, main="Predicción del PIB con ARIMA")
abline(h = 0, col = "red", lty = 2)




