#limpieza datos reto 5
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

dir()
df1<-read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")
df2<-read.csv("Datos-20240913/exogenas_paises_punto2.csv")
df3<-read.csv("Datos-20240913/unemployment_germany.csv")

# Filtrar datos para Alemania
df1 <- df1 %>%
  filter(Code == "DEU")
df2 <- df2 %>%
  filter(Code == "DEU")

# Filtrar datos fuera del periodo de COVID-19
# Supongamos que el periodo de COVID-19 es de 2020 a 2022
df1 <- df1 %>%
  filter(!(Year >= 2020))

# Visualizar los datos filtrados
head(df1)
str(df1)
sum(is.na(df1))

# Preparar datos de IPC y PIB
ipc <- df1[, -c(1, 3, 6)]
pib <- df1[, -c(1, 3, 7)]

# Filtrar PIB por trimestre
pib <- pib %>%
  filter(Month %in% c(3, 6, 9, 12))

#####SERIES TEMPORALES####

#crear serie temporal
ts_ipc <- ts(ipc$Consumer.Price.Index..CPI., start = c(1996, 1), end = c(2019, 12), frequency = 12)
ts_pib <- ts(pib$GDP.billion.currency.units, start = c(1996, 3), end = c(2019, 4), frequency = 4)

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
diff_ipc<-diff(ts_ipc)
diff_pib<-diff(ts_pib)

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
accuracy_drift_ipc<-accuracy(drift_ipc,test_ipc) 
accuracy_snaive_ipc<-accuracy(snaive_ipc,test_ipc) 
accuracy_naive_ipc<-accuracy(naive_ipc,test_ipc) 
accuracy_ma_ipc<-accuracy(ma_ipc,test_ipc) 
accuracy_ar_ipc<-accuracy(ar_ipc_forecast,test_ipc) 
accuracy_arma_ipc<-accuracy(arma_ipc_forecast,test_ipc) 
accuracy_arima_ipc<-accuracy(arima_forecast_ipc,test_ipc) 


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
arima_forecast_pib<-forecast(modelo_arima_pib,h=length(test_pib))

#calcular precisión de modelos de predicción
accuracy_drift_pib<-accuracy(drift_pib,test_pib) 
accuracy_snaive_pib<-accuracy(snaive_pib,test_pib) 
accuracy_naive_pib<-accuracy(naive_pib,test_pib) 
accuracy_ma_pib<-accuracy(ma_pib,test_pib) 
accuracy_ar_pib<-accuracy(ar_pib_forecast,test_pib) 
accuracy_arma_pib<-accuracy(arma_pib_forecast,test_pib) 
accuracy_arima_pib<-accuracy(arima_forecast_pib,test_pib)  








# Predecir el último trimestre de 2022
# Usar modelo ARIMA para pronosticar
library(forecast)

# Ajustar el modelo ARIMA para IPC
model_ipc <- auto.arima(random_ipc)
forecast_ipc <- forecast(model_ipc, h = 36) # Predecir 3 meses
summary(forecast_ipc)
# Ajustar el modelo ARIMA para PIB
model_pib <- auto.arima(random_pib)
forecast_pib <- forecast(model_pib, h = 12) # Predecir 1 trimestre
summary(forecast_pib)
# Graficar pronósticos
plot(forecast_ipc, main = "Pronóstico del IPC para el último trimestre de 2022")
plot(forecast_pib, main = "Pronóstico del PIB para el último trimestre de 2022")


