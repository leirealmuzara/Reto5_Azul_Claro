# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)
library(tidyr)
<<<<<<< HEAD
dir()
=======

>>>>>>> de4d3d895feca2781ed8b8ea06168eebf4c7acde
# Cargar datos
df1 <- read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")
exogenas <- read.csv("Datos-20240913/exogenas_paises_punto2.csv")
paro<- read.csv("Datos-20240913/unemployment_germany.csv")

########################### FILTRAR Y LIMPIAR DATOS ###########################

# Filtrar Alemania
df1 <- df1 %>% filter(Code == "DEU")
exogenas <- exogenas %>% filter(Code == "DEU")

# Revisar la estructura de los datos filtrados
head(df1)
str(df1)
sum(is.na(df1))
head(exogenas)
str(exogenas)
sum(is.na(exogenas))
head(paro)
str(paro)
sum(is.na(paro))

#Eliminar columnas irrelevantes
df1 <- df1[,-c(1,3)]
exogenas<-exogenas[,-7]
exogenas <- exogenas[,-c(1,3)]

#Para el paro, separar la columna en año y mes
paro <- paro %>%
  separate(DATE, into = c("Year", "Month", "Day"), sep = "-")
print(head(paro))
paro<-paro[,-3]
paro <- paro %>%
  mutate(Month = recode(Month,'01' = 3, '04' = 6, '07' = 9, '10' = 12))

# Filtrar datos trimestrales
df1 <- df1 %>% filter(Month %in% c(3,6,9,12))
exogenas <- exogenas %>% filter(Month %in% c(3,6,9,12))

# Calcular el crecimiento interanual respecto al año anterior
df1$GDP.billion.currency.units<- (df1$GDP.billion.currency.units / lag(df1$GDP.billion.currency.units, 4) - 1) * 100
df1$Consumer.Price.Index..CPI. <- (df1$Consumer.Price.Index..CPI. / lag(df1$Consumer.Price.Index..CPI., 4) - 1) * 100
exogenas$Money.supply.billion.currency.units <- (exogenas$Money.supply.billion.currency.units / lag(exogenas$Money.supply.billion.currency.units, 4) - 1) * 100
exogenas$Stock.market.index <- (exogenas$Stock.market.index / lag(exogenas$Stock.market.index, 4) - 1) * 100
paro$UNEMPLOYMENT_RATE <- (paro$UNEMPLOYMENT_RATE / lag(paro$UNEMPLOYMENT_RATE, 4) - 1) * 100

#Unir todos los datos en un mismo df
df_final <- inner_join(df1,exogenas,by = c("Year","Month"))
head(df_final)
df_final<-df_final[,-c(1,6)]
paro$Year<- as.integer(paro$Year)
df_final<- inner_join(df_final,paro, by = c("Year","Month"))

########################### SERIES TEMPORALES ###########################
colnames(df_final)<- c('Año', 'Mes','PIB','IPC','oferta_monetaria','indice_bursatil','paro')

# Crear series temporales
ts_ipc <- ts(df_final$IPC, start = c(1997,1), end = c(2022,2), frequency = 4) #asi no coge los dos ultimos trimestres
ts_pib <- ts(df_final$PIB, start = c(1997,1), end = c(2022,2), frequency = 4)
ts_masa_monetaria <- ts(df_final$oferta_monetaria, start = c(1997,1), end = c(2022,2), frequency = 4) #asi no coge los dos ultimos trimestres
ts_indice_bursatil <- ts(df_final$indice_bursatil, start = c(1997,1), end = c(2022,2), frequency = 4)
ts_paro <- ts(df_final$paro, start = c(1997,1), end = c(2022,2), frequency = 4)

# Graficar series temporales
plot(ts_ipc, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
plot(ts_pib, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")
plot(ts_masa_monetaria, main="Serie Temporal de la masa monetaria en Alemania", ylab="crecimiento", xlab="Tiempo")
plot(ts_indice_bursatil, main="Serie Temporal del índice bursátil en Alemania", ylab="%", xlab="Tiempo")
plot(ts_paro, main="Serie Temporal del paro en Alemania", ylab="%", xlab="Tiempo")


# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)
ggplot_na_distribution(ts_indice_bursatil)
ggplot_na_distribution(ts_masa_monetaria)
ggplot_na_distribution(ts_paro)


# Comprobar si hay valores faltantes
sum(is.na(ts_pib))
sum(is.na(ts_masa_monetaria))
sum(is.na(ts_indice_bursatil))
sum(is.na(ts_ipc))
sum(is.na(ts_paro))
# los que hay son los valores a predecir


########################### ANÁLISIS DE AUTOCORRELACIÓN ###########################
# ACF y PACF
ts_ipc <- na.omit(ts_ipc)
ts_pib <- na.omit(ts_pib)
ts_indice_bursatil <- na.omit(ts_indice_bursatil)
ts_masa_monetaria <- na.omit(ts_masa_monetaria)
ts_paro <- na.omit(ts_paro)

acf(ts_ipc, main = "ACF del IPC") #analiza la estacionalidad
pacf(ts_ipc, main = "PACF del IPC")

acf(ts_masa_monetaria, main = "ACF de la masa monetaria") 
pacf(ts_masa_monetaria, main = "PACF de la masa monetaria")

acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")

acf(ts_indice_bursatil, main = "ACF del indice bursatil")
pacf(ts_indice_bursatil, main = "PACF del indice bursatil")

acf(ts_paro, main = "ACF del crecimiento del paro")
pacf(ts_paro, main = "PACF del crecimiento del paro")

########################### DESCOMPOSICIÓN DE SERIES ###########################

# Descomposición de la serie de IPC
descomposicion_ipc <- decompose(ts_ipc)
plot(descomposicion_ipc)
descomposicion_ipc$seasonal #estacionalidad
descomposicion_ipc$trend #tendencia
random_ipc<-descomposicion_ipc$random #residuo

# Descomposición de la serie de PIB
descomposicion_pib <- decompose(ts_pib)
plot(descomposicion_pib)
descomposicion_pib$seasonal #estacionalidad
descomposicion_pib$trend #tendencia
random_pib<-descomposicion_pib$random #residuo

# Descomposición de la serie de masa monetaria
descomposicion_tm <- decompose(ts_masa_monetaria)
plot(descomposicion_tm)
descomposicion_tm$seasonal #estacionalidad
descomposicion_tm$trend #tendencia
random_tm<-descomposicion_tm$random #residuo

# Descomposición de la serie de indice bursatil
descomposicion_ib <- decompose(ts_indice_bursatil)
plot(descomposicion_ib)
descomposicion_ib$seasonal #estacionalidad
descomposicion_ib$trend #tendencia
random_ib<-descomposicion_ib$random #residuo

# Descomposición de la serie del paro
descomposicion_paro <- decompose(ts_paro)
plot(descomposicion_paro)
descomposicion_paro$seasonal #estacionalidad
descomposicion_paro$trend #tendencia
random_paro<-descomposicion_paro$random #residuo

########################### PRUEBAS DE ESTACIONARIEDAD ###########################

# Prueba ADF para verificar estacionariedad
adf.test(ts_ipc)
adf.test(ts_pib)

adf.test(ts_masa_monetaria)
adf.test(ts_indice_bursatil)

adf.test(ts_paro)

# Diferenciar las series para eliminar tendencia
ts_ipc <- diff(ts_ipc)
ts_pib <- diff(ts_pib)

ts_masa_monetaria <- diff(ts_masa_monetaria)
ts_indice_bursatil <- diff(ts_indice_bursatil)

ts_paro <- diff(ts_paro)

# Verificar nuevamente la estacionariedad
adf1 <- adf.test(ts_ipc)
adf2<- adf.test(ts_pib)
kpsstest1 <- kpss.test(ts_ipc)
kpsstest2 <- kpss.test(ts_pib)


adf3<- adf.test(ts_masa_monetaria)
adf4<- adf.test(ts_indice_bursatil)
kpsstest3 <- kpss.test(ts_masa_monetaria)
kpsstest4 <- kpss.test(ts_indice_bursatil)

adf5<-adf.test(ts_paro)
kpsstest5 <- kpss.test(ts_paro)

# Graficar series diferenciadas
plot(ts_ipc, main="IPC Diferenciado")
plot(ts_pib, main="PIB Diferenciado")

plot(ts_masa_monetaria, main="IPC Diferenciado")
plot(ts_indice_bursatil, main="PIB Diferenciado")

plot(ts_paro, main="Paro Diferenciado")

# ACF y PACF para series diferenciados
acf(ts_ipc, main="ACF IPC Diferenciado")
pacf(ts_ipc, main="PACF IPC Diferenciado")
acf(ts_pib, main="ACF PIB Diferenciado")
pacf(ts_pib, main="PACF PIB Diferenciado")

acf(ts_masa_monetaria, main="ACF IPC Diferenciado")
pacf(ts_masa_monetaria, main="PACF IPC Diferenciado")
acf(ts_indice_bursatil, main="ACF PIB Diferenciado")
pacf(ts_indice_bursatil, main="PACF PIB Diferenciado")

acf(ts_paro, main="ACF Paro Diferenciado")
pacf(ts_paro, main="PACF Paro Diferenciado")

#Para verificar:
if (adf1$p.value < 0.05) {print("Serie estacionaria")} else {print("Serie NO estacionaria")}
if (adf2$p.value < 0.05) {print("Serie estacionaria")} else {print("Serie NO estacionaria")}
if (kpsstest1$p.value < 0.05) {print("Serie NO estacionaria")} else {print("Serie estacionaria")}
if (kpsstest2$p.value < 0.05) {print("Serie NO estacionaria")} else {print("Serie estacionaria")}

if (adf3$p.value < 0.05) {print("Serie estacionaria")} else {print("Serie NO estacionaria")}
if (adf4$p.value < 0.05) {print("Serie estacionaria")} else {print("Serie NO estacionaria")}
if (kpsstest3$p.value < 0.05) {print("Serie NO estacionaria")} else {print("Serie estacionaria")}
if (kpsstest4$p.value < 0.05) {print("Serie NO estacionaria")} else {print("Serie estacionaria")}

if (adf5$p.value < 0.05) {print("Serie estacionaria")} else {print("Serie NO estacionaria")}
if (kpsstest5$p.value < 0.05) {print("Serie NO estacionaria")} else {print("Serie estacionaria")}



########################### MIRAR ACCURACY ###########################
#queremos predecir datos que ya tenemos para evaluar los modelos
#separar datos en train y test, entrenamiento y prueba
train_ipc<-window(ts_ipc,start=c(1997,1),end=c(2020,4))
test_ipc<-window(ts_ipc,start=c(2021,1),end=c(2022,2))

train_pib<-window(ts_pib,start=c(1997,1),end=c(2020,4))
test_pib<-window(ts_pib,start=c(2021,1),end=c(2022,2))

train_oferta <- window(ts_masa_monetaria,start=c(1997,1), end=c(2020,4))
test_oferta<-window(ts_masa_monetaria,start=c(2021,1),end=c(2022,2))

train_bolsa <- window(ts_indice_bursatil,start=c(1997,1), end=c(2020,4))
test_bolsa<-window(ts_indice_bursatil,start=c(2021,1),end=c(2022,2))

train_paro <- window(ts_paro,start=c(1997,1), end=c(2020,4))
test_paro<-window(ts_paro,start=c(2021,1),end=c(2022,2))



# Ajustar modelos de predicción
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


############################## MODELOS ARIMAX ##############################
# Ajustar modelos arimax individual
arimax_model_ipc <- auto.arima(train_pib, xreg = train_ipc)
arimax_model_oferta <- auto.arima(train_pib, xreg = train_oferta)
arimax_model_bolsa <- auto.arima(train_pib, xreg = train_bolsa)
arimax_model_paro <- auto.arima(train_pib, xreg = train_paro)

#Ajustar arimax multiple
arimax_model2 <- auto.arima(train_pib, xreg = cbind(train_ipc,train_paro,train_oferta,train_bolsa))


#Predicción
autoarima_modelo_ipc <- forecast(arimax_model_ipc, xreg = test_ipc, h = length(test_pib))$mean
autoarima_modelo_oferta <- forecast(arimax_model_oferta, xreg = test_oferta, h = length(test_pib))$mean
autoarima_modelo_bolsa <- forecast(arimax_model_bolsa, xreg = test_bolsa, h = length(test_pib))$mean
autoarima_modelo_paro <- forecast(arimax_model_paro, xreg = test_paro, h = length(test_pib))$mean

forecast_arimax2 <- forecast(arimax_model2, xreg = cbind(test_ipc,test_paro,test_oferta, test_bolsa), h = 2)


# Graficos de resultados
forecast_ipc <- forecast(arimax_model_ipc, xreg = test_ipc, h = length(test_pib))
autoplot(forecast_ipc)
modelo_ipc <- forecast_ipc$mean

forecast_bolsa <- forecast(arimax_model_bolsa, xreg = test_bolsa, h = length(test_pib))
autoplot(forecast_bolsa)
modelo_bolsa <- forecast_bolsa$mean

forecast_oferta <- forecast(arimax_model_oferta, xreg = test_oferta, h = length(test_pib))
autoplot(forecast_oferta)
modelo_oferta <- forecast_oferta$mean

forecast_paro <- forecast(arimax_model_paro, xreg = test_paro, h = length(test_pib))
autoplot(forecast_paro)
modelo_paro <- forecast_paro$mean

forecast_arimax2 <- forecast(arimax_model2, xreg = cbind(test_ipc,test_paro,test_oferta, test_bolsa), h = 2)
modelo_arimax2 <- forecast_arimax2$mean

plot(forecast_ipc, main="Predicción IPC")
plot(forecast_bolsa, main="Predicción Indice Bursátil")
plot(forecast_oferta, main="Predicción Masa monetaria")
plot(forecast_paro, main="Predicción Desempleo")

plot(forecast_arimax2, main="Predicción ARIMAX")

############################### ACCURACY ###############################

accuracy(test_pib, modelo_ipc)  
accuracy(test_pib, modelo_bolsa) 
accuracy(test_pib, modelo_oferta) 
accuracy(test_pib, modelo_paro) #Este el mejor de arimax 

accuracy(test_pib, modelo_arimax2)  

accuracy(drift_pib,test_pib)  
accuracy(snaive_pib,test_pib)  
accuracy(naive_pib,test_pib)  
accuracy(ma_pib,test_pib)  
accuracy(ar_pib_forecast,test_pib)  
accuracy(arma_pib_forecast,test_pib)  
accuracy(arima_forecast_pib,test_pib) #MAPE mas bajo

accuracy(drift_ipc, test_ipc)
accuracy(snaive_ipc, test_ipc)  
accuracy(naive_ipc, test_ipc)  
accuracy(ma_ipc, test_ipc)  
accuracy(ar_ipc_forecast, test_ipc)  
accuracy(arma_ipc_forecast, test_ipc) 
accuracy(arima_forecast_ipc, test_ipc)






##############################################################################

# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_pib)
checkresiduals(modelo_arima_ipc)
#añadir estacionalidad y tendencia a la serie

# Predicción de IPC y PIB con ARIMA a 2 trimestres
modelo_arima_ipc <-auto.arima(ts_ipc)
forecast_ipc <- forecast(modelo_arima_ipc, h = 2)
summary(forecast_ipc)
plot(forecast_ipc, main="Predicción del IPC con ARIMA")
abline(h = 0, col = "red", lty = 2)

modelo_arima_pib <- auto.arima(ts_pib)
forecast_pib <- forecast(modelo_arima_pib, h = 2)
summary(forecast_pib)
plot(forecast_pib, main="Predicción del PIB con ARIMA")
abline(h = 0, col = "red", lty = 2)






