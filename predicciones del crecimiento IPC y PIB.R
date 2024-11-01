#SCRIPT PROCESAMIENTO Y PREDICCIONES PIB + IPC

# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)
library(tidyr)

dir()


# Cargar datos
df1 <- read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")
exogenas <- read.csv("Datos-20240913/exogenas_paises_punto2.csv")
paro<- read.csv("Datos-20240913/unemployment_germany.csv")


##################################################PROCESAMIENTO######################################################


########################### FILTRAR Y LIMPIAR DATOS 

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


########################### CREAR SERIES TEMPORALES

colnames(df_final)<- c('Año', 'Mes','PIB','IPC','oferta_monetaria','indice_bursatil','paro')

# Crear series temporales
ts_ipc <- ts(df_final$IPC, start = c(1997,1),end = c(2022,2),  frequency = 4) #asi no coge los dos ultimos trimestres
ts_pib <- ts(df_final$PIB, start = c(1997,1),end = c(2022,2),  frequency = 4)
ts_masa_monetaria <- ts(df_final$oferta_monetaria, start = c(1997,1),end = c(2022,2),  frequency = 4) #asi no coge los dos ultimos trimestres
ts_indice_bursatil <- ts(df_final$indice_bursatil, start = c(1997,1),end = c(2022,2) , frequency = 4)
ts_paro <- ts(df_final$paro, start = c(1997,1),end = c(2022,2), frequency = 4)

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


########################### ANÁLISIS DE AUTOCORRELACIÓN

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

########################### DESCOMPOSICIÓN DE SERIES 

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


########################### PRUEBAS DE ESTACIONARIEDAD

# Prueba ADF para verificar estacionariedad
adf_ipc<- adf.test(ts_ipc)
adf.test(ts_pib)

adf.test(ts_masa_monetaria)
adf.test(ts_indice_bursatil)

adf.test(ts_paro)

if (adf_ipc$p.value < 0.05) {print("Serie estacionaria")} else {print("Serie NO estacionaria")}

# Diferenciar las series para eliminar tendencia
ts_ipc_diff <- diff(ts_ipc, lag = 1)
ts_pib_diff <- diff(ts_pib, lag = 1)

ts_masa_monetaria <- diff(ts_masa_monetaria,lag = 1)
ts_indice_bursatil <- diff(ts_indice_bursatil, lag = 1)

ts_paro <- diff(ts_paro, lag = 1)

# Verificar nuevamente la estacionariedad
adf1 <- adf.test(ts_ipc_diff)
adf2<- adf.test(ts_pib_diff)
kpsstest1 <- kpss.test(ts_ipc_diff)

kpsstest2 <- kpss.test(ts_pib_diff)


adf3<- adf.test(ts_masa_monetaria)
adf4<- adf.test(ts_indice_bursatil)
kpsstest3 <- kpss.test(ts_masa_monetaria)
kpsstest4 <- kpss.test(ts_indice_bursatil)

adf5<-adf.test(ts_paro)
kpsstest5 <- kpss.test(ts_paro)

# Graficar series diferenciadas
plot(ts_ipc_diff, main="IPC Diferenciado")
plot(ts_pib_diff, main="PIB Diferenciado")

plot(ts_masa_monetaria, main="IPC Diferenciado")
plot(ts_indice_bursatil, main="PIB Diferenciado")

plot(ts_paro, main="Paro Diferenciado")

# ACF y PACF para series diferenciados
acf(ts_ipc_diff, main="ACF IPC Diferenciado")
pacf(ts_ipc_diff, main="PACF IPC Diferenciado")
acf(ts_pib_diff, main="ACF PIB Diferenciado")
pacf(ts_pib_diff, main="PACF PIB Diferenciado")

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


##############################################PREDICCIONES ##############################################


########################### MIRAR ACCURACY ###########################
#queremos predecir datos que ya tenemos para evaluar los modelos
#separar datos en train y test, entrenamiento y prueba
train_ipc<-window(ts_ipc_diff,start= c(1997,1),end=c(2016,1))
test_ipc<-window(ts_ipc_diff,start=c(2016,2), end= c(2022,2))

train_pib<-window(ts_pib_diff,start= c(1997,1),end=c(2016,1))
test_pib<-window(ts_pib_diff,start=c(2016,2), end= c(2022,2))


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

#mean forecast
meanf_pib<-meanf(train_pib,h=length(test_pib))
meanf_ipc<-meanf(train_ipc,h=length(test_ipc))

#promedio movil (MA)
ma_pib <- Arima(train_pib, order = c(0,0,1))
prediccion_ma_pib <- forecast(ma_pib, h = length(test_pib))

ma_ipc <- Arima(train_ipc, order = c(0,0,1))
prediccion_ma_ipc <- forecast(ma_ipc, h = length(test_ipc))

#modelo autoregresivo (AR)
ar_ipc<-Arima(train_ipc,order = c(2,0,0))
ar_ipc_forecast<-forecast(ar_ipc,h=length(test_ipc))

ar_pib<-Arima(train_pib,order = c(2,0,0))
ar_pib_forecast<-forecast(ar_pib,h=length(test_pib))

#ARMA
arma_ipc<-Arima(train_ipc,order = c(1,0,1))
arma_ipc_forecast<-forecast(arma_ipc,h=length(test_ipc))

arma_pib<-Arima(train_pib,order = c(1,0,1))
arma_pib_forecast<-forecast(arma_pib,h=length(test_pib))

#arima
modelo_arima_ipc <- auto.arima(train_ipc,seasonal = FALSE)
summary(modelo_arima_ipc)
arima_forecast_ipc<-forecast(modelo_arima_ipc,h=length(test_ipc))

modelo_arima_pib <- auto.arima(train_pib,seasonal = FALSE)
summary(modelo_arima_pib)
arima_forecast_pib<-forecast(modelo_arima_pib,h=length(test_pib))

#sarima
modelo_sarima_ipc <- auto.arima(train_ipc, seasonal = TRUE)
forecast_sarima_ipc <- forecast(modelo_sarima_ipc, h = length(test_ipc))

modelo_sarima_pib <- auto.arima(train_pib, seasonal = TRUE)
forecast_sarima_pib <- forecast(modelo_sarima_pib, h = length(test_pib))

################################################################
#Analizamos los resultados para ver que modelo es el más preciso
resultados_pib <- data.frame(
  Modelo = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

agregar_resultado <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_pib <<- rbind(resultados_pib, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}


# Evaluar cada modelo y almacenar sus resultados
agregar_resultado("AutoARIMA", arima_forecast_pib, test_pib)
agregar_resultado("Mean Forecast", meanf_pib, test_pib)
agregar_resultado("SNaive", snaive_pib, test_pib)
agregar_resultado("Naive", naive_pib, test_pib)
agregar_resultado("Random Walk Forecast", drift_pib, test_pib)
agregar_resultado("AR", ar_pib_forecast, test_pib)
agregar_resultado("MA", prediccion_ma_pib, test_pib)
agregar_resultado("ARMA", arma_pib_forecast, test_pib)
agregar_resultado("SARIMA", forecast_sarima_pib, test_pib)

# Ordenar la tabla de resultados por la métrica
resultados_pib <- resultados_pib[order(resultados_pib$MAPE), ]
resultados_pib
head(resultados_pib) #SARIMA, ARMA, ARIMA

######

resultados_ipc <- data.frame(
  Modelo = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

agregar_resultado <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_ipc <<- rbind(resultados_ipc, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}


# Evaluar cada modelo y almacenar sus resultados
agregar_resultado("AutoARIMA", arima_forecast_ipc, test_ipc)
agregar_resultado("Mean Forecast", meanf_ipc, test_ipc)
agregar_resultado("SNaive", snaive_ipc, test_ipc)
agregar_resultado("Naive", naive_ipc, test_ipc)
agregar_resultado("Random Walk Forecast", drift_ipc, test_ipc)
agregar_resultado("AR", ar_ipc_forecast, test_ipc)
agregar_resultado("MA", prediccion_ma_ipc, test_ipc)
agregar_resultado("ARMA", arma_ipc_forecast, test_ipc)
agregar_resultado("SARIMA", forecast_sarima_ipc, test_ipc)

# Ordenar la tabla de resultados por la métrica
resultados_ipc <- resultados_ipc[order(resultados_ipc$MAPE), ]
resultados_ipc
head(resultados_ipc) #ARIMA, SARIMA, MEANF


##############################################################################

# Verificar los residuos de los modelos ARMA y ARIMA
checkresiduals(arima_forecast_pib)
checkresiduals(forecast_sarima_pib)
checkresiduals(forecast_sarima_ipc)


# Predicción de PIB con ARIMA a 2 trimestres
modelo_arima_pib <- auto.arima(ts_pib_diff,seasonal = FALSE)
summary(modelo_arima_pib)
arima_forecast_pib<-forecast(modelo_arima_pib,h=6)
summary(arima_forecast_pib)
plot(arima_forecast_pib, main="Predicción del PIB con ARIMA")
abline(h = 0, col = "red", lty = 2)

# Predicción de PIB con SARIMA a 2 trimestres
prediccion_sarima_pib <- auto.arima(ts_pib_diff, seasonal = TRUE)
prediccion_sarima_pib <- forecast(prediccion_sarima_pib, h = 6)
plot(prediccion_sarima_pib, main="Predicción del PIB con SARIMA")
abline(h = 0, col = "red", lty = 2)

#nos quedamos con sarima que es el modelo que mejores resultados muestra para el PIB


# Predicción de IPC con SARIMA a 2 trimestres
# Ajuste del modelo
# Predicción de los dos últimos trimestres de 2022

prediccion_sarima_ipc <- auto.arima(ts_ipc_diff, seasonal = TRUE)
prediccion_sarima_ipc <- forecast(prediccion_sarima_ipc, h = 6)
plot(prediccion_sarima_ipc, main="Predicción del IPC con SARIMA")
abline(h = 0, col = "red", lty = 2)

ts_pib
write.csv(ts_pib, file = "seriePIB.csv", row.names = TRUE)
write.csv(ts_ipc, file = "serieIPC.csv", row.names = TRUE)

############################## MODELOS ARIMAX ##############################

train_oferta <- window(ts_masa_monetaria,start=c(1997,1), end=c(2016,1))
test_oferta<-window(ts_masa_monetaria,start=c(2016,2),end=c(2022,2))

train_bolsa <- window(ts_indice_bursatil,start=c(1997,1), end=c(2016,1))
test_bolsa<-window(ts_indice_bursatil,start=c(2016,2),end=c(2022,2))

train_paro <- window(ts_paro,start=c(1997,1), end=c(2016,1))
test_paro<-window(ts_paro,start=c(2016,2),end=c(2022,2))

# Ajustar modelos arimax individual
arimax_pib_ipc <- auto.arima(train_pib, xreg = train_ipc)
arimax_pib_oferta <- auto.arima(train_pib, xreg = train_oferta)
arimax_pib_bolsa <- auto.arima(train_pib, xreg = train_bolsa)
arimax_pib_paro <- auto.arima(train_pib, xreg = train_paro)

#Ajustar arimax multiple
arimax_multiple_pib <- auto.arima(train_pib, xreg = cbind(train_ipc,train_paro,train_oferta,train_bolsa))


#Predicción
autoarima_pib_ipc <- forecast(arimax_pib_ipc, xreg = test_ipc, h = length(test_pib))$mean
autoarima_pib_oferta <- forecast(arimax_pib_oferta, xreg = test_oferta, h = length(test_pib))$mean
autoarima_pib_bolsa <- forecast(arimax_pib_bolsa, xreg = test_bolsa, h = length(test_pib))$mean
autoarima_pib_paro <- forecast(arimax_pib_paro, xreg = test_paro, h = length(test_pib))$mean

forecast_arimax_multiple_pib <- forecast(arimax_multiple_pib, xreg = cbind(test_ipc,test_paro,test_oferta, test_bolsa), h = 2)


# Graficos de resultados
forecast_pib_ipc <- forecast(arimax_pib_ipc, xreg = test_ipc, h = length(test_pib))
autoplot(forecast_pib_ipc)
modelo_pib_ipc <- forecast_pib_ipc$mean

forecast_pib_bolsa <- forecast(arimax_pib_bolsa, xreg = test_bolsa, h = length(test_pib))
autoplot(forecast_pib_bolsa)
modelo_pib_bolsa <- forecast_pib_bolsa$mean

forecast_pib_oferta <- forecast(arimax_pib_oferta, xreg = test_oferta, h = length(test_pib))
autoplot(forecast_pib_oferta)
modelo_pib_oferta <- forecast_pib_oferta$mean

forecast_pib_paro <- forecast(arimax_pib_paro, xreg = test_paro, h = length(test_pib))
autoplot(forecast_pib_paro)
modelo_pib_paro <- forecast_pib_paro$mean

forecast_pib_multiple <- forecast(forecast_arimax_multiple_pib, xreg = cbind(test_ipc,test_paro,test_oferta, test_bolsa), h = length(test_pib))
autoplot(forecast_pib_multiple)
modelo_arimax_pib <- forecast_pib_multiple$mean

plot(forecast_pib_ipc, main="Predicción IPC")
plot(forecast_pib_bolsa, main="Predicción Indice Bursátil")
plot(forecast_pib_oferta, main="Predicción Masa monetaria")
plot(forecast_pib_paro, main="Predicción Desempleo")

plot(forecast_pib_multiple, main="Predicción ARIMAX")

############################### ACCURACY ARIMAX ###############################
resultados_arimax <- data.frame(
  Modelo = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

agregar_resultado <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_arimax <<- rbind(resultados_arimax, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}

# Evaluar cada modelo y almacenar sus resultados
agregar_resultado("PIB-IPC", forecast_pib_ipc, test_pib)
agregar_resultado("PIB-INDICE BURSATIL", forecast_pib_bolsa, test_pib)
agregar_resultado("PIB-OFERTA MONETARIA", forecast_pib_oferta, test_pib)
agregar_resultado("PIB-DESEMPLEO", forecast_pib_paro, test_pib)
agregar_resultado("PIB-EXOGENAS", forecast_pib_multiple, test_pib)

# Ordenar la tabla de resultados por la métrica
resultados_arimax <- resultados_arimax[order(resultados_arimax$MAPE), ]
resultados_arimax
head(resultados_arimax) #LA OFERTA MONETARIA RELACIONADA CON EL PIB TIENE EL MEJOR VALOR


####################################### REVERTIR DIFERENCIACIÓN
#hay que revertir la estacionalidad para tener las predicciones reales sin los datos diferenciados

#extraer los valores de las predicciones del pib e ipc
pred_pib<-as.data.frame(prediccion_sarima_pib)
class(pred_pib)
colnames(pred_pib)
pred_pib<-pred_pib[,-c(2:5)]


pred_ipc<-as.data.frame(prediccion_sarima_ipc)
class(pred_ipc)
colnames(pred_ipc)
pred_ipc<-pred_ipc[,-c(2:5)]

# Crear un nuevo objeto ts con las predicciones
pib_nuevo <- ts(c(ts_pib_diff, pred_pib), start = start(ts_pib_diff), frequency = frequency(ts_pib_diff))
ipc_nuevo <- ts(c(ts_ipc_diff, pred_ipc), start = start(ts_ipc_diff), frequency = frequency(ts_ipc_diff))

#ultimo valor de la serie original antes de diferenciarla
ultimo_valor_conocido_pib <- tail(ts_pib, 1)
ultimo_valor_conocido_ipc <- tail(ts_ipc, 1)
#predicciones del crecimiento real
pred_real_pib <- diffinv(pib_nuevo, xi = ultimo_valor_conocido_pib)
pred_real_ipc <- diffinv(ipc_nuevo, xi = ultimo_valor_conocido_ipc)

#pasar a data frame los datos
# Crear la matriz de datos (esto es solo un ejemplo basado en lo que compartiste)
pred_real_pib <- matrix(c(pred_real_pib), nrow = 26, byrow = TRUE)

# Nombres de las columnas
colnames(pred_real_pib) <- c("Qtr1", "Qtr2", "Qtr3", "Qtr4")

# Años correspondientes
anios <- 1998:2023  # De 1998 a 2023

# Convertir la matriz a un data frame
df_pib <- as.data.frame(pred_real_pib)

# Añadir la columna de años
df_pib$Año <- anios

# Reorganizar el data frame para tener una columna para los valores
library(tidyr)
df_pib_long <- pivot_longer(df_pib, cols = c("Qtr1", "Qtr2", "Qtr3", "Qtr4"), names_to = "Trimestre", values_to = "Valor")

# Reorganizar las columnas
df_pib_long <- df_pib_long[, c("Año", "Trimestre", "Valor")]

# Ver el data frame final
print(df_pib_long)

#lo mismo con el ipc
pred_real_ipc <- matrix(c(pred_real_ipc), nrow = 26, byrow = TRUE)

# Nombres de las columnas
colnames(pred_real_ipc) <- c("Qtr1", "Qtr2", "Qtr3", "Qtr4")

# Años correspondientes
anios <- 1998:2023  # De 1998 a 2023

# Convertir la matriz a un data frame
df_ipc <- as.data.frame(pred_real_ipc)

# Añadir la columna de años
df_ipc$Año <- anios

# Reorganizar el data frame para tener una columna para los valores
library(tidyr)
df_ipc_long <- pivot_longer(df_ipc, cols = c("Qtr1", "Qtr2", "Qtr3", "Qtr4"), names_to = "Trimestre", values_to = "Valor")

# Reorganizar las columnas
df_ipc_long <- df_ipc_long[, c("Año", "Trimestre", "Valor")]

print(df_ipc_long)


#pasar a .csv (habra que cambiar la ruta supongo)
getwd()

write.csv2(df_pib_long, file = "/Users/leire/OneDrive/Escritorio/Bda2/reto5/Nueva carpeta/Reto5_Azul_Claro/Datos-20240913/pred_real_pib.csv", row.names = FALSE)

write.csv2(df_ipc_long, file = "/Users/leire/OneDrive/Escritorio/Bda2/reto5/Nueva carpeta/Reto5_Azul_Claro/Datos-20240913/pred_real_ipc.csv", row.names = FALSE)


