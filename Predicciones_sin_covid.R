#limpieza datos reto 5
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

# Cargar datos

df1 <- read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")

########################### FILTRAR Y LIMPIAR DATOS ###########################

# Filtrar Alemania
df1 <- df1 %>% filter(Code == "DEU")

#Eliminar columnas irrelevantes
df1 <- df1[,-c(1,3)]

# Filtrar datos trimestrales
df1 <- df1 %>% filter(Month %in% c(3,6,9,12))

#crear df para pib y ipc
pib <- df1[,-5]
ipc <- df1[,-4]

# Filtrar datos fuera del periodo de COVID-19
# Supongamos que el periodo de COVID-19 es de 2020 a 2022
pib_sincovid <- pib %>%
  filter(!(Year >= 2020))
ipc_sincovid <- ipc %>%
  filter(!(Year >= 2020))

# Visualizar los datos filtrados
head(pib_sincovid)
str(pib_sincovid)
sum(is.na(pib_sincovid))

head(ipc_sincovid)
str(ipc_sincovid)
sum(is.na(ipc_sincovid))

#crear columna para el crecimiento interanual
pib_sincovid$Crecimiento_Interanual<- (pib_sincovid$GDP.billion.currency.units / lag(pib_sincovid$GDP.billion.currency.units, 4) - 1) * 100
ipc_sincovid$Crecimiento_Interanual <- (ipc_sincovid$Consumer.Price.Index..CPI. / lag(ipc_sincovid$Consumer.Price.Index..CPI., 4) - 1) * 100

#####SERIES TEMPORALES####

#crear serie temporal
ts_ipc_sincovid <- ts(ipc_sincovid$Crecimiento_Interanual, start = c(1996, 3), end = c(2019, 4), frequency = 4)
ts_pib_sincovid <- ts(pib_sincovid$Crecimiento_Interanual, start = c(1996, 3), end = c(2019, 4), frequency = 4)

# Graficar las series temporales
plot(ts_ipc_sincovid, main = "Serie Temporal del IPC en Alemania", ylab = "IPC", xlab = "Tiempo", col= "#8db41c")
plot(ts_pib_sincovid, main = "Serie Temporal del PIB en Alemania", ylab = "PIB", xlab = "Tiempo", col= "#93044e")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc_sincovid)
ggplot_na_distribution(ts_pib_sincovid)

#Comprobar si hay valores faltantes
sum(is.na(ts_pib_sincovid))
sum(is.na(ts_ipc_sincovid))

####ANALISIS DE AUTOCORRELACION####

#ACF y PACF
ts_ipc_sincovid <- na.omit(ts_ipc_sincovid)
ts_pib_sincovid <- na.omit(ts_pib_sincovid)

acf(ts_ipc_sincovid, main = "ACF del IPC", col= "#8db41c")
pacf(ts_ipc_sincovid, main = "PACF del IPC", col= "#8db41c")

acf(ts_pib_sincovid, main = "ACF del PIB", col= "#93044e")
pacf(ts_pib_sincovid, main = "PACF del PIB", col= "#93044e")

####DESCOMPOSICION DE SERIES TEMPORALES####

# Descomposición de las serie de IPC
descomposicion_ipc_sincovid <- decompose(ts_ipc_sincovid)
plot(descomposicion_ipc_sincovid, col= "#8db41c")
random_ipc_sincovid <- descomposicion_ipc_sincovid$random
descomposicion_ipc_sincovid$seasonal
descomposicion_ipc_sincovid$trend


# Descomposición de las serie de PIB
descomposicion_pib_sincovid <- decompose(ts_pib_sincovid)
plot(descomposicion_pib_sincovid, col= "#93044e")
random_pib_sincovid<-descomposicion_pib_sincovid$random
descomposicion_pib_sincovid$seasonal
descomposicion_pib_sincovid$trend
plot(descomposicion_pib_sincovid, col= "#93044e")

####PRUEBA DE ESTACIOANRIEDAD####

# Prueba ADF para verificar estacionariedad de IPC y PIB
adf.test(ts_ipc_sincovid)
adf.test(ts_pib_sincovid)

# Diferenciar las series para eliminar tendencia
ts_ipc_diff_sincovid<-diff(ts_ipc_sincovid)
ts_pib_diff_sincovid<-diff(ts_pib_sincovid)

# Verificar nuevamente la estacionariedad
adf.test(ts_ipc_diff_sincovid)
adf.test(ts_pib_diff_sincovid)

# Graficar series diferenciadas
plot(ts_ipc_diff_sincovid, main="IPC Diferenciado", col= "#8db41c")
plot(ts_pib_diff_sincovid, main="PIB Diferenciado", col= "#93044e")

# ACF y PACF para IPC y PIB diferenciados
acf(ts_ipc_diff_sincovid, main="ACF IPC Diferenciado", col= "#8db41c")
pacf(ts_ipc_diff_sincovid, main="PACF IPC Diferenciado", col= "#8db41c")
acf(ts_pib_diff_sincovid, main="ACF PIB Diferenciado", col= "#93044e")
pacf(ts_pib_diff, main="PACF PIB Diferenciado", col= "#93044e")


#####COMPROBAR ACCURACY#####
#IPC
train_ipc_sincovid<-window(ts_ipc_diff_sincovid, start = c(1996, 6), end = c(2016, 1))
test_ipc_sincovid <- window(ts_ipc_diff_sincovid, start = c(2016,2), end = c(2019,4))

#naive
naive_ipc_sincovid<-naive(train_ipc_sincovid,h=length(test_ipc_sincovid))

#seasonal naive
snaive_ipc_sincovid<-snaive(train_ipc_sincovid,h=length(test_ipc_sincovid))

#drift
drift_ipc_sincovid<-rwf(train_ipc_sincovid,drift=TRUE,h=length(test_ipc_sincovid))

#mean forecast
meanf_ipc_sincovid<-meanf(train_ipc_sincovid,h=length(test_ipc_sincovid))

#promedio movil (MA)
ma_ipc_sincovid <- Arima(train_ipc_sincovid, order = c(0,0,1))
prediccion_ma_ipc_sincovid <- forecast(ma_ipc_sincovid, h = length(test_ipc_sincovid))

#modelo autoregresivo (AR)
ar_ipc_sincovid<-Arima(train_ipc_sincovid,order = c(2,0,0))
ar_ipc_forecast_sincovid<-forecast(ar_ipc_sincovid,h=length(test_ipc_sincovid))

#ARMA
arma_ipc_sincovid<-Arima(train_ipc_sincovid,order = c(1,0,1))
arma_ipc_forecast_sincovid<-forecast(arma_ipc_sincovid,h=length(test_ipc_sincovid))

#arima
arima_ipc_sincovid <- auto.arima(train_ipc_sincovid,seasonal = FALSE)
summary(arima_ipc_sincovid)
arima_forecast_ipc_sincovid<-forecast(arima_ipc_sincovid,h=length(test_ipc_sincovid))

#sarima
sarima_ipc_sincovid <- auto.arima(train_ipc_sincovid, seasonal = TRUE)
forecast_sarima_ipc_sincovid <- forecast(sarima_ipc_sincovid, h = length(test_ipc_sincovid))


#calcular precisión de modelos de predicción
resultados_ipc_sincovid <- data.frame(
  Modelo = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

agregar_resultado_sincovid <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_ipc_sincovid <<- rbind(resultados_ipc_sincovid, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}


# Evaluar cada modelo y almacenar sus resultados
agregar_resultado_sincovid("AutoARIMA", arima_forecast_ipc_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("Mean Forecast", meanf_ipc_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("SNaive", snaive_ipc_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("Naive", naive_ipc_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("Random Walk Forecast", drift_ipc_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("AR", ar_ipc_forecast_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("MA", prediccion_ma_ipc_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("ARMA", arma_ipc_forecast_sincovid, test_ipc_sincovid)
agregar_resultado_sincovid("SARIMA", forecast_sarima_ipc_sincovid, test_ipc_sincovid)

# Ordenar la tabla de resultados por la métrica
resultados_ipc_sincovid <- resultados_ipc_sincovid[order(resultados_ipc_sincovid$MAPE), ]
resultados_ipc_sincovid
head(resultados_ipc_sincovid) #ARIMA, AR, MA



#PIB
train_pib_sincovid<-window(ts_pib_diff, start = c(1996, 3), end = c(2016, 1))
test_pib_sincovid<- window(ts_pib_diff, start = c(2016,2), end = c(2019,4))

#naive
naive_pib_sincovid<-naive(train_pib_sincovid,h=length(test_pib_sincovid))

#seasonal naive
snaive_pib_sincovid<-snaive(train_pib_sincovid,h=length(test_pib_sincovid))

#drift
drift_pib_sincovid<-rwf(train_pib_sincovid,drift=TRUE,h=length(test_pib_sincovid))

#mean forecast
meanf_pib_sincovid<-meanf(train_pib_sincovid,h=length(test_pib_sincovid))

#promedio movil (MA)
ma_pib_sincovid <- Arima(train_pib_sincovid, order = c(0,0,1))
prediccion_ma_pib_sincovid <- forecast(ma_pib_sincovid, h = length(test_pib_sincovid))

#modelo autoregresivo (AR)
ar_pib_sincovid<-Arima(train_pib_sincovid,order = c(2,0,0))
ar_pib_forecast_sincovid<-forecast(ar_pib_sincovid,h=length(test_pib_sincovid))

#ARMA
arma_pib_sincovid<-Arima(train_pib_sincovid,order = c(1,0,1))
arma_pib_forecast_sincovid<-forecast(arma_pib_sincovid,h=length(test_pib_sincovid))

#arima
arima_pib_sincovid <- auto.arima(train_pib_sincovid,seasonal = FALSE)
summary(arima_pib_sincovid)
arima_forecast_pib_sincovid<-forecast(arima_pib_sincovid,h=length(test_pib_sincovid))

#sarima
sarima_pib_sincovid <- auto.arima(train_pib_sincovid, seasonal = TRUE)
forecast_sarima_pib_sincovid <- forecast(sarima_pib_sincovid, h = length(test_pib_sincovid))


#calcular precisión de modelos de predicción
resultados_pib_sincovid <- data.frame(
  Modelo = character(),
  MAE = numeric(),
  RMSE = numeric(),
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

agregar_resultado_sincovid <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_pib_sincovid <<- rbind(resultados_pib_sincovid, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}


# Evaluar cada modelo y almacenar sus resultados
agregar_resultado_sincovid("AutoARIMA", arima_forecast_pib_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("Mean Forecast", meanf_pib_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("SNaive", snaive_pib_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("Naive", naive_pib_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("Random Walk Forecast", drift_pib_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("AR", ar_pib_forecast_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("MA", prediccion_ma_pib_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("ARMA", arma_pib_forecast_sincovid, test_pib_sincovid)
agregar_resultado_sincovid("SARIMA", forecast_sarima_pib_sincovid, test_pib_sincovid)

# Ordenar la tabla de resultados por la métrica
resultados_pib_sincovid <- resultados_pib_sincovid[order(resultados_pib_sincovid$MAPE), ]
resultados_pib_sincovid
head(resultados_pib_sincovid) #NAIVE, SARIMA, ARIMA


################PREDICCIONES######################################
#IPC
# Verificar los residuos de los modelos SARIMA
checkresiduals(sarima_ipc_sincovid, col= "#8db41c")
checkresiduals(arima_pib_sincovid, col= "#93044e")


# Predicción de IPC con ARIMA para los 2 ultimos trimestres de 2022
sarima_ipc_sincovid <- auto.arima(ts_ipc_diff, seasonal = TRUE)
forecast_sarima_ipc_sincovid <- forecast(sarima_ipc_sincovid, h = 6)
plot(forecast_sarima_ipc_sincovid, main="Predicción del IPC sin época covid con SARIMA", col= "#8db41c")
abline(h = 0, col = "red", lty = 2)

#PIB

# Predicción de PIB con ARIMA a 2 trimestres
sarima_pib_sincovid <- auto.arima(ts_pib_diff, seasonal = TRUE)
forecast_sarima_pib_sincovid <- forecast(sarima_pib_sincovid, h = 6)
plot(forecast_sarima_pib_sincovid, main="Predicción del PIB sin época covid con SARIMA", col= "#93044e")
abline(h = 0, col = "red", lty = 2)




## CAMBIAR 2020 por 2022
forecast_pib_sincovid <- data.frame(forecast_sarima_pib_sincovid)
row.names(forecast_pib_sincovid) <- gsub("2020", "2022", row.names(forecast_pib_sincovid))

forecast_pib_sincovid

forecast_ipc_sincovid <- data.frame(forecast_sarima_ipc_sincovid)
row.names(forecast_ipc_sincovid) <- gsub("2020", "2022", row.names(forecast_ipc_sincovid))

forecast_ipc_sincovid


####################################### REVERTIR DIFERENCIACIÓN
#hay que revertir la estacionalidad para tener las predicciones reales sin los datos diferenciados

#extraer los valores de las predicciones del pib e ipc
pred_pib_sincovid<-as.data.frame(forecast_pib_sincovid)
class(pred_pib_sincovid)
colnames(pred_pib_sincovid)
pred_pib_sincovid<-pred_pib_sincovid[,-c(2:5)]


pred_ipc_sincovid<-as.data.frame(forecast_ipc_sincovid)
class(pred_ipc_sincovid)
colnames(pred_ipc_sincovid)
pred_ipc_sincovid<-pred_ipc_sincovid[,-c(2:5)]

# Crear un nuevo objeto ts con las predicciones
pib_nuevo_sincovid <- ts(c(ts_pib_diff_sincovid, pred_pib_sincovid), start = start(ts_pib_diff_sincovid), frequency = frequency(ts_pib_diff_sincovid))
ipc_nuevo_sincovid <- ts(c(ts_ipc_diff_sincovid, pred_ipc_sincovid), start = start(ts_ipc_diff_sincovid), frequency = frequency(ts_ipc_diff_sincovid))

#ultimo valor de la serie original antes de diferenciarla
ultimo_valor_conocido_pib_sincovid <- tail(ts_pib_sincovid, 1)
ultimo_valor_conocido_ipc_sincovid <- tail(ts_ipc_sincovid, 1)
#predicciones del crecimiento real
predreal_pib_sincovid <- diffinv(pib_nuevo_sincovid, xi = ultimo_valor_conocido_pib_sincovid)
predreal_ipc_sincovid <- diffinv(ipc_nuevo_sincovid, xi = ultimo_valor_conocido_ipc_sincovid)
#pasar a data frame los datos
#PIB

# Crear los datos en forma de lista para cada trimestre
Qtr1pib <- c(NA, 3.8535817, 7.5004310, 7.6932148, 6.9775781, 7.6631410, 7.2063511, 5.2963409, 
          6.1947424, 6.1761418, 8.7203233, 9.6806240, 6.6227569, 1.2751389, 10.0968066, 
          9.5668798, 6.3824897, 7.9574835, 8.4666088, 8.1899142, 7.8659623, 9.2526074, 
          7.0450327, 8.2430645, 8.0312218)
Qtr2pib <- c(NA, 4.9179981, 7.8256592, 7.6846024, 5.5767840, 8.4533126, 5.5946146, 5.6221906, 
          6.5570492, 6.5102478, 10.2262440, 8.6832737, 3.8267755, 3.9915260, 10.0637781, 
          8.2464424, 6.2539820, 7.6202873, 9.1548708, 8.6307773, 7.6897929, 9.7311788, 
          7.5410044, 7.6290168, 8.0312218)
Qtr3pib <- c(2.3141983, 8.1205832, 7.6995964, 8.6908578, 7.8607646, 4.9648985, 5.6633619, 
          7.5667704, 4.8870030, 9.2227033, 10.6012730, 7.8232640, -0.3218256, 8.6193578, 
          11.7013441, 7.7805538, 5.3889896, 9.8541040, 7.6832972, 8.3131123, 9.5256789, 
          7.9110496, 8.2288945, 7.4715989, NA)
Qtr4pib <- c(5.0863883, 5.7397961, 7.9233119, 7.8240488, 7.3187300, 6.2153635, 5.1513338, 
          8.2560923, 6.2283108, 7.9956421, 9.5957436, 8.6942187, -1.5249488, 9.8688621, 
          10.0677850, 6.5916556, 7.9192771, 8.3471785, 8.2535984, 9.8217046, 7.6595326, 
          8.8605619, 7.1357682, 8.0312218, NA)

# Crear el data frame
predreal_pib_sincovid  <- data.frame(Year = 1997:2021, Qtr1pib, Qtr2pib, Qtr3pib, Qtr4pib)

# Imprimir el data frame para visualizar la matriz alineada
print(predreal_pib_sincovid, row.names = FALSE)


# Nombres de las columnas
colnames(predreal_pib_sincovid) <- c("year","Qtr1", "Qtr2", "Qtr3", "Qtr4")

# Reorganizar el data frame para tener una columna para los valores
library(tidyr)
dfsin_pib_long_sincovid <- pivot_longer(predreal_pib_sincovid, cols = c("Qtr1", "Qtr2", "Qtr3", "Qtr4"), names_to = "Trimestre", values_to = "PIB")
dfsin_pib_long_sincovid$year[dfsin_pib_long_sincovid$year > 2019] <- 2022
dfsin_pib_long_sincovid$year[c(97, 98,99,100)] <- 2023
# Ver el data frame final
print(dfsin_pib_long_sincovid)



#IPC

# Crear los datos en forma de lista para cada trimestre
# Crear el data frame con los datos
  Qtr1ipc <- c(NA, 1.6608384, 0.7310436, 0.9867569, 1.7500246, 1.9792399, 1.3281884, 1.3158576, 
           2.1533107, 2.8306107, 1.1396373, 2.8552412, 3.2350793, -0.2232195, 1.4124364, 
           3.0163096, 2.4151308, 1.7516042, 1.0204610, 1.0139703, 0.8085780, 2.0908905, 
           2.4445728, 1.5328676, 1.5696943)
  Qtr2ipc <- c(NA, 1.6589315, 0.3412648, 1.6364336, 2.3826997, 1.5865631, 1.3213041, 1.3091241, 
           2.5034811, 2.3342046, 1.5972444, 3.2867339, 1.3165355, 1.0858800, 2.0533832, 
           2.4456056, 2.2927285, 1.4348047, 0.3122680, 0.4127703, 1.8179888, 1.6937765, 
           1.8668698, 1.9508976, 1.7447169)
  Qtr3ipc <- c(1.5325670, 0.7330755, 0.7303698, 1.6309179, 1.9927336, 2.0865631, 1.4385570, 
           1.3024722, 2.0101242, 2.0961332, 2.1768810, 3.4995449, 0.5408715, 1.4150642, 
           2.5899415, 2.5346887, 2.0691482, 0.9207829, 0.5133740, 0.3118640, 1.7145691, 
           1.8897467, 1.5707864, 2.0608399, NA)
  Qtr4ipc <- c(1.6627504, 0.9917972, 0.5986599, 1.6254448, 2.6196746, 1.0778997, 1.1931582, 
           2.1556700, 1.8802162, 2.3217624, 2.1633427, 3.5899415, 0.2115631, 1.0830228, 
           2.5873730, 2.1102973, 2.1784368, 1.1252687, 1.3182029, 0.3110656, 1.9014239, 
           2.2643490, 1.7441301, 1.6608311, NA)

# Crear el data frame
predreal_ipc_sincovid  <- data.frame(Year = 1997:2021, Qtr1ipc, Qtr2ipc, Qtr3ipc, Qtr4ipc)

# Imprimir el data frame para visualizar la matriz alineada
print(predreal_ipc_sincovid, row.names = FALSE)


# Nombres de las columnas
colnames(predreal_ipc_sincovid) <- c("year","Qtr1", "Qtr2", "Qtr3", "Qtr4")

# Reorganizar el data frame para tener una columna para los valores
library(tidyr)
dfsin_ipc_long_sincovid <- pivot_longer(predreal_ipc_sincovid, cols = c("Qtr1", "Qtr2", "Qtr3", "Qtr4"), names_to = "Trimestre", values_to = "IPC")
dfsin_ipc_long_sincovid$year[dfsin_ipc_long_sincovid$year > 2019] <- 2022
dfsin_ipc_long_sincovid$year[c(97, 98,99,100)] <- 2023
# Ver el data frame final
print(dfsin_ipc_long_sincovid)


#pasar a .csv (habra que cambiar la ruta supongo)
getwd()

write.csv2(dfsin_pib_long_sincovid, file = "Datos-20240913/pred_real_pib_sincovid.csv", row.names = FALSE)

write.csv2(dfsin_ipc_long_sincovid, file = "Datos-20240913/pred_real_ipc_sincovid.csv", row.names = FALSE)
