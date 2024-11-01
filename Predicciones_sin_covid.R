#limpieza datos reto 5
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

# Cargar datos
<<<<<<< HEAD
dir()
=======
setwd("C:/Users/leire/OneDrive/Escritorio/Bda2/reto5/Nueva carpeta/Reto5_Azul_Claro")
>>>>>>> 3d25225731b42a5f318c5b5b914ab1e55ede73d1
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
plot(ts_ipc_sincovid, main = "Serie Temporal del IPC en Alemania", ylab = "IPC", xlab = "Tiempo")
plot(ts_pib_sincovid, main = "Serie Temporal del PIB en Alemania", ylab = "PIB", xlab = "Tiempo")

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

acf(ts_ipc_sincovid, main = "ACF del IPC")
pacf(ts_ipc_sincovid, main = "PACF del IPC")

acf(ts_pib_sincovid, main = "ACF del PIB")
pacf(ts_pib_sincovid, main = "PACF del PIB")

####DESCOMPOSICION DE SERIES TEMPORALES####

# Descomposición de las serie de IPC
descomposicion_ipc_sincovid <- decompose(ts_ipc_sincovid)
plot(descomposicion_ipc_sincovid)
random_ipc_sincovid <- descomposicion_ipc_sincovid$random
descomposicion_ipc_sincovid$seasonal
descomposicion_ipc_sincovid$trend


# Descomposición de las serie de PIB
descomposicion_pib_sincovid <- decompose(ts_pib_sincovid)
plot(descomposicion_pib_sincovid)
random_pib_sincovid<-descomposicion_pib_sincovid$random
descomposicion_pib_sincovid$seasonal
descomposicion_pib_sincovid$trend
plot(descomposicion_pib_sincovid)

####PRUEBA DE ESTACIOANRIEDAD####

# Prueba ADF para verificar estacionariedad de IPC y PIB
adf.test(ts_ipc_sincovid)
adf.test(ts_pib_sincovid)

# Diferenciar las series para eliminar tendencia
ts_ipc_diff<-diff(ts_ipc_sincovid)
ts_pib_diff<-diff(ts_pib_sincovid)

# Verificar nuevamente la estacionariedad
adf.test(ts_ipc_diff)
adf.test(ts_pib_diff)

# Graficar series diferenciadas
plot(ts_ipc_diff, main="IPC Diferenciado")
plot(ts_pib_diff, main="PIB Diferenciado")

# ACF y PACF para IPC y PIB diferenciados
acf(ts_ipc_diff, main="ACF IPC Diferenciado")
pacf(ts_ipc_diff, main="PACF IPC Diferenciado")
acf(ts_pib_diff, main="ACF PIB Diferenciado")
pacf(ts_pib_diff, main="PACF PIB Diferenciado")


#####COMPROBAR ACCURACY#####
#IPC
train_ipc_sincovid<-window(ts_ipc_diff, start = c(1996, 6), end = c(2016, 1))
test_ipc_sincovid <- window(ts_ipc_diff, start = c(2016,2), end = c(2019,4))

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

agregar_resultado <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_ipc_sincovid <<- rbind(resultados_ipc_sincovid, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}


# Evaluar cada modelo y almacenar sus resultados
agregar_resultado("AutoARIMA", arima_forecast_ipc_sincovid, test_ipc_sincovid)
agregar_resultado("Mean Forecast", meanf_ipc_sincovid, test_ipc_sincovid)
agregar_resultado("SNaive", snaive_ipc_sincovid, test_ipc_sincovid)
agregar_resultado("Naive", naive_ipc_sincovid, test_ipc_sincovid)
agregar_resultado("Random Walk Forecast", drift_ipc_sincovid, test_ipc_sincovid)
agregar_resultado("AR", ar_ipc_forecast_sincovid, test_ipc_sincovid)
agregar_resultado("MA", prediccion_ma_ipc_sincovid, test_ipc_sincovid)
agregar_resultado("ARMA", arma_ipc_forecast_sincovid, test_ipc_sincovid)
agregar_resultado("SARIMA", forecast_sarima_ipc_sincovid, test_ipc_sincovid)

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

agregar_resultado <- function(nombre, prediccion, real) {
  metrica <- accuracy(prediccion, real)
  resultados_pib_sincovid <<- rbind(resultados_pib_sincovid, data.frame(
    Modelo = nombre,
    MAE = metrica["Test set", "MAE"],
    RMSE = metrica["Test set", "RMSE"],
    MAPE = metrica["Test set", "MAPE"]
  ))
}


# Evaluar cada modelo y almacenar sus resultados
agregar_resultado("AutoARIMA", arima_forecast_pib_sincovid, test_pib_sincovid)
agregar_resultado("Mean Forecast", meanf_pib_sincovid, test_pib_sincovid)
agregar_resultado("SNaive", snaive_pib_sincovid, test_pib_sincovid)
agregar_resultado("Naive", naive_pib_sincovid, test_pib_sincovid)
agregar_resultado("Random Walk Forecast", drift_pib_sincovid, test_pib_sincovid)
agregar_resultado("AR", ar_pib_forecast_sincovid, test_pib_sincovid)
agregar_resultado("MA", prediccion_ma_pib_sincovid, test_pib_sincovid)
agregar_resultado("ARMA", arma_pib_forecast_sincovid, test_pib_sincovid)
agregar_resultado("SARIMA", forecast_sarima_pib_sincovid, test_pib_sincovid)

# Ordenar la tabla de resultados por la métrica
resultados_pib_sincovid <- resultados_pib_sincovid[order(resultados_pib_sincovid$MAPE), ]
resultados_pib_sincovid
head(resultados_pib_sincovid) #NAIVE, SARIMA, ARIMA


################PREDICCIONES######################################
#IPC
# Verificar los residuos de los modelos SARIMA
checkresiduals(sarima_ipc_sincovid)
checkresiduals(arima_pib_sincovid)


# Predicción de IPC con ARIMA para los 2 ultimos trimestres de 2022
sarima_ipc_sincovid <- auto.arima(ts_ipc_diff, seasonal = TRUE)
forecast_sarima_ipc_sincovid <- forecast(sarima_ipc_sincovid, h = 6)
plot(forecast_sarima_ipc_sincovid, main="Predicción del IPC sin época covid con SARIMA")
abline(h = 0, col = "red", lty = 2)

#PIB

<<<<<<< HEAD
# Predicción de PIB con ARIMA a 12 meses
dev.new(width = 7, height = 7)  # Ajusta el tamaño de la ventana gráfica
modelo_arima_pib_sincovid <- auto.arima(ts_pib_sincovid)
forecast_pib_sincovid <- forecast(modelo_arima_pib_sincovid, h = 4 )
summary(forecast_pib_sincovid)
plot(forecast_pib_sincovid, main="Predicción del PIB con ARIMA")
=======
# Predicción de PIB con ARIMA a 2 trimestres
sarima_pib_sincovid <- auto.arima(ts_pib_diff, seasonal = TRUE)
forecast_sarima_pib_sincovid <- forecast(sarima_pib_sincovid, h = 6)
plot(forecast_sarima_pib_sincovid, main="Predicción del PIB sin época covid con SARIMA")
>>>>>>> 3d25225731b42a5f318c5b5b914ab1e55ede73d1
abline(h = 0, col = "red", lty = 2)



length(ts_pib_sincovid)

<<<<<<< HEAD
# Instala y carga las librerías necesarias
# install.packages("forecast")
library(forecast)
=======
## CAMBIAR 2020 por 2022
forecast_pib_sincovid <- data.frame(forecast_sarima_pib_sincovid)
row.names(forecast_pib_sincovid) <- gsub("2020", "2022", row.names(forecast_pib_sincovid))
>>>>>>> 3d25225731b42a5f318c5b5b914ab1e55ede73d1

####Sin COVID

# Modelo ARIMA usando datos hasta 2019
modelo_arima_pib_sincovid <- auto.arima(ts_pib_sincovid)

# Predicción a 4 trimestres (2022)
forecast_pib_sincovid <- forecast(modelo_arima_pib_sincovid, h = 4)

# Crear etiquetas para todos los años desde 1996 hasta 2022 omitiendo 2020 y 2021
eje_x_labels <- as.character(c(1996:2019, 2022))
eje_x_positions <- c(1996:2020)

# Ajustar márgenes para hacer espacio adicional
par(mar = c(5, 4, 4, 2) + 0.1)

# Graficar el pronóstico sin etiquetas automáticas en el eje x
plot(forecast_pib_sincovid, main = "Predicción del PIB con ARIMA",
     xaxt = "n", xlab = "Tiempo", ylab = "PIB",
     ylim = range(c(ts_pib_sincovid, forecast_pib_sincovid$mean)))

# Añadir línea vertical sólida en el año 2020 para indicar el salto
abline(v = 2019.75, col = "blue", lty = 1,lwd=2)  # lty = 1 para línea sólida

# Añadir etiquetas de los años al eje x
axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

# Añadir una línea horizontal en el valor 0
abline(h = 0, col = "red", lty = 2)

# Añadir texto explicativo a la izquierda de la línea en el año 2020
text(2019.5, max(forecast_pib_sincovid$mean) * 1.75, 
     "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2)  # pos = 2 para la izquierda

<<<<<<< HEAD
=======
forecast_ipc_sincovid <- data.frame(forecast_sarima_ipc_sincovid)
row.names(forecast_ipc_sincovid) <- gsub("2020", "2022", row.names(forecast_ipc_sincovid))
>>>>>>> 3d25225731b42a5f318c5b5b914ab1e55ede73d1



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
pib_nuevo <- ts(c(ts_pib_diff, pred_pib_sincovid), start = start(ts_pib_diff), frequency = frequency(ts_pib_diff))
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
