# Cargar librerías necesarias
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)
library(tseries)

# Establecer directorio de trabajo
setwd("D:/Business Data Analytics II/Casos Prácticos II/Reto5_Azul_Claro/Datos-20240913")
<<<<<<< HEAD
setwd("C:/Users/leire/OneDrive/Escritorio/Bda2/reto5/Nueva carpeta/Reto5_Azul_Claro/Datos-20240913")
=======

dir()
>>>>>>> fd88bf06b49ddb553abb343852dfd332021ad63d
# Cargar datos
df1 <- read.csv("Datos-20240913/pib_ipc_
                paises_punto2.csv")
df2 <- read.csv("Datos-20240913/exogenas_paises_punto2.csv")
df3 <- read.csv("Datos-20240913/unemployment_germany.csv")

########################### FILTRAR Y LIMPIAR DATOS ###########################

# Filtrar Alemania en df1 y df2
df1 <- df1 %>% filter(Code == "DEU")
df2 <- df2 %>% filter(Code == "DEU")

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
ts_ipc <- ts(ipc$Consumer.Price.Index..CPI., start = c(1996,1), end = c(2022,2),frequency = 4)
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
# No hay valores NA, no es necesario imputar.

########################### ANÁLISIS DE AUTOCORRELACIÓN ###########################

# ACF y PACF para IPC
acf(ts_ipc, main = "ACF del IPC")
pacf(ts_ipc, main = "PACF del IPC")

# ACF y PACF para PIB
acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")

########################### DESCOMPOSICIÓN DE SERIES ###########################

# Descomposición de la serie de IPC
descomposicion_ipc <- decompose(ts_ipc)
random_ipc<-descomposicion_ipc$random#residuo para predecir
plot(descomposicion_ipc)

# Descomposición de la serie de PIB
descomposicion_pib <- decompose(ts_pib)
random_pib<-descomposicion_pib$random#residuo para predecir
plot(descomposicion_pib)

########################### EXÓGENAS ###########################

# Revisar estructura de df2 (variables exógenas)
head(df2)
str(df2)
sum(is.na(df2))

# Crear series temporales para las variables exógenas
ts_money <- ts(df2$Money.supply.billion.currency.units, start = c(1996,1), end = c(2022,7), frequency = 12)
ts_stock <- ts(df2$Stock.market.index, start = c(1996,1), end = c(2021,12), frequency = 12)

# Graficar series temporales exógenas
plot(ts_money, main="Serie Temporal de la oferta monetaria en Alemania", ylab="billones", xlab="Tiempo")
plot(ts_stock, main="Serie del índice del mercado de valores", ylab="index", xlab="Tiempo")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_money)
ggplot_na_distribution(ts_stock)
# No hay valores faltantes

########################### ANÁLISIS DE AUTOCORRELACIÓN EXÓGENAS ###########################

# ACF y PACF para oferta monetaria
acf(ts_money, main = "ACF del dinero")
pacf(ts_money, main = "PACF del dinero")

# ACF y PACF para el índice bursátil
acf(ts_stock, main = "ACF del stock")
pacf(ts_stock, main = "PACF del stock")

########################### DESCOMPOSICIÓN DE EXÓGENAS ###########################

# Descomposición de la oferta monetaria
descomposicion_money <- decompose(ts_money)
plot(descomposicion_money)
random_money<-descomposicion_money$random#residuo predecir

# Descomposición del índice bursátil
descomposicion_stock <- decompose(ts_stock)
random_stock<-descomposicion_stock$random#residuo predecir

plot(descomposicion_stock)
plot(descomposicion_stock$random)
########################### MODELOS NAIVE ###########################

# Ajustar el modelo naive para oferta monetaria
modelo_naive_money <- naive(ts_money, h = 5)
summary(modelo_naive_money)
plot(modelo_naive_money, main = "Predicción Naive de la Oferta Monetaria")

# Ajustar el modelo naive para el índice bursátil
modelo_naive_stock <- naive(ts_stock, h = 12)
summary(modelo_naive_stock)
plot(modelo_naive_stock, main = "Predicción Naive del Índice Bursátil")

########################### PRUEBA DE RESIDUOS ###########################

# Verificar los residuos del modelo de oferta monetaria
residuos_unemployment <- residuals(modelo_naive_money)
plot(residuos_unemployment, main = "Residuos del Modelo Naive de la Tasa de Desempleo")
abline(h = 0, col = "red")

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

########################### MODELO ARIMA ###########################

# Ajustar modelos ARIMA para IPC y PIB
modelo_arima_ipc <- auto.arima(random_ipc)
summary(modelo_arima_ipc)

modelo_arima_pib <- auto.arima(random_pib)
summary(modelo_arima_pib)

# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_ipc)
checkresiduals(modelo_arima_pib)

#añadir estacionalidad y tendencia a la serie
predecir<-modelo_arima_ipc + descomposicion_ipc$seasonal + descomposicion_ipc$trend


# Predicción de IPC y PIB con ARIMA a 12 meses
forecast_ipc <- forecast(modelo_arima_ipc, h = 12)
summary(forecast_ipc)
plot(forecast_ipc, main="Predicción del IPC con ARIMA")

forecast_pib <- forecast(modelo_arima_pib, h = 12)
summary(forecast_pib)
plot(forecast_pib, main="Predicción del PIB con ARIMA")

########################### MODELO ARIMA PARA EXÓGENAS ###########################

# Prueba ADF para oferta monetaria e índice bursátil
adf.test(ts_money)
adf.test(ts_stock)

# Diferenciar las series
diff_money <- diff(ts_money)
diff_stock <- diff(ts_stock)

# Verificar nuevamente la estacionariedad
adf.test(diff_money)
adf.test(diff_stock)

# Graficar series diferenciadas
plot(diff_money, main="Oferta Monetaria Diferenciada")
plot(diff_stock, main="Índice Bursátil Diferenciado")

# ACF y PACF para las series diferenciadas
acf(diff_money, main="ACF de la Oferta Monetaria")
pacf(diff_money, main="PACF de la Oferta Monetaria")
acf(diff_stock, main="ACF del Índice Bursátil")
pacf(diff_stock, main="PACF del Índice Bursátil")

# Ajustar modelos ARIMA para las exógenas
modelo_arima_money <- auto.arima(random_money)
summary(modelo_arima_money)

modelo_arima_stock <- auto.arima(random_stock)
summary(modelo_arima_stock)

# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_money)
checkresiduals(modelo_arima_stock)

# Predicción de las exógenas con ARIMA a 12 meses
forecast_money <- forecast(modelo_arima_money, h = 12)
plot(forecast_money, main="Predicción ARIMA de la Oferta Monetaria")

forecast_stock <- forecast(modelo_arima_stock, h = 12)
plot(forecast_stock, main="Predicción ARIMA del Índice Bursátil")
