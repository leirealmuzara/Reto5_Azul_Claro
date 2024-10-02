# Filtrar datos para Alemania
df1 <- df1 %>% 
  filter(Code == "DEU")
df2 <- df2 %>% 
  filter(Code == "DEU")

# Filtrar datos fuera del periodo de COVID-19
# Supongamos que el periodo de COVID-19 es de 2020 a 2021
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

# Convertir a series temporales
ts_ipc <- ts(ipc$Consumer.Price.Index..CPI., start = c(1996, 1), end = c(2019, 12), frequency = 12)
ts_pib <- ts(pib$GDP.billion.currency.units, start = c(1996, 3), end = c(2019, 4), frequency = 4)

# Graficar las series temporales
plot(ts_ipc, main = "Serie Temporal del IPC en Alemania", ylab = "IPC", xlab = "Tiempo")
plot(ts_pib, main = "Serie Temporal del PIB en Alemania", ylab = "PIB", xlab = "Tiempo")

# Analizar NA por variable
library(ggplot2)
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)

# Imputar los NA que quedan
sum(is.na(ts_pib))
sum(is.na(ts_ipc))
# Imputar NA con interpolación lineal (si es necesario)
if (any(is.na(ts_pib))) {
  ts_pib <- na.approx(ts_pib)
}
if (any(is.na(ts_ipc))) {
  ts_ipc <- na.approx(ts_ipc)
}

# Análisis de autocorrelación y descomposición
acf(ts_ipc, main = "ACF del IPC")
pacf(ts_ipc, main = "PACF del IPC")

acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")

# Descomposición de las series temporales
descomposicion_ipc <- decompose(ts_ipc)
plot(descomposicion_ipc)

descomposicion_pib <- decompose(ts_pib)
plot(descomposicion_pib)

# Predecir el último trimestre de 2022
# Usar modelo ARIMA para pronosticar
library(forecast)

# Ajustar el modelo ARIMA para IPC
model_ipc <- auto.arima(ts_ipc)
forecast_ipc <- forecast(model_ipc, h = 36) # Predecir 3 meses
summary(forecast_ipc)
# Ajustar el modelo ARIMA para PIB
model_pib <- auto.arima(ts_pib)
forecast_pib <- forecast(model_pib, h = 12) # Predecir 1 trimestre
summary(forecast_pib)
# Graficar pronósticos
plot(forecast_ipc, main = "Pronóstico del IPC para el último trimestre de 2022")
plot(forecast_pib, main = "Pronóstico del PIB para el último trimestre de 2022")


