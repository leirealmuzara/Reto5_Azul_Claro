# Librerías necesarias
library(ggplot2)
library(plotly)
library(forecast)
library(ggfortify)

# Gráficos Serie Temporal en ggplot2 y Plotly

## Serie Temporal IPC - ggplot2
g_serie_ipc <- autoplot(ts_ipc, ts.colour = "blue") +
  ggtitle("Serie Temporal del IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  theme_minimal()
g_serie_ipc

## Serie Temporal IPC - Plotly
p_serie_ipc <- plot_ly(x = time(ts_ipc), y = as.numeric(ts_ipc), type = 'scatter', mode = 'lines') %>%
  layout(title = "Serie Temporal del IPC en Alemania", xaxis = list(title = "Periodo"), yaxis = list(title = "Crecimiento Interanual IPC"))
p_serie_ipc

## Serie Temporal PIB - ggplot2
g_serie_pib <- autoplot(ts_pib, ts.colour = "blue") +
  ggtitle("Serie Temporal del PIB en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual PIB") +
  theme_minimal()
g_serie_pib

## Serie Temporal PIB - Plotly
p_serie_pib <- plot_ly(x = time(ts_pib), y = as.numeric(ts_pib), type = 'scatter', mode = 'lines') %>%
  layout(title = "Serie Temporal del PIB en Alemania", xaxis = list(title = "Periodo"), yaxis = list(title = "Crecimiento Interanual PIB"))
p_serie_pib

# ACF y PACF en ggplot2

## ACF y PACF IPC
g_acf_ipc <- ggAcf(ts_ipc, main = "ACF IPC") + theme_minimal()
g_acf_ipc
g_pacf_ipc <- ggPacf(ts_ipc, main = "PACF IPC") + theme_minimal()
g_pacf_ipc

## ACF y PACF PIB
g_acf_pib <- ggAcf(ts_pib, main = "ACF PIB") + theme_minimal()
g_acf_pib
g_pacf_pib <- ggPacf(ts_pib, main = "PACF PIB") + theme_minimal()
g_pacf_pib

# Descomposición de Series Temporales en ggplot2

## Descomposición IPC
decomp_ipc <- decompose(ts_ipc)
decomp_ipc
g_descomp_ipc <- autoplot(decomp_ipc) +
  ggtitle("Descomposición del IPC en Alemania") +
  theme_minimal()
g_descomp_ipc

## Descomposición PIB
decomp_pib <- decompose(ts_pib)
decomp_pib
g_descomp_pib <- autoplot(decomp_pib) +
  ggtitle("Descomposición del PIB en Alemania") +
  theme_minimal()
g_descomp_pib

# Predicciones ARIMA en ggplot2 y Plotly

## Predicciones IPC
modelo_arima_ipc <- auto.arima(ts_ipc)
forecast_ipc <- forecast(modelo_arima_ipc, h = 12)
g_pred_ipc <- autoplot(forecast_ipc) +
  ggtitle("Predicción del IPC con ARIMA") +
  theme_minimal()
modelo_arima_ipc

p_pred_ipc <- plot_ly(x = time(forecast_ipc$mean), y = forecast_ipc$mean, type = 'scatter', mode = 'lines', name = "Predicción IPC") %>%
  layout(title = "Predicción del IPC con ARIMA", xaxis = list(title = "Periodo"), yaxis = list(title = "IPC"))
p_pred_ipc

## Predicciones PIB
modelo_arima_pib <- auto.arima(ts_pib)
modelo_arima_pib
forecast_pib <- forecast(modelo_arima_pib, h = 4)
forecast_pib
g_pred_pib <- autoplot(forecast_pib) +
  ggtitle("Predicción del PIB con ARIMA") +
  theme_minimal()
g_pred_pib

p_pred_pib <- plot_ly(x = time(forecast_pib$mean), y = forecast_pib$mean, type = 'scatter', mode = 'lines', name = "Predicción PIB") %>%
  layout(title = "Predicción del PIB con ARIMA", xaxis = list(title = "Periodo"), yaxis = list(title = "PIB"))
p_pred_pib

# Mostrar todos los gráficos
list(g_serie_ipc, g_serie_pib, g_acf_ipc, g_pacf_ipc, g_acf_pib, g_pacf_pib, g_descomp_ipc, g_descomp_pib, g_pred_ipc, g_pred_pib,
     p_serie_ipc, p_serie_pib, p_pred_ipc, p_pred_pib)

