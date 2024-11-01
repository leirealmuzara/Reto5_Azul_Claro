# Librerías necesarias
library(ggplot2)
library(plotly)
library(forecast)
library(ggfortify)
#93044e IPB
#8db41c IPC


##### Predicciones
#IPC
g_serie_ipc <- autoplot(forecast_ipc, ts.colour = "#8db41c") +
  ggtitle("Serie Temporal del IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  theme_minimal()
g_serie_ipc

#PIB
g_serie_pib <- autoplot(forecast_pib, ts.colour = "#93044e") +
  ggtitle("Serie Temporal del PIB en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual PIB") +
  theme_minimal()
g_serie_pib

g_serie_ipc <- autoplot(forecast_ipc, ts.colour = "#8db41c") +
  ggtitle("Serie Temporal del IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  theme_minimal()
g_serie_ipc




library(ggplot2)

# Generar gráfico del forecast
g_serie_ipc <- autoplot(forecast_ipc, ts.colour = "#8db41c") +
  ggtitle("Serie Temporal del IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1996, 2022, by = 5), labels = function(x) ifelse(x == 2020, 2022, x))

# Mostrar gráfico
print(g_serie_ipc)
# 
# 
# 
# 
# 
# 
# # Cargar las bibliotecas necesarias
# library(ggplot2)
# library(forecast)
# 
# # Definición de fechas
# fechas <- seq(as.Date('1997-07-01'), as.Date('2020-12-31'), by = 'quarter')
# fechas <- c(fechas, seq(as.Date('2022-07-01'), as.Date('2022-12-31'), by = 'quarter'))
# 
# # Supongamos que ts_ipc_sincovid es un vector con valores numéricos
# # Asegúrate de que `ts_ipc_sincovid` tenga la longitud esperada
# # Ejemplo: Generar un vector de ejemplo con datos ficticios para probar
# # Este es solo un ejemplo; sustituye esto con tus datos reales.
# ts_ipc_sincovid <- rnorm(length(fechas) - 10)  # Datos ficticios
# 
# # Valores del IPC sin COVID con pronósticos
# # Añadimos NA para los períodos donde no hay datos (2021) y luego los valores pronosticados
# num_na <- 8  # Número de NA a agregar para los años sin datos (2021)
# # Asegúrate de que forecast_ipc_sincovid tenga al menos 4 elementos
# forecast_ipc_sincovid <- list(mean = c(NA, NA, 1.2, 1.5))  # Ejemplo ficticio
# 
# # Combinar datos en un solo vector
# values_ipc_sincovid_pred <- c(ts_ipc_sincovid, rep(NA, num_na), forecast_ipc_sincovid$mean[3:4])
# 
# # Comprobamos que los vectores tengan la misma longitud
# if (length(fechas) != length(values_ipc_sincovid_pred)) {
#   stop(paste("Los vectores de fechas y valores no tienen la misma longitud. Fechas:", length(fechas), "Valores:", length(values_ipc_sincovid_pred)))
# }
# 
# # Crear un data frame con las fechas y valores
# df_values_ipc_sincovid_pred <- data.frame(fecha = fechas, valor = values_ipc_sincovid_pred)
# 
# # Convertir los valores a una serie temporal
# ts_ipc_sincovid_pred <- ts(df_values_ipc_sincovid_pred$valor, start = c(1997, 3), frequency = 4)
# 
# # Gráfico de la serie temporal del IPC
# g_serie_ipc <- ggplot(df_values_ipc_sincovid_pred, aes(x = fecha, y = valor)) +
#   geom_line(colour = "#8db41c") +
#   ggtitle("Serie Temporal del IPC en Alemania (Sin COVID)") +
#   labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
#   scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Ejes en años
#   theme_minimal() +
#   geom_vline(xintercept = as.numeric(as.Date('2020-01-01')), linetype = "dashed", color = "red") +  # Línea vertical para marcar 2020
#   annotate("text", x = as.Date('2020-01-01'), y = max(df_values_ipc_sincovid_pred$valor, na.rm = TRUE), 
#            label = "Inicio COVID", vjust = -0.5, color = "red")
# 
# # Mostrar gráfico
# print(g_serie_ipc)
# 
# 
# 






































## sin covid
#IPC
fechas <- seq(as.Date('1997-07-01'), as.Date('2020-12-31'), by='quarter')
fechas <- c(fechas, seq(as.Date('2022-07-01'), as.Date('2022-12-31'), by='quarter'))
fechas
values_ipc_sincovid_pred <- c(ts_ipc_sincovid , c('NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA'), forecast_ipc_sincovid$mean[3:4])
df_values_ipc_sincovid_pred <- data.frame(fecha=fechas, valor=values_ipc_sincovid_pred)
ts_ipc_sincovid_pred <- ts(df_values_ipc_sincovid_pred$valor, start=c(1997, 3), frequency=4)

g_serie_ipc <- autoplot(forecast_ipc_sincovid, ts.colour = "#8db41c") +
  ggtitle("Serie Temporal del IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  scale_x_discrete(fechas) +#AÑADIR fechas
  theme_minimal()
g_serie_ipc




















# Crear secuencia de fechas sin COVID
fechas <- seq(as.Date('1997-07-01'), as.Date('2020-12-31'), by = 'quarter')
fechas <- c(fechas, seq(as.Date('2022-07-01'), as.Date('2022-12-31'), by = 'quarter'))

# Vector de valores IPC sin COVID con valores NA en el intervalo correspondiente
values_ipc_sincovid_pred <- c(ts_ipc_sincovid, rep(NA, 10), forecast_ipc_sincovid$mean[3:4])

# Crear data frame con las fechas y valores
df_values_ipc_sincovid_pred <- data.frame(fecha = fechas, valor = values_ipc_sincovid_pred)

# Convertir la serie temporal con fechas específicas
ts_ipc_sincovid_pred <- ts(df_values_ipc_sincovid_pred$valor, start = c(1997, 3), frequency = 4)

# Gráfico con el eje x adaptado para fechas
g_serie_ipc <- autoplot(forecast_ipc_sincovid, ts.colour = "#8db41c") +
  ggtitle("Serie Temporal del IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") + # Ajustar etiquetas y frecuencia en el eje x
  theme_minimal()

# Mostrar gráfico
g_serie_ipc



























































































#PIB#PIBlabels = 
g_serie_pib <- autoplot(forecast_pib_sincovid, ts.colour = "#93044e") +
  ggtitle("Serie Temporal del PIB en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual PIB") +
  theme_minimal()
g_serie_pib







##################################################################################
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

