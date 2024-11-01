# Librerías necesarias
library(ggplot2)
library(plotly)
library(forecast)
library(ggfortify)
#93044e IPB
#8db41c IPC


##### Predicciones
#IPC
g_serie_ipc <- autoplot(forecast_ipc, ts.color = "#8db41c") +
  ggtitle("Predicciones IPC en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual IPC") +
  theme_minimal()
g_serie_ipc

#PIB
g_serie_pib <- autoplot(forecast_pib, colour = "#93044e") +
  ggtitle("Predicciones PIB en Alemania") +
  labs(x = "Periodo", y = "Crecimiento Interanual PIB") +
  theme_minimal()
g_serie_pib





### SIN COVID
length(ts_pib_sincovid)

# Instala y carga las librerías necesarias
# install.packages("forecast")
library(forecast)

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
plot(forecast_pib_sincovid, main = "Predicción del PIB (sin covid)",
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
     "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2) 



### IPC sincovid

# Modelo ARIMA usando datos hasta 2019
modelo_arima_ipc_sincovid <- auto.arima(ts_ipc_sincovid)

# Predicción a 4 trimestres (2022)
forecast_ipc_sincovid <- forecast(modelo_arima_ipc_sincovid, h = 4)

# Crear etiquetas para todos los años desde 1996 hasta 2022 omitiendo 2020 y 2021
eje_x_labels <- as.character(c(1996:2019, 2022))
eje_x_positions <- c(1996:2020)

# Ajustar márgenes para hacer espacio adicional
par(mar = c(5, 4, 4, 2) + 0.1)

# Graficar el pronóstico sin etiquetas automáticas en el eje x
plot(forecast_ipc_sincovid, main = "Predicción del IPC (sin covid)",
     xaxt = "n", xlab = "Tiempo", ylab = "IPC",
     ylim = range(c(ts_ipc_sincovid, forecast_ipc_sincovid$mean)))

# Añadir línea vertical sólida en el año 2020 para indicar el salto
abline(v = 2019.75, col = "blue", lty = 1,lwd=2)  # lty = 1 para línea sólida

# Añadir etiquetas de los años al eje x
axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

# Añadir una línea horizontal en el valor 0
abline(h = 0, col = "red", lty = 2)

# Añadir texto explicativo a la izquierda de la línea en el año 2020
text(2019.5, max(forecast_ipc_sincovid$mean) * 1.75, 
     "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2)  

