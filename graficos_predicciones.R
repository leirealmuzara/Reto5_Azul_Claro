library(ggplot2)
library(plotly)
library(forecast)
library(ggfortify)
library(plotly)

#93044e IPB
#8db41c IPC

# Crear el periodo como una variable de fecha para facilitar el manejo del eje x
df_ipc_long <- df_ipc_long %>%
  mutate(Periodo = as.Date(paste(Año, substr(Trimestre, 4, 4), "01", sep = "-")))

# Crear una nueva columna para identificar si los valores son predicciones
df_ipc_long <- df_ipc_long %>%
  mutate(Status = case_when(
    Año == 2023 ~ "Predicciones futuras", 
    Año == 2022 & Trimestre %in% c("Qtr3", "Qtr4") ~ "Predicciónes",
    TRUE ~ "Real"  # Valores reales
  ))

g_serie_ipc <- ggplot(df_ipc_long, aes(x = Periodo, y = Valor, group = 1, color = Status)) +
  geom_line(size = 1) +  # Graficar las líneas
  scale_color_manual(values = c("Real" = "#8db41c", 
                                "Predicciónes" = "red",      # Color rojo para Q3 y Q4 de 2022
                                "Predicciones futuras" = "#ff9ea2")) + # Color rojo más claro para 2023
  labs(title = "Serie Temporal del IPC en Alemania", x = "Periodo", y = "IPC") +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Ejes en años, cada 2 años
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar etiquetas para mayor legibilidad

print(g_serie_ipc)




## PIB
df_pib_long <- df_pib_long %>%
  mutate(Periodo = as.Date(paste(Año, substr(Trimestre, 4, 4), "01", sep = "-")))

df_pib_long <- df_pib_long %>%
  mutate(Status = case_when(
    Año == 2023 ~ "Predicciones futuras", 
    Año == 2022 & Trimestre %in% c("Qtr3", "Qtr4") ~ "Predicciónes",
    TRUE ~ "Real"  # Valores reales
  ))

g_serie_pib <- ggplot(df_pib_long, aes(x = Periodo, y = Valor, group = 1, color = Status)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("Real" = "#93044e", 
                                "Predicciónes" = "red",      # Color rojo para Q3 y Q4 de 2022
                                "Predicciones futuras" = "#ff9ea2")) + # Color rojo más claro para 2023
  labs(title = "Serie Temporal del PIB en Alemania", x = "Periodo", y = "PIB") +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Ejes en años, cada 2 años
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar etiquetas para mayor legibilidad

print(g_serie_pib)





######## GRAFICOS PREDICCIONES SIN COVID
length(predreal_pib_sincovid)

# Instala y carga las librerías necesarias
# install.packages("forecast")
library(forecast)

####Sin COVID

# Modelo ARIMA usando datos hasta 2019
modelo_arima_pib_sincovid <- auto.arima(predreal_pib_sincovid)

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
     ylim = c(-2, 12))

# Añadir línea vertical sólida en el año 2020 para indicar el salto
abline(v = 2019.75, col = "blue", lty = 1,lwd=2)  # lty = 1 para línea sólida

# Añadir etiquetas de los años al eje x
axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

# Añadir una línea horizontal en el valor 0
abline(h = 0, col = "red", lty = 2)

# Añadir texto explicativo a la izquierda de la línea en el año 2020
text(x = 2019.5, y = 3,  # Cambia el valor de y a 5 o un valor más bajo
     labels = "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2) 







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
text(x = 2019.5, y = 3.1,  # Cambia el valor de y a 5 o un valor más bajo
     labels = "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2) 

