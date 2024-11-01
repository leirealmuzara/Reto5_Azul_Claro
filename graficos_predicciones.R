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

#### IPC
modelo_arima_ipc_sincovid <- auto.arima(ts_ipc_sincovid)

# Predicción a 4 trimestres (2022)
forecast_ipc_sincovid <- forecast(modelo_arima_ipc_sincovid, h = 4)

# Crear etiquetas para todos los años desde 1996 hasta 2022, omitiendo 2020 y 2021
eje_x_labels <- as.character(c(1996:2019, 2022))
eje_x_positions <- c(1996:2019, 2022)

# Ajustar márgenes para hacer espacio adicional
par(mar = c(5, 4, 4, 2) + 0.1)

# Graficar los datos históricos hasta el último dato de 2019
plot(ts_ipc_sincovid, type = "l", col = "#8db41c", xlim = c(1996, 2023),
     main = "Predicción del IPC (sin covid)", xlab = "Tiempo", ylab = "IPC", xaxt = "n",
     ylim = range(c(ts_ipc_sincovid, forecast_ipc_sincovid$mean, forecast_ipc_sincovid$upper[,2], forecast_ipc_sincovid$lower[,2])))

# Ajustar las posiciones de las predicciones para que empiecen en 2022
prediccion_tiempo <- seq(2022, by = 0.25, length.out = length(forecast_ipc_sincovid$mean))

# Añadir la línea de predicción del pronóstico como una continuación de la serie histórica
lines(prediccion_tiempo, forecast_ipc_sincovid$mean, col = "#8db41c", lwd = 2)

# Añadir bandas de confianza alrededor de la predicción
polygon(c(prediccion_tiempo, rev(prediccion_tiempo)),
        c(forecast_ipc_sincovid$upper[,2], rev(forecast_ipc_sincovid$lower[,2])),
        col = adjustcolor("#8db41c", alpha.f = 0.2), border = NA)

# Añadir línea vertical sólida en el año 2020 para indicar el salto
abline(v = 2019.75, col = "blue", lty = 1, lwd = 2)  # lty = 1 para línea sólida

# Añadir etiquetas de los años al eje x, excluyendo 2020 y 2021
axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

# Añadir una línea horizontal en el valor 0
abline(h = 0, col = "red", lty = 2)

# Añadir texto explicativo a la izquierda de la línea en el año 2020
text(x = 2019.5, y = max(ts_ipc_sincovid) * 0.8,  # Posicionar el texto en función de los datos
     labels = "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2)




#### PIB
par(mar = c(5, 4, 4, 2) + 0.1)

datos_hasta_2019_pib <- window(predreal_pib_sincovid, end = c(2019, 4))

plot(datos_hasta_2019_pib, type = "l", col = "#93044e", xlim = c(1996, 2023), ylim = c(-2, 12),
     main = "Predicción del PIB (sin covid)", xlab = "Tiempo", ylab = "PIB", xaxt = "n")

lines(forecast_pib_sincovid$mean, col = "#93044e", lwd = 2)  

polygon(c(time(forecast_pib_sincovid$mean), rev(time(forecast_pib_sincovid$mean))),
        c(forecast_pib_sincovid$upper[,2], rev(forecast_pib_sincovid$lower[,2])),
        col = adjustcolor("#93044e", alpha.f = 0.2), border = NA)

abline(v = 2019.75, col = "blue", lty = 1, lwd = 2)  # lty = 1 para línea sólida

eje_x_labels <- as.character(c(1996:2019, 2022))
eje_x_positions <- c(1996:2019, 2022)

axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

abline(h = 0, col = "red", lty = 2)

text(x = 2019.5, y = 3, 
     labels = "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2) 

