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
par(mar = c(5, 4, 4, 2) + 0.1)

# Seleccionar solo los datos hasta 2019
datos_hasta_2019 <- window(predreal_ipc_sincovid, end = c(2019, 4))

# Graficar los datos históricos hasta el último dato de 2019
plot(datos_hasta_2019, type = "l", col = "#8db41c", xlim = c(1996, 2023),
     main = "Predicción del IPC (sin covid)", xlab = "Tiempo", ylab = "IPC", xaxt = "n",
     ylim = range(c(datos_hasta_2019, predreal_ipc_sincovid)))

# Crear un vector de tiempos para las predicciones a partir de 2022
prediccion_tiempo_ipc <- seq(2022, by = 0.25, length.out = length(predreal_ipc_sincovid) - length(datos_hasta_2019))

# Añadir la línea de predicción a partir de 2022
lines(prediccion_tiempo_ipc, predreal_ipc_sincovid[(length(datos_hasta_2019) + 1):length(predreal_ipc_sincovid)], 
      col = "#8db41c", lwd = 2)

# Añadir líneas verticales sólidas para indicar saltos temporales
abline(v = 2019.75, col = "blue", lty = 1, lwd = 2)  # Línea para el salto en 2019
abline(v = 2022, col = "blue", lty = 1, lwd = 2)      # Línea para el salto en 2021

# Crear etiquetas para el eje x
# Solo incluimos hasta 2019 y luego 2022, omitiendo 2020 y 2021
eje_x_labels <- as.character(c(1996:2019, 2022))
eje_x_positions <- c(1996:2019, 2022)

# Ajustar las posiciones del eje x para que coincidan con los años que se muestran en el gráfico
axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

# Añadir una línea horizontal en el valor 0
abline(h = 0, col = "red", lty = 2)

# Añadir texto explicativo a la izquierda de la línea en el año 2019
text(x = 2019.5, y = max(datos_hasta_2019) * 0.8,  
     labels = "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2)





#### PIB
par(mar = c(5, 4, 4, 2) + 0.1)

datos_hasta_2019_pib <- window(predreal_pib_sincovid, end = c(2019, 4))

plot(datos_hasta_2019_pib, type = "l", col = "#93044e", xlim = c(1996, 2023),
     main = "Predicción del PIB (sin covid)", xlab = "Tiempo", ylab = "PIB", xaxt = "n",
     ylim = range(c(datos_hasta_2019_pib, predreal_pib_sincovid)))

prediccion_tiempo_pib <- seq(2022, by = 0.25, length.out = length(predreal_pib_sincovid) - length(datos_hasta_2019_pib))

lines(prediccion_tiempo_pib, predreal_pib_sincovid[(length(datos_hasta_2019_pib) + 1):length(predreal_ipc_sincovid)], 
      col = "#93044e", lwd = 2)

abline(v = 2019.75, col = "blue", lty = 1, lwd = 2)  
abline(v = 2022, col = "blue", lty = 1, lwd = 2)

eje_x_labels <- as.character(c(1996:2019, 2022))
eje_x_positions <- c(1996:2019, 2022)

axis(1, at = eje_x_positions, labels = eje_x_labels, las = 2, cex.axis = 0.7)

abline(h = 0, col = "red", lty = 2)

text(x = 2019.5, y = max(datos_hasta_2019_pib) * 0.1,  
     labels = "Salto temporal\n(2020-2021 omitidos)", col = "blue", pos = 2)

