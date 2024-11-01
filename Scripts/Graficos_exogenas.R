#### GRAFICOS DE EXOGENAS
# series temporales

# Gráfico de la masa monetaria
fig_masa_monetaria <- plot_ly(x = time(ts_masa_monetaria), y = as.numeric(ts_masa_monetaria), 
                              type = 'scatter', mode = 'lines', line = list(color = '#c88fb2')) %>%
  layout(title = "Serie Temporal de la masa monetaria en Alemania",
         xaxis = list(title = "Tiempo"),
         yaxis = list(title = "Crecimiento"))
fig_masa_monetaria

# Gráfico del índice bursátil
fig_indice_bursatil <- plot_ly(x = time(ts_indice_bursatil), y = as.numeric(ts_indice_bursatil), 
                               type = 'scatter', mode = 'lines', line = list(color = '#c88fb2')) %>%
  layout(title = "Serie Temporal del índice bursátil en Alemania",
         xaxis = list(title = "Tiempo"),
         yaxis = list(title = "%"))
fig_indice_bursatil

# Gráfico del paro
fig_paro <- plot_ly(x = time(ts_paro), y = as.numeric(ts_paro), type = 'scatter', mode = 'lines', 
                    line = list(color = '#c88fb2')) %>%
  layout(title = "Serie Temporal del paro en Alemania",
         xaxis = list(title = "Tiempo"),
         yaxis = list(title = "%"))
fig_paro

