library(readr)
library(dplyr)
library(plotly)

#PIB
#evolucion
pib$fecha <- as.Date(paste0(pib$Year, "-", pib$Month, "-01"))

crecimiento_pib <- plot_ly(pib, x = ~fecha, y = ~GDP.billion.currency.units, type = 'scatter', mode = 'lines',
                           line = list(color = "#93044e")) %>%
  layout(title = "Evolución del PIB Trimestral en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "PIB"))
crecimiento_pib



# Comparación por trimestres del PIB
trimestres_pib <- plot_ly(pib, x = ~Year, y = ~GDP.billion.currency.units, color = ~factor(Month), type = 'scatter', mode = 'lines',
                          line = list(color = "#93044e")) %>%
  layout(title = "Comparación de Trimestres por Año",
         xaxis = list(title = "Año"),
         yaxis = list(title = "PIB"),
         legend = list(title = list(text = "Trimestres")))
trimestres_pib


# IPC
# Evolución del IPC
ipc$fecha <- as.Date(paste0(ipc$Year, "-", ipc$Month, "-01"))

crecimiento_ipc <- plot_ly(ipc, x = ~fecha, y = ~Consumer.Price.Index..CPI., type = 'scatter', mode = 'lines',
                           line = list(color = "#8db41c")) %>%
  layout(title = "Evolución del IPC en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "IPC"))
crecimiento_ipc

# Crecimiento del IPC a partir del 2010
filtrado <- ipc %>% 
  filter(Year >= 2010 & Year <= 2022)
filtrado$fecha <- as.Date(paste0(filtrado$Year, "-", filtrado$Month, "-01"))
crecimiento_ipc2010 <- plot_ly(filtrado, x = ~fecha, y = ~Consumer.Price.Index..CPI., type = 'scatter', mode = 'lines',
                               line = list(color = "#8db41c")) %>%
  layout(title = "Evolución del IPC en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "IPC"))
crecimiento_ipc2010

# Comparación de trimestres del IPC
seleccion_trimestres <- ipc %>%
  filter(Month %in% c(3, 6, 9, 12)) %>% 
  mutate(trimestre = case_when(
    Month == 3 ~ "Q1",
    Month == 6 ~ "Q2",
    Month == 9 ~ "Q3",
    Month == 12 ~ "Q4"
  ))

trimestres_ipc <- plot_ly(seleccion_trimestres, x = ~Year, y = ~Consumer.Price.Index..CPI., color = ~trimestre, 
                          type = 'scatter', mode = 'lines',
                          line = list(color = "#8db41c")) %>%
  layout(title = "Comparación Trimestral del IPC en Alemania",
         xaxis = list(title = "Año"),
         yaxis = list(title = "IPC"),
         legend = list(title = list(text = "Trimestre")))
trimestres_ipc



# Usando el Crecimiento Interanual

# PIB - Crecimiento Interanual
crecimientoInteranual_pib <- plot_ly(pib, x = ~fecha, y = ~Crecimiento_Interanual, type = 'scatter', mode = 'lines',
                                     line = list(color = "#93044e")) %>%
  layout(title = "Evolución del PIB Trimestral en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "PIB"))
crecimientoInteranual_pib

# Comparación por trimestres del PIB - Crecimiento Interanual
trimestresInteranual_pib <- plot_ly(pib, x = ~Year, y = ~Crecimiento_Interanual, color = ~factor(Month), type = 'scatter', mode = 'lines',
                                    line = list(color = "#93044e")) %>%
  layout(title = "Comparación de Trimestres por Año",
         xaxis = list(title = "Año"),
         yaxis = list(title = "PIB"),
         legend = list(title = list(text = "Trimestres")))
trimestresInteranual_pib



# IPC - Crecimiento Interanual
crecimientoInteranual_ipc <- plot_ly(ipc, x = ~fecha, y = ~Crecimiento_Interanual, type = 'scatter', mode = 'lines',
                                     line = list(color = "#8db41c")) %>%
  layout(title = "Evolución del IPC en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "IPC"))
crecimientoInteranual_ipc

# Crecimiento Interanual del IPC a partir del 2010
crecimientoInteranual_ipc2010 <- plot_ly(filtrado, x = ~fecha, y = ~Crecimiento_Interanual, type = 'scatter', mode = 'lines',
                                         line = list(color = "#8db41c")) %>%
  layout(title = "Evolución del IPC en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "IPC"))
crecimientoInteranual_ipc2010

# Comparación Trimestral del IPC - Crecimiento Interanual
trimestresInteranual_ipc <- plot_ly(seleccion_trimestres, x = ~Year, y = ~Crecimiento_Interanual, color = ~trimestre, 
                                    type = 'scatter', mode = 'lines',
                                    line = list(color = "#8db41c")) %>%
  layout(title = "Comparación Trimestral del IPC en Alemania",
         xaxis = list(title = "Año"),
         yaxis = list(title = "IPC"),
         legend = list(title = list(text = "Trimestre")))
trimestresInteranual_ipc













