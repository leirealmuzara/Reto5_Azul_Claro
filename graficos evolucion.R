library(readr)
library(dplyr)
library(plotly)

source("Predicciones_sin_covid.R")
source("predicciones del crecimiento IPC y PIB.R")

colores_trimestres_pib <- c("#93044e", "#d13d7b", "#f5a3b3", "#ffccd5")  
colores_trimestres_ipc <- c("#8db41c", "#4b8f0e", "#71c837", "#b9e973")  

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
trimestres_pib <- plot_ly(pib, x = ~Year, y = ~GDP.billion.currency.units, color = ~factor(Month), colors = colores_trimestres_pib,
                          type = 'scatter', mode = 'lines') %>%
  layout(title = "Comparación de Trimestres por Año (PIB)",
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
                          colors = colores_trimestres_ipc, type = 'scatter', mode = 'lines') %>%
  layout(title = "Comparación Trimestral del IPC en Alemania",
         xaxis = list(title = "Año"),
         yaxis = list(title = "IPC"),
         legend = list(title = list(text = "Trimestre")))
trimestres_ipc



##### Usando el Crecimiento Interanual
df1$fecha <- as.Date(paste0(pib$Year, "-", pib$Month, "-01"))
# PIB - Crecimiento Interanual
crecimientoInteranual_pib <- plot_ly(df1, x = ~fecha, y = ~GDP.billion.currency.units, type = 'scatter', mode = 'lines',
                                     line = list(color = "#93044e")) %>%
  layout(title = "Evolución del PIB Trimestral en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "PIB"))
crecimientoInteranual_pib

# Comparación por trimestres del PIB - Crecimiento Interanual
trimestresInteranual_pib <- plot_ly(df1, x = ~Year, y = ~GDP.billion.currency.units, color = ~factor(Month), colors = colores_trimestres_pib,
                                    type = 'scatter', mode = 'lines') %>%
  layout(title = "Comparación de Trimestres por Año (Crecimiento Interanual del PIB)",
         xaxis = list(title = "Año"),
         yaxis = list(title = "Crecimiento Interanual del PIB"),
         legend = list(title = list(text = "Trimestres")))
trimestresInteranual_pib


colnames(df1)
# IPC - Crecimiento Interanual
crecimientoInteranual_ipc <- plot_ly(df1, x = ~fecha, y = ~Consumer.Price.Index..CPI., type = 'scatter', mode = 'lines',
                                     line = list(color = "#8db41c")) %>%
  layout(title = "Evolución del IPC en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "IPC"))
crecimientoInteranual_ipc

# Crecimiento Interanual del IPC a partir del 2010
crecimientoInteranual_ipc2010 <- plot_ly(filtrado, x = ~fecha, y = ~Consumer.Price.Index..CPI., type = 'scatter', mode = 'lines',
                                         line = list(color = "#8db41c")) %>%
  layout(title = "Evolución del IPC en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "IPC"))
crecimientoInteranual_ipc2010

# Comparación Trimestral del IPC - Crecimiento Interanual
trimestresInteranual_ipc <- plot_ly(seleccion_trimestres, x = ~Year, y = ~Consumer.Price.Index..CPI., color = ~trimestre,
                                    colors = colores_trimestres_ipc, type = 'scatter', mode = 'lines') %>%
  layout(title = "Comparación Trimestral del IPC en Alemania (Crecimiento Interanual)",
         xaxis = list(title = "Año"),
         yaxis = list(title = "Crecimiento Interanual del IPC"),
         legend = list(title = list(text = "Trimestre")))
trimestresInteranual_ipc




#### EVOLUCION DEL DESEMPLEO

desempleo<-read.csv("Datos-20240913/unemployment_germany.csv")
desempleo
colnames(desempleo)
class(desempleo$DATE)
desempleo$DATE<-as.Date(desempleo$DATE)
class(desempleo$DATE)

evolucion_desempleo <- plot_ly(desempleo, x = ~DATE, y = ~UNEMPLOYMENT_RATE, type = 'scatter', mode = 'lines',
                           line = list(color = "#c88fb2")) %>%
  layout(title = "Evolución del Desempleo en Alemania",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Desempleo"))
evolucion_desempleo



