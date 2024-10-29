library(shiny)
library(ggplot2)
library(plotly)
library(forecast)
library(dplyr)
library(imputeTS)
library(lubridate)
library(tseries)
library(fpp2)


########### GRAFICOS SHINY DE LAS PREDICCIONES 
# (del script predicciones del IPC y PIB)
ui <- fluidPage(
  titlePanel("Visualización del PIB e IPC para Alemania"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("variable", "Selecciona la(s) variable(s):",
                         choices = c("IPC", "PIB"),
                         selected = "IPC"),
      sliderInput("rango_anios", "Selecciona el rango de años:",
                  min = 1996, max = 2022, value = c(2010, 2022), sep = ""),
      actionButton("actualizar", "Actualizar")  
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Serie Temporal", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotlyOutput("serie_ipc_plotly")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotlyOutput("serie_pib_plotly")
                 )),
        
        tabPanel("ACF/PACF", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotOutput("acf_pacf_ipc_ggplot")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotOutput("acf_pacf_pib_ggplot")
                 )),
        
        tabPanel("Descomposición", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotOutput("descomposicion_ipc_ggplot")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotOutput("descomposicion_pib_ggplot")
                 ))
      )
    )
  )
)

# Crear el servidor de Shiny
server <- function(input, output, session) {
  
  # Filtrar los datos según el rango de años seleccionado solo cuando se presione el botón de "Actualizar"
  datos_filtrados_ipc <- eventReactive(input$actualizar, {
    window(ts_ipc, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  datos_filtrados_pib <- eventReactive(input$actualizar, {
    window(ts_pib, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  # Serie Temporal IPC con Plotly
  output$serie_ipc_plotly <- renderPlotly({
    serie <- datos_filtrados_ipc()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines') %>%
      layout(title = "Serie Temporal del IPC en Alemania",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "Crecimiento Interanual IPC"))
  })
  
  # Serie Temporal PIB con Plotly
  output$serie_pib_plotly <- renderPlotly({
    serie <- datos_filtrados_pib()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines') %>%
      layout(title = "Serie Temporal del PIB en Alemania",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "Crecimiento Interanual PIB"))
  })
  
  # ACF/PACF IPC
  output$acf_pacf_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    par(mfrow = c(1, 2)) # Dividir panel en dos gráficos
    acf(serie, main = "ACF IPC")
    pacf(serie, main = "PACF IPC")
  })
  
  # ACF/PACF PIB
  output$acf_pacf_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    par(mfrow = c(1, 2)) # Dividir panel en dos gráficos
    acf(serie, main = "ACF PIB")
    pacf(serie, main = "PACF PIB")
  })
  
  # Descomposición IPC
  output$descomposicion_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    autoplot(decompose(serie)) + 
      ggtitle("Descomposición del IPC en Alemania")
  })
  
  # Descomposición PIB
  output$descomposicion_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    autoplot(decompose(serie)) + 
      ggtitle("Descomposición del PIB en Alemania")
  })
}

# Ejecutar la aplicación de Shiny
shinyApp(ui = ui, server = server)


##### OTROS 
ui <- fluidPage(
  titlePanel("Análisis de Modelos ARIMA para PIB e IPC en Alemania"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("variable", "Selecciona la(s) variable(s):",
                         choices = c("IPC", "PIB"),
                         selected = "IPC"),
      sliderInput("rango_anios", "Selecciona el rango de años:",
                  min = 1996, max = 2022, value = c(2010, 2022), sep = ""),
      actionButton("actualizar", "Actualizar")  
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Residuos del Modelo ARIMA", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotOutput("residuos_arima_ipc_ggplot")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotOutput("residuos_arima_pib_ggplot")
                 )),
        
        tabPanel("Precisión del Modelo", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   tableOutput("precision_ipc")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   tableOutput("precision_pib")
                 )),
        
        tabPanel("Comparación de Modelos", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotlyOutput("comparacion_ipc")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotlyOutput("comparacion_pib")
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtrar los datos según el rango de años seleccionado
  datos_filtrados_ipc <- eventReactive(input$actualizar, {
    window(ts_ipc, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  datos_filtrados_pib <- eventReactive(input$actualizar, {
    window(ts_pib, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  # Residuos del modelo ARIMA IPC
  output$residuos_arima_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    modelo_arima_ipc <- auto.arima(serie)
    checkresiduals(modelo_arima_ipc)
  })
  
  # Residuos del modelo ARIMA PIB
  output$residuos_arima_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    modelo_arima_pib <- auto.arima(serie)
    checkresiduals(modelo_arima_pib)
  })
  
  # Precisión del modelo para IPC
  output$precision_ipc <- renderTable({
    serie <- datos_filtrados_ipc()
    modelo_arima_ipc <- auto.arima(serie)
    forecast_ipc <- forecast(modelo_arima_ipc, h = 12)
    accuracy(forecast_ipc)
  })
  
  # Precisión del modelo para PIB
  output$precision_pib <- renderTable({
    serie <- datos_filtrados_pib()
    modelo_arima_pib <- auto.arima(serie)
    forecast_pib <- forecast(modelo_arima_pib, h = 4)
    accuracy(forecast_pib)
  })
  
  # Comparación de modelos para IPC
  output$comparacion_ipc <- renderPlotly({
    serie <- datos_filtrados_ipc()
    naive_forecast <- naive(serie, h = 12)
    snaive_forecast <- snaive(serie, h = 12)
    drift_forecast <- rwf(serie, drift = TRUE, h = 12)
    
    plot_ly() %>%
      add_lines(x = time(naive_forecast$mean), y = naive_forecast$mean, name = "Naive") %>%
      add_lines(x = time(snaive_forecast$mean), y = snaive_forecast$mean, name = "S-Naive") %>%
      add_lines(x = time(drift_forecast$mean), y = drift_forecast$mean, name = "Random Walk with Drift") %>%
      layout(title = "Comparación de Modelos de Pronóstico - IPC",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "IPC"))
  })
  
  # Comparación de modelos para PIB
  output$comparacion_pib <- renderPlotly({
    serie <- datos_filtrados_pib()
    naive_forecast <- naive(serie, h = 4)
    snaive_forecast <- snaive(serie, h = 4)
    drift_forecast <- rwf(serie, drift = TRUE, h = 4)
    
    plot_ly() %>%
      add_lines(x = time(naive_forecast$mean), y = naive_forecast$mean, name = "Naive") %>%
      add_lines(x = time(snaive_forecast$mean), y = snaive_forecast$mean, name = "S-Naive") %>%
      add_lines(x = time(drift_forecast$mean), y = drift_forecast$mean, name = "Random Walk with Drift") %>%
      layout(title = "Comparación de Modelos de Pronóstico - PIB",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "PIB"))
  })
}

shinyApp(ui = ui, server = server)






