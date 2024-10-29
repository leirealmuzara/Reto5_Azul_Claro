library(shiny)
library(ggplot2)
library(plotly)
library(forecast)
library(dplyr)
library(imputeTS)
library(lubridate)
library(tseries)
library(fpp2)

# Interfaz de usuario
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
                 )),
        
        tabPanel("Predicciones ARIMA", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotlyOutput("pred_arima_ipc_plotly")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotlyOutput("pred_arima_pib_plotly")
                 ))
      )
    )
  )
)

# Servidor de Shiny
server <- function(input, output, session) {
  
  # Valores manuales para agregar
  valores_manual_ipc <- data.frame(
    fecha = as.Date(c("2022-07-01", "2022-10-01")),
    valor = c(0.7876184, 0.0146448)
  )
  
  valores_manual_pib <- data.frame(
    fecha = as.Date(c("2022-07-01", "2022-10-01")),
    valor = c(-6.0473963, -0.9654239)
  )
  
  # Filtrar datos según rango seleccionado cuando se presione "Actualizar"
  datos_filtrados_ipc <- eventReactive(input$actualizar, {
    window(ts_ipc, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  datos_filtrados_pib <- eventReactive(input$actualizar, {
    window(ts_pib, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  # Serie Temporal IPC con Plotly
  output$serie_ipc_plotly <- renderPlotly({
    serie <- datos_filtrados_ipc()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines', name = "IPC") %>%
      add_trace(x = valores_manual_ipc$fecha, y = valores_manual_ipc$valor, 
                type = 'scatter', mode = 'markers', marker = list(color = 'red', size = 10), 
                name = "Predicciones Manuales IPC") %>%
      layout(title = "Serie Temporal del IPC en Alemania",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "Crecimiento Interanual IPC"))
  })
  
  # Serie Temporal PIB con Plotly
  output$serie_pib_plotly <- renderPlotly({
    serie <- datos_filtrados_pib()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines', name = "PIB") %>%
      add_trace(x = valores_manual_pib$fecha, y = valores_manual_pib$valor, 
                type = 'scatter', mode = 'markers', marker = list(color = 'red', size = 10), 
                name = "Predicciones Manuales PIB") %>%
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
    descomposicion_ipc <- decompose(serie)
    autoplot(descomposicion_ipc) + 
      geom_point(data = valores_manual_ipc, aes(x = fecha, y = valor), color = "red", size = 3) +
      ggtitle("Descomposición del IPC en Alemania")
  })
  
  # Descomposición PIB
  output$descomposicion_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    descomposicion_pib <- decompose(serie)
    autoplot(descomposicion_pib) + 
      geom_point(data = valores_manual_pib, aes(x = fecha, y = valor), color = "red", size = 3) +
      ggtitle("Descomposición del PIB en Alemania")
  })
  
  # Predicciones ARIMA IPC
  output$pred_arima_ipc_plotly <- renderPlotly({
    serie <- datos_filtrados_ipc()
    modelo_arima_ipc <- auto.arima(serie)
    forecast_ipc <- forecast(modelo_arima_ipc, h = 12)
    plot_ly(x = time(forecast_ipc$mean), y = forecast_ipc$mean, 
            type = 'scatter', mode = 'lines', name = "Predicción IPC") %>%
      add_trace(x = valores_manual_ipc$fecha, y = valores_manual_ipc$valor, 
                type = 'scatter', mode = 'markers', marker = list(color = 'red', size = 10), 
                name = "Valores Manuales IPC") %>%
      layout(title = "Predicción del IPC con ARIMA",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "IPC"))
  })
  
  # Predicciones ARIMA PIB
  output$pred_arima_pib_plotly <- renderPlotly({
    serie <- datos_filtrados_pib()
    modelo_arima_pib <- auto.arima(serie)
    forecast_pib <- forecast(modelo_arima_pib, h = 4)
    plot_ly(x = time(forecast_pib$mean), y = forecast_pib$mean, 
            type = 'scatter', mode = 'lines', name = "Predicción PIB") %>%
      add_trace(x = valores_manual_pib$fecha, y = valores_manual_pib$valor, 
                type = 'scatter', mode = 'markers', marker = list(color = 'red', size = 10), 
                name = "Valores Manuales PIB") %>%
      layout(title = "Predicción del PIB con ARIMA",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "PIB"))
  })
}

# Correr la aplicación Shiny
shinyApp(ui = ui, server = server)

