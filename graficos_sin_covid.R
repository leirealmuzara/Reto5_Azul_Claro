################################################################################
# GRAFICOS SIN COVID
ui <- fluidPage(
  titlePanel("Análisis de IPC y PIB en Alemania"),
  
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

# Crear el servidor de Shiny
server <- function(input, output, session) {
  
  # Filtrar los datos según el rango de años seleccionado
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
             yaxis = list(title = "IPC"))
  })
  
  # Serie Temporal PIB con Plotly
  output$serie_pib_plotly <- renderPlotly({
    serie <- datos_filtrados_pib()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines') %>%
      layout(title = "Serie Temporal del PIB en Alemania",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "PIB"))
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
  
  # Predicciones ARIMA IPC
  output$pred_arima_ipc_plotly <- renderPlotly({
    serie <- datos_filtrados_ipc()
    modelo_arima_ipc <- auto.arima(serie)
    forecast_ipc <- forecast(modelo_arima_ipc, h = 12)
    plot_ly(x = time(forecast_ipc$mean), y = forecast_ipc$mean, 
            type = 'scatter', mode = 'lines', name = "Predicción IPC") %>%
      layout(title = "Predicción del IPC con ARIMA",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "IPC"))
  })
  
  # Predicciones ARIMA PIB
  output$pred_arima_pib_plotly <- renderPlotly({
    serie <- datos_filtrados_pib()
    modelo_arima_pib <- auto.arima(serie)
    forecast_pib_table <- forecast(modelo_arima_pib, h = 12)
    plot_ly(x = time(forecast_pib_table$mean), y = forecast_pib_table$mean, 
            type = 'scatter', mode = 'lines', name = "Predicción PIB") %>%
      layout(title = "Predicción del PIB con ARIMA",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "PIB"))
  })
}

# Ejecutar la aplicación de Shiny
shinyApp(ui = ui, server = server)
