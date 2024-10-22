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
      selectInput("grafico", "Selecciona el gráfico:",
                  choices = c("Serie Temporal", "ACF/PACF", "Descomposición", "Predicciones ARIMA")),
      checkboxGroupInput("variable", "Selecciona la(s) variable(s):",
                         choices = c("IPC", "PIB"),
                         selected = "IPC"),
      sliderInput("rango_anios", "Selecciona el rango de años:",
                  min = 2000, max = 2024, value = c(2010, 2020), sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        # Condicional para mostrar el gráfico seleccionado según IPC y/o PIB
        conditionalPanel(
          condition = "input.grafico == 'Serie Temporal'",
          tabPanel("Serie Temporal", 
                   conditionalPanel(
                     condition = "input.variable.includes('IPC')",
                     plotOutput("serie_ipc_ggplot")
                   ),
                   conditionalPanel(
                     condition = "input.variable.includes('PIB')",
                     plotOutput("serie_pib_ggplot")
                   ))
        ),
        conditionalPanel(
          condition = "input.grafico == 'ACF/PACF'",
          tabPanel("ACF/PACF", 
                   conditionalPanel(
                     condition = "input.variable.includes('IPC')",
                     plotOutput("acf_pacf_ipc_ggplot")
                   ),
                   conditionalPanel(
                     condition = "input.variable.includes('PIB')",
                     plotOutput("acf_pacf_pib_ggplot")
                   ))
        ),
        conditionalPanel(
          condition = "input.grafico == 'Descomposición'",
          tabPanel("Descomposición", 
                   conditionalPanel(
                     condition = "input.variable.includes('IPC')",
                     plotOutput("descomposicion_ipc_ggplot")
                   ),
                   conditionalPanel(
                     condition = "input.variable.includes('PIB')",
                     plotOutput("descomposicion_pib_ggplot")
                   ))
        ),
        conditionalPanel(
          condition = "input.grafico == 'Predicciones ARIMA'",
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
)

# Crear el servidor de Shiny
server <- function(input, output) {
  
  # Filtrar los datos según el rango de años seleccionado
  datos_filtrados_ipc <- reactive({
    window(ts_ipc, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  datos_filtrados_pib <- reactive({
    window(ts_pib, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  # Serie Temporal IPC
  output$serie_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    autoplot(serie) + 
      ggtitle("Serie Temporal del IPC en Alemania") +
      xlab("Tiempo") + ylab("Crecimiento Interanual IPC")
  })
  
  # Serie Temporal PIB
  output$serie_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    autoplot(serie) + 
      ggtitle("Serie Temporal del PIB en Alemania") +
      xlab("Tiempo") + ylab("Crecimiento Interanual PIB")
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
    forecast_pib <- forecast(modelo_arima_pib, h = 4)
    plot_ly(x = time(forecast_pib$mean), y = forecast_pib$mean, 
            type = 'scatter', mode = 'lines', name = "Predicción PIB") %>%
      layout(title = "Predicción del PIB con ARIMA",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "PIB"))
  })
}

# Ejecutar la aplicación de Shiny
shinyApp(ui = ui, server = server)














































ui <- fluidPage(
  titlePanel("Visualización del PIB e IPC para Alemania"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("grafico", "Selecciona el gráfico:",
                  choices = c("Serie Temporal", "ACF/PACF", "Descomposición", "Predicciones ARIMA")),
      checkboxGroupInput("variable", "Selecciona la(s) variable(s):",
                         choices = c("IPC", "PIB"),
                         selected = "IPC"),
      sliderInput("rango_anios", "Selecciona el rango de años:",
                  min = 2000, max = 2024, value = c(2010, 2020), sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        # Condicional para mostrar el gráfico seleccionado según IPC y/o PIB
        conditionalPanel(
          condition = "input.grafico == 'Serie Temporal'",
          tabPanel("Serie Temporal", 
                   conditionalPanel(
                     condition = "input.variable.includes('IPC')",
                     plotlyOutput("serie_ipc_plotly")
                   ),
                   conditionalPanel(
                     condition = "input.variable.includes('PIB')",
                     plotlyOutput("serie_pib_plotly")
                   ))
        ),
        conditionalPanel(
          condition = "input.grafico == 'ACF/PACF'",
          tabPanel("ACF/PACF", 
                   conditionalPanel(
                     condition = "input.variable.includes('IPC')",
                     plotOutput("acf_pacf_ipc_ggplot")
                   ),
                   conditionalPanel(
                     condition = "input.variable.includes('PIB')",
                     plotOutput("acf_pacf_pib_ggplot")
                   ))
        ),
        conditionalPanel(
          condition = "input.grafico == 'Descomposición'",
          tabPanel("Descomposición", 
                   conditionalPanel(
                     condition = "input.variable.includes('IPC')",
                     plotOutput("descomposicion_ipc_ggplot")
                   ),
                   conditionalPanel(
                     condition = "input.variable.includes('PIB')",
                     plotOutput("descomposicion_pib_ggplot")
                   ))
        ),
        conditionalPanel(
          condition = "input.grafico == 'Predicciones ARIMA'",
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
)

# Crear el servidor de Shiny
server <- function(input, output) {
  
  # Filtrar los datos según el rango de años seleccionado
  datos_filtrados_ipc <- reactive({
    window(ts_ipc, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  datos_filtrados_pib <- reactive({
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
    forecast_pib <- forecast(modelo_arima_pib, h = 4)
    plot_ly(x = time(forecast_pib$mean), y = forecast_pib$mean, 
            type = 'scatter', mode = 'lines', name = "Predicción PIB") %>%
      layout(title = "Predicción del PIB con ARIMA",
             xaxis = list(title = "Tiempo"), yaxis = list(title = "PIB"))
  })
}

# Ejecutar la aplicación de Shiny
shinyApp(ui = ui, server = server)
