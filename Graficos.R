library(shiny)
library(ggplot2)
library(plotly)
library(forecast)
library(dplyr)
library(imputeTS)
library(lubridate)
library(tseries)
library(fpp2)

# Asegúrate de tener tus datos df_pib y df_ipc disponibles aquí

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
        
        tabPanel("Residuos del Modelo SARIMA", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotOutput("residuos_sarima_ipc_ggplot")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotOutput("residuos_sarima_pib_ggplot")
                 )),
        
        # Nuevas pestañas para la variación anual
        tabPanel("Variación Anual", 
                 conditionalPanel(
                   condition = "input.variable.includes('IPC')",
                   plotlyOutput("variacion_ipc_plotly")
                 ),
                 conditionalPanel(
                   condition = "input.variable.includes('PIB')",
                   plotlyOutput("variacion_pib_plotly")
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtrar los datos según el rango de años seleccionado solo cuando se presione el botón de "Actualizar"
  datos_filtrados_ipc <- eventReactive(input$actualizar, {
    window(ts_ipc, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  datos_filtrados_pib <- eventReactive(input$actualizar, {
    window(ts_pib, start = c(input$rango_anios[1], 1), end = c(input$rango_anios[2], 12))
  })
  
  # Calcular el PIB anual y la variación anual
  df_pib_anual <- df_pib %>%
    mutate(
      PIB_anual = Qtr1 + Qtr2 + Qtr3 + Qtr4,  # Sumar los trimestres para obtener el PIB anual
      variacion_pib = (PIB_anual - lag(PIB_anual)) / lag(PIB_anual) * 100  # Calcular variación porcentual
    )
  
  # Graficar la variación del PIB
  output$variacion_pib_plotly <- renderPlotly({
    plot_ly(df_pib_anual, x = ~Año, y = ~variacion_pib, type = 'bar', 
            marker = list(color = '#93044e')) %>%
      layout(title = "Variación Anual del PIB en Alemania",
             xaxis = list(title = "Año"),
             yaxis = list(title = "Crecimiento Anual (%)"))
  })
  
  # Calcular el IPC anual y la variación anual
  df_ipc_anual <- df_ipc %>%
    mutate(
      IPC_anual = Qtr1 + Qtr2 + Qtr3 + Qtr4,
      variacion_ipc = (IPC_anual - lag(IPC_anual)) / lag(IPC_anual) * 100 
    )
  
  # Graficar la variación del IPC
  output$variacion_ipc_plotly <- renderPlotly({
    plot_ly(df_ipc_anual, x = ~Año, y = ~variacion_ipc, type = 'bar', 
            marker = list(color = '#8db41c')) %>%
      layout(title = "Variación Anual del IPC en Alemania",
             xaxis = list(title = "Año"),
             yaxis = list(title = "Crecimiento Anual (%)"))
  })
  
  # Serie Temporal IPC con Plotly
  output$serie_ipc_plotly <- renderPlotly({
    serie <- datos_filtrados_ipc()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines', line = list(color = '#8db41c')) %>%
      layout(title = "Serie Temporal del IPC en Alemania",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "Crecimiento Interanual IPC"))
  })
  
  # Serie Temporal PIB con Plotly
  output$serie_pib_plotly <- renderPlotly({
    serie <- datos_filtrados_pib()
    plot_ly(x = time(serie), y = as.numeric(serie), type = 'scatter', mode = 'lines', line = list(color = '#93044e')) %>%
      layout(title = "Serie Temporal del PIB en Alemania",
             xaxis = list(title = "Tiempo"),
             yaxis = list(title = "Crecimiento Interanual PIB"))
  })
  
  # ACF/PACF IPC
  output$acf_pacf_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    par(mfrow = c(1, 2)) # Dividir panel en dos gráficos
    Acf(serie, main = "ACF IPC", col = "#8db41c")
    Pacf(serie, main = "PACF IPC", col = "#8db41c")
  })
  
  # ACF/PACF PIB
  output$acf_pacf_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    par(mfrow = c(1, 2)) # Dividir panel en dos gráficos
    Acf(serie, main = "ACF PIB", col = "#93044e")
    Pacf(serie, main = "PACF PIB", col = "#93044e")
  })
  
  # Descomposición IPC
  output$descomposicion_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    autoplot(decompose(serie)) + 
      ggtitle("Descomposición del IPC en Alemania") +
      theme_minimal()
  })
  
  # Descomposición PIB
  output$descomposicion_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    autoplot(decompose(serie)) + 
      ggtitle("Descomposición del PIB en Alemania") +
      theme_minimal()
  })
  
  # Residuos del modelo SARIMA IPC
  output$residuos_sarima_ipc_ggplot <- renderPlot({
    serie <- datos_filtrados_ipc()
    modelo_sarima_ipc <- Arima(serie, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))  # Cambiar parámetros según sea necesario
    checkresiduals(modelo_sarima_ipc, col = "#8db41c")
  })
  
  # Residuos del modelo SARIMA PIB
  output$residuos_sarima_pib_ggplot <- renderPlot({
    serie <- datos_filtrados_pib()
    modelo_sarima_pib <- Arima(serie, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))  # Cambiar parámetros según sea necesario
    checkresiduals(modelo_sarima_pib, col = "#93044e")
  })
  
}

shinyApp(ui = ui, server = server)

