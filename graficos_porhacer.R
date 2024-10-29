# Graficar series temporales
plot(ts_ipc, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
plot(ts_pib, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")
plot(ts_masa_monetaria, main="Serie Temporal de la masa monetaria en Alemania", ylab="crecimiento", xlab="Tiempo")
plot(ts_indice_bursatil, main="Serie Temporal del índice bursátil en Alemania", ylab="%", xlab="Tiempo")
plot(ts_paro, main="Serie Temporal del paro en Alemania", ylab="%", xlab="Tiempo")




# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)
ggplot_na_distribution(ts_indice_bursatil)
ggplot_na_distribution(ts_masa_monetaria)
ggplot_na_distribution(ts_paro)





########################### ANÁLISIS DE AUTOCORRELACIÓN ###########################
# ACF y PACF
acf(ts_ipc, main = "ACF del IPC") #analiza la estacionalidad
pacf(ts_ipc, main = "PACF del IPC")

acf(ts_masa_monetaria, main = "ACF de la masa monetaria") 
pacf(ts_masa_monetaria, main = "PACF de la masa monetaria")

acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")

acf(ts_indice_bursatil, main = "ACF del indice bursatil")
pacf(ts_indice_bursatil, main = "PACF del indice bursatil")

acf(ts_paro, main = "ACF del crecimiento del paro")
pacf(ts_paro, main = "PACF del crecimiento del paro")







########################### DESCOMPOSICIÓN DE SERIES ###########################

# Descomposición de la serie de IPC
descomposicion_ipc <- decompose(ts_ipc)
plot(descomposicion_ipc)
descomposicion_ipc$seasonal #estacionalidad
descomposicion_ipc$trend #tendencia
random_ipc<-descomposicion_ipc$random #residuo

# Descomposición de la serie de PIB
descomposicion_pib <- decompose(ts_pib)
plot(descomposicion_pib)
descomposicion_pib$seasonal #estacionalidad
descomposicion_pib$trend #tendencia
random_pib<-descomposicion_pib$random #residuo

# Descomposición de la serie de masa monetaria
descomposicion_tm <- decompose(ts_masa_monetaria)
plot(descomposicion_tm)
descomposicion_tm$seasonal #estacionalidad
descomposicion_tm$trend #tendencia
random_tm<-descomposicion_tm$random #residuo

# Descomposición de la serie de indice bursatil
descomposicion_ib <- decompose(ts_indice_bursatil)
plot(descomposicion_ib)
descomposicion_ib$seasonal #estacionalidad
descomposicion_ib$trend #tendencia
random_ib<-descomposicion_ib$random #residuo

# Descomposición de la serie del paro
descomposicion_paro <- decompose(ts_paro)
plot(descomposicion_paro)
descomposicion_paro$seasonal #estacionalidad
descomposicion_paro$trend #tendencia
random_paro<-descomposicion_paro$random #residuo






# Graficar series diferenciadas
plot(ts_ipc, main="IPC Diferenciado")
plot(ts_pib, main="PIB Diferenciado")

plot(ts_masa_monetaria, main="IPC Diferenciado")
plot(ts_indice_bursatil, main="PIB Diferenciado")

plot(ts_paro, main="Paro Diferenciado")






# ACF y PACF para series diferenciados
acf(ts_ipc, main="ACF IPC Diferenciado")
pacf(ts_ipc, main="PACF IPC Diferenciado")
acf(ts_pib, main="ACF PIB Diferenciado")
pacf(ts_pib, main="PACF PIB Diferenciado")

acf(ts_masa_monetaria, main="ACF IPC Diferenciado")
pacf(ts_masa_monetaria, main="PACF IPC Diferenciado")
acf(ts_indice_bursatil, main="ACF PIB Diferenciado")
pacf(ts_indice_bursatil, main="PACF PIB Diferenciado")

acf(ts_paro, main="ACF Paro Diferenciado")
pacf(ts_paro, main="PACF Paro Diferenciado")





# Graficos de resultados
forecast_ipc <- forecast(arimax_model_ipc, xreg = test_ipc, h = length(test_pib))
autoplot(forecast_ipc)
modelo_ipc <- forecast_ipc$mean

forecast_bolsa <- forecast(arimax_model_bolsa, xreg = test_bolsa, h = length(test_pib))
autoplot(forecast_bolsa)
modelo_bolsa <- forecast_bolsa$mean

forecast_oferta <- forecast(arimax_model_oferta, xreg = test_oferta, h = length(test_pib))
autoplot(forecast_oferta)
modelo_oferta <- forecast_oferta$mean

forecast_paro <- forecast(arimax_model_paro, xreg = test_paro, h = length(test_pib))
autoplot(forecast_paro)
modelo_paro <- forecast_paro$mean

forecast_arimax2 <- forecast(arimax_model2, xreg = cbind(test_ipc,test_paro,test_oferta, test_bolsa), h = 2)
modelo_arimax2 <- forecast_arimax2$mean

plot(forecast_ipc, main="Predicción IPC")
plot(forecast_bolsa, main="Predicción Indice Bursátil")
plot(forecast_oferta, main="Predicción Masa monetaria")
plot(forecast_paro, main="Predicción Desempleo")

plot(forecast_arimax2, main="Predicción ARIMAX")








##############################################################################

# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_pib)
checkresiduals(modelo_arima_ipc)
#añadir estacionalidad y tendencia a la serie

# Predicción de IPC y PIB con ARIMA a 2 trimestres
modelo_arima_ipc <-auto.arima(ts_ipc)
forecast_ipc <- forecast(modelo_arima_ipc, h = 2)
summary(forecast_ipc)
plot(forecast_ipc, main="Predicción del IPC con ARIMA")
abline(h = 0, col = "red", lty = 2)

modelo_arima_pib <- auto.arima(ts_pib)
forecast_pib <- forecast(modelo_arima_pib, h = 2)
summary(forecast_pib)
plot(forecast_pib, main="Predicción del PIB con ARIMA")
abline(h = 0, col = "red", lty = 2)

ts_pib
write.csv(ts_pib, file = "seriePIB.csv", row.names = TRUE)
write.csv(ts_ipc, file = "serieIPC.csv", row.names = TRUE)

