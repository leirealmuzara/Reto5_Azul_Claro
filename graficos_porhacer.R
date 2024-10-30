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




























































































































################################################################################
################################################################################
################################################################################

# Cargar datos
df1 <- read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")

########################### FILTRAR Y LIMPIAR DATOS ###########################

# Filtrar Alemania
df1 <- df1 %>% filter(Code == "DEU")

#Eliminar columnas irrelevantes
df1 <- df1[,-c(1,3)]

# Filtrar datos trimestrales
df1 <- df1 %>% filter(Month %in% c(3,6,9,12))

#crear df para pib y ipc
pib <- df1[,-5]
ipc <- df1[,-4]

# Filtrar datos fuera del periodo de COVID-19
# Supongamos que el periodo de COVID-19 es de 2020 a 2022
pib_sincovid <- pib %>%
  filter(!(Year >= 2020))
ipc_sincovid <- ipc %>%
  filter(!(Year >= 2020))

# Visualizar los datos filtrados
head(pib_sincovid)
str(pib_sincovid)
sum(is.na(pib_sincovid))

head(ipc_sincovid)
str(ipc_sincovid)
sum(is.na(ipc_sincovid))

#crear columna para el crecimiento interanual
pib_sincovid$Crecimiento_Interanual<- (pib_sincovid$GDP.billion.currency.units / lag(pib_sincovid$GDP.billion.currency.units, 4) - 1) * 100
ipc_sincovid$Crecimiento_Interanual <- (ipc_sincovid$Consumer.Price.Index..CPI. / lag(ipc_sincovid$Consumer.Price.Index..CPI., 4) - 1) * 100

#####SERIES TEMPORALES####

#crear serie temporal
ts_ipc_sincovid <- ts(ipc_sincovid$Crecimiento_Interanual, start = c(1996, 3), end = c(2019, 4), frequency = 4)
ts_pib_sincovid <- ts(pib_sincovid$Crecimiento_Interanual, start = c(1996, 3), end = c(2019, 4), frequency = 4)

# Graficar las series temporales
plot(ts_ipc_sincovid, main = "Serie Temporal del IPC en Alemania", ylab = "IPC", xlab = "Tiempo")
plot(ts_pib_sincovid, main = "Serie Temporal del PIB en Alemania", ylab = "PIB", xlab = "Tiempo")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc_sincovid)
ggplot_na_distribution(ts_pib_sincovid)

#Comprobar si hay valores faltantes
sum(is.na(ts_pib_sincovid))
sum(is.na(ts_ipc_sincovid))

####ANALISIS DE AUTOCORRELACION####

#ACF y PACF
ts_ipc_sincovid <- na.omit(ts_ipc_sincovid)
ts_pib_sincovid <- na.omit(ts_pib_sincovid)

acf(ts_ipc_sincovid, main = "ACF del IPC")
pacf(ts_ipc_sincovid, main = "PACF del IPC")

acf(ts_pib_sincovid, main = "ACF del PIB")
pacf(ts_pib_sincovid, main = "PACF del PIB")

####DESCOMPOSICION DE SERIES TEMPORALES####

# Descomposición de las serie de IPC
descomposicion_ipc_sincovid <- decompose(ts_ipc_sincovid)
plot(descomposicion_ipc_sincovid)
random_ipc_sincovid <- descomposicion_ipc_sincovid$random
descomposicion_ipc_sincovid$seasonal
descomposicion_ipc_sincovid$trend


# Descomposición de las serie de PIB
descomposicion_pib_sincovid <- decompose(ts_pib_sincovid)
plot(descomposicion_pib_sincovid)
random_pib_sincovid<-descomposicion_pib_sincovid$random
descomposicion_pib_sincovid$seasonal
descomposicion_pib_sincovid$trend
plot(descomposicion_pib_sincovid)

####PRUEBA DE ESTACIOANRIEDAD####

# Prueba ADF para verificar estacionariedad de IPC y PIB
adf.test(ts_ipc_sincovid)
adf.test(ts_pib_sincovid)

# Diferenciar las series para eliminar tendencia
ts_ipc<-diff(ts_ipc_sincovid)
ts_pib<-diff(ts_pib_sincovid)

# Verificar nuevamente la estacionariedad
adf.test(ts_ipc_sincovid)
adf.test(ts_pib_sincovid)

# Graficar series diferenciadas
plot(ts_ipc_sincovid, main="IPC Diferenciado")
plot(ts_pib_sincovid, main="PIB Diferenciado")

# ACF y PACF para IPC y PIB diferenciados
acf(ts_ipc_sincovid, main="ACF IPC Diferenciado")
pacf(ts_ipc_sincovid, main="PACF IPC Diferenciado")
acf(ts_pib_sincovid, main="ACF PIB Diferenciado")
pacf(ts_pib_sincovid, main="PACF PIB Diferenciado")


#####COMPROBAR ACCURACY#####
#IPC
train_ipc_sincovid<-window(random_ipc_sincovid, start = c(1996, 6), end = c(2018, 1))
test_ipc_sincovid <- window(random_ipc_sincovid, start = c(2019,1), end = c(2019,12))

#naive
naive_ipc_sincovid<-naive(train_ipc_sincovid,h=length(test_ipc_sincovid))

#seasonal naive
snaive_ipc_sincovid<-snaive(train_ipc_sincovid,h=length(test_ipc_sincovid))

#drift
drift_ipc_sincovid<-rwf(train_ipc_sincovid,drift=TRUE,h=length(test_ipc_sincovid))

#promedio movil (MA)
ma_ipc_sincovid<-meanf(train_ipc_sincovid,h=length(test_ipc_sincovid))

#modelo autoregresivo (AR)
ar_ipc_sincovid<-Arima(train_ipc_sincovid,order = c(1,0,0))
ar_ipc_forecast_sincovid<-forecast(ar_ipc_sincovid,h=length(test_ipc_sincovid))

#ARMA
arma_ipc_sincovid<-Arima(train_ipc_sincovid,order = c(1,0,1))
arma_ipc_forecast_sincovid<-forecast(arma_ipc_sincovid,h=length(test_ipc_sincovid))

#arima
modelo_arima_ipc_sincovid <- auto.arima(train_ipc_sincovid)
summary(modelo_arima_ipc_sincovid)
arima_forecast_ipc_sincovid<-forecast(modelo_arima_ipc_sincovid,h=length(test_ipc_sincovid))

#calcular precisión de modelos de predicción
accuracy_drift_ipc_sincovid<-accuracy(drift_ipc_sincovid,test_ipc_sincovid) #0.33044675 
accuracy_snaive_ipc_sincovid<-accuracy(snaive_ipc_sincovid,test_ipc_sincovid)#0.5250969 
accuracy_naive_ipc_sincovid<-accuracy(naive_ipc_sincovid,test_ipc_sincovid)  #0.3304480    
accuracy_ma_ipc_sincovid<-accuracy(ma_ipc_sincovid,test_ipc_sincovid) #0.32001457 
accuracy_ar_ipc_sincovid<-accuracy(ar_ipc_forecast_sincovid,test_ipc_sincovid) #0.28267833 
accuracy_arma_ipc_sincovid<-accuracy(arma_ipc_forecast_sincovid,test_ipc_sincovid) #0.28267206 
accuracy_arima_ipc_sincovid<-accuracy(arima_forecast_ipc_sincovid,test_ipc_sincovid) #0.2067668 --> ES EL MEJOR  


#PIB
train_pib_sincovid<-window(random_pib_sincovid, start = c(1996, 3), end = c(2018, 1))
test_pib_sincovid<- window(random_pib_sincovid, start = c(2019,1), end = c(2019,4))

#naive
naive_pib_sincovid<-naive(train_pib_sincovid,h=length(test_pib_sincovid))

#seasonal naive
snaive_pib_sincovid<-snaive(train_pib_sincovid,h=length(test_pib_sincovid))

#drift
drift_pib_sincovid<-rwf(train_pib_sincovid,drift=TRUE,h=length(test_pib_sincovid))

#promedio movil (MA)
ma_pib_sincovid<-meanf(train_pib_sincovid,h=length(test_pib_sincovid))

#modelo autoregresivo (AR)
ar_pib_sincovid<-Arima(train_pib_sincovid,order = c(1,0,0))
ar_pib_forecast_sincovid<-forecast(ar_pib_sincovid,h=length(test_pib_sincovid))

#ARMA
arma_pib_sincovid<-Arima(train_pib_sincovid,order = c(1,0,1))
arma_pib_forecast_sincovid<-forecast(arma_pib_sincovid,h=length(test_pib_sincovid))

#arima
modelo_arima_pib_sincovid <- auto.arima(train_pib_sincovid)
summary(modelo_arima_pib_sincovid)
arima_forecast_pib_sincovid<-forecast(modelo_arima_pib_sincovid,h=36)
summary(arima_forecast_pib_sincovid)

#calcular precisión de modelos de predicción
accuracy_drift_pib_sincovid<-accuracy(drift_pib_sincovid,test_pib_sincovid) #1.265945 
accuracy_snaive_pib_sincovid<-accuracy(snaive_pib_sincovid,test_pib_sincovid) #1.344134  
accuracy_naive_pib_sincovid<-accuracy(naive_pib_sincovid,test_pib_sincovid)  #1.266053  
accuracy_ma_pib_sincovid<-accuracy(ma_pib_sincovid,test_pib_sincovid) #0.8241532  
accuracy_ar_pib_sincovid<-accuracy(ar_pib_forecast_sincovid,test_pib_sincovid) #0.8113892  
accuracy_arma_pib_sincovid<-accuracy(arma_pib_forecast_sincovid,test_pib_sincovid) #0.7092525 
accuracy_arima_pib_sincovid<-accuracy(arima_forecast_pib_sincovid,test_pib_sincovid) #0.6248854  --> ES LO MEJOR 

################PREDICCIONES######################################
#IPC
# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_ipc_sincovid)
#añadir estacionalidad y tendencia a la serie

# Predicción de IPC con ARIMA a 12 meses
modelo_arima_ipc_sincovid <- auto.arima(ts_ipc_sincovid)
forecast_ipc_sincovid <- forecast(modelo_arima_ipc_sincovid, h = 4 )
summary(forecast_ipc_sincovid)
plot(forecast_ipc_sincovid, main="Predicción del IPC con ARIMA")
abline(h = 0, col = "red", lty = 2)

#PIB
# Verificar los residuos de los modelos ARIMA
checkresiduals(modelo_arima_pib_sincovid)
#añadir estacionalidad y tendencia a la serie

# Predicción de PIB con ARIMA a 12 meses
modelo_arima_pib_sincovid <- auto.arima(ts_pib_sincovid)
forecast_pib_sincovid <- forecast(modelo_arima_pib_sincovid, h = 4 )
summary(forecast_pib_sincovid)
plot(forecast_pib_sincovid, main="Predicción del PIB con ARIMA")
abline(h = 0, col = "red", lty = 2)




## CAMBIAR 2020 por 2022
forecast_pib_table_sincovid <- data.frame(forecast_pib_sincovid)
row.names(forecast_pib_table_sincovid) <- gsub("2020", "2022", row.names(forecast_pib_table_sincovid))

forecast_pib_table_sincovid

forecast_ipc_table_sincovid <- data.frame(forecast_ipc_sincovid)
row.names(forecast_ipc_table_sincovid) <- gsub("2020", "2022", row.names(forecast_ipc_table_sincovid))

forecast_ipc_table_sincovid

