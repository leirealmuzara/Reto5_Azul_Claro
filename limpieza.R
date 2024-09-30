#limpieza datos reto 5
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)

dir()
df1<-read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")
df2<-read.csv("Datos-20240913/exogenas_paises_punto2.csv")
df3<-read.csv("Datos-20240913/unemployment_germany.csv")

#filtrar alemania
df1<- df1 %>% 
  filter(Code =="DEU")
df2<- df2 %>% 
  filter(Code =="DEU")

###################################PIB Y IPC####################################
head(df1)
str(df1)
sum(is.na(df1))

ipc<-df1[,-c(1,3,6)]
pib<-df1[,-c(1,3,7)]

#pib por trimestre
pib<- pib %>% 
  filter(Month %in% c(3,6,9,12))

# Convertir a una serie temporal
ts_ipc <- ts(ipc$Consumer.Price.Index..CPI., start = c(1996,1),end = c(2022,7), frequency = 12)
ts_pib <- ts(pib$GDP.billion.currency.units, start = c(1996,3),end = c(2022,4), frequency = 4)
ts_ipc
ts_pib
# Graficar la serie temporal
plot(ts_ipc, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
plot(ts_pib, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")


#analizar nas por variable
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)

#imputamos los NA-s que quedan
sum(is.na(ts_pib))
sum(is.na(ts_ipc))
#con interpolación lineal o promedio movil o la media
#no hay

#########################
#análisis de autocorrelación y descomposición, para cada variable
#acf mide la correlacion de la serie consigo misma con diferentes desfases y lags
#pacf hace la correlacion entre observaciones de la serie eliminando el efecto de los valores intermedios
acf(ts_ipc, main = "ACF del IPC")
pacf(ts_ipc, main = "PACF del IPC")

acf(ts_pib, main = "ACF del PIB")
pacf(ts_pib, main = "PACF del PIB")

#decompose separa los componentes de la serie en tendencia, estacionalidad y residual
descomposicion_ipc <- decompose(ts_ipc)
plot(descomposicion_ipc)

descomposicion_pib <- decompose(ts_pib)
plot(descomposicion_pib)

###########################EXOGENAS######################################
head(df2)
#oferta monetaria en billones y indice del mercado de valores
str(df2)
df2<-df2[,-c(1,3,7)]
is.ts(df2)
sum(is.na(df2))

ts_money <- ts(df2$Money.supply.billion.currency.units, start = c(1996,1),end = c(2022,7), frequency = 12)
ts_stock <- ts(df2$Stock.market.index, start = c(1996,1),end = c(2021,12), frequency = 12)
ts_money
ts_stock

#graficar
plot(ts_money, main="Serie Temporal de la oferta monetaria en Alemania", ylab="billones", xlab="Tiempo")
plot(ts_stock, main="Serie del indice del mercado de valores", ylab="index", xlab="Tiempo")

#analizamos nas
ggplot_na_distribution(ts_money)
ggplot_na_distribution(ts_stock)

#imputamos los NA-s
sum(is.na(ts_stock))
#no hay en ninguno

######################
#análisis de autocorrelación y descomposición, para cada variable

acf(ts_money, main = "ACF del dinero")
pacf(ts_money, main = "PACF del dinero")

acf(ts_stock, main = "ACF del stock")
pacf(ts_stock, main = "PACF del stock")

#
descomposicion_money <- decompose(ts_money)
plot(descomposicion_money)

descomposicion_stock <- decompose(ts_stock)
plot(descomposicion_stock)

#############
#Ajustar el modelo naive para cada variable
modelo_naive_money <- naive(ts_money, h = 3)
summary(modelo_naive_money)
plot(modelo_naive_money, main = "Predicción Naive de la Oferta Monetaria")

modelo_naive_stock <- naive(ts_stock, h = 3)
summary(modelo_naive_stock)
plot(modelo_naive_stock, main = "Predicción Naive del Índice Bursátil")

#en el reto habrá que probar con varios modelos

###########################??????#chequeando si los residuos son ruido blanco
residuos_unemployment <- residuals(modelo_naive_money)
# Gráfico de los residuos
plot(residuos_unemployment, main = "Residuos del Modelo Naive de la Tasa de Desempleo")
abline(h = 0, col = "red")

#boxcox para estabilizar la varianza
#quitar estacionalidad y tendencia
#tiene que ser un proceso estocastico
#modelar la serie
