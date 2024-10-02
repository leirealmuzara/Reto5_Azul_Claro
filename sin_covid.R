#limpieza datos reto 5
library(imputeTS)
library(lubridate)
library(forecast)
library(dplyr)
library(fpp2)

dir()
df1_covid<-read.csv("Datos-20240913/pib_ipc_paises_punto2.csv")
df2_covid<-read.csv("Datos-20240913/exogenas_paises_punto2.csv")
df3_covid<-read.csv("Datos-20240913/unemployment_germany.csv")

#filtrar alemania
df1_covid<- df1_covid %>% 
  filter(Code =="DEU")
df2_covid<- df2_covid %>% 
  filter(Code =="DEU")

#filtrar sin covid
# Filtramos los datos eliminando los años 2020 y 2021
df1_covid <- df1_covid %>% 
  filter(!(Year %in% c(2020, 2021)))

df2_covid <- df2_covid %>% 
  filter(!(Year %in% c(2020, 2021)))

###################################PIB Y IPC####################################
head(df1_covid)
str(df1_covid)
sum(is.na(df1_covid))

ipc_covid<-df1_covid[,-c(1,3,6)]
pib_covid<-df1_covid[,-c(1,3,7)]

#pib por trimestre
pib_covid<- pib_covid %>% 
  filter(Month %in% c(3,6,9,12))

# Convertir a una serie temporal
ts_ipc_covid <- ts(ipc_covid$Consumer.Price.Index..CPI., start = c(1996,1),end = c(2022,12), frequency = 12)
ts_pib_covid <- ts(pib_covid$GDP.billion.currency.units, start = c(1996,1),end = c(2022,4), frequency = 4)
ts_ipc_covid
ts_pib_covid
# Graficar la serie temporal
plot(ts_ipc_covid, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
plot(ts_pib_covid, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")


#analizar nas por variable
ggplot_na_distribution(ts_ipc_covid)
ggplot_na_distribution(ts_pib_covid)

#imputamos los NA-s que quedan
sum(is.na(ts_pib_covid))
sum(is.na(ts_ipc_covid))
#con interpolación lineal o promedio movil o la media
#no hay

#########################
#análisis de autocorrelación y descomposición, para cada variable
#acf mide la correlacion de la serie consigo misma con diferentes desfases y lags
#pacf hace la correlacion entre observaciones de la serie eliminando el efecto de los valores intermedios
acf(ts_ipc_covid, main = "ACF del IPC")
pacf(ts_ipc_covid, main = "PACF del IPC")

acf(ts_pib_covid, main = "ACF del PIB")
pacf(ts_pib_covid, main = "PACF del PIB")

#decompose separa los componentes de la serie en tendencia, estacionalidad y residual
descomposicion_ipc_covid <- decompose(ts_ipc_covid)
plot(descomposicion_ipc_covid)

descomposicion_pib_covid <- decompose(ts_pib_covid)
plot(descomposicion_pib_covid)

#################################################
#Ajustar el modelo naive para cada variable
modelo_naive_ipc_covid <- naive(window(ts_ipc_covid, end ) c(2022,2)), h = 5)
summary(modelo_naive_ipc_covid)
plot(modelo_naive_ipc_covid, main = "Predicción Naive del IPC")

modelo_naive_pib_covid <- naive(window(ts_pib_covid,end=c(2022, 2)), h = 2)
summary(modelo_naive_pib_covid)
plot(modelo_naive_pib_covid, main = "Predicción Naive del PIB")


