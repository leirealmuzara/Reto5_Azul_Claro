#### DATOS_ REALES ##############################################

# Cargar datos
pib_ipc <- read.csv("Datos/Extra/pib_ipc_paises_punto2.csv")
pib <- read.csv("Datos/Extra/pib_datos_reales.csv")
ipc <- read.csv("Datos/Extra/ipc_datos_reales.csv")


#IPC --> Nos da el crecimiento interanual
#limpiamos df
ipc <- ipc[-c(1:9),]
ipc <- ipc[,-3]
colnames(ipc) <- c("fecha", "Crecimiento_Interanual")
str(ipc)
ipc$Crecimiento_Interanual <- as.numeric(ipc$Crecimiento_Interanual)
ipc$Crecimiento_Interanual <- (ipc$Crecimiento_Interanual / lag(ipc$Crecimiento_Interanual, 12) - 1) * 100

#PIB --> No nos da el crecimiento interanual, por lo que vamos a calcularlo.
# Calcular el crecimiento interanual respecto al año anterior
colnames(pib) <- c("fecha", "Crecimiento_Interanual")
pib$Crecimiento_Interanual <- (pib$Crecimiento_Interanual / lag(pib$Crecimiento_Interanual, 4) - 1) * 100


########################### SERIES TEMPORALES ###########################

# Crear series temporales
ts_ipc <- ts(ipc$Crecimiento_Interanual, start = c(1996,1), end = c(2022,7), frequency = 12) #asi no coge los dos ultimos trimestres
ts_pib <- ts(pib$Crecimiento_Interanual, start = c(1996,1), end = c(2022,2), frequency = 4)

# Graficar series temporales
plot(ts_ipc, main="Serie Temporal del IPC en Alemania", ylab="IPC", xlab="Tiempo")
plot(ts_pib, main="Serie Temporal del PIB en Alemania", ylab="PIB", xlab="Tiempo")

# Revisar la distribución de valores faltantes
ggplot_na_distribution(ts_ipc)
ggplot_na_distribution(ts_pib)

# Comprobar si hay valores faltantes
sum(is.na(ts_pib))
sum(is.na(ts_ipc))


########## GRÁFICO PIB ####################

plot(ts_pib)
last_values <- tail(ts_pib, 4)
last_positions <- time(ts_pib)[(length(ts_pib) - 3):length(ts_pib)]

df_last <- data.frame(
  Fecha = last_positions,
  Valor = last_values,
  nudge_x = c(0.5, 0.4, 0.5, 0)  
)

ggplot(data = df_last, aes(x = Fecha, y = Valor)) +
  geom_line(data = data.frame(Fecha = time(ts_pib), Valor = as.numeric(ts_pib)), aes(x = Fecha, y = Valor), color = "#7c7741") +
  geom_point(color ="#8db41c") +
  geom_text_repel(aes(label = round(Valor, 2)), color = "#8db41c", nudge_x = df_last$nudge_x, nudge_y = 0.05) +
  labs(title = "Crecimiento del PIB real", x = "AÑO", y = "CRECIMIENTO DEL PIB") +
  theme_minimal()

################### GRÁFICO IPC ######################

plot(ts_ipc)
last_values_ipc <- tail(ts_ipc, 4)
last_positions_ipc <- time(ts_ipc)[(length(ts_ipc) - 3):length(ts_ipc)]


df_last_ipc <- data.frame(
  Fecha = last_positions_ipc,
  Valor = last_values_ipc,
  nudge_x = c(3, 0.4, 0.5, 0)  
)

ggplot(data = df_last_ipc, aes(x = Fecha, y = Valor)) +
  geom_line(data = data.frame(Fecha = time(ts_ipc), Valor = as.numeric(ts_ipc)), aes(x = Fecha, y = Valor), color = "#7c7741") +
  geom_point(color ="#8db41c") +
  geom_text_repel(aes(label = round(Valor, 2)), color = "#8db41c", nudge_x = df_last_ipc$nudge_x, nudge_y = 0.05) +
  labs(title = "Crecimiento del IPC real", x = "AÑO", y = "CRECIMIENTO DEL IPC") +
  theme_minimal()

####### GRÁFICO PIB 2021 ############################

pib_bien <- pib[-c(129:134),]
pib_bien <- pib_bien[-c(1:4),]

ts_pib_bien <- ts(pib_bien$Crecimiento_Interanual, start = c(1992,1), end = c(2022,4), frequency = 4)


df <- data.frame(
  Fecha = time(ts_pib_bien),
  Valor = as.numeric(ts_pib_bien)
)

df_last <- data.frame(
  Fecha = tail(time(ts_pib_bien), 2),  
  Valor = tail(as.numeric(ts_pib_bien), 2),  
  nudge_x = c(1.4, 0.4)  
)


ggplot(df, aes(x = Fecha, y = Valor)) +
  geom_line(color = "#7c0e4c") +  
  geom_point(data = df_last, aes(x = Fecha, y = Valor), color = "#93044e", size = 2) +  
  geom_text_repel(
    data = df_last, 
    aes(x = Fecha, y = Valor, label = round(Valor, 2)), 
    nudge_x = df_last$nudge_x, 
    color = "#93440e"
  ) +
  labs(
    title = "Evolución del crecimiento del PIB",
    x = "Año",
    y = "Crecimiento del PIB"
  ) +
  theme_minimal()

####### GRÁFICO IPC 2021 ########

ipc_bien <- ipc[-c(1:21),]

ts_ipc_bien <- ts(ipc_bien$Crecimiento_Interanual, start = c(1998,8), end = c(2022,12), frequency = 12)

plot(ts_ipc_bien)
last_values_ipc <- tail(ts_ipc_bien, 4)
last_positions_ipc <- time(ts_ipc_bien)[(length(ts_ipc_bien) - 3):length(ts_ipc_bien)]


df_last_ipc <- data.frame(
  Fecha = last_positions_ipc,
  Valor = last_values_ipc,
  nudge_x = c(-1, -1, -1, -1)  
)

ggplot(data = df_last_ipc, aes(x = Fecha, y = Valor)) +
  geom_line(data = data.frame(Fecha = time(ts_ipc_bien), Valor = as.numeric(ts_ipc_bien)), aes(x = Fecha, y = Valor), color = "#7c7741") +
  geom_point(color ="#8db41c") +
  geom_text_repel(aes(label = round(Valor, 2)), color = "#8db41c", nudge_x = df_last_ipc$nudge_x, nudge_y = 0.05) +
  labs(title = "Crecimiento del IPC real", x = "Año", y = "Crecimiento del IPC") +
  theme_minimal()

