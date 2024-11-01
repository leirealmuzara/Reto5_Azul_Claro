getwd()
setwd("C:/Users/pieza/OneDrive/Escritorio/Bdata2/RETOS/Reto5/Reto5_Azul_Claro/datos_reales2")
library(readxl)
library(dplyr)
dir()

df <- read_xlsx("desglose.xlsx", sheet = 3)

# LIMPIAR DF 

colnames(df) <- df[7,]

df <- df[-c(1:7),]
df <- df[-1,]

colnames(df)[2] <- "PAIS"
colnames(df)[1] <- "AÑO"

df_alemania <- df[df$PAIS == "Germany", ]

df_alemania <- df_alemania[-c(29:31),]
df_alemania <- df_alemania[,-c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 
                               30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54,
                               56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80)]
df_alemania <- df_alemania[,-c(37:39)]

# VARIABLES IMPORTANTES 

colnames(df_alemania)

desglose <- df_alemania[,c(1, 2, 3, 6, 9, 14, 15, 18, 21, 24)]
colnames(desglose)
colnames(desglose) <- c("AÑO", "PAÍS", "PIB", "GASTO_PUBLICO", "CONSUMO", 
                        "INVERSION", "INVERSION_EXISTENCIAS", 
                        "EXPORTACIONES", "IMPORTACIONES", "BALANZA_COMERCIAL")

# PASAR A CRECIMIENTO 

desglose$PIB <- as.numeric(desglose$PIB)
desglose$GASTO_PUBLICO <- as.numeric(desglose$GASTO_PUBLICO)
desglose$CONSUMO <- as.numeric(desglose$CONSUMO)
desglose$INVERSION <- as.numeric(desglose$INVERSION)
desglose$INVERSION_EXISTENCIAS<- as.numeric(desglose$INVERSION_EXISTENCIAS)
desglose$EXPORTACIONES <- as.numeric(desglose$EXPORTACIONES)
desglose$IMPORTACIONES <- as.numeric(desglose$IMPORTACIONES)
desglose$BALANZA_COMERCIAL <- as.numeric(desglose$BALANZA_COMERCIAL)

crecimiento <- desglose

crecimiento$PIB <- (crecimiento$PIB / lag(crecimiento$PIB, 1) - 1) * 100
crecimiento$GASTO_PUBLICO = (crecimiento$GASTO_PUBLICO / lag(crecimiento$GASTO_PUBLICO, 1) - 1) * 100
crecimiento$CONSUMO = (crecimiento$CONSUMO / lag(crecimiento$CONSUMO, 1) - 1) * 100
crecimiento$INVERSION = (crecimiento$INVERSION / lag(crecimiento$INVERSION, 1) - 1) * 100
crecimiento$INVERSION_EXISTENCIAS = (crecimiento$INVERSION_EXISTENCIAS / lag(crecimiento$INVERSION_EXISTENCIAS, 1) - 1) * 100
crecimiento$EXPORTACIONES = (crecimiento$EXPORTACIONES / lag(crecimiento$EXPORTACIONES, 1) - 1) * 100
crecimiento$IMPORTACIONES = (crecimiento$IMPORTACIONES  / lag(crecimiento$IMPORTACIONES , 1) - 1) * 100
crecimiento$BALANZA_COMERCIAL = (crecimiento$BALANZA_COMERCIAL / lag(crecimiento$BALANZA_COMERCIAL, 1) - 1) * 100

# GRÁFICO PIB 

pib <- plot(crecimiento$AÑO, crecimiento$PIB, 
            type = "l", main = "Evolución del crecimiento del PIB", 
            xlab = "AÑO", ylab = "PIB", col = "blue", lwd = 2)

# GRÁFCIO GASTO PÚBLICO 

gasto_publico <- plot(crecimiento$AÑO, crecimiento$GASTO_PUBLICO, 
            type = "l", main = "Evolución del crecimiento del gasto público", 
            xlab = "AÑO", ylab = "GASTO PÚBLICO", col = "blue", lwd = 2)
 
# GRÁFICO COSUMO 

consumo <- plot(crecimiento$AÑO, crecimiento$CONSUMO, 
                      type = "l", main = "Evolución del crecimiento del consumo", 
                      xlab = "AÑO", ylab = "CONSUMO", col = "blue", lwd = 2)

# GRÁFICO INVERSIÓN 

inversion <- plot(crecimiento$AÑO, crecimiento$INVERSION, 
                      type = "l", main = "Evolución del crecimiento de la inversion", 
                      xlab = "AÑO", ylab = "INVERSIÓN", col = "blue", lwd = 2)

# GRÁFICO INVERSIÓN EN EXISTENCIAS 

inversion_existencias <- plot(crecimiento$AÑO, crecimiento$INVERSION_EXISTENCIAS, 
                      type = "l", main = "Evolución del crecimiento de la inversión en existencias", 
                      xlab = "AÑO", ylab = "INVERSIÓN EN EXISTENCIAS", col = "blue", lwd = 2)

# GRÁFICO EXPORTACIONES DE BIENES Y SERVICIOS 

exportaciones <- plot(crecimiento$AÑO, crecimiento$EXPORTACIONES, 
                      type = "l", main = "Evolución del crecimiento de las exportaciones de bienes y servicios", 
                      xlab = "AÑO", ylab = "EXPORTACIONES DE BIENES Y SERVICIOS", col = "blue", lwd = 2)
 
# GRÁFICO IMPORTACIONES DE BIENES Y SERVICIOS

importaciones <- plot(crecimiento$AÑO, crecimiento$IMPORTACIONES, 
                      type = "l", main = "Evolución del crecimiento de las importaciones de bienes y servicios", 
                      xlab = "AÑO", ylab = "IMPORTACIONES DE BIENES Y SERVICIOS", col = "blue", lwd = 2)

# GRÁFICO BALANZA COMERCIAL

balanza_comercial <- plot(crecimiento$AÑO, crecimiento$BALANZA_COMERCIAL, 
                      type = "l", main = "Evolución del crecimiento de la balanza comercial", 
                      xlab = "AÑO", ylab = "BALANZA COMERCIAL", col = "blue", lwd = 2)

# PESO DE CADA VARIABLE EN EL PIB 

str(desglose) 

peso_gasto_publico_variable = desglose$GASTO_PUBLICO / desglose$PIB * 100
peso_gasto_publico = mean(peso_gasto_publico_variable) # 19.76694%

peso_consumo_variable = desglose$CONSUMO / desglose$PIB * 100
peso_consumo = mean(peso_consumo_variable) # 54.7449%

peso_inversion_variable = desglose$INVERSION / desglose$PIB * 100
peso_inversion = mean(peso_inversion_variable) # 20.58445%

peso_inversion_existencias_variable = desglose$INVERSION_EXISTENCIAS / desglose$PIB * 100
peso_inversion_existencias = mean(peso_inversion_existencias_variable) # 0.4920707%

peso_exportaciones_variable = desglose$EXPORTACIONES / desglose$PIB * 100
peso_exportaciones = mean(peso_exportaciones_variable) # 37.03372%

peso_importaciones_variable = desglose$IMPORTACIONES / desglose$PIB * 100
peso_importaciones = mean(peso_importaciones_variable) # 32.62208%

peso_balanza_comercial_variable = desglose$BALANZA_COMERCIAL / desglose$PIB * 100
peso_balanza_comercial = mean(peso_balanza_comercial_variable) # 4.411641%

porcentaje_total = peso_balanza_comercial + peso_consumo + peso_gasto_publico + peso_inversion + peso_inversion_existencias
  
pib_2019 <- desglose[1,]

# COMPROBACIÓN DESGLOSE DEL PIB

exportaciones_netas = pib_2019$EXPORTACIONES - pib_2019$IMPORTACIONES
pib_2019$BALANZA_COMERCIAL

pib_total = pib_2019$GASTO_PUBLICO + pib_2019$CONSUMO + pib_2019$INVERSION + pib_2019$BALANZA_COMERCIAL + pib_2019$INVERSION_EXISTENCIAS
pib_2019$PIB

###########################################3
# GRÁFICO BARRAS APILADAS
###########################################

# Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrar datos para años desde 2013 hasta 2024 y excluir "INVERSIÓN EN EXISTENCIAS"

  crecimiento_long <- crecimiento %>%
  filter(AÑO >= 2013 & AÑO <= 2024) %>%
  pivot_longer(cols = c("PIB", "GASTO_PUBLICO", "CONSUMO", "INVERSION",
                        "EXPORTACIONES", "IMPORTACIONES"),
               names_to = "Variable", values_to = "Crecimiento") %>%
  mutate(Variable = factor(Variable, levels = c("PIB", "CONSUMO", "EXPORTACIONES", 
                                                "GASTO_PUBLICO", "IMPORTACIONES", "INVERSION")))

# Crear gráfico de barras agrupadas con PIB en rojo y sin etiquetas numéricas
ggplot(crecimiento_long, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Crecimiento por Variable y Año (2013-2024)", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("PIB" = "#93044e", "CONSUMO" = "#8db41c", "EXPORTACIONES" = "#926e89",
                               "GASTO_PUBLICO" = "#7c7741", "IMPORTACIONES" = "#4b2b4b", "INVERSION" = "#c88fb2"))



###########################################
#GRACIFO DE BARRAS AGRUPADAS CON TODAS LAS VARIABLES DEL PIB
###########################################
# Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrar datos para años desde 2013 hasta 2024 y excluir "INVERSIÓN EN EXISTENCIAS"
crecimiento_long <- crecimiento %>%
  filter(AÑO >= 2013 & AÑO <= 2024) %>%
  pivot_longer(cols = c("PIB", "GASTO_PUBLICO", "CONSUMO", "INVERSION",
                        "EXPORTACIONES", "IMPORTACIONES"),
               names_to = "Variable", values_to = "Crecimiento") %>%
  mutate(Variable = factor(Variable, levels = c("PIB", "CONSUMO", "EXPORTACIONES", 
                                                "GASTO_PUBLICO", "IMPORTACIONES", "INVERSION")))

# Crear gráfico de barras apiladas con una sola barra por año
ggplot(crecimiento_long, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Crecimiento por Variable y Año (2013-2024)", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("PIB" = "#93044e", "CONSUMO" = "#8db41c", "EXPORTACIONES" = "#926e89",
                               "GASTO_PUBLICO" = "#7c7741", "IMPORTACIONES" = "#4b2b4b", "INVERSION" = "#c88fb2"))


###########################################
# GRÁFICO DE BARRAS AGRUPADAS CON LA LÍNEA DE LA EVOLUCIÓN DEL PIB Y ETIQUETAS
###########################################

# Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrar datos para años desde 2013 hasta 2024 y excluir "PIB" de las variables apiladas
crecimiento_long <- crecimiento %>%
  filter(AÑO >= 2013 & AÑO <= 2024) %>%
  pivot_longer(cols = c("PIB", "GASTO_PUBLICO", "CONSUMO", "INVERSION",
                        "EXPORTACIONES", "IMPORTACIONES"),
               names_to = "Variable", values_to = "Crecimiento") %>%
  mutate(Variable = factor(Variable, levels = c("PIB", "CONSUMO", "EXPORTACIONES", 
                                                "GASTO_PUBLICO", "IMPORTACIONES", "INVERSION")))

# Crear un dataset separado solo para el PIB
crecimiento_pib <- crecimiento_long %>%
  filter(Variable == "PIB")

# Filtrar datos para las demás variables sin incluir el PIB
crecimiento_long_sin_pib <- crecimiento_long %>%
  filter(Variable != "PIB")

# Crear gráfico de barras apiladas con línea de evolución del PIB y etiquetas
ggplot() +
  geom_bar(data = crecimiento_long_sin_pib, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable), 
           stat = "identity", position = "stack") +
  geom_line(data = crecimiento_pib, aes(x = as.factor(AÑO), y = Crecimiento, group = 1, color = "PIB"), 
            size = 1) +
  geom_point(data = crecimiento_pib, aes(x = as.factor(AÑO), y = Crecimiento, color = "PIB"), 
             size = 2) +
  geom_text(data = crecimiento_pib, aes(x = as.factor(AÑO), y = Crecimiento, label = round(Crecimiento, 1)), 
            vjust = -0.5, color = "white") + # Agrega etiquetas con valores de PIB redondeados encima de cada punto
  scale_color_manual(values = c("PIB" = "#93044e")) + # Color para la línea del PIB
  labs(title = "Crecimiento por Variable y Año (2013-2024)", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("CONSUMO" = "#8db41c", "EXPORTACIONES" = "#926e89",
                               "GASTO_PUBLICO" = "#7c7741", "IMPORTACIONES" = "#4b2b4b", 
                               "INVERSION" = "#c88fb2"))

########################################33
# Filtrar datos para años desde 2013 hasta 2024 y excluir "PIB" de las variables apiladas
crecimiento_long <- crecimiento %>%
  filter(AÑO >= 2013 & AÑO <= 2024) %>%
  pivot_longer(cols = c("PIB", "GASTO_PUBLICO", "CONSUMO", "INVERSION",
                        "BALANZA_COMERCIAL", "INVERSION_EXISTENCIAS"),
               names_to = "Variable", values_to = "Crecimiento") %>%
  mutate(Variable = factor(Variable, levels = c("PIB", "CONSUMO", "GASTO_PUBLICO", 
                                                "BALANZA_COMERCIAL", "INVERSION", "INVERSION_EXISTENCIAS")))

# Crear un dataset separado solo para el PIB
crecimiento_pib <- crecimiento_long %>%
  filter(Variable == "PIB")

# Filtrar datos para las demás variables sin incluir el PIB
crecimiento_long_sin_pib <- crecimiento_long %>%
  filter(Variable != "PIB")

# Crear gráfico de barras apiladas con línea de evolución del PIB y etiquetas
ggplot() +
  geom_bar(data = crecimiento_long_sin_pib, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable), 
           stat = "identity", position = "stack") +
  geom_line(data = crecimiento_pib, aes(x = as.factor(AÑO), y = Crecimiento, group = 1, color = "PIB"), 
            size = 1) +
  geom_point(data = crecimiento_pib, aes(x = as.factor(AÑO), y = Crecimiento, color = "PIB"), 
             size = 2) +
  geom_text(data = crecimiento_pib, aes(x = as.factor(AÑO), y = Crecimiento, label = round(Crecimiento, 1)), 
            vjust = -0.5, color = "white") + # Agrega etiquetas con valores de PIB redondeados encima de cada punto
  scale_color_manual(values = c("PIB" = "#93044e")) + # Color para la línea del PIB
  labs(title = "Crecimiento por Variable y Año (2013-2024)", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("CONSUMO" = "#8db41c", "GASTO_PUBLICO" = "#7c7741", 
                               "BALANZA_COMERCIAL" = "#4b2b4b", "INVERSION" = "#c88fb2",
                               "INVERSION_EXISTENCIAS" = "#005a92")) # Colores para cada variable


