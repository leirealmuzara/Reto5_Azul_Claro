setwd("C:/RETO/reto 05/Reto5_Azul_Claro")
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
colnames(desglose) <- c("AÑO", "PAÍS", "PIB", "GASTO PÚBLICO", "CONSUMO",
                        "INVERSIÓN", "INVERSIÓN EN EXISTENCIAS",
                        "EXPORTACIONES DE BIENES Y SERVICIOS",
                        "IMPORTACIONES DE BIENES Y SERVICIOS", "BALANZA COMERCIAL")

# PASAR A CRECIMIENTO

desglose$PIB <- as.numeric(desglose$PIB)
desglose$`GASTO PÚBLICO` <- as.numeric(desglose$`GASTO PÚBLICO`)
desglose$CONSUMO <- as.numeric(desglose$CONSUMO)
desglose$INVERSIÓN <- as.numeric(desglose$INVERSIÓN)
desglose$`INVERSIÓN EN EXISTENCIAS` <- as.numeric(desglose$`INVERSIÓN EN EXISTENCIAS`)
desglose$`EXPORTACIONES DE BIENES Y SERVICIOS` <- as.numeric(desglose$`EXPORTACIONES DE BIENES Y SERVICIOS`)
desglose$`IMPORTACIONES DE BIENES Y SERVICIOS` <- as.numeric(desglose$`IMPORTACIONES DE BIENES Y SERVICIOS`)
desglose$`BALANZA COMERCIAL` <- as.numeric(desglose$`BALANZA COMERCIAL`)

crecimiento <- desglose

crecimiento$PIB <- (crecimiento$PIB / lag(crecimiento$PIB, 1) - 1) * 100
crecimiento$`GASTO PÚBLICO` = (crecimiento$`GASTO PÚBLICO` / lag(crecimiento$`GASTO PÚBLICO`, 1) - 1) * 100
crecimiento$CONSUMO = (crecimiento$CONSUMO / lag(crecimiento$CONSUMO, 1) - 1) * 100
crecimiento$INVERSIÓN = (crecimiento$INVERSIÓN / lag(crecimiento$INVERSIÓN, 1) - 1) * 100
crecimiento$`INVERSIÓN EN EXISTENCIAS` = (crecimiento$`INVERSIÓN EN EXISTENCIAS` / lag(crecimiento$`INVERSIÓN EN EXISTENCIAS`, 1) - 1) * 100
crecimiento$`EXPORTACIONES DE BIENES Y SERVICIOS` = (crecimiento$`EXPORTACIONES DE BIENES Y SERVICIOS` / lag(crecimiento$`EXPORTACIONES DE BIENES Y SERVICIOS`, 1) - 1) * 100
crecimiento$`IMPORTACIONES DE BIENES Y SERVICIOS` = (crecimiento$`IMPORTACIONES DE BIENES Y SERVICIOS`  / lag(crecimiento$`IMPORTACIONES DE BIENES Y SERVICIOS` , 1) - 1) * 100
crecimiento$`BALANZA COMERCIAL` = (crecimiento$`BALANZA COMERCIAL` / lag(crecimiento$`BALANZA COMERCIAL`, 1) - 1) * 100




# Librerías necesarias
library(ggplot2)
library(tidyr)

# Convertir datos a formato largo para ggplot
crecimiento_long <- crecimiento %>%
  pivot_longer(cols = c("PIB", "GASTO PÚBLICO", "CONSUMO", "INVERSIÓN", 
                        "INVERSIÓN EN EXISTENCIAS", "EXPORTACIONES DE BIENES Y SERVICIOS", 
                        "IMPORTACIONES DE BIENES Y SERVICIOS", "BALANZA COMERCIAL"),
               names_to = "Variable", values_to = "Crecimiento")

# Crear gráfico de barras agrupadas
ggplot(crecimiento_long, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Crecimiento por Variable y Año", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")  # Utiliza una paleta de colores para distinguir variables




# Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrar datos para años desde 2013 hasta 2024
crecimiento_long <- crecimiento %>%
  filter(AÑO >= 2013 & AÑO <= 2024) %>%
  pivot_longer(cols = c("PIB", "GASTO PÚBLICO", "CONSUMO", "INVERSIÓN", 
                        "INVERSIÓN EN EXISTENCIAS", "EXPORTACIONES DE BIENES Y SERVICIOS", 
                        "IMPORTACIONES DE BIENES Y SERVICIOS", "BALANZA COMERCIAL"),
               names_to = "Variable", values_to = "Crecimiento")

# Crear gráfico de barras agrupadas
ggplot(crecimiento_long, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Crecimiento por Variable y Año (2013-2024)", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")  # Utiliza una paleta de colores para distinguir variables

#############################
# Librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Filtrar datos para años desde 2013 hasta 2024 y excluir "INVERSIÓN EN EXISTENCIAS"
crecimiento_long <- crecimiento %>%
  filter(AÑO >= 2013 & AÑO <= 2024) %>%
  pivot_longer(cols = c("PIB", "GASTO PÚBLICO", "CONSUMO", "INVERSIÓN", 
                        "EXPORTACIONES DE BIENES Y SERVICIOS", 
                        "IMPORTACIONES DE BIENES Y SERVICIOS", "BALANZA COMERCIAL"),
               names_to = "Variable", values_to = "Crecimiento")

# Crear gráfico de barras agrupadas
ggplot(crecimiento_long, aes(x = as.factor(AÑO), y = Crecimiento, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Crecimiento por Variable y Año (2013-2024)", x = "Año", y = "Crecimiento (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")  # Utiliza una paleta de colores para distinguir variables
