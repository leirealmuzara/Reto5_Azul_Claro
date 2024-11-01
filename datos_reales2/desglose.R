getwd()
setwd("C:/Users/pieza/OneDrive/Escritorio/Bdata2/RETOS/Reto5/Reto5_Azul_Claro/datos_reales2")
library(readxl)
library(dplyr)
dir()

df <- read_xlsx("desglose.xlsx", sheet = 3)

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

# GRÁFICO PIB 

df_alemania$`Gross domestic product at market prices` <- as.numeric(df_alemania$`Gross domestic product at market prices`)
pib <- plot(df_alemania$AÑO, df_alemania$`Gross domestic product at market prices`, 
            type = "l", main = "Evolución del PIB", xlab = "AÑO", ylab = "PIB", 
            col = "blue", lwd = 2)
