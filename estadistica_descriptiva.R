library(dplyr)
library(ggplot2)

siniestros <- read.csv(
  "C:/Users/William/Downloads/Data_Siniestros en Seguros.csv")

#Antiguedad máxima
media_antiguedad <- mean(siniestros[!is.na(siniestros$Antigüedad_Maxima),]$Antigüedad_Maxima)
siniestros[is.na(siniestros$Antigüedad_Maxima),]$Antigüedad_Maxima <- media_antiguedad

ggplot(data = siniestros, mapping = aes(y=Antigüedad_Maxima)) +
  geom_boxplot(fill = "#F34213", color="#2E2E3A") + 
  scale_x_discrete("") +
  ylab("Antiguedad máxima") +
  theme_gray()
