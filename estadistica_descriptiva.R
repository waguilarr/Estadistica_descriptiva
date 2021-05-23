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

#Nivel de ingresos
media_ingresos <- floor(mean(siniestros[!is.na(siniestros$Nivel_Ingresos),]$Nivel_Ingresos))
siniestros[is.na(siniestros$Nivel_Ingresos),]$Nivel_Ingresos <- media_ingresos

etiqueta <- siniestros %>% group_by(Nivel_Ingresos) %>% summarise(n = n())
siniestros <- left_join(siniestros, etiqueta, by = c("Nivel_Ingresos"))
ggplot(data = siniestros, mapping = aes(x=as.factor(Nivel_Ingresos))) +
  geom_bar(fill="darkblue") +
  geom_text(mapping = aes(x=as.factor(Nivel_Ingresos), y = 2800, label = n)) +
  scale_x_discrete("Nivel de ingresos") +
  ylab("Conteo") +
  theme_gray()

