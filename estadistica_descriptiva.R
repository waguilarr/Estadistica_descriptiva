library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)

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

#Saldo pendiente
ggplot(data = siniestros, mapping = aes(x=Saldo_Pendiente)) +
  geom_density(fill="aquamarine") +
  ylab("Distribución saldo pendiente") +
  theme_gray()

#Crear columna puntaje morosidad
x <- siniestros %>% filter(Puntaje_Morosidad1 > 1)
View(x)
siniestros <- siniestros %>% replace_na(
  list(
    Puntaje_Morosidad1 = 0,
    Puntaje_Morosidad2 = 0,
    Puntaje_Morosidad3 = 0,
    Puntaje_Morosidad4 = 0,
    Puntaje_Morosidad5 = 0,
    Puntaje_Morosidad6 = 0))
summary(siniestros)

ggplot(
  data = siniestros, 
  mapping = aes(x = Puntaje_Morosidad1)) +
  geom_bar()

puntaje_morosidad = data.frame(
  Puntaje = c(
    siniestros$Puntaje_Morosidad1, 
    siniestros$Puntaje_Morosidad2,
    siniestros$Puntaje_Morosidad3,
    siniestros$Puntaje_Morosidad4, 
    siniestros$Puntaje_Morosidad5, 
    siniestros$Puntaje_Morosidad6),
  Categoria = as.factor(c(
    rep("1", times = length(siniestros$Puntaje_Morosidad1)),
    rep("2", times = length(siniestros$Puntaje_Morosidad2)),
    rep("3", times = length(siniestros$Puntaje_Morosidad3)),
    rep("4", times = length(siniestros$Puntaje_Morosidad4)),
    rep("5", times = length(siniestros$Puntaje_Morosidad5)),
    rep("6", times = length(siniestros$Puntaje_Morosidad6))
  )))

x <- puntaje_morosidad %>% group_by(Categoria, Puntaje) %>% summarise(cant = n())

ggplot(data = puntaje_morosidad, mapping = aes(x=Puntaje, fill = Categoria)) +
  geom_bar() + 
  facet_wrap(~Categoria) +
  labs(title = "Cantidades en las Categorías de puntaje por morosidad")

resultado <- puntaje_morosidad %>%
  group_by(Categoria) %>% 
  summarise(suma = sum(Puntaje))
ggplot(data = resultado, mapping = aes(x=Categoria, y=suma)) +
  geom_col(fill="darkgreen") +
  labs(title = "Suma de los puntajes por categoría")

siniestros <- siniestros %>% mutate(
  Deudores = if_else(Saldo_Pendiente_Seg>0, "Si", "No")
)

ggplot(data = siniestros, mapping = aes(x=Deudores, fill=Deudores)) +
  geom_bar()

ggplot(data = siniestros, mapping = aes(y=Saldo_Pendiente_Seg, fill=Deudores)) +
  geom_boxplot() + 
  scale_x_discrete("Deudores") +
  ylab("Saldo Pendiente Seg.") +
  theme_gray()


categoria_siniestro = data.frame(
  Puntaje = c(
    siniestros$Siniestros1, 
    siniestros$Siniestros2,
    siniestros$Siniestros3,
    siniestros$Siniestros4, 
    siniestros$Siniestros5, 
    siniestros$Siniestros6),
  Categoria = as.factor(c(
    rep("1", times = length(siniestros$Siniestros1)),
    rep("2", times = length(siniestros$Siniestros2)),
    rep("3", times = length(siniestros$Siniestros3)),
    rep("4", times = length(siniestros$Siniestros4)),
    rep("5", times = length(siniestros$Siniestros5)),
    rep("6", times = length(siniestros$Siniestros6))
  )))

ggplot(data = categoria_siniestro, mapping = aes(x=Puntaje, fill = Categoria)) +
  geom_histogram(bins = 10) + 
  facet_wrap(~Categoria) +
  labs(title = "Cantidades en las Categorías de siniestro")

resultado <- categoria_siniestro %>%
  group_by(Categoria) %>% 
  summarise(suma = sum(Puntaje))
ggplot(data = resultado, mapping = aes(x=Categoria, y=suma)) +
  geom_col(fill="darkgreen") +
  labs(title = "Suma de los puntajes por categoría")

# Grafico Bivariado
ggplot(
  data = siniestros %>% filter(Saldo_Pendiente_Seg>0), 
  mapping = aes(
    x=Saldo_Pendiente, 
    y=Saldo_Pendiente_Seg, 
    color=as.factor(Nivel_Ingresos))) +
  geom_point(size = 2)

# Modelo
# pre-procesamiento
siniestros <- siniestros %>% 
  select(-c(Cliente_ID, n, Deudores)) %>% 
  mutate(
    Nivel_Ingresos = as.factor(Nivel_Ingresos),
    Estado_Siniestro = as.factor(Estado_Siniestro)
  )

modelo1 <- lm(formula = Saldo_Pendiente ~ ., data = siniestros)
summary(modelo1)

modback <- stepAIC(modelo1, trace=TRUE, direction="backward")
summary(modback)

modboth <- stepAIC(modelo1, trace=FALSE, direction="both")
summary(modboth)

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(modboth)

