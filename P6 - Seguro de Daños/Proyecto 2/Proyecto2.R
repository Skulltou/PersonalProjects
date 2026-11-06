#------------Cargamos paquetes necesarios
library(dplyr)
library(readxl)
library(ggplot2)
library(farver)
library(actuar)
library(moments)
library(fitdistrplus)
library(goftest)
library(rriskDistributions)
#-------------------------Leemos el archivo Excel
datos <- read_excel("D:/Universidad/Clases/6to Semestre/Daños/Proyectos/Proyecto 2/Proyecto 2. Basehistorica_ 2000_a_2023.xlsx")
#---------------A continuación filtramos solo los siniestros hidrometeorológicos
hidrometeorologicos <- datos[datos$`Clasificación del fenómeno` == "Hidrometeorológico", 
                             c("Año", "Estado", "Total de daños (millones de pesos)")]
colnames(hidrometeorologicos) <- c("Año", "Estado","Pago")

#------------------------------------Analizamos los datos
hidrometeorologicos
str(hidrometeorologicos$Pago)
summary(hidrometeorologicos$Pago)

#------------Hay 9 celdas vacías por lo cual hay que limpiar los datos y eliminarlos
#----------Tambien aquellos que sean 0
hidrometeorologicos_limpio <- hidrometeorologicos %>%
  filter(is.finite(Pago))
hidrometeorologicos_limpios <- hidrometeorologicos_limpio %>%
  filter(!is.na(Pago), Pago > 0)

head(hidrometeorologicos_limpios)
summary(hidrometeorologicos_limpios$Pago)
var(hidrometeorologicos_limpios$Pago)
kurtosis(hidrometeorologicos_limpios$Pago)


#----------------------Creamos un histograma de los datos y su función de distribución empirica 
ggplot(hidrometeorologicos_limpios, aes(x = Pago)) +
  geom_histogram(bins = 50, fill = "brown", color = "black", alpha = 0.7) +
  labs(title = "Distribución de los pagos",
       x = "Pago (millones de pesos)",
       y = "Frecuencia") + theme_minimal()

plot(ecdf(hidrometeorologicos_limpios$Pago),col = "purple") #no es bueno que sea tan curvo ni recto (45 grados)
Fx <- ecdf(hidrometeorologicos_limpios$Pago)

mod0 <- fitdist(hidrometeorologicos_limpios$Pago, "gamma", method = "mme") #mle es maximo estim. verosimil
denscomp(mod0) #Funcion de densidad


#------------------Dividimos los eventos hidrometeorologicos entre aquellos que courrieron es estados 
#costeros y aquellos que no.
estados_costeros <- c("Baja California", "Baja California Sur", "Sonora", "Sinaloa", 
                      "Nayarit", "Jalisco", "Colima", "Michoacán", "Guerrero", "Oaxaca", 
                      "Chiapas", "Tamaulipas", "Veracruz", "Tabasco", "Campeche", 
                      "Yucatán", "Quintana Roo")

hidrometeorologicos1 <- hidrometeorologicos_limpios %>%
  filter(Estado %in% estados_costeros)
hidrometeorologicos2 <- hidrometeorologicos_limpios %>%
  filter(!Estado %in% estados_costeros)

#-------------------------------Observamos los datos de cada uno
head(hidrometeorologicos1)#Estados costeross
head(hidrometeorologicos2)#Estados sin costa
summary(hidrometeorologicos1$Pago)
summary(hidrometeorologicos2$Pago)
min(hidrometeorologicos1$Pago)

#----------------------------LLevamos a valor futuro los datos con la tasa de inflación
inflacion <- data.frame(
  Año = 2000:2023,
  Tasa = c(0.0896, 0.044, 0.057, 0.0398, 0.0519, 0.0333, 0.0405, 0.0376, 0.0653, 0.0357, 0.044, 
   0.0382, 0.0357, 0.0397, 0.0408, 0.0213, 0.0336, 0.0677, 0.0483, 0.0283, 0.0315, 0.0736, 0.0782, 0.0466) 
  )

#--------------------------------Creamos una función para ajustar la perdida
actualizar_pago <- function(pago, año) {
  tasas <- inflacion %>%
    filter(Año >= año & Año <= 2023) %>%
    pull(Tasa)                 # Extrae solo las tasas de inflación
  vp <- pago * prod(1 + tasas)# Multiplica el pago por (1 + r) acumulado
  return(vp)
}

#---------------------------------Aplicamos la función a hidrometeorologicos1 y 2 
hidrome1 <- hidrometeorologicos1 %>%
  rowwise() %>%
  mutate(Pago_2023 = actualizar_pago(Pago, Año)) %>%
  ungroup()

hidrome2 <- hidrometeorologicos2 %>%
  rowwise() %>%
  mutate(Pago_2023 = actualizar_pago(Pago, Año)) %>%
  ungroup()
# -------------------------------Observamos resultados
head(hidrome1)
head(hidrome2)


#-----------------------------Graficamos histograma y f. dist. empirica de ambos 
ggplot(hidrome1, aes(x = Pago_2023)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de pagos en estados costeros",
       x = "Pago (millones de pesos)",
       y = "Frecuencia") +theme_minimal()
plot(ecdf(hidrome1$Pago_2023),col = "purple")
Fx <- ecdf(hidrome2$Pago_2023)

ggplot(hidrome2, aes(x = Pago_2023)) +
  geom_histogram(bins = 30, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Pagos en estados no costeros",
       x = "Pago (millones de pesos)",
       y = "Frecuencia") + theme_minimal()

plot(ecdf(hidrome2$Pago_2023),col = "red") 
Fx <- ecdf(hidrome2$Pago_2023)

#Aplicamos logaritmo para ajustar las perdidas
logS <- log(hidrome1$Pago_2023) 
logS2 <- log(hidrome2$Pago_2023) 
summary(logS)
summary(logS2)
hist(logS, breaks = 30)
hist(logS2, breaks = 30)

#PRIMER PROPUESTA
#Consideramos únicamente las pérdidas mayores a 0 para tener mayor variedad de distribuciones
p1 <- logS[logS>0]
p2 <- logS2[logS2>0]
#Vemos la distribución de los datos
hist(p1, breaks = 30)
hist(p2, breaks = 30)

#modelo de datos costeros
mod1 <- fitdist(p1, "trgamma", method = "mle")
denscomp(mod1)
cdfcomp(mod1)
qqcomp(mod1)
ppcomp(mod1)

#Modelo de datos NO costeros
mod2 <- fitdist(p2, "trgamma", method = "mle")
denscomp(mod2)
cdfcomp(mod2)
qqcomp(mod2)
ppcomp(mod2)

#Pruebas de hipótesis
#Costeros
ad.test(p1, "ptrgamma", shape1=mod1$estimate["shape1"], shape2=mod1$estimate["shape2"], scale=mod1$estimate["scale"])
ks.test(p1, "ptrgamma", shape1=mod1$estimate["shape1"], shape2=mod1$estimate["shape2"], scale=mod1$estimate["scale"])
cvm.test(p1, "ptrgamma", shape1=mod1$estimate["shape1"], shape2=mod1$estimate["shape2"], scale=mod1$estimate["scale"])
#No Costeros
ad.test(p2, "ptrgamma", shape1=mod2$estimate["shape1"], shape2=mod2$estimate["shape2"], scale=mod2$estimate["scale"])
ks.test(p2, "ptrgamma", shape1=mod2$estimate["shape1"], shape2=mod2$estimate["shape2"], scale=mod2$estimate["scale"])
cvm.test(p2, "ptrgamma", shape1=mod2$estimate["shape1"], shape2=mod2$estimate["shape2"], scale=mod2$estimate["scale"])

#SEGUNDA PROPUESTA
#Considerando que eliminamos determinados datos, pudimos haber perdido información, por ello apliquemos ahora valor absoluto
p3 <- abs(logS)
p4 <- abs(logS2)
#Vemos la distribución de los datos
hist(p3, breaks = 30)
hist(p4, breaks = 30)

#modelo de datos costeros
mod3 <- fitdist(p3, "trgamma", method = "mle")
denscomp(mod3)
cdfcomp(mod3)
qqcomp(mod3)
ppcomp(mod3)

#Modelo de datos NO costeros
mod4 <- fitdist(p4, "trgamma", method = "mle")
denscomp(mod4)
cdfcomp(mod4)
qqcomp(mod4)
ppcomp(mod4)

#Pruebas de hipótesis
#Costeros
ad.test(p3, "ptrgamma", shape1=mod3$estimate["shape1"], shape2=mod3$estimate["shape2"], scale=mod3$estimate["scale"])
ks.test(p3, "ptrgamma", shape1=mod3$estimate["shape1"], shape2=mod3$estimate["shape2"], scale=mod3$estimate["scale"])
cvm.test(p3, "ptrgamma", shape1=mod3$estimate["shape1"], shape2=mod3$estimate["shape2"], scale=mod3$estimate["scale"])
#No Costeros
ad.test(p4, "ptrgamma", shape1=mod4$estimate["shape1"], shape2=mod4$estimate["shape2"], scale=mod4$estimate["scale"])
ks.test(p4, "ptrgamma", shape1=mod4$estimate["shape1"], shape2=mod4$estimate["shape2"], scale=mod4$estimate["scale"])
cvm.test(p4, "ptrgamma", shape1=mod4$estimate["shape1"], shape2=mod4$estimate["shape2"], scale=mod4$estimate["scale"])

#Así considerando que el valor absoluto no es invertible y complica las cuentas nos quedaremos con el primer modelo
mucosteros <- exp(mtrgamma(1, mod1$estimate["shape1"], mod1$estimate["shape2"], mod1$estimate["scale"]))
munocosteros <- exp(mtrgamma(1, mod2$estimate["shape1"], mod2$estimate["shape2"], mod2$estimate["scale"]))
probacosteros <- 1653/2704
probanocosteros <- 1051/2704

costo_promedio <- mucosteros*probacosteros+munocosteros*probanocosteros
#Dado que la escala estaba en millones de pesos, entonces:
costo_promedio_mdp <- costo_promedio*1000000
