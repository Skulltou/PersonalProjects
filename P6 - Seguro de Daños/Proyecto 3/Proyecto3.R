library(readxl)
library(dplyr)
library(actuar)
library(moments)
library(fitdistrplus)
library(goftest)
#-------------------------Limpieza de los datos-------------------------
#Datos de los accidentes ocurridos
datos <- read_excel("D:/Universidad/Clases/6to Semestre/Daños/Proyectos/Proyecto 3/Proyecto 2. Basehistorica_ 2000_a_2023.xlsx")
#Veamos como se encuentran las fechas
str(datos$`Fecha de Inicio`)
#Dado que los datos se encuentran como números los convertimos con la siguiente instrucción
datos$`Fecha de Inicio` <- as.Date(as.numeric(datos$`Fecha de Inicio`), origin = "1899-12-30")
#Como algunos de las fechas no tenian el formato requerido se volvieron Na's, por lo que las quitamos 
datos <- datos %>% filter(!is.na(`Fecha de Inicio`))
#Unicamente nos interesan el año y mes de la fecha, por lo que cambiamos, la fecha de inicio
datos$`Fecha de Inicio` <- format(datos$`Fecha de Inicio`, "%Y-%m")
#Solo usamos los meses entre enero/2000 y diciembre/2023 e hidrometeorológicos
datos <- datos %>%
  filter(`Fecha de Inicio` >= "2000-01" & `Fecha de Inicio` <= "2023-12") %>%
  filter(`Clasificación del fenómeno` == "Hidrometeorológico")
#Contamos el número de eventos según el més y tipo de fenomeno ocurrido 
d1 <- datos %>%
  group_by(`Fecha de Inicio`, `Tipo de fenómeno`) %>%
  summarise(cantidad_eventos = n(), .groups = "drop") %>%
  arrange(`Fecha de Inicio`)
#Función para clasificarlo en tres diferentes grupos
seg <- function(tipo) {
  if (tipo %in% c("Fuertes vientos", "Granizada", "Lluvias", "Tormenta eléctrica",
                  "Tormenta severa", "Tornado", "Nevada", "Bajas temperaturas", "Temperatura extrema")) {
    return("Fenómenos atmosféricos")
  } else if (tipo %in% c("Ciclón tropical", "Tormenta tropical", "Marea de tormenta")) {
    return("Fenómenos ciclónicos y tropicales")
  } else if (tipo %in% c("Inundación", "Mar de fondo", "Helada", "Sequía")) {
    return("Fenómenos hidrológicos y marinos")
  } else {
    return(NA)                 # Para los no clasificados
  }
}
#Realizamos la segmentación 
d1 <- d1 %>%
  mutate(Grupo = sapply(`Tipo de fenómeno`, seg)) %>%
  filter(!is.na(Grupo))
#Volvemos a agrupar por mes y grupo al que pertenece
d2 <- d1 %>%
  group_by(`Fecha de Inicio`, Grupo) %>%
  summarise(total_eventos = sum(cantidad_eventos), .groups = "drop")

#Tenemos que considerar que de 2000 a 2023 hay 288 meses así 288-cantidad_eventos de meses no hubo accidentes de este tipo ya que el excel no considera cuando no hay ocurrencia
#Fenómenos ciclónicos y tropicales
cic_trop <- d2 %>%
  filter(Grupo == "Fenómenos ciclónicos y tropicales") %>%
  pull(total_eventos)
cic_trop <- c(cic_trop, rep(0, 288-length(cic_trop)))
#Fenómenos hidrológicos y marinos
hid_mar <- d2 %>%
  filter(Grupo == "Fenómenos hidrológicos y marinos") %>%
  pull(total_eventos)
hid_mar <- c(hid_mar, rep(0,288-length(hid_mar)))
#Fenómenos atmosféricos 
atm <- d2 %>%
  filter(Grupo == "Fenómenos atmosféricos") %>%
  pull(total_eventos)
atm <- c(atm, rep(0,288-length(atm)))
#Ponemos el total de eventos por clasificación
d3 <- data.frame(cic_trop, hid_mar, atm)

#-------------------------Análisis descriptivo de los datos-------------------------
estadisticas_grupos <- d3 %>%
  summarise(across(everything(),list(media = mean,
                                     mediana = median,
                                     desviacion_estandar = sd,
                                     asimetria = skewness,
                                     curtosis = kurtosis))
            )
print(estadisticas_grupos)
#Histogramas de total de eventos por clasificación
hist(d3$cic_trop, main = "Distribución de Eventos Ciclónicos y Tropicales", xlab = "Total de Eventos", col = "purple")
hist(d3$hid_mar, main = "Distribución de Eventos Hidrológicos y Marinos", xlab = "Total de Eventos", col = "purple")
hist(d3$atm, main = "Distribución de Eventos Atmosféricos", xlab = "Total de Eventos", col = "purple")

#-------------------------Ajustes de curvas de probabilidad clásicas-------------------------
grupos <- unique(d2$Grupo)
frec_esp <- c() #Aquí se guardaran las frecuencias esperadas del total de eventos
for (g in grupos){
  #Recojemos los datos para la clasificacion dada
  aux <- d2 %>%
    filter(Grupo == g) %>%
    pull(total_eventos)
  #Tenemos que considerar que de 2000 a 2023 hay 288 meses así 288-length(aux) meses no hubo accidentes de este tipo
  aux <- c(aux, rep(0, 288-length(aux)))
  #Realizamos el ajuste para Binomial, Poisson, Geométrica y Binomial Negativa
  #Binomial
  fit_binom <- fitdist(aux, "binom", fix.arg = list(size = 288), method = "mle")
  #Poisson
  fit_pois <- fitdist(aux, "pois", method = "mle")
  #Geométrica
  fit_geom <- fitdist(aux, "geom", method = "mle")
  #Binomial Negativa
  fit_nbinom <- fitdist(aux, "nbinom", method = "mle")
  #Pruebas de bondad de ajuste
  modelos <- list(fit_binom, fit_pois, fit_geom, fit_nbinom)
  nombres <- c("Binomial", "Poisson", "Geométrica", "Binomial Negativa")
  gof <- gofstat(modelos, fitnames = nombres)
  cat("\n\n")
  print("------ Bondad de ajuste ------")
  print(g)
  print(gof)
  par(mfrow = c(2,2))
  denscomp(fit_binom)
  denscomp(fit_pois)
  denscomp(fit_geom)
  denscomp(fit_nbinom)
  #En todos los casos la mejor estimación es de una binomial negativa
  frec_esp <- c(frec_esp, fit_nbinom$estimate["mu"])
}
#Frecuencias Esperadas Mensuales
frec_esp
#Frecuencias Esperadas Anuales
frec_anual_cic <- 12*frec_esp[1]
frec_anual_hid <- 12*frec_esp[2]
frec_anual_atm <- 12*frec_esp[3]


#-----------------------Ajustes de curvas de probabilidad zero modificadas---------------------
for (g in grupos){
  #Recojemos los datos para la clasificacion dada
  aux <- d2 %>%
    filter(Grupo == g) %>%
    pull(total_eventos)
  aux <- c(aux, rep(0, 288-length(aux)))
  p0 <- sum(aux==0)/288
  #Realizamos el ajuste para Binomial, Poisson, Geométrica y Binomial Negativa
  #Binomial
  fit_zmbinom <- fitdist(aux, "zmbinom", fix.arg = list(p0=p0, size = 288), method = "mle", discrete = T)
  #Poisson
  fit_zmpois <- fitdist(aux, "zmpois", fix.arg = list(p0=p0), method = "mle", discrete = T)
  #Geométrica
  fit_zmgeom <- fitdist(aux, "zmgeom", fix.arg = list(p0=p0), method = "mle", discrete = T)
  #Binomial Negativa
  fit_zmnbinom <- fitdist(aux, "zmnbinom", fix.arg = list(p0=p0), method = "mle", discrete = T)
  #Pruebas de bondad de ajuste
  modelos <- list(fit_zmbinom, fit_zmpois, fit_zmgeom, fit_zmnbinom)
  nombres <- c("ZMBinomial", "ZMPoisson", "ZMGeométrica", "ZMBinomial Negativa")
  gof <- gofstat(modelos, fitnames = nombres)
  print("------ Bondad de ajuste ------")
  print(g)
  print(gof)
  par(mfrow = c(2,2))
  denscomp(fit_zmbinom)
  denscomp(fit_zmpois)
  denscomp(fit_zmgeom)
  denscomp(fit_zmnbinom)
}




#------------------------- Regresión Lineal Poisson -------------------------

#Reunimos todos los eventos en un solo vector
acc <- c(cic_trop, hid_mar, atm)
#Creamos las variables dummies
ciclon <- c(rep(1, 288), rep(0, 288), rep(0, 288))
hidro <- c(rep(0, 288), rep(1, 288), rep(0, 288))
atmos <- c(rep(0, 288), rep(0, 288), rep(1, 288))
#Creamos el data frame de los datos
df <- data.frame(acc, ciclon, hidro, atmos)
#Modelo lineal generalizado
mod_final <- glm(acc ~ -1 + atmos + ciclon + hidro,
                 data = df,
                 family = poisson(link = "log"))
#Resumen del modelo
summary(mod_final)

#Extraemos los coeficientes

b_ciclon <- coef(mod_final)["ciclon"]
b_hidro <- coef(mod_final)["hidro"]
b_atmos <- coef(mod_final)["atmos"]
#Esperanza de cada grupo
mu1 <- exp(b_ciclon)
mu2 <- exp(b_hidro)
mu3 <- exp(b_atmos)
#Estimación anual
mu_anual_ciclon <- 12*mu1
mu_anual_hidro <- 12*mu2
mu_anual_atmos <- 12*mu3
