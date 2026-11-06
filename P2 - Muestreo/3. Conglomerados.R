"
Autor: Emiliano Romero Medina
Este un proyecto personal, que intenta cubrir y comparar los diverso marcos de
muestreo estadístico. Este tercer código es un ejemplo de Muestreo por Conglomerados.

La base de datos que ocuparemos en todos los marcos de muestreo será
Sample - Superstore.csv, que contiene información de 5009 ventas. Y las sigui-
entes variables

Row ID => Identificador único de cada fila.
Order ID => Identificador único de cada pedido.
Order Date => Fecha en que se realizó el pedido.
Ship Date => Fecha de envío del producto.
Ship Mode => Modalidad de envío especificada por el cliente.
Customer ID => Identificador único de cada cliente.
Customer Name => Nombre del cliente.
Segment => Segmento al que pertenece el cliente.
Country => País de residencia del cliente.
City => Ciudad de residencia del cliente.
State => Estado de residencia del cliente.
Postal Code => Código postal de cada cliente.
Region => Región a la que pertenece el cliente.
Product ID => Identificador único del producto.
Category => Categoría del producto solicitado.
Sub-Category => Subcategoría del producto solicitado.
Product Name => Nombre del producto.
Sales => Ventas generadas por el producto.
Quantity => Cantidad de unidades del producto.
Discount => Descuento aplicado.
Profit => Ganancia o pérdida obtenida.
"
"
Trataremos de obtener buenas estimaciones de la variable Sales.
Esto pues sería algo relevante para la empresa, saber cuánto va a vender.
Esto en términos de conglomerados a través de la variable Sub.Category
"
#----Librerás----
library(ggplot2)
library(dplyr)
#----Función Gráficar----
comparar_densidades <- function(poblacion, muestra = NULL,
                                titulo = "Distribución de la variable",
                                subtitulo = "Comparación entre población y muestra piloto",
                                etiqueta_x = "Valor",
                                bins = NULL) {
  if (is.null(muestra)) {
    df_poblacion <- data.frame(valor = poblacion)

    ggplot(df_poblacion, aes(x = valor)) +
      # Barras del histograma
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "#2878B5",
        color = "white",
        alpha = 0.9,
        linewidth = 0.4
      ) +
      # Curva de densidad
      geom_density(
        color = "#E67E22",
        linewidth = 1.3,
        fill = "#E67E22",
        alpha = 0.12
      ) +
      # Títulos y etiquetas
      labs(
        title = titulo,
        subtitle = subtitulo,
        x = etiqueta_x,
        y = "Densidad",
        caption = "Fuente: Elaboración propia"
      ) +
      # Tema visual
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 18,
          color = "#263238"
        ),
        plot.subtitle = element_text(
          size = 11,
          color = "#607D8B"
        ),
        axis.title = element_text(
          face = "bold",
          color = "#37474F"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          color = "#EAECEE",
          linewidth = 0.4
        ),
        plot.background = element_rect(
          fill = "#FFFFFF",
          color = NA
        ),
        plot.caption = element_text(
          color = "#7F8C8D",
          size = 9
        )
      )
  } else {
    # Preparar datos
    df_poblacion <- data.frame(valor = poblacion)
    df_muestra <- data.frame(valor = muestra)

    # Gráfico
    ggplot() +

      # Densidad de la población
      geom_density(
        data = df_poblacion,
        aes(x = valor, color = "Población", fill = "Población"),
        linewidth = 1.3,
        alpha = 0.12,
        na.rm = TRUE
      ) +

      # Densidad de la muestra piloto
      geom_density(
        data = df_muestra,
        aes(x = valor, color = "Muestra", fill = "Muestra"),
        linewidth = 1.3,
        alpha = 0.20,
        na.rm = TRUE
      ) +

      # Colores y leyenda
      scale_color_manual(
        name = "Distribución",
        values = c(
          "Población" = "#2878B5",
          "Muestra" = "#E67E22"
        )
      ) +
      scale_fill_manual(
        name = "Distribución",
        values = c(
          "Población" = "#2878B5",
          "Muestra" = "#E67E22"
        )
      ) +

      # Títulos y etiquetas
      labs(
        title = titulo,
        subtitle = subtitulo,
        x = etiqueta_x,
        y = "Densidad",
        caption = "Fuente: Elaboración propia"
      ) +

      # Tema
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 18,
          color = "#263238"
        ),
        plot.subtitle = element_text(
          size = 11,
          color = "#607D8B"
        ),
        axis.title = element_text(
          face = "bold",
          color = "#37474F"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          color = "#EAECEE",
          linewidth = 0.4
        ),
        plot.background = element_rect(
          fill = "#FFFFFF",
          color = NA
        ),
        plot.caption = element_text(
          color = "#7F8C8D",
          size = 9
        )
      )
  }
}
#----Población----
# Cargamos los datos
datos <- read.csv(
  file = "C:/Users/emili/Desktop/Proyectos/P2 - Muestreo/Sample - Superstore.csv",
  fileEncoding = "latin1"
)


N <- length(datos$Sales)

# Total Poblacional
total_or <- sum(datos$Sales)
total_or

# Media Poblacional
media_or <- mean(datos$Sales)
media_or

# Varianza Poblacional
ee_or <- sd(datos$Sales) * (N - 1) / N
ee_or

# Distribución ventas
comparar_densidades(datos$Sales,
  titulo = "Distribución de las ventas",
  subtitulo = "Histograma con curva de densidad",
  etiqueta_x = "Ventas"
)

# Distribución logaritmo de las ventas
comparar_densidades(
  poblacion = log(datos$Sales),
  titulo = "Distribución logarítmica de las ventas",
  subtitulo = "Histograma con curva de densidad",
  etiqueta_x = "Logaritmo de las Ventas"
)
#----Muestra Piloto----
"
Para fijar el tamaño de muestra, procedemos a obtener una muestra piloto
de 20 productos considerando los conglomerados en Sub.Category
"
n_piloto <- 32
conglomerados <- unique(datos$Sub.Category)
M <- length(conglomerados)
m <- 4
set.seed(124)
# Seleccionamos 4 conglomerados
conglomerados_seleccionados <- sample(conglomerados, size = m, replace = FALSE)

# Filtramos la base de datos por los conglomerados escogidos
datos2 <- datos[datos$Sub.Category %in% conglomerados_seleccionados, ]

# Tamaños de conglomerados en muestra
N1 <- length(datos2[datos2$Sub.Category == conglomerados_seleccionados[1], ]$Sales)
N2 <- length(datos2[datos2$Sub.Category == conglomerados_seleccionados[2], ]$Sales)
N3 <- length(datos2[datos2$Sub.Category == conglomerados_seleccionados[3], ]$Sales)
N4 <- length(datos2[datos2$Sub.Category == conglomerados_seleccionados[4], ]$Sales)
N_vec_piloto <- c(N1, N2, N3, N4)
# Muestreamos 8 datos dentro de los 4 conglomerados
n_piloto_vec <- rep(8, m)
set.seed(234)
muestra_piloto_cong_1 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[1], ] %>%
  slice_sample(n = 8)
muestra_piloto_cong_2 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[2], ] %>%
  slice_sample(n = 8)
muestra_piloto_cong_3 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[3], ] %>%
  slice_sample(n = 8)
muestra_piloto_cong_4 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[4], ] %>%
  slice_sample(n = 8)
muestra_piloto <- rbind(
  muestra_piloto_cong_1, muestra_piloto_cong_2, muestra_piloto_cong_3,
  muestra_piloto_cong_4
)

# Comparamos población vs Muestra sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = muestra_piloto$Sales,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparamos población vs Muestra con logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(muestra_piloto$Sales),
  titulo = "Distribución de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

# Calculos para la varianza de conglomerados
var_cong_1 <- var(muestra_piloto_cong_1$Sales)
var_cong_2 <- var(muestra_piloto_cong_2$Sales)
var_cong_3 <- var(muestra_piloto_cong_3$Sales)
var_cong_4 <- var(muestra_piloto_cong_4$Sales)
var_vec_piloto <- c(var_cong_1, var_cong_2, var_cong_3, var_cong_4)

# Medias por conglomerados
media_cong_1 <- mean(muestra_piloto_cong_1$Sales)
media_cong_2 <- mean(muestra_piloto_cong_2$Sales)
media_cong_3 <- mean(muestra_piloto_cong_3$Sales)
media_cong_4 <- mean(muestra_piloto_cong_4$Sales)
media_vec_piloto <- c(media_cong_1, media_cong_2, media_cong_3, media_cong_4)
totales_vec_piloto <- N_vec_piloto * media_vec_piloto

# Varianza entre conglomerados muestra piloto
vec_piloto <- M^2 * (1 - m / M) * var(totales_vec_piloto) / m

# Varianza dentro de conglomerados muestra piloto
vdc_piloto <- (M / m) * sum(N_vec_piloto^2 * (1 - n_piloto_vec / N_vec_piloto) * var_vec_piloto / n_piloto_vec)
"
Para el cálculo de un tamaño de muestra haremos uso de un MAS (su tamaño de muestra)
y posteriormente multiplicarlos por el efecto de diseño Deff
"
# Varianza total conglomerados muestra piloto
var_total_piloto <- vec_piloto + vdc_piloto
vec_piloto / var_total_piloto # Entre clases aporta 0.8416313 a la varianza es bueno
vdc_piloto / var_total_piloto # Dentro de las clases aporta 0.1583687 a la varianza es bueno

# Error estándar piloto de MAS
ee_MAS_piloto <- sd(muestra_piloto$Sales)

# Varianza estimador
var_MAS_piloto <- N^2 * (1 - n_piloto / N) * ee_MAS_piloto^2 / n_piloto
deff_piloto <- var_total_piloto / var_MAS_piloto

# Nivel de confianza (1-alpha) o probabilidad de fallar
alpha <- 0.05
Z <- qnorm(1 - alpha / 2)

# Precisión dada en magnitud de la variable de interés +-
d <- 30
n0 <- ((Z * ee_MAS_piloto) / d)^2
n0 # Tamaño de muestra si hubiera población infinita

# Tamaño final
n <- n0 / (1 + n0 / N)
n <- round(n, digits = 0)
n
n_final_conglomerado <- round(deff_piloto * n)
n_final_conglomerado

#----Muestra Definitiva----
n_h <- rep(round(n_final_conglomerado / m), m)
n_final_conglomerado <- sum(n_h)

# Muestreamos con tamaño 53
set.seed(834)
muestra_cong_1 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[1], ] %>%
  slice_sample(n = 53)
muestra_cong_2 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[2], ] %>%
  slice_sample(n = 53)
muestra_cong_3 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[3], ] %>%
  slice_sample(n = 53)
muestra_cong_4 <- datos2[datos2$Sub.Category == conglomerados_seleccionados[4], ] %>%
  slice_sample(n = 53)
muestra_definitiva <- rbind(
  muestra_cong_1, muestra_cong_2, muestra_cong_3,
  muestra_cong_4
)

# Comparamos población vs Muestra sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = muestra_definitiva$Sales,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparamos población vs Muestra con logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(muestra_definitiva$Sales),
  titulo = "Distribución de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

# Calculos para la varianza de conglomerados
var_cong_1_def <- var(muestra_cong_1$Sales)
var_cong_2_def <- var(muestra_cong_2$Sales)
var_cong_3_def <- var(muestra_cong_3$Sales)
var_cong_4_def <- var(muestra_cong_4$Sales)
var_vec_def <- c(var_cong_1_def, var_cong_2_def, var_cong_3_def, var_cong_4_def)

# Medias por conglomerados
media_cong_1_def <- mean(muestra_cong_1$Sales)
media_cong_2_def <- mean(muestra_cong_2$Sales)
media_cong_3_def <- mean(muestra_cong_3$Sales)
media_cong_4_def <- mean(muestra_cong_4$Sales)
media_vec_def <- c(media_cong_1_def, media_cong_2_def, media_cong_3_def, media_cong_4_def)
totales_vec_def <- N_vec_piloto * media_vec_def

# Estimador del total
estimador_total <- (M / m) * sum(N_vec_piloto / n_h * media_vec_def * 117)
estimador_total
# Varianza entre conglomerados muestra piloto
vec <- M^2 * (1 - m / M) * var(totales_vec_def) / m

# Varianza dentro de conglomerados muestra piloto
vdc <- (M / m) * sum(N_vec_piloto^2 * (1 - n_h / N_vec_piloto) * var_vec_def / n_h)

# Varianza Total
var_total_def <- vec + vdc
ee_total_def <- sqrt(var_total_def)
vec / var_total_def # Contribución de 0.9986983
vdc / var_total_def # Contribución de 0.00130171

# intervalos de confianza Total
# lim inf
lim_inf_total <- estimador_total - Z * ee_total_def
lim_inf_total

# lim sup
lim_sup_total <- estimador_total + Z * ee_total_def
lim_sup_total

c(lim_inf_total, lim_sup_total)
"
Es decir con un nivel de confianza 1-0.05=0.95, el total de ventas está
entre 2,559,029  y 3,805,191 Sabíamos que el total original era de 2,297,200.8603
"

"
El muestreo por conglomerados es el más barato pero también el menos preciso
"
