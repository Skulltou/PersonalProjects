"
Autor: Emiliano Romero Medina
Este un proyecto personal, que intenta cubrir y comparar los diverso marcos de
muestreo estadístico. Este segundo código es un ejemplo de Muestreo Aleatorio
Estratificado (MAE).

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
Esto en términos de la estratificación por la variable Ship.Mode referente
a la preferencia de envío del producto relacionado con el tiempo y modo de
entrega
"
#----Librerás----
library(ggplot2)
library(sampling)
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
# Hacemos un histograma del comportamiento de los datos y calculamos
# medidas relevantes
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
de 20 productos considerando su estratificación
"
# Estratos a utilizar
estratos <- unique(datos$Ship.Mode)
# Ordenamos la población por estrato
datos2 <- datos[order(datos$Ship.Mode), ]
# Tamaños totales de cada estrato
N_h <- table(datos2$Ship.Mode)
N <- sum(N_h)
# Tamaños de muestra piloto por estrato
L <- length(estratos)
n_piloto <- 32
n_h_piloto <- rep(round(n_piloto / L), length(N_h)) # Seleccionaremos 10 por cada estrato


# Aleatorización de los estratos
# Utilizaremos strata del paquete sampling
set.seed(123)
estratos_piloto <- strata(datos2,
  stratanames = "Ship.Mode",
  size = n_h_piloto, method = "srswor"
)
"Los estratos son:
1: First Class
2: Same Day
3: Second Class
4: Standar Class
"
# srswor: Muestreo Aleatorio Simple sin Reemplazo
muestra_piloto <- getdata(datos2, estratos_piloto)
ventas_piloto <- muestra_piloto$Sales

"
Comparemos la muestra piloto con la población
"
# Comparación Ventas sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = ventas_piloto,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparación Ventas con Logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(ventas_piloto),
  titulo = "Distribución logarítmica de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

# Errores estándar por estrato en la muestra
ee_piloto_1 <- sd(muestra_piloto[muestra_piloto$Stratum == 1, ]$Sales)
ee_piloto_2 <- sd(muestra_piloto[muestra_piloto$Stratum == 2, ]$Sales)
ee_piloto_3 <- sd(muestra_piloto[muestra_piloto$Stratum == 3, ]$Sales)
ee_piloto_4 <- sd(muestra_piloto[muestra_piloto$Stratum == 4, ]$Sales)
ee_piloto <- c(ee_piloto_1, ee_piloto_2, ee_piloto_3, ee_piloto_4)

# Nivel de confianza (1-alpha) o probabilidad de fallar
alpha <- 0.05
Z <- qnorm(1 - alpha / 2)

# Precisión dada en magnitud de la variable de interés +-
d <- 30
D2 <- (d / Z)^2

"
Las afijaciones que realizaremos serán 3, igual, proporcional y Neyman
"

#----Afijación Igual en cada Estrato----
n_igual <- round((L * sum(N_h^2 * ee_piloto^2)) / (N^2 * D2 + sum(N_h * ee_piloto^2)))
n_igual
# Muestra para cada estrato
n_1_ig <- round(n_igual / L)
n_2_ig <- round(n_igual / L)
n_3_ig <- round(n_igual / L)
n_4_ig <- round(n_igual / L)
n_igual <- n_1_ig + n_2_ig + n_3_ig + n_4_ig
n_ig_es <- c(n_1_ig, n_2_ig, n_3_ig, n_4_ig)

#----Afijación Proporcional en cada Estrato----
n_prop <- round((N * sum(N_h * ee_piloto^2)) / (N^2 * D2 + sum(N_h * ee_piloto^2)))
n_prop
# Muestra para cada estrato
aux_pr <- round((N_h / sum(N_h)) * n_prop)
n_1_pr <- aux_pr[1]
n_2_pr <- aux_pr[2]
n_3_pr <- aux_pr[3]
n_4_pr <- aux_pr[4]
n_prop <- sum(aux_pr)
n_pr_es <- c(n_1_pr, n_2_pr, n_3_pr, n_4_pr)

#----Afijación Neyman----
n_neyman <- round((sum(N_h * ee_piloto)^2) / (N^2 * D2 + sum(N_h * ee_piloto^2)))
n_neyman
# Muestra para cada estrato
aux_ney <- round((N_h * ee_piloto / sum(N_h * ee_piloto)) * n_neyman)
n_1_ney <- aux_ney[1]
n_2_ney <- aux_ney[2]
n_3_ney <- aux_ney[3]
n_4_ney <- aux_ney[4]
n_neyman <- sum(aux_ney)
n_ney_es <- c(n_1_ney, n_2_ney, n_3_ney, n_4_ney)

#----Muestra Definitiva Afijación Igual----
set.seed(234)
estratos_igual <- strata(datos2,
  stratanames = "Ship.Mode",
  size = n_ig_es, method = "srswor"
)
# Jalamos los datos
muestra_definitiva_igual <- getdata(datos2, estratos_igual)
ventas_igual <- muestra_definitiva_igual$Sales

# Comparamos población vs Muestra sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = ventas_igual,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparamos población vs Muestra con logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(ventas_igual),
  titulo = "Distribución de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

media_estr_igual <- c(
  mean(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 1, ]$Sales),
  mean(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 2, ]$Sales),
  mean(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 3, ]$Sales),
  mean(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 4, ]$Sales)
)
ee_estr_igual <- c(
  sd(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 1, ]$Sales),
  sd(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 2, ]$Sales),
  sd(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 3, ]$Sales),
  sd(muestra_definitiva_igual[muestra_definitiva_igual$Stratum == 4, ]$Sales)
)
W_h <- N_h / sum(N_h)
media_estimada_igual <- sum(W_h * media_estr_igual)
media_estimada_igual

# Varianza del estimador
var_media_igual <- (L / N^2) * sum(N_h^2 * ee_estr_igual^2) / n_igual - (1 / N^2) * sum(N_h * ee_estr_igual^2)
ee_media_igual <- sqrt(var_media_igual)

# intervalos de confianza media
# lim inf
lim_inf_media_igual <- media_estimada_igual - Z * ee_media_igual
lim_inf_media_igual

# lim sup
lim_sup_media_igual <- media_estimada_igual + Z * ee_media_igual
lim_sup_media_igual

c(lim_inf_media_igual, lim_sup_media_igual)
"
Es decir con un nivel de confianza 1-0.05=0.95, la media de las ventas está
entre 187.6172 y 291.9613. Sabíamos que la media original era de 229.8580
"
# Total de ventas
total_estimado_igual <- N * media_estimada_igual
total_estimado_igual

# intervalos de confianza Total
# lim inf
lim_inf_total_igual <- N * lim_inf_media_igual
lim_inf_total_igual

# lim sup
lim_sup_total_igual <- N * lim_sup_media_igual
lim_sup_total_igual

c(lim_inf_total_igual, lim_sup_total_igual)
"
Es decir con un nivel de confianza 1-0.05=0.95, el total de ventas está
entre 1875047 y 2917862 Sabíamos que el total original era de 2297200.8603
"

#----Muestra Definitiva Proporcional al Estrato----
set.seed(125)
estratos_prop <- strata(datos2,
  stratanames = "Ship.Mode",
  size = n_pr_es, method = "srswor"
)
# Jalamos los datos
muestra_definitiva_prop <- getdata(datos2, estratos_prop)
ventas_prop <- muestra_definitiva_prop$Sales

# Comparamos población vs Muestra sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = ventas_prop,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparamos población vs Muestra con logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(ventas_prop),
  titulo = "Distribución de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

media_estr_prop <- c(
  mean(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 1, ]$Sales),
  mean(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 2, ]$Sales),
  mean(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 3, ]$Sales),
  mean(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 4, ]$Sales)
)
ee_estr_prop <- c(
  sd(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 1, ]$Sales),
  sd(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 2, ]$Sales),
  sd(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 3, ]$Sales),
  sd(muestra_definitiva_prop[muestra_definitiva_prop$Stratum == 4, ]$Sales)
)
W_h <- N_h / sum(N_h)
media_estimada_prop <- sum(W_h * media_estr_prop)
media_estimada_prop

# Varianza del estimador
var_media_prop <- (1 / N) * sum(N_h * ee_estr_prop^2) / n_prop - (1 / N^2) * sum(N_h * ee_estr_prop^2)
ee_media_prop <- sqrt(var_media_prop)

# intervalos de confianza media
# lim inf
lim_inf_media_prop <- media_estimada_prop - Z * ee_media_prop
lim_inf_media_prop

# lim sup
lim_sup_media_prop <- media_estimada_prop + Z * ee_media_prop
lim_sup_media_prop

c(lim_inf_media_prop, lim_sup_media_prop)
"
Es decir con un nivel de confianza 1-0.05=0.95, la media de las ventas está
entre 199.7492 y 273.4198 Sabíamos que la media original era de 229.8580
"
# Total de ventas
total_estimado_prop <- N * media_estimada_prop
total_estimado_prop

# intervalos de confianza Total
# lim inf
lim_inf_total_prop <- N * lim_inf_media_prop
lim_inf_total_prop

# lim sup
lim_sup_total_prop <- N * lim_sup_media_prop
lim_sup_total_prop

c(lim_inf_total_prop, lim_sup_total_prop)
"
Es decir con un nivel de confianza 1-0.05=0.95, el total de ventas está
entre 1996293 y 2732557 Sabíamos que el total original era de 2297200.8603
"

#----Muestra Definitiva Neyman----
set.seed(562)
estratos_ney <- strata(datos2,
  stratanames = "Ship.Mode",
  size = n_ney_es, method = "srswor"
)
# Jalamos los datos
muestra_definitiva_ney <- getdata(datos2, estratos_ney)
ventas_ney <- muestra_definitiva_ney$Sales

# Comparamos población vs Muestra sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = ventas_ney,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparamos población vs Muestra con logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(ventas_ney),
  titulo = "Distribución de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

media_estr_ney <- c(
  mean(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 1, ]$Sales),
  mean(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 2, ]$Sales),
  mean(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 3, ]$Sales),
  mean(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 4, ]$Sales)
)
ee_estr_ney <- c(
  sd(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 1, ]$Sales),
  sd(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 2, ]$Sales),
  sd(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 3, ]$Sales),
  sd(muestra_definitiva_ney[muestra_definitiva_ney$Stratum == 4, ]$Sales)
)
W_h <- N_h / sum(N_h)
media_estimada_ney <- sum(W_h * media_estr_ney)
media_estimada_ney

# Varianza del estimador
var_media_ney <- (1 / N^2) * sum(N_h * ee_estr_ney)^2 / n_neyman - (1 / N^2) * sum(N_h * ee_estr_ney^2)
ee_media_ney <- sqrt(var_media_ney)

# intervalos de confianza media
# lim inf
lim_inf_media_ney <- media_estimada_ney - Z * ee_media_ney
lim_inf_media_ney

# lim sup
lim_sup_media_ney <- media_estimada_ney + Z * ee_media_ney
lim_sup_media_ney

c(lim_inf_media_ney, lim_sup_media_ney)
"
Es decir con un nivel de confianza 1-0.05=0.95, la media de las ventas está
entre 194.1954 y 275.8269 Sabíamos que la media original era de 229.8580
"
# Total de ventas
total_estimado_ney <- N * media_estimada_ney
total_estimado_ney

# intervalos de confianza Total
# lim inf
lim_inf_total_ney <- N * lim_inf_media_ney
lim_inf_total_ney

# lim sup
lim_sup_total_ney <- N * lim_sup_media_ney
lim_sup_total_ney

c(lim_inf_total_ney, lim_sup_total_ney)
"
Es decir con un nivel de confianza 1-0.05=0.95, el total de ventas está
entre 1940788 y 2756614 Sabíamos que el total original era de 2297200.8603
"

"
Así el mejor comportamiento lo tiene el estimador proporcional al tamaño.
Aunque sin tener muchos supuestos, el estimador igual tiene buen desempeño
Mientras que el estimador de Neyman, no cubrió al parámetro de interés.
"
