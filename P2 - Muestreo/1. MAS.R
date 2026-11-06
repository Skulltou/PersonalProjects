"
Autor: Emiliano Romero Medina
Este un proyecto personal, que intenta cubrir y comparar los diverso marcos de
muestreo estadístico. Este primer código es un ejemplo de Muestreo Aleatorio
Simple (MAS).

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
"
#----Librerás----
library(ggplot2)
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

"
Media Poblacional: 229.858
Varianza Poblacional: 623.1827
"

"
Debido a lo leptocúrtica de la variable, haremos una tranformación loga-
ritmica únicamente para ver los gráficos e histogramas.
"
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
de 20 productos o bien si ya se tiene información de el comportamiento
de la variable (sobre todo su varianza), lo podemos utilizar
"
n_piloto <- 32
set.seed(123) # Reproducibilidad Semilla 123
muestra_piloto <- sample(datos$Sales, size = n_piloto, replace = FALSE)

# Comparación Ventas sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = muestra_piloto,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparación Ventar con Logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(muestra_piloto),
  titulo = "Distribución logarítmica de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

"
Podemos observar que la muestra piloto de tamaño 20 es una aproximación
deficiente de la población, por ello esta muestra únicamente nos va permi-
tir fijar el tamaño de muestra definitivo
"

media_piloto <- mean(muestra_piloto)
media_piloto

ee_piloto <- sd(muestra_piloto)
ee_piloto

# Nivel de confianza (1-alpha) o probabilidad de fallar
alpha <- 0.05
Z <- qnorm(1 - alpha / 2)

# Precisión dada en magnitud de la variable de interés +-
d <- 30
n0 <- ((Z * ee_piloto) / d)^2
n0 # Tamaño de muestra si hubiera población infinita

# Tamaño final
n <- n0 / (1 + n0 / N)
n <- round(n, digits = 0)
n

"
Así obtenemos que el tamaño de muestra debe e ser de 656, por lo que pro-
cedemos a tomar la muestra definitiva.
"
#----Muestra Definitiva----
set.seed(234)
muestra_definitiva <- sample(datos$Sales, size = n, replace = FALSE)

# Comparamos la densidad poblacional con la muestral obtenida

# Comparación Ventas sin logaritmo
comparar_densidades(
  poblacion = datos$Sales,
  muestra = muestra_definitiva,
  titulo = "Distribución de las ventas",
  etiqueta_x = "Ventas"
)

# Comparación Ventar con Logaritmo
comparar_densidades(
  poblacion = log(datos$Sales),
  muestra = log(muestra_definitiva),
  titulo = "Distribución logarítmica de las ventas",
  etiqueta_x = "Logaritmo de las Ventas"
)

media_estimada <- mean(muestra_definitiva)
media_estimada

ee_estimada <- sd(muestra_definitiva)
ee_estimada

var_estimador <- (1 - n / N) * ee_estimada^2 / n
ee_estimador <- sqrt(var_estimador)
ee_estimador

# intervalos de confianza media
# lim inf
lim_inf_media <- media_estimada - Z * ee_estimador
lim_inf_media

# lim sup
lim_sup_media <- media_estimada + Z * ee_estimador
lim_sup_media


c(lim_inf_media, lim_sup_media)
"
Es decir con un nivel de confianza 1-0.05=0.95, la media de las ventas está
entre 172.4751 y 238.0007. Sabíamos que la media original era de 229.8580
"

total_estimado <- N * media_estimada
total_estimado

# intervalos de confianza Total
# lim inf
lim_inf_total <- N * lim_inf_media
lim_inf_total

# lim sup
lim_sup_total <- N * lim_sup_media
lim_sup_total

c(lim_inf_total, lim_sup_total)
"
Es decir con un nivel de confianza 1-0.05=0.95, el total de ventas está
entre 1723716 y 2378579 Sabíamos que el total original era de 2297200.8603
"
