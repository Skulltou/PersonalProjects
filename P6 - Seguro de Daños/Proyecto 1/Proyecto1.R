library(actuar)
library(shiny)
library(ggplot2)

#Creación de la interfaz
ui <- fluidPage(
  titlePanel(title = "Variables aleatorias"),
  sidebarPanel(
    #Eleccion de continua, discreta
    selectInput("tipo", "Tipo de variable:", 
                choices = c("Contínua", "Discreta")),
    #Inicialización de las sliderbars, en valores cualquiera
    selectInput("dist", "Distribución:", choices = ""),
    sliderInput("p1", "α",value = 1/2,min = 0.1,
                max = 100,step = 0.1),
    sliderInput("p2", "θ",value = 1/2,min = 0.1,
                max = 100,step = 0.1),
    sliderInput("p3", "Ɣ",value = 1/2,min = 0.1,
                max = 100, step = 0.1),
    sliderInput("n", "Tamaño Muestra",value = 100,min = 100,
                max = 1000, step = 1)
  ),
  #Creación de los espacios para las gráficas
  mainPanel(
    tabsetPanel(
      tabPanel("Funciones Teóricas",
               plotOutput("densplot"),
               plotOutput("distplot"),
               plotOutput("sobplot"),
               plotOutput("tasplot")),
      
      tabPanel("Muestra Simulada",
               plotOutput("histplot"),
               plotOutput("boxplot"),
               plotOutput("qqplot"),
               tableOutput("resumen"))
    )
  )
)


#Lógica del Servidor
server <- function(input, output){
  #Cambia las distribuciones según sea su tipo (Contínua, Discreta)
  observeEvent(input$tipo,{
    if (input$tipo == "Contínua"){
      updateSelectInput(inputId = "dist", 
                        choices = c("Normal","Exponencial", "Gamma", "Pareto", 
                                    "Weibull", "Lognormal", "Burr", "Beta", 
                                    "Uniforme", "Cauchy", "F", "t de Student"),
                        selected = "Normal")}
    if (input$tipo == "Discreta"){
      updateSelectInput(inputId = "dist", 
                        choices = c("Bernulli", "Binomial", "Poisson", 
                                    "Geométrica", "Binomial Negativa"),
                        selected = "Bernulli")}
    })
  #Cambio de los Sliders de los parámetros según su distribucón
  #Nota: Hay un pequeño problema con la distribución uniforme, en caso de que
  #el cógido termine, reiniciar e ir directamente a la distribución antes dicha
  #Nota2. De algún esto es un plus a nuestro código pues evita errores de valor
  observeEvent(input$dist,{
    if (input$dist == "Normal"){
      updateSliderInput(inputId = "p1", label = "Media", value = 0, min = -20,
                        max = 20, step = 0.01)
      updateSliderInput(inputId = "p2", label = "Desviación",value = 1, min = 0.01,
                        max = 20, step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Exponencial"){
      updateSliderInput(inputId = "p1", label = "Tasa", value = 1/2, min = 0.01,
                        max = 20, step = 0.01)
      updateSliderInput(inputId = "p2", label = "No aplica", value = 0,
                        min = 0, max = 0)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Gamma"){
      updateSliderInput(inputId = "p1", label = "Forma", value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Tasa",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Pareto"){
      updateSliderInput(inputId = "p1",label = "Forma",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Escala",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Weibull"){
      updateSliderInput(inputId = "p1",label = "Forma",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Escala",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Lognormal"){
      updateSliderInput(inputId = "p1",label = "Media",value = 0,min = -20,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Desviación",value = 1,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Burr"){
      updateSliderInput(inputId = "p1",label = "Forma 1",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Forma2",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3",label = "Escala",value = 1/2,min = 0.01,
                        max = 20, step = 0.01)}
    if (input$dist == "Beta"){
      updateSliderInput(inputId = "p1",label = "Forma 1",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Forma 2",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Uniforme"){
      updateSliderInput(inputId = "p1",label = "Mínimo",value = 0,min = -20,
                        max = 20, step = 0.01)
      updateSliderInput(inputId = "p2", label = "Máximo (Mayor a Mínimo)",value = 1, min=-20,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Cauchy"){
      updateSliderInput(inputId = "p1",label = "Localización",value = 1/2,
                        min = 0.01,max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Escala",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "F"){
      updateSliderInput(inputId = "p1",label = "Grado de libertad (m)",value = 1,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Grado de libertad (n)",value = 1,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "t de Student"){
      updateSliderInput(inputId = "p1",label = "Grado de libertad (n)",value = 1,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2", label = "No aplica", value = 0,
                        min = 0, max = 0)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Bernulli"){
      updateSliderInput(inputId = "p1",label = "Probabilidad de Éxito",value = 1/2,min = 0.01,
                        max = 0.99,step = 0.01)
      updateSliderInput(inputId = "p2", label = "No aplica", value = 0,
                        min = 0, max = 0)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Binomial"){
      updateSliderInput(inputId = "p1",label = "Número de Intentos",value = 1,min = 1,
                        max = 20,step = 1)
      updateSliderInput(inputId = "p2",label = "Probabilidad de Éxito",value = 1/2,min = 0.01,
                        max = 0.99,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Poisson"){
      updateSliderInput(inputId = "p1",label = "Media",value = 1/2,min = 0.01,
                        max = 20,step = 0.01)
      updateSliderInput(inputId = "p2", label = "No aplica", value = 0,
                        min = 0, max = 0)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Geométrica"){
      updateSliderInput(inputId = "p1",label = "Probabilidad",value = 1/2,min = 0.01,
                        max = 0.99,step = 0.01)
      updateSliderInput(inputId = "p2", label = "No aplica", value = 0,
                        min = 0, max = 0)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
    if (input$dist == "Binomial Negativa"){
      updateSliderInput(inputId = "p1", label = "r", value = 1,
                        min = 0.01, max = 20,step = 0.01)
      updateSliderInput(inputId = "p2",label = "Probabilidad",value = 1/2,min = 0.01,
                        max = 0.99,step = 0.01)
      updateSliderInput(inputId = "p3", label = "No aplica", value = 0,
                        min = 0, max = 0)}
  })
  
  #Hacemos que las gráficas y en general todo el código cambie, si algún parámetro
  #cambia
  observeEvent(list(input$dist, input$p1, input$p2, input$p3, input$n),{
    dist <- input$dist
    #Diferentes x's para optimizar la impresión de las densidades
    x1 <- seq(-15,15, by=0.01)
    x2 <- seq(0.01,20, by=0.01)
    x3 <- seq(0,1, by=0.001)
    x4 <- seq(0,20, by = 1)
    #Cambiar el valor de la densidad según la distribución seleccionada
    densidad <- switch(dist,
                       "Normal" = dnorm(x1, mean = input$p1, sd = input$p2),
                       "Exponencial" = dexp(x2, rate =  input$p1),
                       "Gamma" = dgamma(x2, shape  = input$p1 , rate = input$p2),
                       "Pareto" = dpareto(x2, scale = input$p1, shape = input$p2),
                       "Weibull" = dweibull(x2, shape = input$p1, scale = input$p2),
                       "Lognormal" = dlnorm(x2, meanlog = input$p1, sdlog = input$p2),
                       "Burr" = dburr(x2, shape1 = input$p1, shape2 = input$p2, rate = input$p3),
                       "Beta" = dbeta(x3, shape1 = input$p1, shape2 = input$p2),
                       "Uniforme" = dunif(seq(input$p1, input$p2, by=0.01), min = input$p1, max = input$p2),
                       "Cauchy" = dcauchy(x1, location = input$p1, scale = input$p2),
                       "F" = df(x2, df1 = input$p1, df2 = input$p2),
                       "t de Student" = dt(x1, df = input$p1),
                       "Bernulli" = dbinom(c(0,1), size = 1, prob = input$p1),
                       "Binomial" = dbinom(seq(0, input$p1, by=1), size = input$p1, prob = input$p2),
                       "Poisson" = dpois(x4, lambda = input$p1),
                       "Geométrica" = dgeom(x4, prob = input$p1),
                       "Binomial Negativa" = dnbinom(x4, size = input$p1, prob = input$p2))
    #Cambiar el valor de la función de distribución según la distribución seleccionada
    distribucion <- switch(dist,
                           "Normal" = pnorm(x1, mean = input$p1, sd = input$p2),
                           "Exponencial" = pexp(x2, rate =  input$p1),
                           "Gamma" = pgamma(x2, shape  = input$p1 , rate = input$p2),
                           "Pareto" = ppareto(x2, scale = input$p1, shape = input$p2),
                           "Weibull" = pweibull(x2, shape = input$p1, scale = input$p2),
                           "Lognormal" = plnorm(x2, meanlog = input$p1, sdlog = input$p2),
                           "Burr" = pburr(x2, shape1 = input$p1, shape2 = input$p2, rate = input$p3),
                           "Beta" = pbeta(x3, shape1 = input$p1, shape2 = input$p2),
                           "Uniforme" = punif(seq(input$p1, input$p2, by=0.01), min = input$p1, max = input$p2),
                           "Cauchy" = pcauchy(x1, location = input$p1, scale = input$p2),
                           "F" = pf(x2, df1 = input$p1, df2 = input$p2),
                           "t de Student" = pt(x1, df = input$p1),
                           "Bernulli" = pbinom(0:1, size = 1, prob = input$p1),
                           "Binomial" = pbinom(seq(0, input$p1, by=1), size = input$p1, prob = input$p2),
                           "Poisson" = ppois(x4, lambda = input$p1),
                           "Geométrica" = pgeom(x4, prob = input$p1),
                           "Binomial Negativa" = pnbinom(x4, size = input$p1, prob = input$p2))
    #Cálculo de la sobrevivencia
    sobrevivencia <- 1-distribucion
    #Cálculo de la tasa de fallo
    tasa_fallo <- densidad/sobrevivencia
    #Llenado de la gráfica de la densidad
    output$densplot <- renderPlot({
      if(dist == "Normal"|dist == "Cauchy"|dist =="t de Student"){
        plot(x1, densidad, type = "l",col="blue",ylim=c(0,0.5), main = "Función de Densidad")
      }
      if(dist == "Gamma" |dist == "Burr"| dist =="Pareto"|dist =="Weibull"|dist =="Lognormal"|
         dist =="F"|dist =="Exponencial"){
        plot(x2, densidad, type = "l",col="blue",ylim=c(0,1.5), main = "Función de Densidad")
      }
      if(dist == "Beta"){
        plot(x3, densidad, type = "l",col="blue",ylim=c(0,5), main = "Función de Densidad")
      }
      if(dist == "Uniforme"){
        plot(seq(input$p1, input$p2, by=0.01), densidad, type = "l",col="blue",
             xlab = "x", ylim=c(0,2), main = "Función de Densidad")
      }
      if(dist == "Bernulli"){
        plot(c(0,1), densidad,col="blue",type = "h",
             xlab = "x", ylim = c(0,1), main = "Función de Probabilidad")
        points(c(0,1), densidad, pch = 16, col = "blue", cex = 1.5)
      }
      if(dist == "Binomial"){
        plot(seq(0, input$p1, by=1), densidad,col="blue",type = "h",
             xlab = "x", ylim = c(0,0.7), main = "Función de Probabilidad")
        points(seq(0, input$p1, by=1), densidad, pch = 16, col = "blue", cex = 1.5)
      }
      if(dist == "Poisson"|dist == "Geométrica"|dist == "Binomial Negativa"){
        plot(x4, densidad,col="blue",type = "h",
             xlab = "x", ylim = c(0,0.7), main = "Función de Probabilidad")
        points(x4, densidad, pch = 16, col = "blue", cex = 1.5)
      }
    })
    #Llenado de la gráfica de la distribucón
    output$distplot <- renderPlot({
      if(dist == "Normal"|dist == "Cauchy"|dist =="t de Student"){
        plot(x1, distribucion, type = "l",col="blue",ylim=c(0,1), main = "Función de Distribución")
      }
      if(dist == "Gamma" |dist == "Burr"| dist =="Pareto"|dist =="Weibull"|dist =="Lognormal"|
         dist =="F"|dist =="Exponencial"){
        plot(x2, distribucion, type = "l",col="blue",ylim=c(0,1), main = "Función de Distribución")
      }
      if(dist == "Beta"){
        plot(x3, distribucion, type = "l",col="blue",ylim=c(0,1), main = "Función de Distribución")
      }
      if(dist == "Uniforme"){
        plot(seq(input$p1, input$p2, by=0.01), distribucion, type = "l",
             xlab = "x", col="blue",ylim=c(0,1), main = "Función de Distribución")
      }
      if(dist == "Bernulli"){
        plot(0:1, distribucion,col="blue",type = "s",
             xlab = "x", ylim = c(0,1), main = "Función de Distribución")
      }
      if(dist == "Binomial"){
        plot(0:input$p1, distribucion,col="blue",type = "s",
             xlab = "x", ylim = c(0,1), main = "Función de Distribución")
      }
      if(dist == "Poisson"|dist == "Geométrica"|dist == "Binomial Negativa"){
        plot(x4, distribucion,col="blue",type = "s",
             xlab = "x", ylim = c(0,1), main = "Función de Distribución")
      }
    })
    #Llenado de la gráfica de la sobrevivencia
    output$sobplot <- renderPlot({
      if(dist == "Normal"|dist == "Cauchy"|dist =="t de Student"){
        plot(x1, sobrevivencia, type = "l",col="blue",ylim=c(0,1), main = "Función de Sobrevivencia")
      }
      if(dist == "Gamma" |dist == "Burr"| dist =="Pareto"|dist =="Weibull"|dist =="Lognormal"|
         dist =="F"|dist =="Exponencial"){
        plot(x2, sobrevivencia, type = "l",col="blue",ylim=c(0,1), main = "Función de Sobrevivencia")
      }
      if(dist == "Beta"){
        plot(x3, sobrevivencia, type = "l",col="blue",ylim=c(0,1), main = "Función de Sobrevivencia")
      }
      if(dist == "Uniforme"){
        plot(seq(input$p1, input$p2, by=0.01), sobrevivencia, 
             xlab = "x", type = "l",col="blue",ylim=c(0,1), main = "Función de Sobrevivencia")
      }
      if(dist == "Bernulli"){
        plot(0:1, sobrevivencia,col="blue",type = "s",
             xlab = "x", ylim = c(0,1), main = "Función de Sobrevivencia")
      }
      if(dist == "Binomial"){
        plot(0:input$p1, sobrevivencia,col="blue",type = "s",
             xlab = "x", ylim = c(0,1), main = "Función de Sobrevivencia")
      }
      if(dist == "Poisson"|dist == "Geométrica"|dist == "Binomial Negativa"){
        plot(x4, sobrevivencia,col="blue",type = "s",
             xlab = "x", ylim = c(0,1), main = "Función de Sobrevivencia")
      }
    })
    #Llenado de la gráfica de la tasa de fallo
    output$tasplot <- renderPlot({
      if(dist == "Normal"|dist == "Cauchy"|dist =="t de Student"){
        plot(x1, tasa_fallo, type = "l",col="blue",ylim=c(0,7), main = "Tasa de fallo")
      }
      if(dist == "Gamma" |dist == "Burr"| dist =="Pareto"|dist =="Weibull"|dist =="Lognormal"|
         dist =="F"|dist =="Exponencial"){
        plot(x2, tasa_fallo, type = "l",col="blue",ylim=c(0,7), main = "Tasa de fallo")
      }
      if(dist == "Beta"){
        plot(x3, tasa_fallo, type = "l",col="blue",ylim=c(0,7), main = "Tasa de fallo")
      }
      if(dist == "Uniforme"){
        plot(seq(input$p1, input$p2, by=0.01), tasa_fallo, type = "l",col="blue",
             xlab = "x",ylim=c(0,5), main ="Tasa de fallo")
      }
      if(dist == "Bernulli"){
        plot(c(0,1), tasa_fallo,col="blue",
             xlab = "x", ylim = c(0,1), main = "Tasa de fallo")
        points(c(0,1), tasa_fallo, pch = 16, col = "blue", cex = 1.5)
      }
      if(dist == "Binomial"){
        plot(seq(0, input$p1, by=1), tasa_fallo,col="blue",
             xlab = "x", ylim = c(0,1), main = "Tasa de fallo")
        points(seq(0, input$p1, by=1), tasa_fallo, pch = 16, col = "blue", cex = 1.5)
      }
      if(dist == "Poisson"|dist == "Geométrica"|dist == "Binomial Negativa"){
        plot(x4, tasa_fallo,col="blue",
             xlab = "x", ylim = c(0,1), main = "Tasa de fallo")
        points(x4, tasa_fallo, pch = 16, col = "blue", cex = 1.5)
      }
    })
    
    #Generación de muestras aleatorias según distribución escogida para comparativo
    #teórico contra muestral
    simulacion <- switch (dist,
                          "Normal" = rnorm(input$n, mean = input$p1, sd = input$p2),
                          "Exponencial" = rexp(input$n, rate =  input$p1),
                          "Gamma" = rgamma(input$n, shape  = input$p1 , rate = input$p2),
                          "Pareto" = rpareto(input$n, scale = input$p1, shape = input$p2),
                          "Weibull" = rweibull(input$n, shape = input$p1, scale = input$p2),
                          "Lognormal" = rlnorm(input$n, meanlog = input$p1, sdlog = input$p2),
                          "Burr" = rburr(input$n, shape1 = input$p1, shape2 = input$p2, rate = input$p3),
                          "Beta" = rbeta(input$n, shape1 = input$p1, shape2 = input$p2),
                          "Uniforme" = runif(input$n, min = input$p1, max = input$p2),
                          "Cauchy" = rcauchy(input$n, location = input$p1, scale = input$p2),
                          "F" = rf(input$n, df1 = input$p1, df2 = input$p2),
                          "t de Student" = rt(input$n, df = input$p1),
                          "Bernulli" = rbinom(input$n, size = 1, prob = input$p1),
                          "Binomial" = rbinom(input$n, size = input$p1, prob = input$p2),
                          "Poisson" = rpois(input$n, lambda = input$p1),
                          "Geométrica" = rgeom(input$n, prob = input$p1),
                          "Binomial Negativa" = rnbinom(input$n, size = input$p1, prob = input$p2))
    #Llenado de histograma, boxplot, qqplot y resumen
    output$histplot <- renderPlot({
      ggplot(data.frame(x = simulacion), aes(x)) + geom_histogram(bins=70, fill = "purple", alpha = 0.5) + labs(title = "Histograma de la Muestra")})
    output$boxplot <- renderPlot({
      ggplot(data.frame(x = simulacion), aes(y = x)) + geom_boxplot(fill = "blue", alpha = 0.5) + labs(title = "Boxplot")})
    output$qqplot <- renderPlot({
      qqnorm(simulacion)
      qqline(simulacion, col = "orange")})
    output$resumen <- renderTable({
      data.frame(Estadístico = c("Media", "Mediana", "Varianza", "Desviación Estándar"),
                 Valor = c(mean(simulacion), median(simulacion), var(simulacion), sd(simulacion)))})

  })
}

#Aplicación
shinyApp(ui = ui, server = server)


