# Titulo: Probabilidad y valor esperado,  Variabilidad y distribuciones. ####

# Propósito: Esta actividad tiene como objetivo simular eventos 
# aleatorios simples, ver distribuciones discretas comunes
# y distribuciones continuas comunes, 
# Aprende sobre la ley de los grandes números y el teorema del límite central.

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de complementación del script: "2022-10-11 14:41:44 -03" ----
# Fecha de ultima modificación: "2022-10-11 14:41:44 -03" ----


# Preámbulo 

# Instalar paquetes ####

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse) 

# 0. Funciones útiles ####

# - Se utiliza sample() para generar muestras aleatorias a partir de un 
# conjunto finito de elementos.

# - Úselo set.seed() para establecer una semilla aleatoria para obtener 
# resultados reproducibles.

# - rbinom() para tomar muestras de las distribuciones de Bernoulli y binomial.

# - dbinom() para calcular la función de densidad de probabilidad (PDF) de 
# las distribuciones de Bernoulli/binomial.

# - pbinom() para calcular la función de distribución acumulativa (CDF) 
# de las distribuciones de Bernoulli/binomial.

# - qbinom() para calcular la función cuantil de las distribuciones de 
# Bernoulli/binomial.

# - Utilice las funciones análogas rpois(), dpois(), ppois()y qpois(), 
# para la distribución de Poisson.

# - Use rnorm(), dnorm(), pnorm()y qnorm(), funciones análogas para 
# la distribución normal.

# - Utilice las funciones análogas runif(), dunif(), punif()y qunif(), para 
# la distribución uniforme.

# - Use rexp(), dexp(), pexp()y qexp(), funciones análogas para la 
# distribución exponencial.

# - Úselo replicate() para ejecutar el mismo cálculo aleatorio una y otra vez 
# muchas veces.

# 1. Lanzamiento de monedas o datos ####

sample(c("heads", "tails"), size = 5, replace = TRUE)

data <- sample(c("heads", "tails"), size = 4, replace = TRUE)
barplot(table(data))

sample(c(1:6), size = 6, replace = FALSE)

sample(c(1:6), size = 10, replace = FALSE)

sample(c(1:6), size = 10, replace = TRUE)

# lanzamiento de un dado injusto 

data <- sample(1:6, size = 100, replace = TRUE,
               prob = c(1/10, 1/10, 1/10, 1/10, 1/10, 5/10))
barplot(table(data))

# 2. Distribuciones discretas ####

#Las variables aleatorias discretas solo pueden tomar valores 
# en un espacio muestral finito o contable específico, es decir, 
# los elementos en él pueden indexarse mediante números enteros 
# (por ejemplo, {a1,a2,a3,…}).

# La distribución de Bernoulli

# X∼Bernoulli(p){0,1}p 

# Ejemplo lanzamiento de monedas (funciones binomiales)

rbinom(n = 1, size = 1, p = 0.7)

rbinom(n = 20, size = 1, p = 0.7)

rbinom(n = 20, size = 3, p = 0.7)

# Funciones de densidad: dbinom

barplot(names.arg = 0:1, 
        height = dbinom(0:1, size = 1, p = 0.5),
        main = "Bernoulli PDF", xlab = 'X', ylab = 'Probability')

barplot(names.arg = 0:1, 
        height = dbinom(0:1, size = 1, p = 0.3),
        main = "Bernoulli PDF", xlab = 'X', ylab = 'Probability')

# 3. Distribuciones continuas ####

#  La distribución normal o gaussiana 

# Las distribuciones continuas no están restringidas a tener un espacio
# muestral finito o contable y (dependiendo de la distribución) 
# pueden tomar cualquier valor en la línea real.

# Muestras aleatorias

rnorm(n = 5, mean = 5, sd = 2)


rnorm(n = 5) # normal estándar

# rnorm(n = 500000000) %>%  mean()
# Error: cannot allocate vector of size 3.7 Gb

rnorm(n = 50000) %>%  mean() # esperado 0

rnorm(n = 50000) %>%  sd() # esperado 1

# Histograma de frecuecnias 

samp <- rnorm(1000000)
hist(samp, freq = FALSE, 
     main = "Histogram of Normal data")

#  aproximación a la densidad continua

plot(density(samp), 
     xlab = "x", ylab = "Density",
     main = "Approximate Distribution")

# Funciones de densidad

# Si nuestra muestra tuviera un número infinito de sorteos 
# de la distribución normal

curve(dnorm(x), 
      xlim = c(-3, 3),
      main = "The Standard Normal Distribution", ylab = "Density")


curve(dnorm(x, mean = 2, sd = 0.5), 
      xlim = c(-4, 4), col = "red",
      main = "The Normal Distribution", ylab = "Density")
curve(dnorm(x, mean = -1, sd = 1),
      add = TRUE, 
      col = "blue")
text(x = c(-1, 2), y = c(0.2, 0.4),         # adds some text to the plot
     labels = c("N(-1, 1.0)", "N(2, 0.5)"),
     col = c("blue", "red"))

# 4. Normalidad ####

qqnorm(samp, pch = 16, 
       col = rgb(0, 0, 0, alpha = 0.5)) #transparent grey
qqline(samp, 
       col = "red")


# ¿qué son los "cuantiles"? (="percentiles")
# son puntos en sus datos por debajo de los cuales cae una cierta 
# proporción de sus datos
# El 95 por ciento de los datos se encuentran por debajo de 1,64. 

# Veamos un ejemplo en código 

quantile(rnorm(200),probs = seq(0.01,0.99,0.01))

# Los gráficos QQ toman sus datos de muestra, los clasifican en 
# orden ascendente y luego los representan frente a los cuantiles 
# calculados a partir de una distribución teórica.

# conjunto de ejemplo
x = data_frame(
  x1 = rnorm(30,10,2),
  x2 = rnorm(30,10,2)
)

# saquemos un sumario estadístico 

x %>% summarise(
  n = n(),
  mean = mean(x1),
  sd = sd(x1)
)

x %>% summarise(
  n = n(),
  mean = mean(x2),
  sd = sd(x2)
)

# Convirtamos algunos valores en atípicos 

x$x1[c(4,5,6)] = c(x$x1[4] + 5*sd(x$x1),
                   x$x1[5] + 6*sd(x$x1),
                   x$x1[6] + 4*sd(x$x1))

# saquemos un sumario estadístico 

x %>% summarise(
  n = n(),
  mean = mean(x1),
  sd = sd(x1)
)

x %>% summarise(
  n = n(),
  mean = mean(x2),
  sd = sd(x2)
)

par(mfrow = c(1, 3))
hist(x$x1, main = "Histogram")
boxplot(x$x1, main = "Boxplot")
qqnorm(x$x1, main = "Normal Q-Q plot")

# Veamos la variable x2 como control 
par(mfrow = c(1, 3))
hist(x$x2, main = "Histogram")
boxplot(x$x2, main = "Boxplot")
qqnorm(x$x2, main = "Normal Q-Q plot")


# 5. Distribución exponencial ####

# Se necesitan dos argumentos:

  # n: cuántos puntos de datos queremos muestrear
# rate: la tasa de “éxitos” ocurren

rexp(n = 5, rate = 0.2)

# Como la distribución exponencial es continua, usamos un histograma 
# para graficar los datos. Tomamos muestras de 10,000 puntos de 
# datos de esta rate = 0.2 distribución exponencial:

data = rexp(n = 10000, rate = 0.2)
hist(data, main = "Histogram of Exp(rate = 0.2)")

# 6. Probabilidad de frecuencia a largo plazo ####

# El sexo biológico de un niño se define por el cromosoma sexual aportado por
# el padre. Las madres aportan siempre un cromosoma C={X}, 
# Mientras que el padre puede aportar un cromosoma C={X,Y} 
# Los cuales definen si será niño o niña
# Por lo tanto la probabilidad esperada de tener una niña en un nacimiento 
# en principio es 1*1/2=1/2

# Simulemos y hagamos un gráfico de 50, luego 1000 y luego 
# 1 millon de nacimientos 

flips <- sample(c("XX", "XY"), size = 1000000, replace = TRUE)

str(flips)

head(flips)

table(flips)

plot(cumsum(flips == "XX") / (1:length(flips)), 
     type = "l", ylim = c(0,1),  
     main = "Niña o niño", 
     xlab = "Número de nacimientos", ylab = "Proporción de niñas")
abline(h = 0.5, col = "red")

# Ejemplos distribución continua ----

# Como ejemplo, la estatura de una persona (1.72m, 1.719m, 1.7186m….). 
# Otro ejemplo, puede ser el tiempo que toma un atleta en recorrer 100 metros
# planos, ya que este tiempo puede tomar valores como 9,623 segundos; 
# 10,456485 segundos; 12,456412 segundos; es decir, un intervalo de valores.

# Por ejemplo, recordemos que en el data frame heights tenemos las estaturas 
# de un grupo de estudiantes de una universidad.

pacman::p_load("dslabs")

data(heights, package = "dslabs")
summary(heights)

heights %>% 
  filter(sex == "Male") %>% # Filtramos solo a los hombres
  mutate(estatura = height/39.37) %>% # Convertimos a centrímetros
  ggplot() +
  aes(sex, estatura, color = estatura) +
  geom_point(position = position_jitterdodge())

# Hace más sentido analizar la data por intervalos, como bien se puede 
# apreciar en este histograma que agrupa por intervalos de 0.05 metros = 5 cm.

heights %>% 
  filter(sex == "Male") %>% # Filtramos solo a los hombres
  mutate(estatura = height/39.37) %>% # Convertimos a centrímetros
  ggplot() +
  aes(estatura) +
  geom_histogram(binwidth = 0.05, color = "black")

# Distribución Empírica

# Por ejemplo, para nuestro caso podemos crear el vector hombres conformado
# por todos los valores de la estatura de los hombres:

hombres <- heights %>% 
  filter(sex == "Male") %>% 
  mutate(estatura = height/39.37) %>% 
  .$estatura

# función de distribución acumulada (FDA)

# Luego, podemos crear la función FDA que tome como variable x y nos calcule 
# la proporción de hombres que miden menos o igual a x dentro de los datos 
# encontrados en el vector hombres.

FDA <- function(x){
  mean(hombres <= x)
}

# Si ahora queremos calcular la probabilidad de que alguien escogido al azar 
# sea más alto que 1.80m primero calculamos la FDA para 1.8 y luego obtenemos 
# el complemento.

# P(x>1.8) = 1-P(x<=1.8)

# Probabilidad de que mida 1.80m o menos
prob <- FDA(1.8)

# Probabilidad de que mida más de 1.80m
1 - prob

# Si ahora quisiéramos saber la probabilidad de que al escoger a alguien al 
# azar éste mida más de 1.6m, pero no más de 1.95m tendríamos.

# P(x>1.6 \int x <= 1.95) = P(x<=1.95)-P(x<=1.6)

prob_1 <- FDA(1.95)

prob_2 <- FDA(1.6)

prob_1 - prob_2

# Distribución Teórica

# Una función teorica que pretende repersentar distribuciones empiricas 

# Entre ellas tenemos la distribución normal, la distribución binomial
# y la distribución de Poisson.


# Por ejemplo, calculemos nuevamente la probabilidad de que al escoger un
# hombre al azar éste mida 1.65m o menos, podríamos utilizar la misma FDA y 
# ahora la función pnorm().

# Utilizando la distribución empírica (data real):
FDA(1.9)
#> [1] 0.9396552

# Utilizando la distribución teórica normal (data approx.):
promedio <- mean(hombres)
desv_est <- sd(hombres)

?pnorm


probabilidad <- pnorm(1.9, promedio, desv_est)
probabilidad

# Matemáticamente estamos calculando el área bajo la curva la cual se ve 
# de color azul:

sec <- seq(-4, 4, length = 100) * desv_est + promedio
normal <- dnorm(sec, promedio, desv_est)

normal %>% 
  as.data.frame() %>% 
  rename(valor = ".") %>% 
  ggplot() +
  aes(sec, valor) +
  geom_line() +
  theme(axis.text.y = element_blank()) +
  xlab("Estatura") +
  ylab("") +
  ggtitle("Distribución normal") +
  geom_area(aes(x = ifelse(sec < 1.9, sec, 0)), fill = "blue") +
  xlim(min(sec), max(sec)) +
  labs(subtitle = paste("P(x <= 1.9) =", probabilidad)) +
  theme_classic()

# volvamos a calcular la probabilidad de que al escoger a alguien al azar 
# éste mida más de 1.6m, pero no más de 1.95m tendríamos.

# Utilizando la distribución empírica (data real):
prob_1 <- FDA(1.95)
prob_2 <- FDA(1.6)
prob_1 - prob_2
#> [1] 0.9445813

# Utilizando la distribución teórica normal (data approx.):
promedio <- mean(hombres)
desv_est <- sd(hombres)

probabilidad <- pnorm(1.95, promedio, desv_est) - pnorm(1.6, promedio, desv_est)
probabilidad

# Matemáticamente estamos calculando el área bajo la curva la cual se ve de color azul:

sec <- seq(-4, 4, length = 100) * desv_est + promedio
normal <- dnorm(sec, promedio, desv_est)

normal %>% 
  as.data.frame() %>% 
  rename(valor = ".") %>% 
  ggplot() +
  aes(sec, valor) +
  geom_line() +
  theme(axis.text.y = element_blank()) +
  xlab("Estatura") +
  ylab("") +
  ggtitle("Distribución normal") +
  geom_area(aes(x = ifelse(sec > 1.6 & sec <= 1.95, sec, 0)), fill = "blue") +
  xlim(min(sec), max(sec)) +
  labs(subtitle = paste("P(1.6 < x <= 1.95) =", probabilidad)) +
  theme_classic()

# Podemos graficar un diagrama Q-Q, el cual es un diagrama de dispersión 
# creado al trazar dos conjuntos de cuantiles uno contra el otro.
# La función stat_qq(x) crea un diagrama Q-Q normal.

heights %>% 
  filter(sex == "Male") %>%
  mutate(estatura = height/39.37) %>%
  ggplot() +
  aes(sample = estatura) + 
  stat_qq() +
  stat_qq_line() + 
  theme_bw()

# La inspección visual no siempre es confiable. Es posible utilizar 
# una prueba de significación que compare la distribución de la muestra 
# con una normal para determinar si los datos muestran o no una desviación
# grave de la normalidad. El test más utilizado para estas pruebas es la
# prueba de normalidad de Shapiro-Wilk.

shapiro.test(hombres)


# 7. El teorema del límite central ####

# El teorema que garantiza esto se llama Ley de los Grandes Números (LLN) , 
# un resultado fundamental probado por el matemático Jacob Bernoulli en 1713.

# Tomemos tres muestras de diferentes tamaños, a saber, 10, 20 y 50

set.seed(1727498)
e10 <- rexp(n = 10, rate = 0.2)
e20 <- rexp(n = 20, rate = 0.2)
e50 <- rexp(n = 50, rate = 0.2)

# Si examinamos las muestras, podemos ver cómo cambia el comportamiento de una 
# distribución exponencial a medida que aumenta el tamaño de la muestra:

par(mfrow = c(1, 3))  # plot one one page, in 3 columns)
hist(e10, xlim = c(0, 30), 
     xlab = "x", main = "10 Exp(0.2) observations")
hist(e20, xlim = c(0, 30), 
     xlab = "x", main = "20 Exp(0.2) observations")
hist(e50, xlim = c(0, 30), 
     xlab = "x", main = "50 Exp(0.2) observations")

# Veamos si los datos de cada muestra tienen una distribución 
# aproximadamente normal.

par(mfrow = c(1, 3))

qqnorm(e10, pch = 16, col = rgb(0, 0, 0, alpha = 0.5), 
       main = "Q-Q Plot: 10 Exp(0.2) observations")
qqline(e10, col = rgb(1, 0, 0, 0.6))

qqnorm(e20, pch = 16, col = rgb(0, 0, 0, alpha = 0.5), 
       main = "Q-Q Plot: 20 Exp(0.2) observations")
qqline(e20, col = rgb(1, 0, 0, 0.6))

qqnorm(e50, pch = 16, col = rgb(0, 0, 0, alpha = 0.5), 
       main = "Q-Q Plot: 50 Exp(0.2) observations")
qqline(e50, col = rgb(1, 0, 0, 0.6))


# Consideremos las medias de cada una de estas muestras

c(mean(e10), mean(e20), mean(e50))

#  Pruebe una nueva muestra de tamaño 10:

mean(rexp(n = 10, rate = 0.2))

# El valor de la media muestral puede ser diferente cada vez que
# ejecutamos el experimento, ¡así que la media muestral también 
# es una variable aleatoria! Tiene una distribución, con 
# una media y una varianza.

# ¿cuál es la distribución de la media muestral? 
# ¿Es la misma que la distribución de la variable aleatoria original? 
#¿O diferente?

# simulemos la distribución de la media muestra para responder la pregunta

# necesitaremos extraer muestras repetidas de la variable aleatoria 
# y calcular la media de cada muestra.

# replicate()

# La distribución resultante se denomina 
# “distribución muestral de la media muestral”.

set.seed(81793)
e10bar <- replicate(n = 1000, mean(rexp(n = 10, rate = 0.2)))
e20bar <- replicate(n = 1000, mean(rexp(n = 20, rate = 0.2)))
e50bar <- replicate(n = 1000, mean(rexp(n = 50, rate = 0.2)))


e1000bar <- replicate(n = 1000, mean(rexp(n = 1000, rate = 0.2)))

# Observemos el histograma que resulta 

par(mfrow = c(1, 3))
hist(e10bar, breaks = 10,
     xlab = "x", main = "1000 means from samples of size 10")
hist(e20bar, breaks = 10,
     xlab = "x", main = "1000 means from samples of size 20")
hist(e50bar, breaks = 10,
     xlab = "x", main = "1000 means from samples of size 50")


hist(e1000bar, breaks = 10,
     xlab = "x", main = "1000 means from samples of size 50")

# ¿Es esta una distribución normal? Para responder eso, 
# podemos usar un gráfico QQ:

par(mfrow = c(1, 3))
qqnorm(e10bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 10",
       sub = "1000 repetitions")
qqline(e10bar, col = rgb(1, 0, 0, 0.6))
qqnorm(e20bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 20",
       sub = "1000 repetitions")
qqline(e20bar, col = rgb(1, 0, 0, 0.6))
qqnorm(e50bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 50",
       sub = "1000 repetitions")
qqline(e50bar, col = rgb(1, 0, 0, 0.6))


qqnorm(e1000bar, pch = 16, col = rgb(0, 0, 0, alpha = 0.5),
       main = "Sample size = 1000",
       sub = "1000 repetitions")
qqline(e1000bar, col = rgb(1, 0, 0, 0.6))

kruskal.test(e1000bar)

?kruskal.test


e1000bar


shapiro.test(e1000bar)

# La distribución muestral de la media de la muestra parece relativamente 
# normal incluso con muestras de tamaño 10, pero la aproximación 
# a la normalidad mejora sustancialmente cuando el tamaño de la 
# muestra aumenta a 50.

# ¡Esto no es una coincidencia!

# El teorema del límite central (CLT) establece, aproximadamente, que 
# para la mayoría de las distribuciones que nos interesan, 
# a medida que aumentamos el tamaño de muestra utilizado para
# calcular la media muestral, la distribución muestral de la 
# media muestral se volverá cada vez más normal.

# 8. Materiales consultados ####

browseURL("https://uw-statistics.github.io/Stat311Tutorial/exploring-distributions-of-random-variables-through-simulation.html")

