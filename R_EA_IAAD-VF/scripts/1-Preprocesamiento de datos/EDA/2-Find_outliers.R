# Titulo: Detección y tratamiento de valores atípicos 

# Propósito: Este script introduce algunos métodos para la 
# detección y el tratamiento de valores atípicos

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de implementación del script: "2022-10-07 21:40:04 -03"
# Fecha de ultima modificación: "2022-10-07 21:40:04 -03"


# Preámbulo 

# Instalar paquetes 

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse)

if (!require('outliers'))
  install.packages("outliers")
library(outliers)

# 0. Consultas ####
# Fuentes consultadas y lecturas recomendadas 
browseURL("https://www.reneshbedre.com/blog/find-outliers.html")

browseURL("https://statsandr.com/blog/outliers-detection-in-r/")

browseURL("https://www.r-bloggers.com/2016/12/outlier-detection-and-treatment-with-r/")

browseURL("https://towardsdatascience.com/why-1-5-in-iqr-method-of-outlier-detection-5d07fdc82097#:~:text=Well%2C%20as%20you%20might%20have,perceived%20as%20outlier(s).")

# 1. Valores atípicos (outlier) ####

# Un valor atípico es una observación inusual que no es consistente con las
# observaciones restantes en un conjunto de datos

# Un análisis de datos busca patrones representativos en un conjunto de datos. 
# Los valores atípicos pueden sesgar sumarios estadísticos representativos. 

# - la presencia de valores atípicos distorsiona la media y la desviación 
#   estándar del conjunto de datos

# - la presencia de valores atípicos puede afectar la distribución normal 
#   del conjunto de datos, lo cual es una suposición básica en la mayoría 
#   de las pruebas paramétricas basadas en hipótesis

# 2. Métodos para encontrar valores atípicos ####

# a. Gráfico: histograma, diagrama de dispersión y diagrama de caja
# b. Media y desviación estándar
# c. Mediana y Desviación Absoluta de la Mediana (MAD) 
# d. Rango intercuartílico (IQR)
# e. Test Q de Dixon 
# f. Prueba de Grubb


# 3. Histograma, diagrama de dispersión y diagrama de caja ####

# RStudio console 
library(gridExtra)
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)

x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14)

hist(x, main = "Histogram")

boxplot(x, main = "Boxplot")

qqnorm(x, main = "Normal Q-Q plot")

?qqnorm

y = rnorm(n=30,mean=10,sd=3)

qqnorm((y-mean(y))/sd(y), main = "Normal Q-Q plot")

# ¿qué son los "cuantiles"? (="percentiles")
# son puntos en sus datos por debajo de los cuales cae una cierta 
# proporción de sus datos
# El 95 por ciento de los datos se encuentran por debajo de 1,64. 

# Veamos un ejemplo en código 

quantile(rnorm(200),probs = seq(0.01,0.99,0.01))

# Los gráficos QQ toman sus datos de muestra, los clasifican en 
# orden ascendente y luego los representan frente a los cuantiles 
# calculados a partir de una distribución teórica.

# histogram, Q-Q plot, and boxplot
par(mfrow = c(1, 3))
hist(x, main = "Histogram")
boxplot(x, main = "Boxplot")
qqnorm(x, main = "Normal Q-Q plot")

?hist

# 4. Media y desviación estándar ####

# Para aplicarlo definimos un umbral para valores atípicos tal que 

# T_{min},T_{max} = mean +-(a*SD)

# donde a es el factor umbral para definir el numero de SD
# En general mas de tres veces la SD con respecto a la media 
# se considera atípico, es común utilizar a=2.  

#  funciona bien si los datos se distribuyen normalmente y 
# cuando hay porcentajes muy bajos de valores atípicos

x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)

# calculamos la media y desviación estandar 
mean = mean(x)
std = sd(x)

# Definimos los umbrales 
a=2

Tmin = mean-(a*std)
Tmax = mean+(a*std)

# ahora podemos encontrar los valores atipicos
x[which(x < Tmin | x > Tmax)]

# Cambie el factor de umbral a 2 y describa el resultado 

# Removemos los valores atípicos 
x1 = x[which(x > Tmin & x < Tmax)]
x1

mean(x)
mean(x1)

sd(x1)

# Visualicemos el resultado 

# histogram, Q-Q plot, and boxplot
par(mfrow = c(1, 3))
hist(x, main = "Histogram")
boxplot(x, main = "Boxplot")
qqnorm(x, main = "Normal Q-Q plot")

par(mfrow = c(1, 3))
hist(x1, main = "Histogram")
boxplot(x1, main = "Boxplot")
qqnorm(x1, main = "Normal Q-Q plot")

shapiro.test(x1)

?shapiro.test


shapiro.test(x)


# 5. Mediana y Desviación Absoluta de la Mediana ####

# Qué es la mediana? 
browseURL("https://es.wikipedia.org/wiki/Mediana_(estad%C3%ADstica)")

# La mediana representa el valor de la variable de posición central 
# en un conjunto de datos ordenados

# Para aplicar este método primero necesitamos calcular la 
# Desviación Absoluta de la Mediana, esta se calcula sobre 
# los residuos (desviaciones) de la mediana de los datos, 
# la MAD es la mediana de sus valores absolutos .

# MAD = b * median(|x_i - median(x)|)

# donde b es un factor de escala que se establece en 1.4826 cuando los datos 
# se distribuyen normalmente.

# Definimos un umbral para los valores atípicos dado por 

# # T_{min},T_{max} = median +-(a*MAD)


# conjunto de ejemplo
x = data.frame(
  x1 = rnorm(20,10,2),
  x2 = rnorm(20,10,2)
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

x$x1[c(4,5,6)]

x$x1[c(4,5,6)] = c(x$x1[4] + 19*sd(x$x1),
                 x$x1[5] + 5*sd(x$x1),
                 x$x1[6] + 30*sd(x$x1))

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

t.test(x$x1,x$x2)
# Visualicemos 

par(mfrow = c(1, 3))
hist(x$x1, main = "Histogram")
boxplot(x$x1, main = "Boxplot")
qqnorm(x$x1, main = "Normal Q-Q plot")

# Veamos la variable x2 como control 
par(mfrow = c(1, 3))
hist(x$x2, main = "Histogram")
boxplot(x$x2, main = "Boxplot")
qqnorm(x$x2, main = "Normal Q-Q plot")

# Ahora apliquemos el método de 
# Mediana y Desviación Absoluta de la Mediana 

# Calculemos la mediana 
med = median(x$x1)
# Calculemos la desviación absoluta 
abs_dev = abs(x$x1-med)
# calculemos la MAD
mad = 1.4826 * median(abs_dev)

# Calculemos el umbral para valores atípicos 
Tmin = med-(3*mad) 
Tmax = med+(3*mad) 

# encontremos los valores atipicos 
out = x$x1[which(x$x1 < Tmin | x$x1 > Tmax)]
out 

# Remueva los valores atípicos 
x$x1[which(x$x1 < Tmin | x$x1 > Tmax)] = c(NA,NA,NA)

head(x)

x

# Visualicemos 

par(mfrow = c(1, 3))
hist(x$x1, main = "Histogram")
boxplot(x$x1, main = "Boxplot")
qqnorm(x$x1, main = "Normal Q-Q plot")

x %>% summarise(
  n = n(),
  mean = mean(x1, na.rm = T),
  sd = sd(x1, na.rm = T)
)

x %>% summarise(
  n = n(),
  mean = mean(x2),
  sd = sd(x2)
)

t.test(x$x1,x$x2)

# 6. Rango intercuartílico (IQR)

browseURL("https://towardsdatascience.com/why-1-5-in-iqr-method-of-outlier-detection-5d07fdc82097#:~:text=Well%2C%20as%20you%20might%20have,perceived%20as%20outlier(s).")

#  IQR es una diferencia entre los puntos de datos que se ubican en 
# el percentil 25 (primer cuartil o Q1) y el percentil 75 
# (tercer cuartil o Q3) en el conjunto de datos ( IQR = Q3 - Q1 ).

# Los umbrales para valores atípicos se definen como 

# T_{min} = Q1 -(c*IQR)
# T_{max} = Q3 +(c*IQR)

# c es un factor de escala tal que 1.5 detecta valores atípicos leves 
# y 3 detecta valores atipicos extremos 

# El método IQR es útil cuando los datos no siguen una distribución normal.
x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,29,35)
boxplot(x, horizontal = TRUE)

# Vamos los valores Q1, Q3, e IQR
summary(x)

Q1 = summary(x)[2]
Q1
Q3 = summary(x)[5]
Q3

iqr = Q3-Q1

iqr
#  IQR
IQR(x)

View(IQR)


# definimos los umbrales para atípicos 
Tmin =Q1-(1.5*IQR(x)) 
Tmax = Q3+(1.5*IQR(x)) 

# encontremos los atipicos 
x[which(x < Tmin | x > Tmax)]


# ahora podemos removerlos 
x_1 = x[which(x > Tmin & x < Tmax)]

x_1

summary(x_1)

summary(x)

# - Alrededor del 68,26 % de todos los datos se encuentran dentro 
# de una desviación estándar ( <σ ) de la media (μ)

# - Alrededor del 95,44 % de todos los datos se encuentran dentro de 
# dos desviaciones estándar ( 2σ ) de la media (μ)

# - Alrededor del 99,72 % de todos los datos se encuentran dentro de las 
# tres desviaciones estándar ( <3σ ) de la media (μ)

# - Y el 0,28% restante de todos los datos se encuentra fuera de las tres 
# desviaciones estándar ( >3σ ) de la media (μ), teniendo en 
# cuenta ambos lados, la pequeña región roja de la figura. Y esta 
# parte de los datos se considera como valores atípicos .

# Los cuartiles primero y tercero, Q1 y Q3 , se encuentran
# a -0.675σ y +0.675σ de la media, respectivamente.

# 6. Test Q de Dixon ####

# La prueba Q de Dixon analiza la siguiente hipótesis,

# Hipótesis nula (H 0 ) : El valor máximo o mínimo no es un 
# valor atípico (no hay valores atípicos)

# Hipótesis alternativa (H a ) : El valor máximo o mínimo es 
# un valor atípico (hay un valor atípico)

# La hipótesis nula se rechaza cuando el estadístico Q es mayor
# que el valor Q crítico ( Q teórico que se espera que ocurra con 
# un nivel de significación del 5 % y un tamaño de muestra dado). 
# Existen múltiples variantes de la prueba Q de Dixon según 
# los tamaños de muestra.


y = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14)

dixon.test(y)

View(dixon.test)

# 7. Prueba de Grubb #### 

?grubbs.test

# La prueba de Grubb se usa para identificar un solo valor atípico 
# (valor mínimo o máximo en un conjunto de datos) en un 
# conjunto de datos univariado. A diferencia de la prueba Q de Dixon, 
# la prueba de Grubb debe usarse cuando el tamaño de la muestra (n) > 6 
# y los datos se distribuyen normalmente. Si n ≤ 6, la prueba de 
# Grubb puede encontrar los no atípicos como atípicos.

# Hipótesis nula ( H 0 ): El valor máximo o mínimo no es un valor 
# atípico (no hay ningún valor atípico)

# Hipótesis alternativa ( H a ): El valor máximo o mínimo es 
# un valor atípico (hay un valor atípico)

x = c(10,4,6,8,9,8,7,6,12,14,11,9,8,4,5,10,14,12,15,7,10,14,24,28)

grubbs.test(x)

