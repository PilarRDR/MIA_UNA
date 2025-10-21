# Titulo: EDA e imputación de datos perdidos ####

# Propósito: Los conjuntos de datos pueden presentar valores faltantes, 
# existen métodos propuestos para predecir esos valores. 
# Estos procedimientos reciben el nombre de imputación de datos, 
# En esta actividad practica veremos una in traducción al problema y la aplicación 
# de algunas soluciones. 

# Ademas, se muestran algunas funciones para un análisis exploratorio (EDA)
# de patrones de datos perdidos 

# Para comprender de que se trata, veremos un ejemplo de 
# Imputación de datos faltantes con R; Paquete MICE

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de implementación del script: "2022-10-11 14:41:44 -03" ----
# Fecha de ultima modificación: "2022-10-11 14:41:44 -03" ----


# Preámbulo 

# Instalar paquetes 

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse)

if (!require('rio'))
  install.packages("rio")
library(rio) # import/export

if (!require('mice'))
  install.packages("mice")
library(mice) # missing data imputation

if (!require('VIM'))
  install.packages("VIM")
library(VIM)

if (!require('naniar'))
  install.packages("naniar")
library(naniar) # assess and visualize missingness



# 0. Fuentes consultadas y lecturas recomendadas ####

browseURL("https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/")

browseURL("https://datascienceplus.com/imputing-missing-data-with-r-mice-package/")

browseURL("https://stefvanbuuren.name/fimd/ch-introduction.html")

browseURL("https://bookdown.org/mwheymans/bookmi/single-missing-data-imputation.html")

browseURL("https://rmisstastic.netlify.app/tutorials/erler_course_multipleimputation_2018/erler_practical_mice_2018#getting_to_know_the_data")

browseURL("https://www.jstatsoft.org/article/view/v045i03")

# 1. Importar un conjunto de datos ####

dat <- read.csv(url("https://goo.gl/4DYzru"), header=TRUE, sep=",") 

dat %>% 
  View()

head(dat)

tail(dat)

dim(dat)



# 2. Verificar datos perdidos ####

is.na(dat) %>% View()

sapply(dat, function(x) sum(is.na(x)))

# Guardemos los datos originales en memoria 

original <- dat

# 3. Transformar datos: agregando datos perdidos ####

set.seed(10)
dat[sample(1:nrow(dat), 20), "Cholesterol"] <- NA
dat[sample(1:nrow(dat), 20), "Smoking"] <- NA
dat[sample(1:nrow(dat), 20), "Education"] <- NA
dat[sample(1:nrow(dat), 5), "Age"] <- NA
dat[sample(1:nrow(dat), 5), "BMI"] <- NA

# Veamos los datos perdidos ahora 

is.na(dat) %>% View()

sapply(dat, function(x) sum(is.na(x)))


# Transformar variables en factores o numéricas 

str(dat) # estructura de los datos

library(dplyr) 
dat <- dat %>%
  mutate(
    Smoking = as.factor(Smoking),
    Education = as.factor(Education),
    Cholesterol = as.numeric(Cholesterol)
  )

# Veamos la estructura de los datos ahora

str(dat)

# 4. Imputación ####

# Para imputar los valores faltantes, el paquete de 'mice' usa un 
# algoritmo de tal manera que usa información de otras variables en 
# el conjunto de datos para predecir e imputar los valores faltantes. 

# Veamos que métodos son mejores y que variables predicen otras variables 

library(mice)

?mice


init = mice(dat, maxit=0) 

meth = init$method
predM = init$predictorMatrix

predM # Matriz de predictores 

meth # Métodos 

# Para excluir una variable como predictor 

predM[, c("BMI")]=0

predM

# Si desea omitir una variable de la imputación, use el código a continuación

meth[c("Age")]=""

# Ahora especifiquemos los métodos para imputar los valores faltantes. 
# Existen métodos específicos para variables continuas, binarias y ordinales.

?mice # Veamos antes los métodos disponibles 

meth[c("Cholesterol")]="norm" 
meth[c("Smoking")]="logreg" 
meth[c("Education")]="polyreg"

# Ahora podemos ejecutar la imputación múltiple (m=5)

set.seed(103)
imputed = mice(dat, method=meth, predictorMatrix=predM, m=5)

class(imputed)

# Crear un conjunto de datos después de la imputación 

imputed <- complete(imputed)

# Veamos los valores perddos 

sapply(imputed, function(x) sum(is.na(x)))

# 5. Rendimiento o exactitud ####

# podemos comprobar la exactitud de la imputación

# Al ser datos simulados se espera una baja exactitud 

# Colesterol

original$Cholesterol


Observed <- original$Cholesterol[is.na(dat$Cholesterol)]

predicted <- imputed$Cholesterol[is.na(dat$Cholesterol)]

mean(Observed, na.rm = T)
mean(predicted)

plot(predicted,Observed)

hist(predicted-Observed)

cor.test(predicted,Observed)

# Smoking
Observed  <- original$Smoking[is.na(dat$Smoking)] 
predicted <- imputed$Smoking[is.na(dat$Smoking)] 
table(Observed)
table(predicted)

table(predicted,Observed)

plot(table(predicted,Observed))

# 5. Conjunto de datos real ####

airquality %>% View()

# Vamos a eliminar algunos puntos de datos del conjunto de datos

data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

# echamos un vistazo a los datos

data <- data[-c(5,6)]
summary(data)

# 6. Clasificación rápida de datos faltantes ####

# MCAR (missing completely at random): falta completamente al azar. 
# Este es el escenario deseable en caso de que falten datos.

# MNAR: (missing not at random) La falta de datos no aleatorios es un 
# problema más grave.

# Suponiendo que los datos sean MCAR, demasiados datos faltantes 
# también pueden ser un problema. 

# verificamos las columnas y las filas donde falta
# más del 5% de los datos usando una función simple

data %>% dim()

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

# al ozono le falta casi el 25 % de los puntos de datos,
# por lo tanto, podríamos considerar eliminarlo del análisis o recopilar
# más mediciones. Las otras variables están por debajo del umbral del 5%, 
# por lo que podemos mantenerlas. 

# Para las filas (casos), la falta de un solo atributo conduce a 
# un 25 % de datos faltantes por muestra. 
# Las muestras a las que les faltan 2 o más características (>50 %)
# deben descartarse si es posible.

# 7. observar el patrón de datos faltantes (EDA) ####

# La función md.pattern() ayuda a comprender mejor el patrón 
# de datos faltantes

md.pattern(data)

# El resultado nos dice que 104 casos están completas, 
# 34 casos solo pierden la medición de ozono, 4 muestras solo pierden el valor 
# Solar.R y así sucesivamente.


# Otra librería y función alternativa es aggr en VIM

library(VIM)
aggr_plot <-
  aggr(
    data,
    col = c('navyblue', 'red'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(data),
    cex.axis = .7,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )

# 70 % de las muestras no les falta
# al 22 % les falta el valor de ozono y 
# las restantes muestran otros patrones faltantes

# Enfoque bivariado para datos numéricos 

marginplot(data[c(1,2)])

# caja roja de la izquierda; distribución de Solar.R sin Ozono
# caja azul muestra la distribución de los puntos de datos restantes
# Si se cumple el supuesto de MCAR, entonces esperamos que 
# los diagramas de caja roja y azul sean muy similares.

# package  naniar

pct_miss(data) # % de valores perdidos 

pct_miss_case(data) # % de filas con valores perdidos 

pct_complete_case(data) # % de filas completas 

# Visualizando la ausencia (EDA) 

gg_miss_var(data, show_pct = TRUE)

vis_miss(data)

# 8. Imputación de los datos faltantes ####

?mice

tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

# recuperar el conjunto de datos completo 

completedData <- complete(tempData,1)

# Los valores que faltan se han reemplazado con los valores imputados 
# en el primero y los cinco conjuntos de datos.

# 9. Visualizar la distribución de los datos originales e imputados ####

xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)

# Ver si magenta (imputados) coincida con la forma de los azules (observados)

# density plot:

densityplot(tempData)

# datos imputados se muestra en magenta, mientras que la densidad de los 
# datos observados se muestra en azul.

# Scatter plot, distribuciones de las variables como puntos individuales.

stripplot(tempData, pch = 20, cex = 1.2)

