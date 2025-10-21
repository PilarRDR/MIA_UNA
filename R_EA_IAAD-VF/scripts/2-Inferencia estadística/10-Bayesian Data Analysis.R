# Titulo: Análisis de regresión bayesiana en R

# Objetivo: introducir y e implementar modelos de regresión con un enfoque
# de inferencias bayesiana

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de complementación del script: "2025-10-10 16:02:52 -03" ----
# Fecha de ultima modificación: "2025-10-10 16:02:52 -03" ----

# 0. Intro ----

# Existen muchas buenas razones para analizar datos con métodos bayesianos.
#Sin embargo, históricamente, estos métodos han requerido un alto consumo
#computacional y son difíciles de implementar, requiriendo el conocimiento
#de plataformas y lenguajes de programación a veces complejos, como WinBUGS ,
# JAGS o Stan . Sin embargo, los paquetes de R más recientes, como
# r2jags , rstanarm y brms, han simplificado considerablemente la creación
# de modelos de regresión bayesiana en R.

# Con un proposito demostrativo vamos a reproducir un código para ejecutar
# algunos modelos de regresión simples con el paquete brms.

# 1. Instalación de paquetes ----

require(pacman)

pacman::p_load(brms, tidyverse, ggplot2, 
               gdata, dplyr, parallel, cowplot)

# 2. Importar datos

# Utilizaré el conjunto de datos de diamantes de ggplot2. Dado que es
# bastante grande, vamos a crear un subconjunto.

set.seed(2018)
data(diamonds)

diamonds.full <- na.omit(diamonds) %>% drop.levels # remove missing data

diamonds.full$rows <- as.numeric(rownames(diamonds.full))

diamonds.train <- diamonds.full %>% group_by(color, clarity) %>% 
  sample_n(size = 30) # subset

diamonds.keep <-   filter(diamonds.full, 
                          !rows %in% diamonds.train$rows) # remove row in training set

diamonds.test <- diamonds.keep[sample(nrow(diamonds.keep), 20000), ]

# get rid of zero values and outliers

diamonds.train <- diamonds.train %>% 
  filter(x > 0, y > 0, z > 0, price > 0, price < max(diamonds$price))

diamonds.test <- diamonds.test %>% 
  filter(x > 0, y > 0, z > 0, price > 0, price < max(diamonds$price))

# 3. Entorno de computación  ----

# Establecer el número de núcleos: Dado que estos análisis a veces pueden 
# ser algo lentos, se recomienda configurar el número máximo de 
# núcleos disponibles. Puede comprobar cuántos núcleos tiene disponibles 
# con el siguiente código.

# No configures más núcleos de los que realmente tiene tu máquina. 
# Puedes verificarlo con

parallel::detectCores()


options(mc.cores = 8)  # Usa 8 núcleos

getOption("mc.cores", 1)

# Utilizaremos este fragmento de código nuevamente cuando ejecutemos nuestros
# modelos y hagamos la selección de modelos.

#  revisar el uso de memoria

gc()  # Muestra uso de memoria por tipo de objeto
object.size(diamonds.full)  # Tamaño de un objeto específico
pryr::mem_used()  # Requiere instalar el paquete 'pryr'

# 4. Examinar y visualizar datos ----

# Lo que me interesa es la precisión con la que las propiedades de un diamante 
# predicen su precio. ¿Importa el tamaño del diamante? ¿Cuál es la 
# importancia relativa del color frente a la claridad? Dado que la
# respuesta a ambas preguntas es casi con certeza afirmativa, veamos si 
# los modelos nos indican lo mismo.

help(diamonds)

# Primero, graficamos el precio como función del quilate, una métrica bien 
# conocida que mide la calidad del diamante. Aquí, graficamos 
# los datos brutos y luego transformamos logarítmicamente ambas variables.


plot_grid(
  ggplot()+
    
    geom_point(data = diamonds.train, aes(y = price, x = carat))+
    
    geom_smooth()+ theme_minimal(), 
  
  ggplot()+
    
    geom_point(data = diamonds.train, aes(y = log(price), x = log(carat)))+
    
    geom_smooth()+ theme_minimal(), ncol = 2
  
) 

# Primero, visualicemos cómo la claridad y el color influyen en el precio. 
# Aquí, primero trazaré diagramas de caja del precio por nivel de
# claridad y color, y luego el precio vs. quilates, con colores que 
# representan los niveles de claridad y color.

plot_grid(
  
  ggplot(data = diamonds.train, aes(y = log(price), x = clarity)) + geom_boxplot(), 
  
  ggplot(data = diamonds.train, aes(y = log(price), x = color)) + geom_boxplot(), 
  
  ggplot(data = diamonds.train, aes(y = log(price), x = log(carat), color = clarity)) + 
    
    geom_point() + geom_smooth(method = "lm") + 
    
    ylim(c(min(log(diamonds$price)),  max(log(diamonds$price)))), 
  
  ggplot(data = diamonds.train, aes(y = log(price), x = log(carat), color = color)) + 
    
    geom_point() + geom_smooth(method = "lm") + 
    
    ylim(c(min(log(diamonds$price)), max(log(diamonds$price)))), 
  
  ncol = 2
)

# A partir de estos gráficos, parece que puede haber diferencias en las
# intersecciones y pendientes (especialmente para la claridad) entre 
# las clases de color y claridad. Podemos modelar esto mediante un 
# modelo de efectos mixtos. Pero comencemos con una regresión múltiple
# simple.

# 5. Ajuste de modelo ---- 

# Para este primer modelo, veremos qué tan bien se correlaciona el quilate del 
# diamante con el precio.

brm.1 <- brm(log(price) ~ log(carat), 
             
             brmsfamily("gaussian"), 
             
             data = na.omit(diamonds.train), 
             
             chains = 4, #specify the number of Markov chains
             
             cores = getOption("mc.cores", 8),
             
             iter = 3000, warmup = 1500, thin = 5,
             
             prior =  c(prior(normal(0, 3), "b"), # set normal prior on regression coefficients (mean of 0, location of 3)
                        
                        prior(normal(0, 3), "Intercept"))) # set normal prior on intercept (mean of 0, location of 3)

# Esto puede tardar unos minutos en ejecutarse, dependiendo de
# la velocidad de su máquina.

# Una herramienta realmente fantástica para interrogar a tu modelo es 
# utilizar la función 'launch_shinystan', que puedes llamar como:

lanzar_shinystan(brm.1)

# or ahora, analizaremos un resumen de los modelos en R, así como gráficos
# de las distribuciones posteriores y las cadenas de Markov. El resumen 
# muestra que nuestras cadenas han convergido lo suficiente (rhat = 1).

# ambién podemos obtener una estimación de R-cuadrado para nuestro modelo

summary(brm.1) # note Population-Level Effects = 'fixed effects'

plot(brm.1)

bayes_R2(brm.1)

# Tenga en cuenta que el logaritmo de los quilates explica claramente gran 
# parte de la variación en el precio del diamante (como era de
# esperar), con una pendiente significativamente positiva (1,52 ± 0,01).

# 6. Validación del modelo ----

# mediante validación cruzada aproximada de dejar uno fuera

# Otra forma de obtener el ajuste del modelo es mediante una validación 
# cruzada aproximada con exclusión de uno, mediante el paquete loo , 
# desarrollado por Vehtari, Gelman y Gabry ( 2017a , 2017b ). Con 
# loo, podemos calcular un LOOIC, similar a un AIC , con el que 
# algunos lectores pueden estar familiarizados.

loo(brm.1, cores = getOption("mc.cores", 1))

# 7. Modelos de efectos mixtos ----

brm.2 <- brm(log(price) ~ log(carat) + (1|clarity), 
             
             brmsfamily("gaussian"), 
             
             data = na.omit(diamonds.train), 
             
             chains = 4, #specify the number of Markov chains
             
             cores = getOption("mc.cores", 1),
             
             iter = 3000, warmup = 1500, thin = 5,
             
             prior =  c(prior(normal(0, 3), "b"), # set normal prior on regression coefficients (mean of 0, location of 3)
                        
                        prior(normal(0, 3), "Intercept"))) # set normal prior on intercept (mean of 0, location of 3)

loo(brm.1, brm.2)


# Color 

brm.3 <- brm(log(price) ~ log(carat)  + (1|color), 
             
             brmsfamily("gaussian"), 
             
             data = na.omit(diamonds.train), 
             
             chains = 4, #specify the number of Markov chains
             
             cores = getOption("mc.cores", 1),
             
             iter = 3000, warmup = 1500, thin = 5,
             
             prior =  c(prior(normal(0, 3), "b"), # set normal prior on regression coefficients (mean of 0, location of 3)
                        
                        prior(normal(0, 3), "Intercept"))) # set normal prior on intercept (mean of 0, location of 3)

loo(brm.1, brm.2, brm.3)

# Y aquí hay un modelo con el logaritmo de quilates como efecto fijo y
# el color y la claridad como efectos a nivel de grupo.

brm.4 <- brm(log(price) ~ log(carat) + (1|color) + (1|clarity), 
             
             brmsfamily("gaussian"), 
             
             data = na.omit(diamonds.train), 
             
             chains = 4, #specify the number of Markov chains
             
             cores = getOption("mc.cores", 1),
             
             iter = 3000, warmup = 1500, thin = 5,
             
             prior =  c(prior(normal(0, 3), "b"), # set normal prior on regression coefficients (mean of 0, location of 3)
                        
                        prior(normal(0, 3), "Intercept"))) # set normal prior on intercept (mean of 0, location of 3)

loo(brm.1, brm.2, brm.3, brm.4)

bayes_R2(brm.4)

summary(brm.4)

coef(brm.4)

# Todos los modelos de efectos mixtos que hemos analizado hasta ahora solo
# permitían que las intersecciones de los grupos variaran, pero, 
# como vimos al analizar los datos, parece que los diferentes 
# niveles de nuestros grupos también podrían tener diferentes
# pendientes. Podemos especificar un modelo que permita que la 
# pendiente de la relación precio/quilates varíe tanto por color 
# como por claridad.

brm.5 <- brm(log(price) ~ log(carat)  + (1 + log(carat)|color) + (1 + log(carat)|clarity), 
             
             brmsfamily("gaussian"), 
             
             data = na.omit(diamonds.train), 
             
             chains = 4, #specify the number of Markov chains
             
             cores = getOption("mc.cores", 1),
             
             iter = 3000, warmup = 1500, thin = 5,
             
             prior =  c(prior(normal(0, 3), "b"), # set normal prior on regression coefficients (mean of 0, location of 3)
                        
                        prior(normal(0, 3), "Intercept"))) # set normal prior on intercept (mean of 0, location of 3)

loo(brm.1, brm.2, brm.3, brm.4, brm.5, cores = getOption("mc.cores", 1))

bayes_R2(brm.4)

summary(brm.4)

# 8. Predicción del modelo ----

pred.1 <- predict(brm.5, newdata = diamonds.test)

r.sq <- as.character(round(summary(lm(log(diamonds.test$price) ~ pred.1[, 1]))$r.squared, 2))

lb1 <- paste("R^2 == ", r.sq)

ggplot()+
  
  geom_point(aes(x = pred.1[,1], y = log(diamonds.test$price))) +
  
  geom_errorbarh(aes(x = pred.1[,1], y = log(diamonds.test$price), 
                     
                     xmin = pred.1[,1] - pred.1[, 2], 
                     xmax = pred.1[,1] + pred.1[, 2])) + 
  
  geom_smooth(aes(x = pred.1[,1], y = log(diamonds.test$price)), method = "lm", color = "red", lty = 2)+
  
  geom_text(aes(x=6, y=9.5, label = lb1, size = 8), parse=TRUE, show.legend = F) +
  
  xlab("Predicted") +
  
  ylab("Observed")


# Lecturas ----

browseURL("https://tem11010.github.io/regression_brms/")

browseURL("https://thinkinator.com/2016/01/12/r-users-will-now-inevitably-become-bayesians/")

browseURL("https://bookdown.org/marklhc/notes_bookdown/model-comparison-and-regularization.html")

browseURL("https://www.youtube.com/watch?v=gjrsYDJbRh0&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=9")

browseURL("https://mc-stan.org/loo/reference/loo_compare")
