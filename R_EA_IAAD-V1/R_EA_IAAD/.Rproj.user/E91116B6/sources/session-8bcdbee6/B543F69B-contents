
# Actividades prácticas Unidad 2. Probabilidad básica ----


# Fuente: https://bookdown.org/dparedesi/data-science-con-r/probabilidades-discretas.html
# Propósito: Este script tiene como objetivo ilustrar ejemplos basicos de
# implementación de problemas de probabilidad en R
# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py


# Cálculo usando la definición matemática ----


# si tenemos en un salón sentados a 4 mujeres y 6 hombres e hiciésemos
# un sorteo de 1 premio intuitivamente sabríamos que la probabilidad de que
# el sorteado sea hombre es de 60%.
# La probabilidad que obtuvimos por intuición en el ejemplo anterior
# se puede expresar de la siguiente forma:
# P(A) = Veces que el evento A se puede datos/Total de resultados posibles
# P(hombres) = 6/10
P_h = 6 / 10
P_h
# Simulación de Montecarlo para variables discretas ----


# La simulación o método de Montecarlo es un método estadístico utilizado
# para resolver problemas matemáticos complejos a través de la generación
# de variables aleatorias.

# Usaremos simulación Montecarlo para estimar la proporción qué obtendríamos
# si repitiésemos este experimento de forma aleatoria un número determinado
# de veces. Es decir, la probabilidad del evento usando esta estimación sería
# la proporción de veces en que ocurrió ese evento en nuestra simulación.

estudiantes <- c(
  "mujer",
  "mujer",
  "mujer",
  "mujer",
  "hombre",
  "hombre",
  "hombre",
  "hombre",
  "hombre",
  "hombre"
) # S


sample(estudiantes, 1) # usemos la función sample(), para escoger uno al azar.
# También podríamos usar la función rep() para crear más rápido el vector
# estudiantes.

estudiantes <- rep(c("mujer", "hombre"), times = c(4, 6))

estudiantes


# Ahora tenemos que simular un determinado número de veces el experimento de
# sacar un elemento aleatorio.

estudiantes <- rep(c("mujer", "hombre"), times = c(4, 6))
num_veces <- 100
resultados <- replicate(num_veces, {
  sample(estudiantes, 1)
})

resultados

# Ahora usaremos la función table() para transformar nuestro vector resultados
# en una tabla resumida que nos muestre cuantas veces apareció cada valor.

tabla_resultados <- table(resultados)

prop.table(tabla_resultados) # prop.table() para saber la proporción de cada valor

# Por ejemplo, repliquemos este experimento ahora 10000 veces.

estudiantes <- rep(c("mujer", "hombre"), times = c(4, 6))
num_veces <- 10000
resultados <- replicate(num_veces, {
  sample(estudiantes, 1)
})

tabla_resultados <- table(resultados)
prop.table(tabla_resultados)

# Valor esperado: Finalmente, para este ejemplo sencillo también podríamos
# haber utilizado la función mean(). Si bien esto nos calcula el promedio
# de un conjunto de números, podríamos convertir nuestro vector estudiantes
# a valores númericos, donde cada valor lo convirtamos en 1 o 0 dependiendo
# de alguna condicion.

mean(estudiantes == "hombre")

library(pacman)
# Primero instalamos el paquete gtools
pacman::p_load("gtools")
data_scientists <-
  c(
    "Jenny",
    "Freddy",
    "Yasan",
    "Iver",
    "Pamela",
    "Alexandra",
    "Bladimir",
    "Enrique",
    "Karen",
    "Christiam"
  )

resultados <- permutations(10, 3, v = data_scientists)

# Total de resultados: nrow(resultados)
total <- nrow(resultados)

# Probabilidad de que Frede gane:

mean(resultados[, 1] == "Freddy" & resultados[, 2] == "Pamela")
View(resultados)
resultados <- combinations(10, 2, v = data_scientists)

# Total de resultados:

nrow(resultados)
# Probabilidad:
mean((resultados[, 1] == "Pamela" & resultados[, 2] == "Enrique") |
       (resultados[, 1] == "Enrique" & resultados[, 2] == "Pamela")
)
n <- 10000
resultado <- replicate(n, {
  equipo <- sample(data_scientists, 2)
  cumple_condicion <-
    (equipo[1] == "Pamela" & equipo[2] == "Enrique") |
    (equipo[2] == "Pamela" & equipo[1] == "Enrique")
  cumple_condicion
})

mean(resultado)

num_veces <- 10 * 2 ^ (1:17)

num_veces

probabilidad_por_muestra <- function(n) {
  resultado <- replicate(n, {
    equipo <- sample(data_scientists, 2)
    cumple_condicion <-
      (equipo[1] == "Pamela" & equipo[2] == "Enrique") |
      (equipo[2] == "Pamela" & equipo[1] == "Enrique")
    cumple_condicion
  })
  mean(resultado)
}
# Probabilidad utilizando funciones:

probabilidad_por_muestra(10000)
prob <- sapply(num_veces, probabilidad_por_muestra)
prob
probabilidades <- data.frame(n = num_veces,
                             probabilidad = prob)


probabilidades %>%
  ggplot() +
  aes(n, probabilidad) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  xlab("# de veces del experimento")

probabilidades %>%
  ggplot() +
  aes(n, probabilidad) +
  geom_line() +
  geom_point() +
  xlab("# de veces del experimento") +
  scale_x_continuous(trans = "log2") +
  geom_hline(yintercept = 0.022222,
             color = "blue",
             lty = 2)

#  Caso: Cumpleaños en clases ----

# Repasemos los conceptos aprendidos con otro ejemplo. En una clase 
# de Data Science for Managers hay 50 estudiantes. Utilizando simulación 
# de Montecarlo estimemos cuál es la probabilidad de qué hayan 
# al menos dos personas que cumplan años el mismo día.
# (Obviemos a los que cumplen años el 29 de febrero).


dias <- 1:365

# Generemos una muestra aleatoria de 50 números del vector dias

colegas <- sample(dias, 50, replace = TRUE)

colegas

# Para validar si alguno de los valores se repite usaremos la función duplicated() 
# que nos valida si dentro del vector hay valores duplicados:

duplicated(colegas)

any(duplicated(colegas))

# Simulación de Montecarlo con 10 mil repeticiones

num_veces <- 10000
resultados <- replicate(num_veces, {
  colegas <- sample(dias, 50, replace = TRUE)
  
# Retorna un valor lógico de si hay duplicados
  
  any(duplicated(colegas))
})

# Probabilidad:

mean(resultados)

# Creamos la función

estima_probabilidad <- function(clase, num_veces = 10000) {
  resultados <- replicate(num_veces, {
    # Retorna un vector lógico
    colegas <- sample(dias, clase, replace = TRUE)
    any(duplicated(colegas))
  })
  # Probabilidad:
  mean(resultados)
}

estima_probabilidad(25)

clases <- 1:80

# Estimamos la probabilidad dependiendo del número de estudiantes por salón

prob <- sapply(clases, estima_probabilidad)
prob

probabilidades <- data.frame(n = clases,
                             probabilidad = prob)

probabilidades %>%
  ggplot() +
  aes(n, probabilidad) +
  geom_point() +
  xlab("Número de alumnos en cada clase")