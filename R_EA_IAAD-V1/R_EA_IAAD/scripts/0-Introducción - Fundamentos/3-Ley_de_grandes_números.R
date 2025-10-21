# Ley de los grandes números ----

# Ver: https://colab.research.google.com/drive/13LRTDom9dfocrTK77pfXUPU0vTIIHVv8#scrollTo=0kdFXx-PdSEV

# Veamos algunos ejemplos simulados ----

# simulando probabilidad con monedas
moneda <- c("cara", "cluz")
acumular<-NULL
n <- 10000   # numero de veces de laznar  moneda

acumular <- sample(moneda, n, replace = TRUE)

acumular

resultados <- c()

set.seed(1)
for (i in 1:100) {
  muestra <- replicate(n = i, expr = sample(x = moneda, size = 1))
  resultados[i] = prop.table(table(muestra))[1]
}

resultados

plot(resultados, type = "l")
abline(h = 0.5, col = "red")

pacman::p_load(tidyverse, ggplot2) #gramatica de manipulación de tidydata y gramatica para visializaciones (graficas)

resultados2 <- c()

set.seed(1)
for (i in 1:2000) {
  muestra2 <- replicate(n = i, expr = sample(x = moneda, size = 1))
  resultados2[i] = prop.table(table(muestra2))[1]
}

resultados2 %>%
  enframe(name = "lanzamiento", value = "proporcion") %>%
  ggplot(aes(x = lanzamiento, y = proporcion)) +
  geom_line() +
  geom_hline(yintercept = 0.5, color = "red")

resultados2 %>%
  enframe(name = "lanzamiento", value = "proporcion") %>%
  ggplot(aes(x = proporcion)) +
  geom_density() +
  geom_vline(xintercept = 0.5, color = "red")

# Veamos el ejemplo del lanzamiento de dado ----

dado <- c(1, 2, 3, 4, 5, 6) # Espacio muestral

resultados_dado <- c()

set.seed(1)
for (i in 1:200) { # cambiar a 200
  muestra_dado <- replicate(n = i, expr = sample(x = dado, size = 1))
  resultados_dado[i] = prop.table(table(muestra_dado))[1]
}

resultados_dado %>%
  enframe(name = "lanzamiento", value = "proporcion") %>%
  ggplot(aes(x = lanzamiento, y = proporcion)) +
  geom_line() +
  geom_hline(yintercept = 1/6, color = "red")


resultados_dado %>%
  enframe(name = "lanzamiento", value = "proporcion") %>%
  ggplot(aes(x = proporcion)) +
  geom_density() +
  geom_vline(xintercept = 1/6, color = "red")
