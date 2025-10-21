# Titulo: TRansformación de datos

# Propósito: Este script crea introduce formas de implementar procesos 
# de transformación de datos 

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de implementación del script: "2022-09-27 15:18:43 -04" ----
# Fecha de ultima modificación: "2022-09-27 15:18:43 -04" ----


# Preámbulo 

# Instalar paquetes 

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse)

if (!require('rio'))
  install.packages("rio")
library(rio)

# Importar y exportar datos 

View(mtcars)

starwars %>% View()

export(mtcars, "data/raw_data/mtcars.csv", ";")

export(starwars, "data/raw_data/starwars.csv",";")


# Veamos una alternativa apra importar datos de forma manual 

library(readr)
mtcars <- read_delim("data/raw_data/mtcars.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(mtcars)

# Replicar la misma actividad para el conjunto de datos starwars

# 0. cheatsheets ####

browseURL("https://www.rstudio.com/resources/cheatsheets/")

# 1. Sumario de casos ####

# Aplique funciones de resumen a las columnas para crear una nueva tabla 
# de estadísticas de resumen. Las funciones de resumen toman vectores como 
# entrada y devuelven un valor.

# 'summarise' calcula una tabla sumario  

?summarise

# funciones más utilizadas 

# de tendencia central: mean(), median()

# de dispersión: sd(), IQR(), mad()

# de rangos: min(), max(), quantile()

# de posición: first(), last(), nth(),

# Conteos: n(), n_distinct()

# Logica : any(), all()

View(mtcars)

summarise(mtcars, 
          n = n(),
          media = mean(mpg),
          median = median(mpg),
          sd = sd(mpg),
          min = min(mpg),
          max = max(mpg),
          IQR = IQR(mpg))

?IQR

# count: Cuenta el número de filas en cada grupo definido por las variables en...

View(mtcars)

count(mtcars, cyl)

# 2. Agrupar casos ####

# Use group_by(.data, …, .add = FALSE, .drop = TRUE) para crear una copia
# "agrupada" de una tabla agrupada por columnas en... Las funciones dplyr 
# manipularán cada "grupo" por separado y combinarán los resultados.

mtcars %>%
  group_by(cyl) %>%
  summarise(avg = mean(mpg))



# Por un factor 

sumario_est = mtcars %>%
  group_by(cyl) %>%
  summarise(n = n(),
            avg = mean(mpg),
            median = median(mpg),
            sd = sd(mpg),
            min = min(mpg),
            max = max(mpg),
            IQR = IQR(mpg))

class(sumario_est)

View(sumario_est)

export(sumario_est,"data/processed_data/sumario_est.csv",";")

export(sumario_est,"data/processed_data/sumario_est.xlsx")

nombre_sumario_est = "Sumario_estadistico_mpg_by_cyl"

export(sumario_est,paste("data/processed_data/",
                         nombre_sumario_est,
                         ".csv"),";")

# Por dos factores 

sumario_est2 = mtcars %>%
  group_by(cyl,gear) %>%
  summarise(n = n(),
            avg = mean(mpg),
            median = median(mpg),
            sd = sd(mpg),
            min = min(mpg),
            max = max(mpg),
            IQR = IQR(mpg))

class(sumario_est2)

View(sumario_est2)

export(sumario_est2,"data/processed_data/sumario.est2.csv",";")

nombre_sumario_est2 = "Sumario_estadistico_mpg_by_cyl_&_gear"

export(sumario_est,paste("data/processed_data/",
                         nombre_sumario_est2,
                         ".csv"),";")

# rowwise, operaciones en una fila 

?rowwise

# rowwise() le permite calcular en un marco de datos una fila a la vez.
# Esto es más útil cuando no existe una función vectorizada.

# La mayoría de los verbos dplyr conservan la agrupación por filas. 
# La excepción es summarise(), que devuelve un grouped_df. Puede desagrupar 
# explícitamente con ungroup() o as_tibble(), o convertir a grouped_df
# con group_by().

# runif genera distribuciones uniformes 

?runif

df <- tibble(x = runif(30, 1, 10), y = runif(30, 1, 10), z = runif(30, 1, 10))

# Compute the mean of x, y, z in each row
df %>% rowwise() %>% mutate(mean = mean(c(x, y, z)))

df %>% rowwise() %>% mutate(mean = mean(c(x, y)))

# use c_across() to more easily select many variables
df %>% rowwise() %>% mutate(mean = mean(c_across(x:z)))

# Compute the minimum and mean of x, y and z in each row
df2 = df %>% rowwise() %>% 
  mutate(min = min(c(x, y, z))) %>%
  mutate(mean = mean(c(x, y, z)))

class(df2)

View(df2)

# Veamos este caso de error 

df3 = df %>% rowwise() %>% 
  mutate(min = min(c(x, y, z),
         mean = mean(c(x, y, z)))) 

class(df3)

View(df3)

# ACTIVIDAD: exportar df2 a la carpeta processed_data en formato csv 
# sepadrado por ","

# rowwise() también es útil al hacer simulaciones
params <- tribble(
  ~sim, ~n, ~mean, ~sd,
  1,  4,     1,   1,
  2,  2,     2,   4,
  3,  3,    -1,   2
)

class(params)

# Aquí proporciono variables para preservar después del resumen

rnorm(5, 10, 1)

params %>%
  rowwise(sim) %>%
  summarise(z = rnorm(n, mean, sd))

# Si desea una fila por simulación, coloque los resultados en una lista ()

sim = params %>%
  rowwise(sim) %>%
  summarise(z = list(rnorm(n, mean, sd)))

view(sim)

sim

# 3. Manipulación de datos ####

# Extraer casos 

mtcars2 = filter(mtcars, mpg > 20)

# Remover filas con valores duplicados 

distinct(mtcars, gear)

# Seleccionar algunas filas por posición 

slice(mtcars, 10:15)


# Seleccionar (muestrear) algunas filas aleatorias

slice_sample(mtcars, n = 5, replace = TRUE)

# Seleccionar filas con valores mas pequeños o mas grandes 

slice_min(mtcars, mpg, prop = 0.25)

slice_max(mtcars, mpg, prop = 0.25)

# Seleccionar las primeras o las ultimas filas 

slice_head(mtcars, n = 5)

slice_tail(mtcars, n = 5)


# ACTIVIDAD: exportar en processed data un subconjunto de datos mtcars 
# con muestras aleatorias con remplazo

browseURL("https://dplyr.tidyverse.org/reference/filter.html")

# Filtros logicos

View(starwars)

# Filtrar por un criterio 

filter(starwars, species == "Human")


filter(starwars, mass > 1000)

# Filtrar por múltiples criterios con una expresión lógica simple 

filter(starwars, hair_color == "none" & eye_color == "black")

filter(starwars, hair_color == "none" | eye_color == "black")

# Aplicar un filtro tomando como criterio el retorno de una función 

starwars %>% filter(mass > mean(mass, na.rm = TRUE))

# Mientras que esto mantiene filas con 'mass' mayor que el gender 

starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))

# Criterios basados en columnas múltiples

my_data = iris

my_data %>% View()

my_data %>%
  filter(Sepal.Length > 6.7, Sepal.Width <= 3) %>%
  View()

# Criterio de igualdad

my_data %>% filter(Sepal.Length > 6.7, Species == "versicolor") %>%
  View()

# Usando el operador OR (|)

my_data %>% filter(Sepal.Length > 6.7,
                   Species == "versicolor" | Species == "virginica") %>%
  View()

# de manera equivalente, use este atajo ( operador %in% ):

my_data %>% filter(
  Sepal.Length > 6.7, 
  Species %in% c("versicolor", "virginica" )
) %>% View()

# Excluir valores faltantes (NA)

friends_data <- data_frame(
  name = c("A", "B", "C", "D"),
  age = c(27, 25, 29, 26),
  height = c(180, NA, NA, 169),
  married = c("yes", "yes", "no", "no")
) 

friends_data

# Extraer filas donde la altura es NA:

friends_data_na = friends_data %>% 
  filter(is.na(height)) 
friends_data_na

# Excluir (eliminar) filas con NA:

friends_data_w_na = friends_data %>% 
  filter(!is.na(height)) 
friends_data_w_na

friends_data %>% drop_na() %>%  View()

# Otras formas de seleccionar filas aleatorias 

dim(iris)

# Removamos las filas duplicadas
my_data = unique(my_data)

dim(my_data)

#  Extraer 5 filas aleatorios sin remplazo 

my_data %>% sample_n(5, replace = FALSE)

# Extraer 80% de las filas aleatorias con remplazo 
my_data_80 = my_data %>% sample_frac(1.2, replace = T)

my_data_80 %>% dim()

# Extraer 80% de las filas aleatorias con remplazo 
my_data_80_by_spp = my_data %>% 
  group_by(Species) %>%
  sample_frac(0.8, replace = T)

my_data_80_by_spp_wr = my_data %>% 
  group_by(Species) %>%
  sample_frac(0.8, replace = F)

my_data_80_by_spp

dim(my_data)

dim(my_data_80_by_spp)

View(my_data)

View(my_data_80_by_spp)

duplicated(my_data)

dim(my_data_80_by_spp)

duplicated(my_data_80_by_spp) %>%  as.data.frame() %>% dim()
   
duplicated(my_data_80_by_spp_wr)

my_data_80_by_spp %>% 
  anyDuplicated() 
  mutate(dup = as.data.frame(duplicated(my_data_80_by_spp)))

# Otros ejemplos 

browseURL("https://www.statmethods.net/management/subset.html")

# 4. Reordenar casos ####

arrange(mtcars, mpg)

arrange(mtcars, desc(mpg))

# 5. Agregar un  caso 

View(cars)

add_row(cars, speed = 1, dist = 1)

# 6. Manipulación de variables ####

# Extraer variables 

# pull() extrae los valores de una columna como un vector por nombre o indice 


mtcars 


pull(mtcars, wt)

# select() extrae columnas de una tabla 

select(mtcars, mpg, wt)

# relocate() mueve una columna a una nueva localización 

names(mtcars)

relocate(mtcars, mpg, cyl, .after = last_col())

# 7. Manipulación de múltiples variables en una ####

# Resumir o mutar varias columnas de la misma manera.

mtcars %>% summarise(across(everything(), mean))

# Calcule a través de columnas en datos por filas.

df

transmute(rowwise(df), total = sum(c_across(1:3)))

df %>% rowwise() %>% 
transmute(total = sum(c_across(1:3)))

# Crear nuevas variables 

# mutate() computa una nueva columna 

mutate(mtcars, gpm = 1 / mpg)

# Calcular nueva columna (s), soltar otras

transmute(mtcars, gpm = 1 / mpg)

mtcars %>%  
transmute(gpm = 1 / mpg)

# renombrar una columna 

rename(cars, distance = dist)

cars %>% rename(distance = dist)

?mutate

# Crear nuevas variables 
starwars %>%
  select(name, mass) %>%
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )

# Puedes usar mutate para modificar los valores de una variable 
starwars %>%
  select(name, height, mass, homeworld) %>%
  mutate(
    mass = NULL,
    height = height * 0.0328084 # convert to feet
  )

# 8. funciones vectorizadas 

browseURL("https://bookdown.org/rdpeng/rprogdatascience/vectorized-operations.html")

# Muchas operaciones en R están vectorizadas , lo que significa que las 
# operaciones ocurren en paralelo en ciertos objetos R. Esto 
# le permite escribir código que sea eficiente, conciso y más fácil 
# de leer que en lenguajes no vectorizados.

if (!require('tictoc'))
  install.packages("tictoc")
library(tictoc)

# Ejemplo: 

sumVec = function(x,y) {
  x = as.numeric(x)
  y = as.numeric(y)
  z <- x + y
  return(z)
}

a <- 0:100000
b <- 100000:200000 

tic("sumVec")
sumVec(a,b)
toc()

sum_iter = function(x,y) {
z <- numeric(length(x))
for(i in seq_along(x)) {
  z[i] <- x[i] + y[i]
}
return(z)
}

tic("sum_iter")
sum_iter(a,b)
toc()

# Ejemplos 

# lag 

# lead

dfxy = data_frame(x = rnorm(30,10,2),
              y = rnorm(30,12,2)) %>%  
  mutate(x_lag = lag(x),
         y_lead = lead(y)) 

# agregación acumulada 

# media acumulada 

x <- c(1, 3, 5, 2, 2)
cummean(x)

seq_along(x)

# Sumatoria sucesivas

cumsum(x)

cumsum(x) / seq_along(x)

dfxy %>%  mutate(media_acum_movil = cumsum(x) / seq_along(x))


# `cumall()` return logicals
cumall(x < 5)

# funciones matematicas 

#log(), log2(), log10()

dfxy %>%  mutate(logx = log(x))

# 8. Case_when, if_else() para multiples casos ####

View(starwars)


starwars2 = starwars %>%
  mutate(type = case_when(
    height > 200 | mass > 200 ~ "large",
    species == "Droid" ~ "robot",
    TRUE ~ "other")
  )

unique(starwars2$type)

table(starwars2$type,starwars2$sex)

# 9. nombres de filas ####

nf = "id"

a <- rownames_to_column(mtcars,
                        var = nf)
a

# 10. Combinar tablas #### 

browseURL(paste0(getwd(),"/","data-transformation.pdf"))

browseURL("https://dplyr.tidyverse.org/reference/mutate-joins.html")

browseURL("https://dplyr.tidyverse.org/reference/mutate-joins.html")

# Ejemplos 

band_members

band_instruments

# Unión interna 

band_members %>% inner_join(band_instruments)

# Unión por la izquierda 

band_members %>% left_join(band_instruments)

# Unión por la derecha 

band_members %>% right_join(band_instruments)

# Full unión 

band_members %>% full_join(band_instruments)

# Como buena practica Indicar la variable de unión 

band_members %>% inner_join(band_instruments, by = "name")

# indicar mas de una variable para la unión 

band_members %>% full_join(band_instruments2, by = c("name" = "artist"))

table1 <- data.frame(key = c("a", "a", "b", "b", "c"), 
                     var = 1:5)
table1

table2 <- data.frame(key = c("a", "a", "b", "b", "c"), 
                     var = LETTERS[1:5])
table2

full_join(x = table1, y = table2, by = "key")

# Para lograr combinar dos conjuntos de datos debes tener columnas 
# con nombres en común y tipos de datos en común 

# 11. Resuelve el problema ####

# Dado los siguientes conjuntos de datos: 
  
table_A <- data.frame(key = as.factor(c(1:5)), 
                     var = 1:5)
table_A$var = as.factor(table_A$var)

table_B <- data.frame(key = c(1:5), 
                     var = LETTERS[1:5])
table_B

# Has una unión de las dos tablas 



