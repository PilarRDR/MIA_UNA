# Introducción rapida al uso de R 

# Ver https://intro2r.com/chap1.html

# 0. Veamos cómo crear un script y un proyecto en R ----

# Cómo crear secciones y comentarios? 

# Ver https://intro2r.com/rsprojs.html 

# 1. Operaciones en R ----

# Aritméticas 

2 + 2
## [1] 4

2 * 2

2 / 3 

log(1)              # logarithm to base e
## [1] 0
log10(1)            # logarithm to base 10
## [1] 0
exp(1)              # natural antilog
## [1] 2.718282
sqrt(4)             # square root
## [1] 2
4^2                   # 4 to the power of 2
## [1] 16
pi                    # not a function but useful
## [1] 3.141593

# De relación 

1 < 2

2 + 2 == 2 * 2

# Lógicas 

2 + 2 == 2 * 2 &  2 + 2 > 2 * 2

2 + 2 == 2 * 2 | 2 + 2 > 2 * 2

# 2. Creación de objetos en R ----

my_obj <- 48

my_obj

my_obj2 = 27

my_obj2

my_obj3 <- "R is cool"

my_obj3 

my_obj4 <- R is cool

pepito <- my_obj + my_obj2

# 3. Uso de funciones de R ----

my_vec <- c(2, 3, 1, 6, 4, 3, 3, 7)

my_vec

mean(my_vec)    # returns the mean of my_vec
## [1] 3.625
var(my_vec)     # returns the variance of my_vec
## [1] 3.982143
sd(my_vec)      # returns the standard deviation of my_vec
## [1] 1.995531
length(my_vec)  # returns the number of elements in my_vec
## [1] 8


vec_mean <- mean(my_vec)    # returns the mean of my_vec
vec_mean
## [1] 3.625

my_seq <- 1:10     # create regular sequence
my_seq
##  [1]  1  2  3  4  5  6  7  8  9 10
my_seq2 <- 10:1    # in decending order
my_seq2
##  [1] 10  9  8  7  6  5  4  3  2  1

my_seq2 <- seq(from = 1, to = 5, by = 0.5)
my_seq2
## [1] 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0

my_seq4 <- rep("abc", times = 3)    # repeats ‘abc’ 3 times 
my_seq4
## [1] "abc" "abc" "abc"

my_seq > 4
## [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE

vec_sort <- sort(my_vec)
vec_sort
## [1]    7  100  100  500 1000 1000 1000 1000

# Datos perdidos o missing data 

temp  <- c(7.2, NA, 7.1, 6.9, 6.5, 5.8, 5.8, 5.5, NA, 5.5)
temp
##  [1] 7.2  NA 7.1 6.9 6.5 5.8 5.8 5.5  NA 5.5


mean_temp <- mean(temp, na.rm = TRUE)
mean_temp
## [1] 6.2875

# 4. R help ----

help("mean")

?mean

#  5. code demonstrations and package vignettes ----

help.search("mean")

??mean

# 6. Clases de objetos ----

# Tipos de datos 

# - Los datos numéricos son números que contienen un decimal. 
# En realidad, también pueden ser números enteros, pero no 
# nos ocuparemos de eso.

# - Los números enteros son números naturales (aquellos números 
# sin punto decimal).

# - Los datos lógicos toman el valor de TRUEo FALSE. 
# También existe otro tipo especial de datos lógicos 
# que se utilizan NApara representar valores faltantes.

# - Los datos de caracteres se utilizan para representar valores 
# de cadena. Puedes pensar en las cadenas de caracteres como algo 
# así como una palabra (o varias palabras). Un tipo especial de 
# cadena de caracteres es un factor , que es una cadena pero 
# con atributos adicionales (como niveles o un orden). 
# Hablaremos de los factores más adelante.

num <- 2.2
class(num)
## [1] "numeric"

char <- "hello"
class(char)
## [1] "character"

logi <- TRUE
class(logi)
## [1] "logical"

is.numeric(num)
## [1] TRUE

is.character(num)
## [1] FALSE

is.character(char)
## [1] TRUE

is.logical(logi)
## [1] TRUE

#  7. Instalar y cargar paquetes ---- 

# Preguntemos a copilot y a gemini en colab 

pacman::p_load("rio","tidyverse")

# 8. Importar y exportar datos (Descargar datos)

# https://www.semanticscholar.org/paper/Estimation-of-Obesity-Levels-with-a-Trained-Neural-Ya%C4%9F%C4%B1n-G%C3%BCl%C3%BC/2c1eab51db154493d225c8b86ba885bbaf147a2c
# https://www.mdpi.com/2076-3417/13/6/3875
# https://www.kaggle.com/datasets/niharika41298/nutrition-details-for-most-common-foods
# https://corgis-edu.github.io/corgis/csv/food/ 
# https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/food-surveys-research-group/docs/fndds-download-databases/
# https://github.com/prasertcbs/basic-dataset/blob/master/nutrients.csv
# https://www.kaggle.com/code/niharika41298/food-nutrition-analysis-eda


food = rio::import('datos/original/food.csv')

View(food)

head(food)

tail(food)

summary(food)

summary(food$`Data.Beta Carotene`)

nutrients = rio::import('datos/original/nutrients.csv')

Obesity = rio::import('datos/original/ObesityDataSet_raw_and_data_sinthetic.csv')

# 9. Sumarios generales de dataset ----

if (!require('summarytools'))
  install.packages("summarytools")
library(summarytools)

pacman::p_load('summarytools')

st_options(lang = "es") #Translations
summarytools::view(dfSummary(food), 
                   footnote = NA, 
                   valid.col = FALSE, 
                   file = paste("./resultados/tablas/","summario food.html", sep =""))

# Exploremos un sumario general de nutrients y Obesity

# 10. Tidydata

# Consultemos a chatgpt4 sobre el conceoto de tidydata, 
# su relevancia e impacto en el analisis de datos 

# Veamos algunas funciones utiles para manipular datos explorando 
# algunos cheat sheets en el menu Helps


