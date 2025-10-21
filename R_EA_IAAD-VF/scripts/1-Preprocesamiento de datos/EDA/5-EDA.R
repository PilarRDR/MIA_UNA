# Titulo: Análisis exploratorio de datos EDA 

# Propósito: Este script presenta ejemplos de EDA univariado  y bivariado

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de implementación del script: "2022-09-30 19:58:49 -04" 
# Fecha de ultima modificación: "2023-03-22 17:20:34 -03"


# Preámbulo 

# 0-Instalar paquetes ####

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse)

if (!require('rio'))
  install.packages("rio")
library(rio)

if (!require('dlookr'))
  install.packages("dlookr")
library(dlookr)

if (!require('ISLR'))
  install.packages("ISLR")
library(ISLR)

library(ISLR)

library(dplyr)

if (!require('explore'))
  install.packages("explore")
library(explore)

if (!require('ggraptR'))
  install.packages("ggraptR")
library(ggraptR)

if (!require('summarytools'))
  install.packages("summarytools")
library(summarytools)


# 1-Paquete dlookr ####

# A continuación  presenta los métodos EDA (Análisis exploratorio de datos) 
# proporcionados por el paquete dlookr.

# https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html

# Conjuntos de datos

# Para ilustrar el uso básico de EDA en el paquete dlookr, utilizo un
# Carseats. Carseats del paquete ISLR es un conjunto de datos simulados 
# que contiene ventas de asientos para niños en 400 tiendas diferentes. 
# Estos datos son un marco de datos creado con el propósito de predecir 
# el volumen de ventas.


library(ISLR)
str(Carseats)

Carseats %>%  View()

st_options(lang = "es") #Translations
summarytools::view(dfSummary(Carseats), 
                   footnote = NA, 
                   valid.col = FALSE, 
                   file = paste("./resultados/tablas/","summario Carseats.html", sep =""))

# El contenido de las variables individuales es el siguiente.

# - Sales
# Ventas unitarias (en miles) en cada ubicación
# - CompPrice
# Precio cobrado por el competidor en cada ubicación
# - Income
# Nivel de ingresos de la comunidad (en miles de dólares)
# - Advertising
# Presupuesto de publicidad local para la empresa en cada 
# ubicación (en miles de dólares)
# - Population
# Tamaño de la población en la región (en miles)
# - Price
# Precio de los cargos de la compañía por los asientos de seguridad en cada sitio
# - ShelveLoc
# Un factor con niveles Malo, Bueno y Medio que indica la calidad de la 
# ubicación de las estanterías para los asientos de seguridad en cada sitio
# - Age
# Edad media de la población local
# - Education
# Nivel de educación en cada lugar
# - Urban
# Un factor con niveles No y Sí para indicar si la tienda está en
# una ubicación urbana o rural
# - US
# Un factor con niveles No y Yes para indicar si la tienda está en EE. UU. o no

carseats <- ISLR::Carseats

# Cuando se realiza el análisis de datos, con frecuencia se encuentran 
# datos que contienen valores faltantes. Sin embargo, 'Carseats' son datos 
# completos sin valores faltantes. Entonces, el siguiente script creó
# los valores faltantes y los guardó como carseats.

suppressWarnings(RNGversion("3.5.0"))
#set.seed(123)
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA

suppressWarnings(RNGversion("3.5.0"))
#set.seed(456)
carseats[sample(seq(NROW(carseats)), 10), "Urban"] <- NA

# 2-Análisis exploratorio de datos ####

# El análisis exploratorio de datos (EDA) es la forma en que 
# los científicos y analistas de datos encuentran información 
# significativa en forma de relaciones en los datos.

# La siguiente es una lista de las funciones EDA incluidas en el paquete dlookr.:

?describe()
# proporciona estadísticas descriptivas para datos numéricos.
?normality()
?plot_normality()
#realizar la normalización y visualización de datos numéricos.
?correlate()
?plot.correlate()
# calcular el coeficiente de correlación entre dos datos 
# numéricos y proporcionar visualización.
?target_by()
#define la variable objetivo y 
?relate()
# describe la relación con las variables de interés correspondientes 
# a la variable objetivo.
?plot.relate()
# visualiza la relación con la variable de interés correspondiente
# a la variable de destino.
eda_report()
#realiza un análisis exploratorio de datos e informa los resultados.

# 2.1-Datos univariados ####

dlookr::describe(Carseats)

# A continuación se explican las estadísticas descriptivas solo 
# para unas pocas variables seleccionadas:

dlookr::describe(carseats, Sales, CompPrice, Income)

dlookr::describe(Carseats, -(Sales:Income))

# La función describe() se puede ordenar por 
# left or right skewed size(sesgo) usando dplyr.:

Carseats %>%
  dlookr::describe() %>%
  select(described_variables, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

# La función describe() admite la sintaxis de función group_by() de dplyr.

carseats %>%
  group_by(US) %>% 
  dlookr::describe(Sales, Income) 

carseats %>%
  group_by(US, Urban) %>% 
  dlookr::describe(Sales, Income)

# Prueba de normalidad sobre variables numéricas utilizando normality()

# Las variables de tbl_dfobjeto devueltas por normality()son las siguientes.

# statistic: Estadísticas de la prueba de Shapiro-Wilk
# p_value: valor p de la prueba de Shapiro-Wilk
# sample: Número de observaciones de muestra realizadas Prueba de Shapiro-Wilk

dlookr::normality(carseats)

# El siguiente ejemplo realiza una prueba de normalidad en solo
# unas pocas variables seleccionadas.

dlookr::normality(carseats, Sales, CompPrice, Income)

dlookr::normality(carseats, Sales:Income)

dlookr::normality(carseats, -(Sales:Income))

# Puede usar dplyrpara ordenar las variables que no siguen una 
# distribución normal en orden de p_value:

library(dplyr)

carseats %>%
  dlookr::normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

# La normality()función admite la función  group_by() en el dplyr.

carseats %>%
  group_by(ShelveLoc, US) %>%
  dlookr::normality(Income) %>% 
  arrange(desc(p_value))

carseats %>%
  mutate(log_income = log(Income)) %>%
  group_by(ShelveLoc, US) %>%
  dlookr::normality(log_income) %>%
  filter(p_value > 0.01)

# Visualización de normalidad de variables numéricas medianteplot_normality()

# La información visualizada por plot_normality()es la siguiente:

# Histogram of original data
# Q-Q plot of original data
# histogram of log transformed data
# Histogram of square root transformed data

dlookr::plot_normality(carseats, Sales, CompPrice)

# La plot_normality()función también admite la group_by() de dplyr

carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  dlookr::plot_normality(Income)

# 2.2-Datos bivariados ####

# Cálculo de coeficiente de correlación: 

library(ISLR)
Carseats %>% view()

dlookr::correlate(Carseats)

dlookr::correlate(Carseats, Sales, CompPrice, Income)

dlookr::correlate(Carseats, Sales:Income)

dlookr::correlate(Carseats, -(Sales:Income))

# El correlate() también es compatible con  group_by() 
#  en el paquete dplyr.

tab_corr <- Carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  dlookr::correlate(Sales) %>%
  filter(abs(coef_corr) > 0.5)

tab_corr

# Visualización de la matriz de correlación

Carseats %>% 
  dlookr::correlate() %>% 
  plot()

# Selecionando solo algunas columnas de interes
dlookr::correlate(Carseats, Sales, Price) %>% 
  plot()

#  también admite la group_by() para evaluar la correlación 
# segun algun o algunos factores 

Carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban) %>%
  dlookr::correlate() %>%
  plot()

# 2.3 EDA basado en la variable objetivo

# Para realizar EDA basado en variable target, 
# debe crear unobjeto de clase target_by.

categ <- target_by(Carseats, US)

# Casos donde los predictores son variables numéricas

cat_num <- relate(categ, Sales)
cat_num %>% View()



# plot() visualiza el  objeto de clase relate creado por relate() 
# como relación entre la variable objetivo y la variable predictora.

plot(cat_num)

# Casos donde los predictores son variables categóricas

cat_cat <- relate(categ, ShelveLoc)
cat_cat

summary(cat_cat)

# Figura de mosaico

plot(cat_cat)

# EDA cuando la variable objetivo es una variable numérica

# Realicemos EDA cuando la variable de destino sea numérica. 
# Cuando la variable numérica Saleses la variable objetivo, examinamos 
# la relación entre la variable objetivo y el predictor.

num <- target_by(Carseats, Sales)

# Casos donde los predictores son variables numéricas

num_num <- relate(num, Price)
num_num

summary(num_num)

plot(num_num)

# El diagrama de dispersión de los datos con un gran número 
# de observaciones se genera como puntos superpuestos. Esto hace que 
# sea difícil juzgar la relación entre las dos variables.
# En este caso, el problema anterior se puede resolver mediante hexabin plot.

plot(num_num, hex_thres = 350)

# Casos donde los predictores son variables categóricas

# El siguiente ejemplo muestra la relación entre ShelveLocy
# la variable de destino Sales. 

num_cat <- relate(num, ShelveLoc)
num_cat

#remove.packages("dlookr")

summary(num_cat)

plot(num_cat)

# 3. Informe automatizado ####

# dlookr proporciona dos informes EDA automatizados 

# Cree un informe dinámico utilizando eda_web_report()

# https://rdrr.io/github/choonghyunryu/dlookr/man/eda_web_report.data.frame.html
# https://www.heart.org/en/health-topics/heart-failure/what-is-heart-failure 

  # create the dataset
  heartfailure2 <- dlookr::heartfailure
  heartfailure2[sample(seq(NROW(heartfailure2)), 20), "sodium"] <- NA
  heartfailure2[sample(seq(NROW(heartfailure2)), 5), "smoking"] <- NA
  
  # create html file. file name is EDA_Report.html
  eda_web_report(heartfailure2)
  
  # file name is EDA.html. and change logo image
  logo <- file.path(system.file(package = "dlookr"), "report", "R_logo_html.svg")
  eda_web_report(heartfailure2, logo_img = logo, title_color = "black",
                 output_file = "EDA.html")
  
  # file name is ./EDA_heartfailure.html, "blue" theme and not browse
  eda_web_report(heartfailure2, target = "death_event", output_dir = ".", 
                 author = "Choonghyun Ryu", output_file = "EDA_heartfailure.html", 
                 theme = "blue", browse = FALSE)
  
#


heartfailure %>%
  eda_web_report(target = "death_event", subtitle = "heartfailure", 
      output_dir = ".", output_file = "resultados/tablas/EDA.html", theme = "blue")

# Cree un informe EDA usando eda_paged_report()

heartfailure %>%
  eda_paged_report(target = "death_event", subtitle = "heartfailure", 
                   output_dir = ".", output_file = "resultados/tablas/EDA.pdf", 
                   theme = "blue")


# Exploración interactiva de datos 

iris %>% explore(Sepal.Length, target = Species)

iris %>% 
  select(Sepal.Length, Sepal.Width, Species) %>% 
  explore_all(target = Species)

iris %>% explore_all(target = Species)

# Explorar la correlación entre dos variables

iris %>% explore(Sepal.Length, Petal.Length, target = Species)


explore(iris)

# Crear cuaderno 

create_notebook_explore(
  output_dir =  "./",
  output_file = "notebook-explore.Rmd")

# Diccionario de datos 

iris  %>%  data_dict_md(output_dir = "./")

# 4. ggraptR ####

if (interactive()) {
  ggraptR(iris)
}


ggplot(iris, aes(x = Petal.Width)) + 
  geom_density(
  aes(
    fill = as.factor(Species),
    colour = Species,
    y = ..density..
  ),
  stat = "density",
  position = "identity",
  alpha = 0.2
) +  facet_wrap( ~ Species, scales = "free") + 
  theme_wsj() +
  theme(text =element_text(family = "sans",
                           face = "plain",
                           color = "#000000",
                           size = 15,
                           hjust = 0.5,
                           vjust = 0.5
  )) +  guides(fill = guide_legend(title = "Species"),
            colour = guide_legend(title = "Species")) +
  xlab("Petal Width") + ylab("Density")



# Lecturas 

# https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html

# https://cran.r-project.org/web/packages/explore/vignettes/explore.html

# https://r4ds.hadley.nz/eda

# https://bookdown.org/steve_midway/DAR/exploratory-data-analysis.html

# https://www.r-bloggers.com/2022/09/explore-simplified-exploratory-data-analysis-eda-in-r/
