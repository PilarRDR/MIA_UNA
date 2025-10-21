# Titulo: Intervalos de confianza, prueba de hipótesis y valor p ####

# Propósito: Introducir temas relacionados a la inferencia estadística,
# estimación de intervalos de confianza, prueba de hipótesis, interpretación 
# del valor p, ejemplos de aplicación a variables continuas

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de complementación del script: "2022-10-11 14:41:44 -03" ----
# Fecha de ultima modificación: "2022-10-14 17:33:51 -03" ----


# Preámbulo 

# Instalar paquetes ####

if (!require('pacman'))
  install.packages("pacmane")
library(pacman) 

pacman::p_load('tidyverse',
               'ggplot2',
               'BSDA',
               'finalfit',
               'gapminder',
               'summarytools',
               'lubridate',
               'broom',
               "ggfortify",
               'patchwork',
               'cowplot',
               'knitr',
               'kableExtra',
               'lmPerm')


# En estadística como hemos visto existen dos enfoques para analizar datos 

# 1. Estadística descriptiva (enfocado a muestras de poblaciones)

# 2. Estadística inferencias (Busca estimaciones para poblaciones a partir 
# de muestras representativas)

# En este practico nos enfocaremos en aprender métodos de 
# inferencia estadística 

# Aprenderemos como estimar intervalos de confianza 
# Como escribir y contrastar hipótesis estadísticas utilizando p-valor 
# Aprenderemos el rol de las hipótesis estadísticas para poner a prueba 
# las hipótesis de investigación 

# 1. Intervalos de confianza ####

#___________________________________________________________________#
# La interpretación de los intervalos de confianza se puede resumir
# de la siguiente manera: 
# si se tomaran muestras repetidas de una población y se calculara 
# el intervalo de confianza del 95 % para cada muestra, el 95 % de 
# los intervalos contendrían la media de la población a largo plazo.
#
# La estimación de intervalos de confianza permite saber si muestras 
# comparadas pertenecen a la misma población o son de poblaciones
# diferentes 
#___________________________________________________________________#

# Digamos que enviamos un grupo de personas a hospitales  
# para probar y medir la esperanza de vida en una población. 
# Cada uno consigue una muestra de 20 observaciones y construye un 
# intervalo de confianza del 95% a partir de su muestra. 
# Por ejemplo, la siguiente sería una posible realización de un 
# intervalo de confianza calculado de esta manera:

e_v <- rnorm(n = 20, mean = 70, sd = 2)

e_v

summary(e_v)

test3 <- z.test(e_v, sigma.x = 2, conf.level = 0.95)

test3$conf.int

# Contemos cuántos de los intervalos de confianza cubrieron, o incluyeron, 
# la verdadera media. El código siguiente simula el proceso de muestreo 
# repetido de lotes de 20 observaciones de la distribución real y la 
# construcción de intervalos de confianza a partir de cada muestra

# A fines prácticos el código que se muestra a continuación es 
# solo para ilustrar el concepto 

attempts = 1000 # Simulamos 1000 muestras 
curve(expr = dnorm(x, mean = 70, sd = 2), from = 65, to = 75,
      main = "95% CI simulation results for sample size = 20",
      xlab = "Life expectancy", ylab = "Density",
      lwd=2, col="blue")
abline(v = 40, col = "purple", lwd = 2)
failed_to_contain <- 0
for (i in 1:attempts) {
  col <- rgb(0,0,0,0.5)
  e_v <- rnorm(n = 20, mean = 70, sd = 2) 
  myCI <- z.test(e_v, sigma.x = 2, conf.level = 0.95)$conf.int
  if (min(myCI) > 70 | max(myCI) < 70) {
    failed_to_contain <- failed_to_contain + 1
    col <- rgb(1,0,0,1)
  }
  segments(min(myCI), 0.2 * i / attempts,
           max(myCI), 0.2 * i / attempts,
           lwd = 1, col = col)
}

failed_to_contain

failed_to_contain*100 / attempts

# En este caso, para un intervalo de confianza del 95 %, observe 
# que aproximadamente el 5 % de los intervalos de confianza que 
# construimos en realidad SÍ pierden la media verdadera
# (los que están en rojo).

# 2. Prueba de hipótesis ####

# 2.1. Comparación de datos continuos ####

# Un objetivo en  una pregunta sobre datos de salud puede ser sacar 
# una conclusión sobre una comparación entre grupos. 
# Por ejemplo, comprender las diferencias en la esperanza de vida entre 
# los años 2002 y 2007 es más útil que simplemente 
# describir la esperanza de vida promedio a lo largo del tiempo.

# Describir diferencias en la esperanza de vida promedio entre distintos años 
# no es suficiente para evidenciar algún cambio que no se atribuible al azar 
# De hecho es muy poco probable que dos años consecutivos den exactamente 
# la misma media, así que si visualizamos solo los promedios la tendencia  
# sería siempre concluir que hay cambios 

# Por ejemplo, se ha hipnotizado que la epidemia de COVID 19 a impactado en  
# la esperanza de vida de la población

# 3. Obtener y comprobar los datos ####

# Comenzaremos observando la esperanza de vida de las poblaciones a lo 
# largo del tiempo y en diferentes regiones geográficas.

# Los datos que utilizaremos son del proyecto https://www.gapminder.org/


# creamos un objeto gapdata desde el objeto gapminder
gapdata = gapminder

# Es importante que los conjuntos de datos se inspeccionen
# cuidadosamente cuando se lean por primera

class(gapdata)

head(gapdata)

tail(gapdata)

st_options(lang = "es") #Translations
summarytools::view(dfSummary(gapdata), 
                   footnote = NA, 
                   valid.col = FALSE,
                   file = "./resultados/summariodf.html")

browseURL("https://www.focus-economics.com/economic-indicator/gdp-per-capita")

browseURL("https://ourworldindata.org/life-expectancy#life-expectancy-what-does-this-actually-mean")

glimpse(gapdata) # se muestra cada variable como una linea 
# el tipo de variable 
# y los primeros valores 

missing_glimpse(gapdata) # valores perdidos para cada variable 

ff_glimpse(gapdata) # sumario estadístico para cada variable 

help(ff_glimpse)

# 4. Visualizar datos ####

# Comenzaremos comparando la esperanza de vida entre los 5 continentes 
# del mundo en dos años diferentes.

# Estamos interesados en la distribución. 
# La forma de los datos. ¿Es normal? 
# ¿Está sesgado? ¿Difiere entre regiones y años?

# Existen  tres visualizaciones útiles que pueden ayudar aquí:
  
# - Histogramas: examine la forma de los datos y compare grupos;
# - QQ plot: ¿están los datos normalmente distribuidos?
# - Diagramas de caja: identifique valores atípicos, compare formas y grupos.

# 4.1. Histogramas ####

gapdata %>% 
  filter(year %in% c(2002, 2007)) %>% # filtramos los dos ultimos años de datos 
  ggplot(aes(x = lifeExp)) +       # remember aes() (estetica)
  geom_histogram(bins = 20) +      # histogram with 20 bars (geometría)
  facet_grid(year ~ continent, scales = "free")  # optional: add scales="free"

# ¿Qué podemos ver? 

# La esperanza de vida en África es menor que en otras regiones. 
# Que tenemos pocos datos de Oceanía dado que solo se incluyen dos países, 
# Australia y Nueva Zelanda. 
# Que África y Asia tienen una mayor variabilidad en la esperanza de vida 
# por país que América o Europa. 
# Que los datos siguen una forma razonablemente normal, 
# con África 2002 un poco sesgada a la derecha.

# definir la gráfica como un objeto: 
p_Histogramas = ggplot(gapdata %>% 
                         filter(year %in% c(2002, 2007)) ,
                       aes(x = lifeExp)) +      
  geom_histogram(bins = 20) +      
  facet_grid(year ~ continent, scales = "free")  

# Guardar la gráfica en el directorio 

ggsave(
  paste0("p_Histogramas ", today(), ".svg"),
  plot = p_Histogramas,
  width = 32,
  height = 32,
  units = "cm",
  dpi = 600,
  scale = 1
)

ggsave(
  paste0("p_Histogramas ", today(), ".pdf"),
  plot = p_Histogramas,
  width = 32,
  height = 32,
  units = "cm",
  dpi = 600,
  scale = 1
)

?ggsave 

# 4.2. QQ plot ####

# Un gráfico QQ simplemente traza los cuantiles de nuestros datos contra 
# los cuantiles teóricos para una distribución particular
# (el valor predeterminado que se muestra a continuación es
# la distribución normal). Si nuestros datos siguen esa distribución 
# (por ejemplo, normal), entonces nuestros puntos de datos caen 
# en la línea recta teórica.


gapdata %>% 
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(sample = lifeExp)) +      # Q-Q plot requires 'sample'
  geom_qq() +                          # defaults to normal distribution
  geom_qq_line(colour = "blue") +      # add the theoretical line
  facet_grid(year ~ continent, scales = "free") 

  
  gapdata %>% 
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(sample = lifeExp)) +      # Q-Q plot requires 'sample'
  geom_qq() +                          # defaults to normal distribution
  geom_qq_line(colour = "blue") +      # add the theoretical line
  facet_wrap(year ~ continent, scales = "free")

# 4.3. Diagrama de caja ####

gapdata %>% 
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(x = continent, y = lifeExp)) + # elementos de estética 
  geom_boxplot() + # elementos de geometría 
  facet_wrap(~ year) # elementos de diagramación 
# Boxplot: Esperanza de vida por país por continente y año.

# ver elementos de estética, geometría y diagramación 

gapdata %>%  
  filter(year %in% c(2002, 2007)) %>%
  ggplot(aes(x = factor(year), y = lifeExp)) +
  geom_boxplot(aes(fill = continent)) +     # add colour to boxplots
  geom_jitter(aes(fill = continent), alpha = 0.4) +                # alpha = transparency
  facet_wrap(~ continent, ncol = 5) +       # spread by continent
  theme(legend.position = "none") +         # remove legend
  xlab("Year") +                            # label x-axis
  ylab("Life expectancy (years)") +         # label y-axis
  ggtitle(
    "Life expectancy by continent in 2002 v 2007") # add title

# Boxplot con puntos de jitter: Esperanza de vida por país por continente y año.

# 5.  Comparar las medias de dos grupos ####

# comparemos la esperanza de vida entre Asia y Europa para 2007

#  Las distribuciones se superponen, pero parece probable que Europa
# tenga una esperanza de vida más alta que Asia.

gapdata %>%                    # save as object ttest_data
  filter(year == 2007) %>%                   # 2007 only
  filter(continent %in% c("Asia", "Europe")) %>% # Asia/Europe only
  group_by(continent) %>% 
  summarise(mean=mean(lifeExp),
            sd=sd(lifeExp))


ttest_data <- gapdata %>%                    # save as object ttest_data
  filter(year == 2007) %>%                   # 2007 only
  filter(continent %in% c("Asia", "Europe")) # Asia/Europe only

ttest_result <- ttest_data %>%               # example using pipe
  t.test(lifeExp ~ continent, data = .)      # note data = ., see below
ttest_result


# La prueba t de dos muestras de Welch es la más flexible y 
# hace frente a las diferencias en la varianza (variabilidad) entre grupos,
# como en este ejemplo.

tt_test = tidy(ttest_result) # en el paquete broom

rio::export(tt_test, file = "resultados/t_test_dos_muestras.xlsx")

glance(ttest_result)

# Se puede acceder al valor p , por ejemplo, así:

ttest_result$p.value # Extracted element of result object

# El intervalo de confianza de la diferencia en la esperanza 
# de vida media entre los dos continentes:

ttest_result$conf.int # Extracted element of result object

# 6. Pruebas t pareadas ####

browseURL("https://en.wikipedia.org/wiki/Student%27s_t-test#Dependent_t-test_for_paired_samples")

# Considere que queremos comparar la diferencia en la esperanza de vida 
# en los países asiáticos entre 2002 y 2007. 

# Podemos graficar las diferencias a nivel de país directamente.

paired_data <- gapdata %>%             # save as object paired_data
  filter(year %in% c(2002, 2007)) %>%  # 2002 and 2007 only
  filter(continent == "Asia")          # Asia only

paired_data %>%      
  ggplot(aes(x = year, y = lifeExp, 
             group = country)) +       # for individual country lines
  geom_line()

# Diagrama de líneas: cambio en la esperanza de vida en los
# países asiáticos de 2002 a 2007.

# ¿Cuál es la diferencia en la esperanza de vida para cada país individual?

head(paired_data)

# reordenas datos con pivot_wider del paquete tidyr 

paired_data %>%        # save object paired_data
  select(country, year, lifeExp) %>%   # select vars interest
  pivot_wider(names_from = year,       # put years in columns
  values_from = lifeExp) %>% head()

paired_table <- paired_data %>%        # save object paired_data
  select(country, year, lifeExp) %>%   # select vars interest
  pivot_wider(names_from = year,       # put years in columns
              values_from = lifeExp) %>% 
  mutate(
    dlifeExp = `2007` - `2002`         # difference in means
  )

View(paired_table)


# Mean of difference in years
paired_table %>% summarise( mean = mean(dlifeExp),
                            sd =  sd(dlifeExp))

# En promedio, por lo tanto, hay un aumento en la esperanza de vida de 1,5 
# años en los países asiáticos entre 2002 y 2007. 

# Probemos si este número difiere de cero con una prueba t pareada :

paired_data %>% 
  t.test(lifeExp ~ year, data = ., paired = TRUE)

paired_data %>% 
  t.test(lifeExp ~ year, data = .)

# ¿Son significativas las diferencias? 

# Como ejercicio, podemos repetir este análisis comparando estos datos de 
# forma no pareada como en el ejemplo previo.  
# El valor p resultante (no apareado) es 0,460.

# 7. Pruebas t de una muestra ####

# Podemos usar una prueba t para determinar si la media de una 
# distribución es diferente a un valor específico. 

# Por ejemplo, podemos probar si la esperanza de vida media en 
# cada continente era significativamente diferente de los 77 años de 2007.

# El siguiente código ejecuta varias pruebas en una función de tubería

gapdata %>% 
  filter(year == 2007) %>%          # 2007 only
  group_by(continent) %>%           # split by continent
  do(                               # dplyr function
    t.test(.$lifeExp, mu = 54) %>%  # compare mean to 77 years 
      tidy()                        # tidy into tibble
  )

tt = gapdata %>% 
  filter(year == 2007) %>%          # 2007 only
  group_by(continent) %>%           # split by continent
  do(                               # dplyr function
    t.test(.$lifeExp, mu = 77) %>%  # compare mean to 77 years 
      tidy()                        # tidy into tibble
  )


?t.test

ggplot(tt, aes(y = estimate, x = as.factor(continent))) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.6) +
  geom_hline(yintercept = 77,
             linetype = "dotted",
             size = 1) +
  labs(y = "lifeExp", x = "continent") +
  theme_minimal() 

# 8. Intercambiabilidad de las pruebas t ####

# Podemos usar estas diferencias para cada par de observaciones 
# (la esperanza de vida del país en 2002 y 2007) para ejecutar 
# una prueba t simple de una muestra 

t.test(paired_table$dlifeExp, mu = 0)

# ¿Es diferente este resultado de la prueba t pareada? 

# 9. Comparar las medias de más de dos grupos ####

# Puede ser que nuestra pregunta se establezca en torno a una hipótesis que 
# involucre a más de dos grupos. Por ejemplo, puede que 
# nos interese comparar la esperanza de vida en 3 continentes, 
# como América, Europa y Asia.

gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% 
           c("Americas", "Europe", "Asia")) %>% 
  ggplot(aes(x = continent, y=lifeExp)) +
  geom_boxplot()

# Diagrama de caja: Esperanza de vida en continentes seleccionados para 2007.

# 10. ANOVA ####

# En forma base R, produce una tabla ANOVA que incluye una prueba F.

aov_data <- gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia"))

fit = aov(lifeExp ~ continent, data = aov_data) 

summary(fit)

library(broom)

df_aov = gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp~continent, data = .) %>% 
  tidy()

class(df_aov)

rio::export(df_aov, file= 'resultados/df_aov.csv')

# ¿Qué podemos concluir a partir de estos resultados? 

# 10.1. Supuestos ####

library(ggfortify)
autoplot(fit)

# Gráficos de diagnóstico: modelo ANOVA de esperanza 
# de vida por continente para 2007.

# 11. Pruebas por pares y comparaciones múltiples ####

# Cuando la prueba F es significativa, a menudo querremos 
# determinar dónde se encuentran las diferencias. 

pairwise.t.test(aov_data$lifeExp, aov_data$continent, 
                p.adjust.method = "bonferroni") # menos sensible 


pairwise.t.test(aov_data$lifeExp, aov_data$continent, 
                p.adjust.method = "fdr") # mas sensible 

?pairwise.t.test

# 12. Cuando no se cumplen los supuestos ####

# ¿Qué sucede si sus datos tienen una forma diferente a la normal o 
# no se cumplen los supuestos de ANOVA

# Por ejemplo, si está examinando la duración de la estancia 
# en el hospital, es probable que sus datos estén muy sesgados 
# hacia la derecha: la mayoría de los pacientes son dados de alta 
# del hospital en unos pocos días, mientras que un número menor 
# permanece durante mucho tiempo.

# 12.1. Transformación de datos ####

# Transformaciones que se pueden aplicar a datos asimétricos. 

# - Para datos asimétricos a la izquierda, 
#    reste todos los valores de una constante mayor que el valor máximo.

# - Sesgo a la derecha moderado (+), Raíz cuadrada,	sqrt()

# - Sesgo sustancial a la derecha (++)	Logaritmo natural*, log()

# - Sesgo sustancial a la derecha (+++)	Logaritmo en base 10*, log10()

# - Si los datos contienen valores cero, agregue una pequeña constante
# a todos los valores.

africa2002 <- gapdata %>%              # save as africa2002
  filter(year == 2002) %>%             # only 2002
  filter(continent == "Africa") %>%    # only Africa
  select(country, lifeExp) %>%         # only these variables
  mutate(
    lifeExp_log = log(lifeExp)         # log life expectancy
  )
head(africa2002)                       # inspect

africa2002 %>% 
  # pivot lifeExp and lifeExp_log values to same column (for easy plotting):
  pivot_longer(contains("lifeExp")) %>% 
  ggplot(aes(x = value)) +             
  geom_histogram(bins = 15) +          # make histogram
  facet_wrap(~name, scales = "free")    # facet with axes free to vary


africa2002 %>% 
  ggplot(aes(sample = lifeExp)) +      # Q-Q plot requires 'sample'
  geom_qq() +                          # defaults to normal distribution
  geom_qq_line(colour = "blue")  

africa2002 %>% 
  ggplot(aes(sample = lifeExp_log)) +      # Q-Q plot requires 'sample'
  geom_qq() +                          # defaults to normal distribution
  geom_qq_line(colour = "blue")  

#  Ahora se puede realizar una prueba paramétrica como una prueba t .

# 12.2. Prueba no paramétrica ####

# La prueba U de Mann-Whitney también se denomina prueba de suma de rangos 
# de Wilcoxon y utiliza un método basado en rangos para comparar dos
# grupos (tenga en cuenta que la prueba de rangos con signo de Wilcoxon
# es para datos pareados). 

# Primero hagamos un histograma, un diagrama QQ y un diagrama de caja.

africa_data <- gapdata %>%                          
  filter(year %in% c(1982, 2007)) %>%      # only 1982 and 2007
  filter(continent %in% c("Africa"))       # only Africa

p1 <- africa_data %>%                      # save plot as p1
  ggplot(aes(x = lifeExp)) + 
  geom_histogram(bins = 15) +
  facet_wrap(~year)

p2 <- africa_data %>%                      # save plot as p2
  ggplot(aes(sample = lifeExp)) +          # `sample` for Q-Q plot
  geom_qq() + 
  geom_qq_line(colour = "blue") + 
  facet_wrap(~year)

p3 <- africa_data %>%                      # save plot as p3
  ggplot(aes(x = factor(year),             # try without factor(year) to
             y = lifeExp)) +               # see the difference
  geom_boxplot(aes(fill = factor(year))) + # colour boxplot
  geom_jitter(alpha = 0.4) +               # add data points
  theme(legend.position = "none")          # remove legend

library(patchwork)                         # great for combining plots
p1 / p2 | p3

# Gráficos de paneles: Histograma, QQ, diagrama de caja para 
# la esperanza de vida en África 1992 v 2007.

africa_data %>% 
  wilcox.test(lifeExp ~ year, data = .)

# ¿Según el resultado, son las diferencias significativas? 

# 13. Prueba no paramétrica para comparar más de dos grupos ####

# El equivalente no paramétrico de ANOVA es la prueba de Kruskal-Wallis. 

library(broom)
gapdata %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  kruskal.test(lifeExp~continent, data = .) %>% 
  tidy()

# Que puede concluir a partir de estos resultados? 


dependent <- "year"
explanatory <- c("lifeExp", "pop", "gdpPercap")
africa_data %>%         
  mutate(
    year = factor(year)
  ) %>% 
  summary_factorlist(dependent, explanatory,
                     cont = "median", p = TRUE)

# 14. Prueba por permutación ####

#_______________________________________________________#
# ¿cómo funcionan realmente las pruebas de permutación?
#_______________________________________________________#

# El propósito de una prueba de permutación es estimar la distribución
# de la población, la distribución de donde provienen nuestras observaciones. 
# A partir de ahí, podemos determinar qué tan raros son nuestros valores 
# observados en relación con la población.

# Un enfoque más robusto e intuitivo para NHST (prueba de significación 
# de hipótesis nula) es reemplazar las distribuciones y tablas listas 
# para usar con una simulación creada directamente a partir de 
# nuestro conjunto de datos.

# El flujo de trabajo de cualquier prueba de este tipo se 
# muestra a continuación.

browseURL("https://i2.wp.com/rileyking.netlify.com/img/workflow.png?w=578&ssl=1")

# La principal diferencia aquí es que creamos la distribución de 
# los datos bajo la hipótesis nula usando simulación en lugar de 
# confiar en una distribución de referencia. Es intuitivo, 
# potente y divertido.

# Una hipótesis nula común para comparar grupos es que
# no hay diferencia entre ellos.

# 1. mezclamos y asignamos aleatoriamente puntos de datos en dos 
# grupos del tamaño original. 

# 2. calculamos y almacenamos la estadística de prueba para
# el efecto observado (diferencia de medias entre los dos grupos)

# 2. vemos dónde se encuentran nuestros datos reales 
# en relación con los simulados.

# Existen funciones y librerías para realizar este tipo de pruebas 
# Veamos un ejemplo paso a pasa para comprender como funcionan 

# 14.1 comparación de dos medias ####

# Ejemplo: 

browseURL("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3163409/")

# Imagine que acabamos de diseñar un experimento en el que tenemos
# la intención de medir la presión (en mm Hg) a la que un par 
# de endoprótesis superpuestas comenzaron a migrar o 
# desconectarse cuando se desplegaron en un gran *aneurisma torácico*.

browseURL("https://www.google.com/search?q=aneurisma+tor%C3%A1cico&rlz=1C1CHBF_esPY990PY990&oq=aneurisma+tor%C3%A1cico&aqs=chrome..69i57j0i22i30l9.3520j0j4&sourceid=chrome&ie=UTF-8")


#Migration pressure for predicate device
predicate <-  c(186, 188, 189, 189, 192, 193, 194, 194, 194, 
                195, 195, 196, 196, 197, 197, 198, 198, 199, 199, 
                201, 206, 207, 210, 213, 216, 218)

browseURL("https://www.google.com/search?q=predicate+device+definition&rlz=1C1CHBF_esPY990PY990&oq=predicate+device&aqs=chrome.2.69i57j0i512l9.2606j0j4&sourceid=chrome&ie=UTF-8")

#Migration pressure for next_gen device
next_gen <-  c(189, 190, 192, 193, 193, 196, 199, 
               199, 199, 202, 203, 204, 205, 206, 206,
               207, 208, 208, 210, 210, 212, 214, 216, 216, 217, 218)

browseURL("https://www.google.com/search?rlz=1C1CHBF_esPY990PY990&sxsrf=ALiCzsYgpkhOkNd0IAsSG-gLQhGRfknHUw:1666806289635&q=next+gen+device+definition&spell=1&sa=X&ved=2ahUKEwiM1rWmuf76AhXYqZUCHZwYAp0QBSgAegQICBAB&biw=1920&bih=872&dpr=1")

# asigne cada grupo a una variable y conviértalo al formato tibble:

predicate_tbl <- tibble(Device = "Predicate",
                        Pressure = predicate)

next_gen_tbl <- tibble(Device = "Next_Gen",
                       Pressure = next_gen)

#Combine in tibble
results_tbl <- bind_rows(predicate_tbl, next_gen_tbl)
results_tbl %>% 
  View()

# Ahora hacemos un análisis exploratorio de datos para identificar 
# la forma y distribución general.

# Visualize w/ basic boxplot
boxplot_eda <- results_tbl %>% 
  ggplot(aes(x=Device, y=Pressure)) +
  #theme_minimal() +
  #theme_bw() +
  geom_boxplot(
    alpha  = .6,
    width  = .4,
    size   = .8,
    fatten = .5,
    fill   = c("#FDE725FF","#20A486FF")) +
  labs(
    y        = "Pressure (mm Hg)",
    title    = "Predicate and Next-Gen Data",
    subtitle = "Modular Disconnect Pressure"
  )

boxplot_eda

#Visualize with density plot
density_eda <- results_tbl %>% 
  ggplot(aes(x = Pressure)) + # Estética 
  theme_minimal() +
  geom_density(aes(fill = Device), # geometria 
               color = "black",
               alpha = 0.6
  ) +
  scale_fill_manual(values = c("#FDE725FF","#20A486FF")) + # diagramación 
  labs(
    x        = "Pressure (mm Hg)",
    title    = "Predicate and Next-Gen Data",
    subtitle = "Modular Disconnect Pressure"
  )

density_eda


results_tbl  %>%                      # save plot as p2
  ggplot(aes(sample = Pressure)) +          # `sample` for Q-Q plot
  geom_qq() + 
  geom_qq_line(colour = "blue") + 
  facet_wrap(~Device)

# Función para intercambiar índices vectoriales y luego calcular 
# la diferencia en las medias de los grupos
permFun <- function(x, n1, n2){
  n <- n1 + n2
  group_B <- sample(1:n, n1)
  group_A <- setdiff(1:n, group_B)
  mean_diff <- mean(x[group_B] - mean(x[group_A]))
  return(mean_diff)
}

# Establecemos el número de permutaciones a realizar 
n_sims <- 10000

# Creamos un vector de inicialización
perm_diffs <- rep(0,n_sims)
perm_diffs %>% head()  # vector de 0 que luego serán remplazados  

# En cada iteración permFun(), almacenaremos los resultados en el índice 
# correspondiente dentro de perm_diffs que inicializamos anteriormente.

#Set seed for reproducibility
set.seed(2015)

#Iterate over desired number of simulations using permutation function
for (i in 1:n_sims)
  perm_diffs[i] = permFun(results_tbl$Pressure, 26, 26)

head(perm_diffs)

# Ahora tenemos 10 000 réplicas de nuestra prueba de permutación 
# almacenadas en perm_diffs. Queremos visualizar los datos con ggplot,
# así que los convertimos en un marco tibble usando tibble().

#Convert results to a tibble and look at it
perm_diffs_df <- tibble(perm_diffs)
perm_diffs_df %>% View()

# Visualice la diferencia de medias como un 
# histograma y un diagrama de densidad:

#Visualize difference in means as a histogram
diffs_histogram_plot <- perm_diffs_df %>% ggplot(aes(perm_diffs)) +
  geom_histogram(fill = "#2c3e50", color = "white", binwidth = .3, alpha = 0.8) +
  labs(
    x = "Diff Pressure (mm Hg)",
    title = "Histogram of Difference in Means",
    subtitle = "Generated Under Null Hypothesis"
  )

#Visualize difference in means as a density plot
diffs_density_plot <-  perm_diffs_df %>% ggplot(aes(perm_diffs)) +
  geom_density(fill = "#2c3e50", color = "white", alpha = 0.8) +
  labs(
    x = "Diff Pressure (mm Hg)",
    title = "Density Plot of Difference in Means",
    subtitle = "Generated Under Null Hypothesis"
  )

plot_grid(diffs_histogram_plot, diffs_density_plot)

# Estos datos virtuales nos dan una buena comprensión de qué tipo de 
# diferencia en las medias podríamos observar si realmente no hubiera 
# diferencia entre los grupos. 
# Como era de esperar, la mayor parte del tiempo la diferencia
# es de alrededor de 0. 
# Pero ocasionalmente hay una diferencia notable en las medias 
# simplemente por casualidad.

# ¿qué tan grande fue la diferencia en las medias de nuestro conjunto de 
# datos del mundo real? Llamaremos a esto “diferencia de línea de base”.

#Evaluate difference in means from true data set
predicate_pressure_mean <- mean(predicate_tbl$Pressure)
next_gen_pressure_mean <- mean(next_gen_tbl$Pressure)

baseline_difference <- predicate_pressure_mean - next_gen_pressure_mean
baseline_difference  %>% 
  signif(digits = 3) 

# ¿Esto es grande o pequeño? es mayor o no a lo esperado por la casualidad 

#Visualize real data in context of simulations
g1 <- diffs_histogram_plot + 
  geom_vline(xintercept = baseline_difference, 
             linetype   = "dotted", 
             color      = "#2c3e50", 
             size       = 1
  ) 

g2 <- diffs_density_plot + 
  geom_vline(xintercept = baseline_difference, 
             linetype   ="dotted", 
             color      = "#2c3e50", 
             size       = 1
  ) 

plot_grid(g1,g2)

#  consideremos la posibilidad de que este efecto no se deba únicamente al azar. 
# 0,05 es un umbral de uso común para declarar significancia estadística. 

#Calculate the 5% quantile of the simulated distribution for difference in means
the_five_percent_quantile <- quantile(perm_diffs_df$perm_diffs, probs = 0.05)
the_five_percent_quantile

#Visualize the 5% quantile on the histogram and density plots
g3 <- g1 +
  geom_vline(xintercept = the_five_percent_quantile, 
             color      = "#2c3e50", 
             size       = 1
  )

g4 <- g2 +
  geom_vline(xintercept = the_five_percent_quantile, 
             color      = "#2c3e50", 
             size       = 1
  )

plot_grid(g3,g4)

# Podemos ver aquí que nuestros datos son más extremos que
# el cuantil del 5 %, lo que significa que nuestro valor p es
# inferior a 0,05. Esto satisface la definición frecuentista 
# tradicional de estadísticamente significativo. 

# Si queremos un valor p real, tenemos que determinar el 
# porcentaje de datos simulados que son tan extremos o más
# extremos que nuestros datos observados.

#Calculate percentage of simulations as extreme or more extreme than the observed data (p-value)
p_value <- perm_diffs_df %>% 
  filter(perm_diffs <= baseline_difference) %>%
  nrow() / n_sims

paste("The empirical p-value is: ", p_value) 

# 14.2 ANOVA usando pruebas de permutación ####

?aovp

## Un ejemplo simple de bloques aleatorios. 
# Hay 7 bloques y 6 tratamientos. Un primer análisis 
# con bloques como factor muestra que los bloques son significativos 
# y los tratamientos no. 

data(Hald17.4)

Hald17.4

#aovp(Y~T+block,Hald17.4)

summary(aovp(Y~T+block,Hald17.4))


## De Venables y Ripley (2000)
# Este es un factorial 2^3 en las variables N,P,K. Se fracciona usando la 
# interacción de tres vías, NPK, en dos fracciones de 4. 
# Cada una de estas fracciones se 
# asigna a 3 bloques, haciendo 6 bloques en total. Un análisis con bloque como 
# variable es el siguiente. Como puede verse, aovp() 
# descarta la interacción NPK confundida. 

data(NPK)

NPK

aovp = aovp(yield ~ block + N*P*K, NPK)

summary(aovp)

# 15. Fuentes consultadas y lecturas recomendadas ####

browseURL("https://uw-statistics.github.io/Stat311Tutorial/confidence-intervals.html")

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/chap06-h1.html")

# Pruebas por permutación: 

browseURL("https://rcompanion.org/rcompanion/d_06a.html")

browseURL("https://rdrr.io/cran/lmPerm/man/aovp.html")

browseURL("https://rileyking.netlify.app/post/permutation-test-for-nhst-of-2-samples-in-r/#fn2")

browseURL("https://towardsdatascience.com/how-to-use-permutation-tests-bacc79f45749")

