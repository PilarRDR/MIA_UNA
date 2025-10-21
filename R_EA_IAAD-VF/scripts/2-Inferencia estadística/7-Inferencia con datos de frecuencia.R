# Titulo: Variables categóricas y medidas de riesgo 

# Propósito: Introducir temas relacionados: 
# - Contraste de hipotesis con variables de resultado categóricas
# - Medidas de riesgo 


# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de complementación del script: "2022-11-01 21:41:56 -03" ----
# Fecha de ultima modificación: "2022-11-01 21:41:56 -03" ----


# Preámbulo 

# Instalar paquetes ####

pacman::p_load(epitools)

if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse) 

if (!require('dplyr'))
  install.packages("dplyr")
library(dplyr) 

if (!require('tidyr'))
  install.packages("tidyr")
library(tidyr) 

if (!require('ggplot2'))
  install.packages("ggplot2")
library(ggplot2) 

if (!require('finalfit'))
  install.packages("finalfit")
library(finalfit)

if (!require('rio'))
  install.packages("rio")
library(rio)


if (!require('broom'))
  install.packages("broom")
library(broom )

if (!require('summarytools'))
  install.packages("summarytools")
library(summarytools)

if (!require('RVAideMemoire'))
  install.packages("RVAideMemoire")
library(RVAideMemoire)

if (!require('DescTools'))
  install.packages("DescTools")
library(DescTools)


if (!require('epitools'))
  install.packages("epitools")
library(epitools)



# 1. Datos categóricos ####

# Datos continuos se pueden medir,  
# mientras que los datos categóricos se pueden contar 

# Factores 
# - Un conjunto de nomrbes/cadenas o números 
# - Pueden tener un orden inherente (1º, 2º 3º) - factor ordinal
# - o no (femenino y masculino)

# Character (caracteres)

# - Secuencia de letras, números o simbolos 

# Lógicos 
# - falso o verdadero 

# 2. Ejemplos de datos categóricos en salud ####

# Supervivencia del melanoma maligno

# Los datos consisten en mediciones realizadas en pacientes 
# con melanoma maligno, un tipo de cáncer de piel. A cada paciente
# se le extirpó el tumor mediante cirugía en el Departamento de 
# Cirugía Plástica del Hospital Universitario de Odense, 
# Dinamarca, entre 1962 y 1977.

# Estamos interesados en evaluar la asociación entre la ulceración del tumor
# y la muerte por melanoma.

meldata <- boot::melanoma

# Metadatos 

browseURL("https://stat.ethz.ch/R-manual/R-devel/library/boot/html/melanoma.html")

?boot::melanoma

# 3. EDA de los datos #### 


theme_set(theme_bw())
meldata %>% glimpse()

meldata %>% ff_glimpse()

des.sta = meldata %>% 
  ff_glimpse()

export(des.sta$Continuous,file = "summary.xlsx")


st_options(lang = "es") #Translations
summarytools::view(dfSummary(meldata), 
                   footnote = NA, 
                   valid.col = FALSE,
                   file = 'resultados/sumario.html')

# 4. Transformación de datos ####

# Recodificar 

# Usando el diccionario de datos, 
# convertiremos las variables categóricas en factores.

meldata <- meldata %>% 
  mutate(sex.factor =             # Make new variable  
           sex %>%                # from existing variable
           factor() %>%           # convert to factor
           fct_recode(            # forcats function
             "Female" = "0",      # new on left, old on right
             "Male"   = "1") %>% 
           ff_label("Sex"),       # Optional label for finalfit
         
         # same thing but more condensed code:
         ulcer.factor = factor(ulcer) %>% 
           fct_recode("Present" = "1",
                      "Absent"  = "0") %>% 
           ff_label("Ulcerated tumour"),
         
         status.factor = factor(status) %>% 
           fct_recode("Died melanoma"       = "1",
                      "Alive"               = "2",
                      "Died - other causes" = "3") %>% 
           ff_label("Status"))

# ¿Debo convertir una variable continua en una variable categórica?

browseURL("https://www.nature.com/articles/s41598-021-81073-2")
browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/should-i-convert-a-continuous-variable-to-a-categorical-variable.html")

# Summary of age
meldata$age %>% 
  summary()

meldata %>% 
  ggplot(aes(x = age)) + 
  geom_histogram()

#  Intervalos iguales vs cuantiles

# Intervalos iguales:

meldata <- meldata %>% 
  mutate(
    age.factor = 
      age %>%
      cut(4)
  )
meldata$age.factor %>%
  summary()


# Cuantiles:

meldata <- meldata %>% 
  mutate(
    age.factor = 
      age %>%
      Hmisc::cut2(g=4) # Note, cut2 comes from the Hmisc package
  )
meldata$age.factor %>% 
  summary()

# Usando la función de corte, una variable continua 
# se puede convertir en una categórica:

meldata <- meldata %>% 
  mutate(
    age.factor = 
      age %>%
      cut(breaks = c(4,20,40,60,95), include.lowest = TRUE) %>% 
      fct_recode(
        "≤20"      =  "[4,20]",
        "21 to 40" = "(20,40]",
        "41 to 60" = "(40,60]",
        ">60"      = "(60,95]"
      ) %>% 
      ff_label("Age (years)")
  )
head(meldata$age.factor)

# 5. Visualización de datos ####

# contamos el número de pacientes con tumores ulcerados que fallecieron

p1 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill = status.factor)) + 
  geom_bar() + 
  theme(legend.position = "none")

p2 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill = status.factor)) + 
  geom_bar(position = "fill") + 
  ylab("proportion")

library(patchwork)
p1 + p2

# Gráfico de barras: Resultado después de la cirugía 
# para pacientes con melanoma ulcerado.

# Este orden de esta variable en particular es inusual; sería más común tener,
# por ejemplo alive = 0, died = 1. 
# Una opción rápida es simplemente invertir el orden de los 
# niveles en la trama.

p1 <- meldata %>% 
ggplot(aes(x = ulcer.factor, fill = status.factor)) + 
  geom_bar(position = position_stack(reverse = TRUE)) + 
  theme(legend.position = "none")

p2 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill = status.factor)) + 
  geom_bar(position = position_fill(reverse = TRUE)) + 
  ylab("proportion")

library(patchwork)
p1 + p2

# Gráfico de barras: resultado después de la cirugía para pacientes 
# con melanoma ulcerado, niveles invertidos.

# También podemos estar interesados en explorar la modificación del efecto 
# potencial, las interacciones y los factores de confusión.

p1 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill=status.factor)) + 
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_grid(sex.factor ~ age.factor) + 
  theme(legend.position = "none")

p2 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill=status.factor)) + 
  geom_bar(position = position_fill(reverse = TRUE)) +
  facet_grid(sex.factor ~ age.factor)+ 
  theme(legend.position = "bottom")

p1 / p2

# Gráfica de barras facetadas: resultado después de la cirugía 
# para pacientes con melanoma ulcerado agregado por sexo y edad.

# Para nuestros propósitos aquí, generaremos una variable de mortalidad
# específica de la enfermedad ( status_dss),
# combinando "Murió - otras causas" y "Vivo".

meldata <- meldata %>%
  mutate(
    status_dss = fct_collapse(
      status.factor,
      "Alive" = c("Alive", "Died - other causes"))
  )

# Cambiar el orden de los valores dentro de un factor

# dss - disease specific survival
meldata$status_dss %>% levels()

# si queremos hacer comparaciones relativas a “Alive”, 
# necesitamos moverlo al frente usando fct_relevel().

meldata <- meldata %>% 
  mutate(status_dss = status_dss %>%
           fct_relevel("Alive")
  )

# 6. Resumir factores confinalfit ####

library(finalfit)
meldata %>% 
  summary_factorlist(dependent   = "status_dss", 
                     explanatory = "ulcer.factor")

# Se puede agregar cualquier número de variables 
# explicativas continuas o categóricas.

s_factorlist = meldata %>% 
  summary_factorlist(dependent = "status_dss", 
                     explanatory = 
                       c("ulcer.factor", "age.factor", 
                         "sex.factor", "thickness")
  ) 

as.data.frame(s_factorlist)

export(as.data.frame(s_factorlist),"s_factorlist.xlsx")

# 7.  Chi-cuadrado de Pearson y pruebas exactas de Fisher ####

# 7.1 Chi-cuadrado de Pearson  ####

# Prueba de chi-cuadrado de hipótesis de independencia

# - Hipótesis nula (H0): La variable 1 y la variable 2 no están 
# relacionadas en la población; Las proporciones de la variable 1 son
# las mismas para diferentes valores de la variable 2.

# - Hipótesis alternativa (Ha): La variable 1 y la variable 2 están 
# relacionadas en la población; Las proporciones de la variable 1 no 
# son las mismas para diferentes valores de la variable 2.

# Se puede construir una tabla de conteos, ya sea usando $
# para identificar columnas, o usando la with()función.

table(meldata$ulcer.factor, meldata$status_dss) # both give same result
with(meldata, table(ulcer.factor, status_dss))

plot(table(meldata$ulcer.factor, meldata$status_dss))

# La tabla de conteos se puede pasar a prop.table() para proporciones.

meldata %$%
  table(ulcer.factor, status_dss) %>% 
  prop.table(margin = 1)     # 1: row, 2: column etc.

# Errores de sesgo por tasa de base: 
#  González o Pérez como factores de riesgo de covid 19 

# se puede pasar la tabla de conteos a chisq.test() 
# para realizar la prueba de chi-cuadrado.

meldata %$%        # note $ sign here
  table(ulcer.factor, status_dss) %>% 
  chisq.test()

# Exportar los resultados en el directorio de trabajo (a fichero)

library(broom)
meldata %$%        # note $ sign here
  table(ulcer.factor, status_dss) %>% 
  chisq.test() %>% 
  tidy() %>% export(.,"chisq_test.xlsx")

# cuando mayor es el valore de chi cuadrado mayor es la diferencia entre lo 
# esperado y lo observado (Es una métrica que informa sobre la bondad de ajuste)

# La corrección implica restar 0,5 de la diferencia absoluta
# entre cada valor observado y esperado. Esto es particularmente útil 
# cuando los recuentos son bajos, pero puede eliminarse 
# si lo desea chisq.test(..., correct = FALSE).

# 7.2 Prueba exacta de Fisher ####

# Una suposición comúnmente declarada de la prueba de chi-cuadrado 
# es el requisito de tener un recuento esperado de al menos 5 
# en cada celda de la tabla 2x2. 

# Para tablas más grandes, todos los  recuentos esperados deben ser 
# y no más del 20 % de todas las celdas deben tener recuentos esperados. 
# Si no se cumple este supuesto, una prueba alternativa es 
# la prueba exacta de Fisher.

browseURL("https://en.wikipedia.org/wiki/Fisher%27s_exact_test")

meldata %$%        # note $ sign here
  table(age.factor, status_dss)

meldata %$%        # note $ sign here
  table(age.factor, status_dss) %>% 
  chisq.test()

# Warning message:
#  In chisq.test(.) : Chi-squared approximation may be incorrect

# por la prueba exacta de Fisher

meldata %$%        # note $ sign here
  table(age.factor, status_dss) %>% 
  fisher.test()

# 7.3 Chi-cuadrado / Prueba exacta de Fisher usando finalfit ####

library(finalfit)
meldata %>% 
  summary_factorlist(dependent   = "status_dss", 
                     explanatory = "ulcer.factor",
                     p = TRUE)

# más variables:

meldata %>% 
  summary_factorlist(dependent = "status_dss", 
                     explanatory = 
                       c("ulcer.factor", "age.factor", 
                         "sex.factor", "thickness"),
                     p = TRUE)

# por prueba exacta de Fisher:

meldata %>% 
  summary_factorlist(dependent = "status_dss", 
                     explanatory = 
                       c("ulcer.factor", "age.factor", 
                         "sex.factor", "thickness"),
                     p = TRUE,
                     p_cat = "fisher"
                     )

# Múltiples variables por resultado con pruebas de hipótesis: 
# Resultado después de la cirugía de melanoma por paciente y factores 
# de la enfermedad (prueba exacta de Fisher).

# Incluir otros argumentos 

meldata %>% 
  summary_factorlist(dependent = "status_dss", 
                     explanatory = 
                       c("ulcer.factor", "age.factor", 
                         "sex.factor", "thickness"),
                     p = TRUE,
                     p_cat = "fisher",
                     digits = 
                       c(1, 1, 4, 2) #1: mean/median, 2: SD/IQR 
                     # 3: p-value, 4: count percentage
  )

rm.na 
# 7.4 Resultados gráficos ####

# Trafico de barras 

observed = c(70, 79, 3, 4)

expected = c(0.54, 0.40, 0.05, 0.01)

total = sum(observed)

observed.prop = observed / total

observed.prop

### Re-enter data as a matrix

Input =("
Value     Douglas.fir  Ponderosa.pine  Grand.fir   Western.larch
Observed  0.4487179    0.5064103       0.01923077  0.02564103
Expected  0.5400000    0.4000000       0.05000000  0.01000000  
")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

barplot(Matriz,
        beside=TRUE,
        legend=TRUE,
        ylim=c(0, 0.6),
        xlab="Tree species",
        ylab="Foraging proportion")


# Gráfico de barras con intervalos de confianza con ggplot2

### -
### Ejemplo de gráfico, bondad de ajuste de chi-cuadrado, pág. 49
### Uso de ggplot2
### Gráfico adaptado de:
### shinyapps.stat.ubc.ca/r-graph-catalog/  
### -


Input =("
Tree              Value      Count   Total Proportion  Expected
'Douglas fir'     Observed   70      156   0.4487      0.54
'Douglas fir'     Expected   54      100   0.54        0.54
'Ponderosa pine'  Observed   79      156   0.5064      0.40
'Ponderosa pine'  Expected   40      100   0.40        0.40
'Grand fir'       Observed    3      156   0.0192      0.05
'Grand fir'       Expected    5      100   0.05        0.05
'Western larch'   Observed    4      156   0.0256      0.01
'Western larch'   Expected    1      100   0.01        0.01
")

Forage = read.table(textConnection(Input),header=TRUE)


### Specify the order of factor levels. Otherwise R will alphabetize them.

library(dplyr)

Forage =
  mutate(Forage,
         Tree = factor(Tree, levels=unique(Tree)),
         Value = factor(Value, levels=unique(Value))
  )


### Add confidence intervals

Forage =
  mutate(Forage,      
         low.ci = apply(Forage[c("Count", "Total", "Expected")],
                        1,
                        function(x)
                          binom.test(x["Count"], x["Total"], x["Expected"]
                          )$conf.int[1]),
         
         upper.ci = apply(Forage[c("Count", "Total", "Expected")],
                          1,
                          function(x)
                            binom.test(x["Count"], x["Total"], x["Expected"]
                            )$conf.int[2])
  )

Forage$low.ci [Forage$Value == "Expected"] = 0
Forage$upper.ci [Forage$Value == "Expected"] = 0

Forage


### Plot adapted from:
###   shinyapps.stat.ubc.ca/r-graph-catalog/

library(ggplot2)
library(grid)

ggplot(Forage,
       aes(x = Tree, y = Proportion, fill = Value,
           ymax=upper.ci, ymin=low.ci))  +
  geom_bar(stat="identity", position = "dodge", width = 0.7) +
  geom_bar(stat="identity", position = "dodge",
           colour = "black", width = 0.7,
           show_guide = FALSE)  +
  scale_y_continuous(breaks = seq(0, 0.60, 0.1),
                     limits = c(0, 0.60),
                     expand = c(0, 0))  +
  scale_fill_manual(name = "Count type" ,
                    values = c('grey80', 'grey30'),
                    labels = c("Observed value",
                               "Expected value"))  +
  geom_errorbar(position=position_dodge(width=0.7),
                width=0.0, size=0.5, color="black")  +
  labs(x = "Tree species",
       y = "Foraging proportion")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5),
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5)
  )



# 8. G-test ####

browseURL("https://rpubs.com/juanhklopper/g_tests_for_categorical_variables")

# G = 2 sum i a  n obs_i * ln(obs_i/esp_i)

### Vaccination example, G-test of independence, pp. 68–69 (A)


Input =("
 Injection.area  No.severe  Severe      
 Thigh           4788       30 
 Arm             8916       76
")  # Thigh pierna, Arm Brazo

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

library(DescTools)

GTest(Matriz,
      correct="none")            # "none" "williams" "yates"

library(RVAideMemoire)

G.test(Matriz)


# G-tests with DescTools and RVAideMemoire


### Helmet example, G-test of independence, p. 72


Input =("
 PSE       Head.injury  Other.injury
 Helemt    372          4715
 No.helmet 267          1391
") # Helemt = casco, injury = lesión 

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

library(DescTools)

GTest(Matriz,
      correct="none")            # "none" "williams" "yates"


library(RVAideMemoire)

G.test(Matriz)


### Gardemann apolipoprotein example, G-test of independence,
###   p. 72


Input =("
 Genotype  No.disease Coronary.disease
 ins.ins   268        807
 ins.del   199        759
 del.del    42        184
") # Apolipoproteína de Gardemann

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE,
                              row.names=1))

Matriz

library(DescTools)

GTest(Matriz,
      correct="none")            # "none" "williams" "yates"

library(RVAideMemoire)

G.test(Matriz)

library(RVAideMemoire)

pairwise.G.test(Matriz,
                p.method = "none")           # Can adjust p-values;
# see ?p.adjust for options

# 9. Medidas de riesgo ####

# Medidas de Asociación

# Pruebas estadísticas 

# - Chi-cuadrado 
# - Test exacto de Fisher 
# - G-test 

# Salud

# - RR (Riesgo relativo)
# - OR (Odds Ratio)
# - Efectividad 

# 9.1 Razon de riesgo (RR)

# Definición de ratio de riesgo

# Veamos algunos ejemplos da Medidas de riesgo

browseURL("https://www.cdc.gov/csels/dsepd/ss1978/lesson3/section5.html")


# Razón de riesgo y OR e intervalo de confianza 

# cuando le damos a un paciente una dosis más alta, arreglamos 
# cualquier indicador que estamos tratando de remediar y también 
# vemos una mayor tasa de eventos adversos (EA).

# A continuación evaluamos si este es el caso en el siguiente ensayo 

treatments <- c("Placebo", "Low Dose", "High Dose")
ae_present <- c("No", "Yes")

dat <- matrix(c(85, 1, 80, 5, 77, 8), nrow = 3, ncol = 2, byrow = TRUE)
dimnames(dat) <- list("Treatments" = treatments, "AE Present" = ae_present)

dat

# Para calcular la razón de probabilidades, usaremos el epitools

library(epitools)
or_fit <- oddsratio(dat)

or_fit

# Usando el mismo epitools, también podemos calcular el 
# riesgo relativo (razón de riesgo)

rr_fit <- riskratio(dat)

rr_fit 

# 11. Fuentes consultadas y lecturas recomendadas 

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/chap08-h1.html")
browseURL("https://rcompanion.org/rcompanion/b_06.html") #(A)
browseURL("https://www.cdc.gov/csels/dsepd/ss1978/lesson3/section5.html")
browseURL("https://jarrettmeyer.com/2019/07/23/odds-ratio-in-r")
browseURL("https://timeseriesreasoning.com/contents/estimation-of-vaccine-efficacy-using-logistic-regression/")
browseURL("https://rpubs.com/ericnovik/692460")