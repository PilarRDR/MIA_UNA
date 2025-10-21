# Elaboración de tablas de resumenes estadisticos 

# Importar paquetes ----

pacman::p_load("rio", "tidyverse")

# Importar datos -----

food = rio::import('datos/original/food.csv')

nutrients = rio::import('datos/original/nutrients.csv')

Obesity = rio::import('datos/original/ObesityDataSet_raw_and_data_sinthetic.csv')

# Ejemplos ----- 

# stargazer ----
# https://libguides.princeton.edu/R-stargazer

pacman::p_load('stargazer')

# Generar la tabla de estadísticas descriptivas
st_food =  stargazer(food, type = "text")

st_food 

class(st_food)

stargazer(food, type = "st_food", title="Descriptive statistics", digits=2, 
          out="resultados/tablas/table1.txt")

stargazer(mydata, type = "text", title="Descriptive statistics", digits=1, 
          out="resultados/tablas/table2.txt", flip=TRUE)

# Descriptive statistic: selected variables and by group ----

pacman::p_load("dplyr","tidyr")

mydata <- mtcars

newdata<-mydata %>%
  select(am,mpg,hp,drat) %>%
  group_by(am) %>%
  mutate(id = 1:n()) %>%
  ungroup() %>%
  gather(temp, val, mpg, hp,drat) %>%
  unite(temp1, am, temp, sep = '_') %>%
  spread(temp1, val) %>%
  select(-id) %>%
  as.data.frame()%>%
  stargazer(type = 'text')

# Create HTML Tables of Descriptive Statistics ----


# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

# https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html

#install.packages("knitr")
pacman::p_load(vtable,boot)
pacman::p_load(table1,gapminder,knitr)
#options(knitr.table.format = "kable_kable") # Set the output format to 'kable'

st(iris, group = 'Species', group.test = TRUE)


#Why not three columns?
sumtable(mtcars, col.breaks = c(4,8))

library("boot")

melanoma2 <- melanoma

# Factor the basic variables that
# we're interested in
melanoma2$status <-
  factor(melanoma2$status,
         levels=c(2,1,3),
         labels=c("Alive", # Reference
                  "Melanoma death",
                  "Non-melanoma death"))

table1(~ factor(sex) + age + factor(ulcer) + thickness | status, data=melanoma2)

melanoma2$sex <-
  factor(melanoma2$sex, levels=c(1,0),
         labels=c("Male",
                  "Female"))

melanoma2$ulcer <-
  factor(melanoma2$ulcer, levels=c(0,1),
         labels=c("Absent",
                  "Present"))

label(melanoma2$sex)       <- "Sex"
label(melanoma2$age)       <- "Age"
label(melanoma2$ulcer)     <- "Ulceration"
label(melanoma2$thickness) <- "Thicknessᵃ"

units(melanoma2$age)       <- "years"
units(melanoma2$thickness) <- "mm"

caption  <- "Basic stats"
footnote <- "ᵃ Also known as Breslow thickness"

table1(~ sex + age + ulcer + thickness | status, data=melanoma2,
       overall=c(left="Total"), caption=caption, footnote=footnote) 

# Galería de R ----

# https://r-graph-gallery.com/

# https://r-graph-gallery.com/table.html
