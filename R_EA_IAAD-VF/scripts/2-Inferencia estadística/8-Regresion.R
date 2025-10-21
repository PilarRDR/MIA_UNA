# Titulo: Regresión Lineal 
# Contraste de hipótesis por criterio de información  de Akaike  


# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

Sys.time()

# Fecha de complementación del script: "2022-11-15 18:32:35 -03" ----
# Fecha de ultima modificación: "2022-11-15 18:32:35 -03" ----

browseURL("https://phet.colorado.edu/sims/html/least-squares-regression/latest/least-squares-regression_en.html")

browseURL("https://phet.colorado.edu/sims/html/curve-fitting/latest/curve-fitting_en.html")

# Regresión lineal simple 
browseURL("https://argoshare.is.ed.ac.uk/simple_regression/")

# Fundamentos 
browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/regression.html")

# ¿Cuando el ajuste esta bien? 
browseURL("https://argoshare.is.ed.ac.uk/simple_regression_diagnostics/")

# Simulación regresión lineal multiple 

browseURL("https://argoshare.is.ed.ac.uk/multi_regression/")

library(tidyverse)
library(gapminder) # dataset
library(finalfit)
library(broom)

# 0. Datos ####

theme_set(theme_bw())
gapdata <- gapminder

# 1. Verificar datos ####

glimpse(gapdata) # each variable as line, variable type, first values
missing_glimpse(gapdata) # missing data for each variable
ff_glimpse(gapdata) # summary statistics for each variable


# 2. Visualizar datos ####


gapdata %>%                        
  filter(continent == "Europe") %>%    # Europe only
  ggplot(aes(x = year, y = lifeExp)) + # lifeExp~year  
  geom_point() +                       # plot points
  facet_wrap(~ country) +              # facet by country
  scale_x_continuous(
    breaks = c(1960, 2000)) +          # adjust x-axis 
  geom_smooth(method = "lm")           # add regression lines

# 3. Regresión lineal simple ####

gapdata %>% 
  filter(country %in% c("Turkey", "United Kingdom")) %>% 
  ggplot(aes(x = year, y = lifeExp, colour = country)) + 
  geom_point()

# United Kingdom
fit_uk <- gapdata %>%
  filter(country == "United Kingdom") %>% 
  lm(lifeExp~year, data = .)

fit_uk %>% 
  summary()


# Turkey
fit_turkey <- gapdata %>%
  filter(country == "Turkey") %>% 
  lm(lifeExp~year, data = .)

fit_turkey %>% 
  summary()

# 4. Regresión multivariada ####

# Simple M1
myfit = lm(lifeExp ~ year, data = gapdata)


# Efecto aditivo M2

myfit2 = lm(lifeExp ~ year + country, data = gapdata)

# Interacción M3

myfit3 = lm(lifeExp ~ year * country, data = gapdata)


# M1 

gapdata <- gapdata %>% 
  mutate(year_from1952 = year - 1952)

# UK and Turkey dataset
gapdata_UK_T <- gapdata %>% 
  filter(country %in% c("Turkey", "United Kingdom"))

fit_both1 <- gapdata_UK_T %>% 
  lm(lifeExp ~ year_from1952, data = .)
fit_both1

# M2 

fit_both2 <- gapdata_UK_T %>% 
  lm(lifeExp ~ year_from1952 + country, data = .)
fit_both2

gapdata_UK_T %>% 
  mutate(pred_lifeExp = predict(fit_both2)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(aes(x = year, y = pred_lifeExp, colour = country))

# M3 

fit_both3 <- gapdata_UK_T %>% 
  lm(lifeExp ~ year_from1952 * country, data = .)
fit_both3

gapdata_UK_T %>% 
  mutate(pred_lifeExp = predict(fit_both3)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = lifeExp, colour = country)) +
  geom_line(aes(x = year, y = pred_lifeExp, colour = country))


# El mejor modelo?  

browseURL("https://en.wikipedia.org/wiki/Akaike_information_criterion#:~:text=The%20Akaike%20information%20criterion%20(AIC,a%20means%20for%20model%20selection.")

mod_stats1 <- glance(fit_both1)
mod_stats2 <- glance(fit_both2)
mod_stats3 <- glance(fit_both3)

bind_rows(mod_stats1, mod_stats2, mod_stats3)

# De forma alternativa 

list(fit_both1, fit_both2, fit_both3) %>% 
  map_df(glance)

# Fuente: ####

browseURL("https://argoshare.is.ed.ac.uk/healthyr_book/fitting-simple-models.html")





