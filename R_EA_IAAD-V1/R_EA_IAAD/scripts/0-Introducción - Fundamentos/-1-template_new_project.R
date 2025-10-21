# Titulo: Crear una estructura de directorio

# Propósito: Este script crea la estructura para un directorio de trabajo

# Proyecto número: #007

# Autor: Pastor E. Pérez Estigarribia
# e-mail: peperez.estigarribia@pol.una.py

#Sys.time()

# Fecha de implementación del script: "2022-09-01 14:14:48 -04" ----
# Fecha de ultima modificación: "2022-09-01 14:15:37 -04" ----

# Preámbulo
# Asignar un directorio de trabajo

rstudioapi::getSourceEditorContext()$path

dirname(rstudioapi::getSourceEditorContext()$path)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# package dependencias

if (!require('PopSnouter'))
  install.packages("PopSnouter")
library(PopSnouter)
if (!require('xfun'))
  install.packages("xfun")
library(xfun)

#print('put your lovely R code here')


# 1. Crear una estructura de directorio ####

# Un enfoque es usar las dir.create() funciones
# y `list.files() en R Console

# Root
#   |
#   |_____ data
#   |        |
#   |        |_____ raw_data
#   |        |_____ processed_data
#   |        |_____ metadata
#   |
#   |_____ R_functions
#   |_____ Rmd
#   |_____ scripts
#   |_____ output

# crear un directorio llamado 'data'
np = "new_project"

dir.create(np)

#?paste

# crear un subdirectorio llamado 'data'
dir.create(paste(np, '/data', sep = ""))

# Crear subsubdirectorios en el directorio data
dir.create(paste(np, '/data/raw_data', sep = ""))
dir.create(paste(np, '/data/processed_data', sep = ""))
dir.create(paste(np, '/data/metadata', sep = ""))

# crear un subdirectorio llamado 'R_functions'
dir.create(paste(np, '/R_functions', sep = ""))

# crear un subdirectorio llamado 'Rmd'
dir.create(paste(np, '/Rmd', sep = ""))

# crear un subdirectorio llamado 'scripts
dir.create(paste(np, '/scripts', sep = ""))

# crear un subdirectorio llamado 'output'
dir.create(paste(np, '/output', sep = ""))

# Lista de archivos y directorios
list.files(recursive = TRUE, include.dirs = TRUE)


# 2. Fuentes consultadas ####

# https://intro2r.com/proj_doc.html

# https://intro2r.com/dir_struct.html

# Una buena practica es incluí siempre la información de la sección
sink(paste("session_info", Sys.Date(), sep = " ", ".txt"))
xfun::session_info()
sink()


file.show(paste("session_info", Sys.Date(), sep = " ", ".txt"))


#_________________________________________________________________________
# Nota: ----
# Escribir un código es hacer el recorrido necesario para llegar
# a una meta determinada, implementar una ruta
# Existe dos tipos de navegantes, de mapa y de brújula
#
# Los navegantes de mapa, requieren al menos un
# croquis del recorrido que realizarán para llegar a su meta
# Los informáticos usan para ese propósito los seudocódigos
#  https://dle.rae.es/croquis
#
# Los navegantes de brújula van tomando decisiones de direcciones
# para llegar a su meta en la medida en la que avanzan
#
# El recorrido total para llegar a una meta es la solución a un problema
# Para saber donde quiere llegar asegúrese de formular de
# forma correcta el problema
#____________________________________________________________________________
