#================================================================================
#Proyecto: Análisis de la informalidad laboral usando datos de la ENAHO
#Script: Cargar los módulos y hacer los joins
#Autor: Guillermo Coronado
#Fecha: 12-06-2026
#===============================================================================

#1.Carga de librerías---------------------------
library(rio)
library(tidyverse)
library(janitor)
library(readr)
renv::snapshot()

#2. Importar datos--------------------
mod300 <- import("datos/crudos/Enaho01A-2025-300.csv", encoding = "Latin-1")
mod400 <- import("datos/crudos/Enaho01A-2025-400.csv", encoding = "Latin-1")
mod500 <- import("datos/crudos/Enaho01A-2025-500.csv", encoding = "Latin-1")
mod_gob <- import("datos/crudos/Enaho01B-2025-1.csv", encoding = "Latin-1")

#3.
