# ====================================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: Exploración (EDA)
# Autor: Guillermo Coronado
# Fecha: 19-06-2026
# Objetivo: Describir la distribución original de las variables antes de clasificarlas
# =====================================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(survey)      
library(srvyr)       
library(flextable)   
library(scales)      
library(officer)
renv::snapshot()

# Cargamos la base de datos limpia (Acondicionada)
enaho_limpia <- read_parquet("datos/procesados/enaho_2025_19_06_25.parquet")

# ------------------------------------------------------------------------------
# 1. PREPARACIÓN DE ETIQUETAS--------------------------------------------------- 
# ------------------------------------------------------------------------------
#Hay que tener en cuenta que todas las variables que exploraremos deben haber sido acondicionadas correctamente
#y, en caso sea necesario, se debe haber tratado correctamente los NAs (en este caso solo lo hicimos con tres
#variables por motivos de tiempo)

