# ====================================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: EDA con variables analíticas 
# Autor: Guillermo Coronado
# Fecha: 24-06-2026
# Objetivo: Explorar las variables analíticas creadas
# =====================================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS---------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(survey)      
library(srvyr)       
library(here)
library(gtsummary)
library(flextable)
renv::snapshot()

# Cargamos la base de datos que contiene nuestras variables analíticas
enaho_limpia <- read_parquet(here("datos", "procesados", "enaho_2025_24_06_26.parquet"))