# ====================================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: Clasificar 
# Autor: Guillermo Coronado
# Fecha: 24-06-2026
# Objetivo: Crear variables analíticas con los datos seleccionados
# =====================================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS---------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(survey)      # Para declarar diseños muestrales complejos (factor de expansión)
library(srvyr)       # Para usar dplyr con encuestas complejas
library(here)
renv::snapshot()

# Cargamos la base de datos limpia
enaho_limpia <- read_parquet(here("datos", "procesados", "enaho_2025_19_06_25.parquet"))

# ==============================================================================
# 1. PREPARACIÓN DE VARIABLES ANALÍTICAS 
# ==============================================================================

# Limpieza previa de formatos numéricos (Necesario para los cálculos posteriores)
enaho_limpia <- enaho_limpia %>%
  mutate(
    factorA07 = as.numeric(str_replace_all(factorA07, ",", ".")),
    conglome  = as.numeric(conglome),
    estrato   = as.numeric(estrato),
    ingreso_prin_imputado = as.numeric(ingreso_prin_imputado),
    edad = as.numeric(edad)
  )

# Calculamos valores ponderados globales necesarios para la estandarización
media_edad_pond <- weighted.mean(enaho_limpia$edad, enaho_limpia$factorA07, na.rm = TRUE)
sd_edad_pond <- sqrt(Hmisc::wtd.var(enaho_limpia$edad, enaho_limpia$factorA07, na.rm = TRUE))

# Construcción de la base analítica
enaho_analitica <- enaho_limpia %>%
  mutate(
    # --------------------------------------------------------------------------
    # A. Matriz de Informalidad (Sector y Empleo)-------------- TIPOLOGÍAS
    # --------------------------------------------------------------------------
    formalidad_sector = case_when(
      tiene_ruc %in% c(1, 2) ~ "Sector Formal",
      tiene_ruc == 3 ~ "Sector Informal",
      TRUE ~ NA_character_
    ),
    
    formalidad_empleo = case_when(
      # 1. Trabajador familiar no remunerado (Siempre informal por definición)
      categoria_ocupacional == 5 ~ "Empleo Informal",
      
      # 2. Independientes y Empleadores (Formal solo si su unidad productiva tiene RUC)
      categoria_ocupacional %in% c(1, 2) & tiene_ruc == 3 ~ "Empleo Informal",
      categoria_ocupacional %in% c(1, 2) & tiene_ruc %in% c(1, 2) ~ "Empleo Formal",
      
      # 3. Dependientes (Dependen del contrato)
      categoria_ocupacional %in% c(3, 4, 6, 7) & tipo_contrato == 7 ~ "Empleo Informal",
      categoria_ocupacional %in% c(3, 4, 6, 7) & tipo_contrato %in% c(1, 2, 3, 4, 5, 6, 8) ~ "Empleo Formal",
      
      TRUE ~ NA_character_
    ),
    
    #Tipología MECE Multidimensional (Cruce de Sector y Empleo)
    tipologia_laboral = case_when(
      formalidad_sector == "Sector Formal" & formalidad_empleo == "Empleo Formal" ~ "1. Formal Absoluto",
      formalidad_sector == "Sector Formal" & formalidad_empleo == "Empleo Informal" ~ "2. Informal en Sector Formal",
      formalidad_sector == "Sector Informal" & formalidad_empleo == "Empleo Formal" ~ "3. Formal en Sector Informal",
      formalidad_sector == "Sector Informal" & formalidad_empleo == "Empleo Informal" ~ "4. Informal Absoluto",
      TRUE ~ NA_character_
    ),
    
    # --------------------------------------------------------------------------
    # B. Demográficas y Sociales ------ RECODIFICACIONES 
    # --------------------------------------------------------------------------
    sexo = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # Ejemplo 1: Recodificación guiada por la TEORÍA (Cortes fijos)
    grupo_edad_teoria = case_when(
      edad < 30 ~ "18 a 29 años",
      edad < 45 ~ "30 a 44 años",
      edad < 60 ~ "45 a 59 años",
      TRUE ~ "60 años a más"
    ),
    
    # Ejemplo 2.1: Recodificación guiada por los DATOS (Quintiles de ingreso)
    quintil_ingreso = ntile(ingreso_prin_imputado, 5),
    
    # Ejemplo 2.2: Recodificación guiada por los DATOS (Terciles de edad óptimos)
    grupo_edad_datos = ntile(edad, 3), # Genera 3 grupos del mismo tamaño muestral
    
    # Ejemplo 3: ESTANDARIZACIÓN (Puntaje Z de Edad, conservando la granularidad)
    edad_z = (edad - media_edad_pond) / sd_edad_pond,
    
    # Discapacidad
    discapacidad = ifelse(
      discapacidad_moverse == 1 | discapacidad_visual == 1 | 
        discapacidad_comunicarse == 1 | discapacidad_auditiva == 1 | 
        discapacidad_cognitiva == 1 | discapacidad_social == 1, 
      "Con discapacidad", "Sin discapacidad"
    ),
    
    # --------------------------------------------------------------------------
    # C. Protección Social ------- MÁS TIPOLOGÍAS
    # --------------------------------------------------------------------------
    afiliacion_salud = ifelse(
      afiliado_essalud == 1 | afiliado_privado == 1 | afiliado_eps == 1 | 
        afiliado_FFAA_Policiales == 1 | afiliado_SIS == 1 | 
        afiliado_universitario == 1 | afiliado_escolar == 1 | afiliado_otro == 1, 
      "Afiliado a salud", "No afiliado a salud"
    ),
    
    afiliacion_pensiones = case_when(
      afiliado_SPP == 1 ~ "AFP (Privado)",
      afiliado_SNP_19990 == 2 | afiliado_SNP_20530 == 3 | afiliado_SNP_otro == 4 ~ "ONP/Otros (Público)",
      no_afiliado_pensiones == 5 ~ "No afiliado a pensiones",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # --------------------------------------------------------------------------
# D. Índices de Confianza Institucional ------ ÍNDICES
# --------------------------------------------------------------------------
# 1. Convertimos los 5 (No sabe) y 9 (Missing value) a verdaderos NAs
mutate(across(starts_with("confianza_"), ~na_if(., 5))) %>%
  mutate(across(starts_with("confianza_"), ~na_if(., 9))) %>%
  
  # Operaciones por fila para calcular los índices del individuo
  rowwise() %>%
  mutate(
    # Índice 1: Promedio Simple
    promedio_confianza = mean(c_across(starts_with("confianza_")), na.rm = TRUE),
    
    # Índice 2: Media Geométrica (Penaliza desequilibrios, requiere valores > 0)
    # Sumamos 1 temporalmente a todo porque la escala original es 1-4
    indice_confianza_geom = exp(mean(log(c_across(starts_with("confianza_"))), na.rm = TRUE))
  ) %>%
  ungroup() %>%
  
  # Normalizamos ambos índices al rango [0, 1]
  # Fórmula: (Valor - Mínimo) / (Máximo - Mínimo) -> (x - 1) / (4 - 1)
  mutate(
    indice_confianza_simple = (promedio_confianza - 1) / (4 - 1),
    indice_confianza_geom = (indice_confianza_geom - 1) / (4 - 1)
  )

# Actualizamos el diseño muestral con la nueva base analítica
enaho_diseno_analitico <- enaho_analitica %>%
  filter(!is.na(factorA07)) %>%
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE)


