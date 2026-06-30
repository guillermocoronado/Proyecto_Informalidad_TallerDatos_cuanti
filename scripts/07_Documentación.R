# ==============================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: Documentar 
# Autor: Guillermo Coronado
# Fecha: 30-06-2026
# Objetivo: Añadir metadatos a la base analítica y generar el codebook final.
# ==============================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y PAQUETES
# ------------------------------------------------------------------------------
install.packages(c("labelled", "codebook", "dataMaid"))

library(tidyverse)
library(arrow)
library(here)
library(labelled)  # Para inyectar etiquetas y metadatos en las variables
library(codebook)  # Para automatizar el libro de códigos interactivo
library(dataMaid)  # Para auditoría y reportes rápidos de calidad de datos
renv::snapshot()

# Cargamos nuestra base de datos analítica final (fruto de EXTRAER a CLASIFICAR)
enaho_final <- read_parquet(here("datos", "procesados", "enaho_2025_24_06_26.parquet"))

# ==============================================================================
# 1. INYECTAMOS LOS METADATOS---------------------------------------------------
# ==============================================================================
# Un codebook requiere la etiqueta descriptiva y la fuente original de cada variable.
# Usamos var_label() para darles un nombre humano y coherente.

# A. Variables Base Exploradas (Etiquetadas)
var_label(enaho_final$edad) <- "Edad del encuestado (Fuente: P208A)"
var_label(enaho_final$sexo) <- "Sexo del encuestado (Fuente: P207)"
var_label(enaho_final$tiene_ruc_etiqueta) <- "Registro en SUNAT del centro de trabajo (Fuente: P510A1)"
var_label(enaho_final$categoria_ocupacional_etiqueta) <- "Categoría Ocupacional Principal (Fuente: P507)"
var_label(enaho_final$tipo_contrato_etiqueta) <- "Tipo de Contrato Laboral (Fuente: P511A)"
var_label(enaho_final$confianza_congreso) <- "Confianza en el Congreso de la República (Fuente: P1$12)"
var_label(enaho_final$ingreso_prin_imputado) <- "Ingreso total ocupación principal (Soles corrientes) (Fuente: P524A1)"

# B. Variables Analíticas (Clasificadas)
var_label(enaho_final$formalidad_sector) <- "Tipología: Formalidad del Sector Económico"
var_label(enaho_final$formalidad_empleo) <- "Tipología: Formalidad del Empleo"
var_label(enaho_final$tipologia_laboral) <- "Tipología: Matriz Multidimensional de Informalidad"
var_label(enaho_final$grupo_edad_teoria) <- "Grupo de Edad (Cortes Teóricos)"
var_label(enaho_final$quintil_ingreso) <- "Quintil de Ingresos (Corte Estadístico)"
var_label(enaho_final$edad_z) <- "Edad estandarizada (Puntaje Z)"
var_label(enaho_final$afiliacion_salud) <- "Condición de afiliación al sistema de salud"
var_label(enaho_final$afiliacion_pensiones) <- "Tipo de sistema previsional afiliado"
var_label(enaho_final$indice_confianza_simple) <- "Índice de Confianza Institucional (Promedio Normalizado 0-1)"

# ==============================================================================
# 2. DOCUMENTACIÓN DE DECISIONES METODOLÓGICAS (TRATAMIENTO DE NAs)
# ==============================================================================
# El paquete 'codebook' permite agregar descripciones extendidas. Aquí dejamos 
# registro de las reglas de imputación y recodificación.

# Variables con tratamientos complejos de NAs
dict_metadata <- list(
  ingreso_prin_imputado = "Los valores 999999 se recodificaron a NA. Los casos perdidos (MNAR) fueron imputados usando el algoritmo MICE (Predictive Mean Matching) condicionado al nivel educativo.",
  confianza_congreso = "Escala original 1-4. Valores 5 (No sabe) y 9 (Missing) convertidos a NA. NAs imputados con la mediana para la población adulta.",
  tipologia_laboral = "Construida a partir del cruce de formalidad_sector (proxy: RUC) y formalidad_empleo (proxy: Categoría ocupacional y tipo de contrato).",
  indice_confianza_simple = "Promedio simple de 21 instituciones. Escala original re-escalada al rango [0, 1] mediante normalización Min-Max."
)

# Aplicamos las descripciones iterativamente a las columnas correspondientes
for (var in names(dict_metadata)) {
  attr(enaho_final[[var]], "description") <- dict_metadata[[var]]
}

# ==============================================================================
# 3. GENERACIÓN AUTOMATIZADA DE DOCUMENTACIÓN
# ==============================================================================

# ------------------------------------------------------------------------------
# OPCIÓN A: El paquete 'dataMaid' (Reporte rápido)
# ------------------------------------------------------------------------------
# Ideal para quienes detecten anomalías finales antes de publicar.
# Genera un PDF/HTML con la distribución, NAs y valores atípicos.

makeDataReport(
  enaho_final,
  output = "html",
  # Cambiamos la extensión a .Rmd para evitar el warning
  file = here("outputs", "CodeBook_dataMaid.Rmd"),
  replace = TRUE,
  # Filtramos solo las variables de interés (quitando el duplicado al final)
  vars = c("edad", "sexo", "tiene_ruc_etiqueta", "categoria_ocupacional_etiqueta", "tipo_contrato_etiqueta",
           "formalidad_sector", "formalidad_empleo", "grupo_edad_teoria", "quintil_ingreso",
           "afiliacion_salud", "afiliacion_pensiones", "indice_confianza_simple",
           "ingreso_prin_imputado", "tipologia_laboral"),
  reportTitle = "CodeBook del proyecto - Análisis de la Informalidad Laboral utilizando datos de la ENAHO 2025"
)

# ------------------------------------------------------------------------------
# OPCIÓN B: El paquete 'codebook' (El estándar de Ciencia Abierta)
# ------------------------------------------------------------------------------
# Genera un CodeBook más completo, incluyendo frecuencias, tipos, etiquetas 
# y estadísticos básicos de manera interactiva.

# Seleccionamos las variables objetivo para el codebook final
base_codebook <- enaho_final %>%
  select(
    conglome, estrato, factorA07, # Llaves y diseño
    edad, sexo, tiene_ruc_etiqueta, categoria_ocupacional_etiqueta, tipo_contrato_etiqueta,
    ingreso_prin_imputado, confianza_congreso,
    formalidad_sector, formalidad_empleo, tipologia_laboral, grupo_edad_teoria,
    quintil_ingreso, edad_z, afiliacion_salud, afiliacion_pensiones, indice_confianza_simple
  )

# Agregamos metadatos a nivel de ESTUDIO (Ficha Técnica)
metadata(base_codebook)$name <- "Base de Datos Analítica - Informalidad Laboral ENAHO 2025"
metadata(base_codebook)$description <- "Submuestra de la Encuesta Nacional de Hogares (2025) restringida a PEA Ocupada, mayores de 18 años, con lengua materna definida."
metadata(base_codebook)$creator <- "Guillermo Coronado"

# Generamos el libro de códigos interactivo en HTML
codebook(
  base_codebook,
  title = "Codebook: Informalidad Laboral en el Perú",
  subtitle = "Proyecto Final - Taller de Procesamiento de Datos",
  output = codebook::codebook_html(
    dir = here("outputs"),
    file = "Codebook_Informalidad_2025.html"
  )
)

# Con este HTML generado, cualquier tercero puede entender la base sin necesidad 
# de preguntarle directamente al autor.