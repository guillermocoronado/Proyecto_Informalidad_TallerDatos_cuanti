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
# 1. SELECCIÓN DE VARIABLES PARA EL CODEBOOK
# ==============================================================================

#Creamos una base de datos solo con las variables que nos interesa documentar 
#(las que usamos en el reporte descriptivo + nuestras variables analíticas)

enaho_codebook <- enaho_final %>%
  select(
    edad, sexo, tiene_ruc_etiqueta, categoria_ocupacional_etiqueta, 
    tipo_contrato_etiqueta, formalidad_sector, formalidad_empleo, 
    grupo_edad_teoria, quintil_ingreso, afiliacion_salud, 
    afiliacion_pensiones, indice_confianza_simple, 
    ingreso_prin_imputado, tipologia_laboral
  ) %>%
  mutate(across(where(is.character), as.factor)) #Convertimos las variables de character a factor para que "Codebook" detecte nuestras etiquetas

#Exportamos como la base de datos final de nuestro proyecto
write_parquet(enaho_codebook, here("datos", "procesados", "enaho_2025_02_07_27.parquet"))


# ==============================================================================
# 2. INYECTAMOS LOS METADATOS---------------------------------------------------
# ==============================================================================
# Un codebook requiere la etiqueta descriptiva y la fuente original de cada variable.
# Usamos var_label() para darles un nombre humano y coherente.

# A. Variables Base Exploradas (Etiquetadas)
var_label(enaho_codebook$edad) <- "Edad del encuestado (Fuente: P208A)"
var_label(enaho_codebook$sexo) <- "Sexo del encuestado (Fuente: P207)"
var_label(enaho_codebook$tiene_ruc_etiqueta) <- "Registro en SUNAT del centro de trabajo (Fuente: P510A1)"
var_label(enaho_codebook$categoria_ocupacional_etiqueta) <- "Categoría Ocupacional Principal (Fuente: P507)"
var_label(enaho_codebook$tipo_contrato_etiqueta) <- "Tipo de Contrato Laboral (Fuente: P511A)"
var_label(enaho_codebook$ingreso_prin_imputado) <- "Ingreso total ocupación principal (Soles corrientes) (Fuente: P524A1)"

# B. Variables Analíticas (Clasificadas)
var_label(enaho_codebook$formalidad_sector) <- "Tipología: Formalidad del Sector Económico"
var_label(enaho_codebook$formalidad_empleo) <- "Tipología: Formalidad de la Condición de Empleo"
var_label(enaho_codebook$tipologia_laboral) <- "Tipología MECE: Matriz Multidimensional de Informalidad"
var_label(enaho_codebook$grupo_edad_teoria) <- "Grupo de Edad (Cortes Teóricos)"
var_label(enaho_codebook$quintil_ingreso) <- "Quintil de Ingresos (Corte Estadístico)"
var_label(enaho_codebook$afiliacion_salud) <- "Condición de afiliación al sistema de salud"
var_label(enaho_codebook$afiliacion_pensiones) <- "Tipo de sistema previsional afiliado"
var_label(enaho_codebook$indice_confianza_simple) <- "Índice de Confianza Institucional (Promedio Normalizado 0-1)"

# ==============================================================================
# 3. DOCUMENTACIÓN DE DECISIONES METODOLÓGICAS (TRATAMIENTO DE NAs)
# ==============================================================================

# Diccionario de decisiones metodológicas
dict_metadata <- list(
  ingreso_prin_imputado = "Los valores 999999 se recodificaron a NA. Los casos perdidos (MNAR) fueron imputados usando el algoritmo MICE condicionado al nivel educativo.",
  tipologia_laboral = "Construida a partir del cruce de formalidad_sector (proxy: RUC) y formalidad_empleo (proxy: Categoría ocupacional y tipo de contrato).",
  indice_confianza_simple = "Promedio simple de 21 instituciones. Escala original re-escalada al rango [0, 1] mediante normalización Min-Max. NAs imputados con la mediana."
)

# Aplicamos las descripciones iterativamente a las columnas correspondientes
for (var in names(dict_metadata)) {
  attr(enaho_codebook[[var]], "description") <- dict_metadata[[var]]
}

# Agregamos metadatos a nivel de ESTUDIO (Ficha Técnica)
metadata(enaho_codebook)$name <- "Base de Datos Analítica - Informalidad Laboral ENAHO 2025"
metadata(enaho_codebook)$description <- "Submuestra de la Encuesta Nacional de Hogares (2025) restringida a PEA Ocupada, mayores de 18 años, con lengua materna definida."
metadata(enaho_codebook)$creator <- "Guillermo Coronado"

#Guardamos nuestra base de datos con toda esta metadata e info adicional que hemos incluido (acá chancaremos)
write_parquet(enaho_codebook, here("datos", "procesados", "enaho_2025_02_07_27.parquet"))

# ==============================================================================
# 4. GENERACIÓN AUTOMATIZADA DE DOCUMENTACIÓN (ambas opciones son válidas!)
# ==============================================================================

# ------------------------------------------------------------------------------
# OPCIÓN A: El paquete 'dataMaid' (Reporte rápido)
# ------------------------------------------------------------------------------
# Ideal para quienes detecten anomalías finales antes de publicar.
# Genera un PDF/HTML con la distribución, NAs y valores atípicos.

makeDataReport(
  enaho_codebook, 
  output = "html", 
  file = here("outputs", "CodeBook_dataMaid.Rmd"),
  replace = TRUE,
  reportTitle = "CodeBook del proyecto - Análisis de la Informalidad Laboral utilizando datos de la ENAHO 2025"
)

# ------------------------------------------------------------------------------
# OPCIÓN B: El paquete 'codebook' (Reporte más detallado)
# ------------------------------------------------------------------------------
# Genera un CodeBook más completo, incluyendo frecuencias, tipos, etiquetas 
# y estadísticos básicos de manera interactiva.

codebook(enaho_codebook)
