# ==============================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: Acondicionamiento 
# Autor: Guillermo Coronado
# Fecha: 18-06-2026
# Objetivo: Acondicionar la base de datos consolidada (Tipado, Selección, 
#           Renombrado, Tratamiento de NAs).
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN DEL ENTORNO--------------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(janitor)
library(naniar)
renv::snapshot()

# ------------------------------------------------------------------------------
# 1. CARGA, SELECCIÓN, RENOMBRADO Y DIAGNÓSTICO---------------------------------
# ------------------------------------------------------------------------------
# Leemos la base que consolidamos la clase pasada en formato parquet. 
enaho_raw <- read_parquet("datos/procesados/enaho_2025_120626.parquet")

#Seleccionamos las variables de nuestro interés (así trabajamos con una base menos pesada)
#Al mismo tiempo que seleccionamos, renombramos usando las herramientas de dplyr
enaho_seleccion <- enaho_raw %>%
  select(
    # Llaves de integración y factores de expansión (Manteniendo la estructura)
    año = AÑO,
    mes = MES,
    conglome = CONGLOME, 
    nconglome = NCONGLOME,
    subconglome = SUB_CONGLOME,
    vivienda = VIVIENDA, 
    hogar    = HOGAR, 
    codperso  = CODPERSO, 
    codinfor = CODINFOR,
    ubigeo   = UBIGEO, 
    dominio  = DOMINIO, 
    estrato  = ESTRATO, 
    factorA07  = FACTORA07,
    factor07 = FACTOR07.x,
    fac500a = FAC500A,
    
    # Demográficas y Educación (módulos 300, 400 y 500)
    sexo           = P207,
    edad           = P208A,
    lengua_materna = P300A,
    nivel_edu      = P301A,
    etnicidad      = P558C,
    discapacidad_moverse = P401H1,
    discapacidad_visual = P401H2,
    discapacidad_comunicarse = P401H3,
    discapacidad_auditiva = P401H4,
    discapacidad_cognitiva = P401H5,
    discapacidad_social = P401H6,
    malestar_cronico = P401,
    
    
    # Mercado Laboral (Módulo 500)
    trabajo_semana_pasada = P501,
    empleo_fijo_volvera  = P502,
    negocio_volvera  = P503,
    trabajo_negocio = P5041,
    trabajo_servicio = P5042,
    trabajo_casa_vender = P5043,
    trabajo_venta_belleza = P5044,
    trabajo_artesanal = P5045,
    trabajo_practicas = P5046,
    trabajo_hogar_particular = P5047,
    trabajo_fabrica_producto = P5048,
    trabajo_chacra_amimales = P5049,
    trabajo_familiar_no_remunerado = P50410,
    trabajo_otro = P50411,
    hizo_semana_pasada = P546,
    queria_trabajar = P547,
    disponible_trabajar = P548,
    categoria_ocupacional = P507,
    sector_empleo = P510,
    
    # Dimensiones de Informalidad (Sector y Empleo)
    tiene_ruc      = P510A1,   # Proxy de Sector Formal (Registro en SUNAT)
    tipo_contrato  = P511A,   # Proxy de Empleo Formal (Contrato escrito)
    tamano_empresa = P512A,
    
    # Ingresos  
    temporalidad_pago = P523,
    ingreso_prin   = P524A1, # Ingreso total ocupación principal
    ingreso_prin_no_sabe = P524A2,
    ingreso_sec    = P538A1,  # Ingreso total ocupaciones secundarias
    ingreso_sec_no_sabe = P538A2,
    recibe_ingreso_utilidades = P5445A,
    ingreso_utilidades = P5445B,
    
    # Seguridad Social: Pensiones
    afiliado_SPP = P558A1,
    afiliado_SNP_19990 = P558A2,
    afiliado_SNP_20530 = P558A3,
    afiliado_SNP_otro = P558A4,
    no_afiliado_pensiones = P558A5,
    ultimo_mes_aporte = P558B1,
    ultimo_ano_aporte = P558B2,
    ultimo_aporte_no_sabe = P558B3,
    
    #Seguridad Social: Salud
    afiliado_essalud = P4191,
    afiliado_privado = P4192,
    afiliado_eps = P4193,
    afiliado_FFAA_Policiales = P4194,
    afiliado_SIS = P4195,
    afiliado_universitario = P4196,
    afiliado_escolar = P4197,
    afiliado_otro = P4198,
    
    # Gobernabilidad y Confianza Institucional (Módulo 1B)
    confianza_jne = `P1$01`,
    confianza_onpe = `P1$02`,
    confianza_reniec = `P1$03`,
    confianza_muni_provincial = `P1$04`,
    confianza_muni_distrital = `P1$05`,
    confianza_pnp = `P1$06`,
    confianza_FFAA = `P1$07`,
    confianza_gore = `P1$08`,
    confianza_PJ = `P1$09`,
    confianza_minedu = `P1$10`,
    confianza_defensoria = `P1$11`,
    confianza_congreso = `P1$12`,
    confianza_partidos = `P1$13`,
    confianza_prensa_escrita = `P1$14`,
    confianza_radioTV = `P1$15`,
    confianza_iglesia = `P1$16`,
    confianza_procuraduria = `P1$17`,
    confianza_fiscalia = `P1$18`,
    confianza_contraloria = `P1$19`,
    confianza_sunat = `P1$20`,
    confianza_comision_anticorrupcion = `P1$21`,
  )

# Hacemos una inspección rápida
dim(enaho_seleccion)        # ¿Cuántas filas y columnas tenemos tras los joins previos?
names(enaho_seleccion)      # Verificamos si los nombres son legibles
glimpse(enaho_seleccion)    # Revisión crítica de cómo R interpretó los tipos de datos

# ------------------------------------------------------------------------------
# 3. DIAGNÓSTICO DE NAs Y REPORTE-----------------------------------------------
# ------------------------------------------------------------------------------

# 3.1 Visualización Gráfica (naniar)

# Creamos un gráfico de barras que muestra la cantidad de NAs por variable
grafico_nas <- gg_miss_var(enaho_seleccion, show_pct = TRUE) +
  labs(
    title = "Porcentaje de Valores Perdidos (NAs) por Variable",
    subtitle = "Proyecto: Análisis de la Informalidad laboral usando datos de la ENAHO (2025)",
    y = "% de Valores Perdidos",
    x = "Variables"
  ) +
  theme_minimal()

# Mostramos el gráfico en el panel de RStudio
print(grafico_nas)

# Exportamos el gráfico a nuestra carpeta de outputs
ggsave("outputs/Grafico_NAs_Informalidad.png", plot = grafico_nas, 
       width = 8, height = 6, bg = "white")

# 3.2 Reporte Tabular 
reporte_nas <- enaho_seleccion %>%
  summarise(across(everything(), ~ round(sum(is.na(.)) / n() * 100, 2))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "porcentaje_na") %>%
  arrange(desc(porcentaje_na))

write_csv(reporte_nas, "outputs/Reporte_Datos_Perdidos_ENAHO.csv")

table(enaho_seleccion$confianza_congreso, useNA = "ifany")
sum(is.na(enaho_seleccion$ingreso_prin))
sum(enaho_seleccion$ingreso_prin == 999999, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 4. TRATAMIENTO DE NAs---------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# CASO 1: MCAR (Missing Completely At Random) / Ausencia Estructural
# Variable: lengua_materna
# Problema: Hay "99" (No especificado) y celdas vacías (NA, menores de 3 años).
# Estrategia sugerida: Eliminación (Listwise)
# ------------------------------------------------------------------------------

# PASO 1.1: Diagnóstico
# Vemos cuántos NAs reales hay y cuántos "99" existen.
diagnostico_lengua <- enaho_seleccion %>%
  count(lengua_materna, is.na(lengua_materna)) %>%
  arrange(desc(n))
print(diagnostico_lengua)

# PASO 1.2: Tratamiento (Conversión y Eliminación Listwise)
# Primero, estandarizamos todo: convertimos los 99 a verdaderos NA.
# Luego, como es MCAR/Estructural, aplicamos la estrategia de Eliminación usando drop_na().
enaho_tratada <- enaho_seleccion %>%
  mutate(lengua_materna = na_if(lengua_materna, 99)) %>%
  # Eliminación Listwise: Botamos de la base a los que no tienen lengua materna
  # (Esto eliminará automáticamente a los menores de 3 años, lo cual es correcto 
  # si nuestro proyecto de informalidad laboral solo analiza adultos).
  drop_na(lengua_materna)

# ------------------------------------------------------------------------------
# CASO 2: MAR (Missing At Random)
# Variable: confianza_congreso
# Problema: No hay 9s, pero hay celdas vacías (NA) que pueden depender de otras variables (ej.: educacion).
# Estrategia sugerida: Imputación Simple (Mediana / Moda) - tener cuidado!
# ------------------------------------------------------------------------------

# PASO 2.1: Diagnóstico
# Como mencionaste que no hay nueves, solo verificamos cuántos NAs puros tenemos.
diagnostico_confianza <- enaho_tratada %>%
  summarise(
    total_nas = sum(is.na(confianza_congreso)),
    porcentaje = mean(is.na(confianza_congreso)) * 100
  )
print(diagnostico_confianza)

# PASO 2.2: Tratamiento (Imputación Simple)
# Dado que es una variable categórica/ordinal (Likert de confianza), la media 
# no tiene sentido. La diapositiva permite usar la Mediana o la Moda. 
# Usaremos la Mediana (el valor central de confianza).
mediana_confianza <- median(enaho_tratada$confianza_congreso, na.rm = TRUE)

enaho_tratada <- enaho_tratada %>%
  mutate(
    # replace_na busca los vacíos y los rellena con el valor que le indiquemos
    confianza_congreso = replace_na(confianza_congreso, mediana_confianza)
  )
