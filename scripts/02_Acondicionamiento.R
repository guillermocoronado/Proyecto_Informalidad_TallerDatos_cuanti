# ==============================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: Acondicionamiento 
# Autor: Guillermo Coronado
# Fecha: 18-06-2026
# Objetivo: Acondicionar la base de datos consolidada (Tipado, Selección, 
#           Renombrado, Tratamiento de NAs).
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN DEL ENTORNO
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(janitor)
library(naniar)
renv::snapshot()

# ------------------------------------------------------------------------------
# 1. CARGA, SELECCIÓN, RENOMBRADO Y DIAGNÓSTICO 
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
# 3. EL TRATAMIENTO DE VALORES PERDIDOS (NAs)
# ------------------------------------------------------------------------------

enaho_acondicionada <- enaho_seleccion %>%
  # DECISIÓN 1: Confianza Institucional (MNAR/MAR)
  # Los códigos 8 (No sabe) y 9 (No responde) se pasan a NA.
  mutate(
    across(starts_with("p1_"), ~na_if(., 8)),
    across(starts_with("p1_"), ~na_if(., 9))
  ) %>%
  
  # DECISIÓN 2: Ingresos Laborales (MNAR)
  # El código 999999 es de sistema. Es crítico pasarlo a NA antes de promediar.
  mutate(
    ingreso_prin = ifelse(ingreso_prin == 999999, NA, ingreso_prin),
    ingreso_sec  = ifelse(ingreso_sec == 999999, NA, ingreso_sec)
  ) %>%
  
  # DECISIÓN 3: Filtros de Informalidad (MCAR Estructural)
  # El código 9 ("No sabe/No aplica") en RUC y Contrato se pasa a NA.
  # Si alguien no trabaja, el salto del cuestionario lo deja vacío lógicamente.
  mutate(
    tiene_ruc     = na_if(tiene_ruc, 9),
    tipo_contrato = na_if(tipo_contrato, 9)
  )
