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

# Cargamos la base de datos limpia (ACONDICIONADA)
enaho_limpia <- read_parquet("datos/procesados/enaho_2025_19_06_25.parquet")

# ------------------------------------------------------------------------------
# 1. PREPARACIÓN DE ETIQUETAS--------------------------------------------------- 
# ------------------------------------------------------------------------------
#Hay que tener en cuenta que todas las variables que exploraremos deben haber sido acondicionadas correctamente
#y, en caso sea necesario, se debe haber tratado correctamente los NAs (en este caso solo lo hicimos con tres
#variables por motivos de tiempo)

enaho_explorar <- enaho_limpia %>%
  mutate(
    # A. Laborales (Crudas)
    tiene_ruc_etiqueta = factor(tiene_ruc, 
                                levels = c(1, 2, 3), 
                                labels = c("Persona Jurídica", "Persona Natural", "No tiene RUC")),
    
    categoria_ocupacional_etiqueta = factor(categoria_ocupacional, 
                                            levels = 1:7, 
                                            labels = c("Empleador o patrono", "Trabajador independiente", 
                                                       "Empleado", "Obrero", "Trabajador familiar no remunerado", 
                                                       "Trabajador del hogar", "Otro")),
    
    # NUEVO: Tipo de contrato para trabajadores dependientes
    tipo_contrato_etiqueta = factor(tipo_contrato,
                                    levels = 1:8,
                                    labels = c("A plazo indefinido", "A plazo fijo", "Periodo de prueba", 
                                               "Prácticas", "Locación de servicios", "CAS", 
                                               "Sin contrato", "Otro")),
    
    # B. Demográficas (Crudas)
    sexo_etiqueta = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # C. Limpieza Numérica Estricta - esto lo hacemos porque el factor de expansión no está en numérico
    factorA07 = as.numeric(str_replace_all(factorA07, ",", ".")),
    conglome  = as.numeric(conglome),
    estrato   = as.numeric(estrato),
    edad      = as.numeric(edad),
    ingreso_prin_imputado = as.numeric(ingreso_prin_imputado)
  ) %>%
  
  # D. Limpieza de NAs en Confianza Institucional (Cruda 1 a 4) - Recordemos cómo lo tratamos en realidad!
  mutate(across(starts_with("confianza_"), ~na_if(., 5))) %>% # 5 = No sabe
  mutate(across(starts_with("confianza_"), ~na_if(., 9)))     # 9 = Missing

# ------------------------------------------------------------------------------
# 2. DISEÑO MUESTRAL------------------------------------------------------------
# ------------------------------------------------------------------------------
enaho_diseno <- enaho_explorar %>%
  filter(!is.na(factorA07)) %>%
  as_survey_design(
    ids = conglome,          
    strata = estrato,        
    weights = factorA07,     
    nest = TRUE              
  ) #Así utilizaremos el factor de expansión

# ==============================================================================
# 3. EXPLORACIÓN UNIVARIADA: TABLAS DESCRIPTIVAS
# ==============================================================================

# Helper function para crear el formato Flextable estandarizado
formato_flextable <- function(tabla, titulo) {
  flextable(tabla) %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos expandidos a nivel poblacional.") %>%
    autofit() %>% theme_vanilla() %>% align(align = "center", part = "all") %>% 
    align(j = 1, align = "left", part = "body") %>% bold(part = "header") %>%
    align(align = "left", part = "footer") %>% fontsize(size = 9, part = "footer") %>% 
    hline_bottom(part = "footer", border = officer::fp_border(width = 0))
}

# ------------------------------------------------------------------------------
# 3.1 Tabla Cruda: Categoría Ocupacional
# ------------------------------------------------------------------------------
tabla_ocupacion <- enaho_diseno %>%
  filter(!is.na(categoria_ocupacional_etiqueta)) %>%
  group_by(categoria_ocupacional_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Categoría Ocupacional` = categoria_ocupacional_etiqueta, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_ocupacion <- formato_flextable(tabla_ocupacion, "Tabla 1. Perú: Distribución de la PEA ocupada según categoría ocupacional")

# ------------------------------------------------------------------------------
# 3.2 Tabla Cruda: Condición de RUC
# ------------------------------------------------------------------------------
tabla_ruc <- enaho_diseno %>%
  filter(!is.na(tiene_ruc_etiqueta)) %>%
  group_by(tiene_ruc_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Condición de RUC` = tiene_ruc_etiqueta, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_ruc <- formato_flextable(tabla_ruc, "Tabla 2. Perú: Distribución del centro de trabajo según tipo de registro (RUC)")

# ------------------------------------------------------------------------------
# 3.3 Tabla Cruda: Tipo de Contrato (Solo dependientes)
# ------------------------------------------------------------------------------
tabla_contrato <- enaho_diseno %>%
  filter(!is.na(tipo_contrato_etiqueta)) %>%
  group_by(tipo_contrato_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Tipo de Contrato` = tipo_contrato_etiqueta, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_contrato <- formato_flextable(tabla_contrato, "Tabla 3. Perú: Trabajadores dependientes según tipo de contrato laboral")

# ------------------------------------------------------------------------------
# 3.4 Bloque Salud (Múltiples variables combinadas)
# ------------------------------------------------------------------------------
tabla_salud <- enaho_explorar %>%
  select(conglome, estrato, factorA07, starts_with("afiliado_"), -afiliado_SPP, -starts_with("afiliado_SNP")) %>%
  pivot_longer(cols = starts_with("afiliado_"), names_to = "Seguro", values_to = "Afiliado") %>%
  filter(Afiliado == 1) %>% 
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE) %>%
  group_by(Seguro) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  mutate(
    Porcentaje = (Poblacion / sum(enaho_explorar$factorA07, na.rm = TRUE)) * 100,
    Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%"),
    Seguro = str_to_title(str_remove(Seguro, "afiliado_"))
  ) %>%
  arrange(desc(parse_number(str_remove(Poblacion, ",")))) %>%
  rename(`Sistema de Salud` = Seguro, `Afiliados (N)` = Poblacion, `% de la PEA` = Porcentaje)

ft_salud <- formato_flextable(tabla_salud, "Tabla 4. Perú: Población ocupada afiliada a sistemas de salud")

# ------------------------------------------------------------------------------
# 3.5 Bloque Pensiones (NUEVO: Múltiples variables combinadas)
# ------------------------------------------------------------------------------
tabla_pensiones <- enaho_explorar %>%
  select(conglome, estrato, factorA07, afiliado_SPP, starts_with("afiliado_SNP"), no_afiliado_pensiones) %>%
  pivot_longer(cols = c(afiliado_SPP, starts_with("afiliado_SNP"), no_afiliado_pensiones), names_to = "Sistema", values_to = "Valor") %>%
  filter(Valor > 0) %>% # Retenemos solo la opción que marcaron (1, 2, 3, 4 o 5)
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE) %>%
  group_by(Sistema) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  mutate(
    Porcentaje = (Poblacion / sum(enaho_explorar$factorA07, na.rm = TRUE)) * 100,
    Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%"),
    Sistema = case_when(
      Sistema == "afiliado_SPP" ~ "AFP (Sistema Privado)",
      Sistema == "afiliado_SNP_19990" ~ "ONP (Régimen 19990)",
      Sistema == "afiliado_SNP_20530" ~ "Cédula Viva (Régimen 20530)",
      Sistema == "afiliado_SNP_otro" ~ "Otro sistema público",
      Sistema == "no_afiliado_pensiones" ~ "Sin afiliación previsional"
    )
  ) %>%
  arrange(desc(parse_number(str_remove(Poblacion, ",")))) %>%
  rename(`Sistema de Pensiones` = Sistema, `Afiliados (N)` = Poblacion, `% de la PEA` = Porcentaje)

ft_pensiones <- formato_flextable(tabla_pensiones, "Tabla 5. Perú: Población ocupada afiliada a sistemas de pensiones")

# ------------------------------------------------------------------------------
# 3.6 Bloque Confianza Institucional (NUEVO: Ranking de promedios ponderados)
# ------------------------------------------------------------------------------
tabla_confianza <- enaho_explorar %>%
  select(conglome, estrato, factorA07, starts_with("confianza_")) %>%
  pivot_longer(cols = starts_with("confianza_"), names_to = "Institucion", values_to = "Nivel") %>%
  filter(!is.na(Nivel)) %>%
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE) %>%
  group_by(Institucion) %>%
  summarise(Promedio = survey_mean(Nivel, vartype = NULL)) %>%
  mutate(
    Institucion = str_to_title(str_replace_all(str_remove(Institucion, "confianza_"), "_", " ")),
    Promedio = round(Promedio, 2)
  ) %>%
  select(Institucion, Promedio) %>% 
  arrange(desc(Promedio)) %>%
  rename(`Institución Evaluada` = Institucion, `Nivel Promedio (1=Nada, 4=Bastante)` = Promedio)

ft_confianza <- formato_flextable(tabla_confianza, "Tabla 6. Perú: Ranking de confianza institucional en la PEA ocupada")

# ------------------------------------------------------------------------------
# 3.7 Estadísticos de variables numéricas continuas (Edad e Ingreso)
# ------------------------------------------------------------------------------
stats_continuas <- enaho_diseno %>%
  summarise(
    `Edad: Promedio` = survey_mean(edad, na.rm = TRUE, vartype = NULL),
    `Edad: Mediana`  = survey_median(edad, na.rm = TRUE, vartype = NULL),
    `Ingreso: Promedio` = survey_mean(ingreso_prin_imputado, na.rm = TRUE, vartype = NULL),
    `Ingreso: Mediana`  = survey_median(ingreso_prin_imputado, na.rm = TRUE, vartype = NULL)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Indicador", values_to = "Valor") %>%
  mutate(Valor = scales::comma(round(Valor, 1)))

ft_stats <- formato_flextable(stats_continuas, "Tabla 7. Perú: Estadísticos de resumen para Edad e Ingreso Principal")

# ==============================================================================
# 4. EXPLORACIÓN UNIVARIADA: GRÁFICOS
# ==============================================================================

# 4.1 Histograma: Edad (Ponderado)
plot_edad <- ggplot(enaho_explorar %>% filter(!is.na(edad) & !is.na(factorA07)), 
                    aes(x = edad, weight = factorA07)) +
  geom_histogram(fill = "#4A7C59", color = "white", binwidth = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 1. Distribución de edad de la PEA ocupada", x = "Edad (años)", y = "Frecuencia Poblacional", caption = "Fuente: ENAHO 2025.") + theme_minimal()

# 4.2 Histograma: Ingreso Principal (Ponderado)
plot_ingreso <- ggplot(enaho_explorar %>% filter(!is.na(ingreso_prin_imputado) & !is.na(factorA07)), 
                       aes(x = ingreso_prin_imputado, weight = factorA07)) +
  geom_histogram(fill = "#2E5B88", color = "white", bins = 50) +
  coord_cartesian(xlim = c(0, 10000)) + scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 2. Distribución del ingreso principal", x = "Ingreso (Soles)", y = "Frecuencia Poblacional", caption = "Fuente: ENAHO 2025. Nota: Eje X truncado en S/10,000.") + theme_minimal()

# 4.3 Barras: Confianza en el JNE 
plot_confianza <- ggplot(enaho_explorar %>% filter(!is.na(confianza_JNE) & !is.na(factorA07)), 
                         aes(x = as.factor(confianza_JNE), weight = factorA07)) +
  geom_bar(fill = "#E69F00", alpha = 0.8) +
  scale_x_discrete(labels = c("1" = "1. Nada", "2" = "2. Poco", "3" = "3. Suficiente", "4" = "4. Bastante")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 3. Nivel de confianza en el Jurado Nacional de Elecciones (JNE)", x = "Nivel de Confianza", y = "Población", caption = "Fuente: ENAHO 2025. Nota: Excluye NS/NR.") + theme_minimal()

# ==============================================================================
# 5. EXPORTACIÓN DE RESULTADOS
# ==============================================================================
if (!dir.exists("outputs/exploracion_cruda")) dir.create("outputs/exploracion_cruda", recursive = TRUE)

# Tablas
save_as_docx(ft_ocupacion, path = "outputs/exploracion_cruda/Tabla1_Ocupacion.docx")
save_as_docx(ft_ruc,       path = "outputs/exploracion_cruda/Tabla2_RUC.docx")
save_as_docx(ft_contrato,  path = "outputs/exploracion_cruda/Tabla3_Contrato.docx")
save_as_docx(ft_salud,     path = "outputs/exploracion_cruda/Tabla4_Salud.docx")
save_as_docx(ft_pensiones, path = "outputs/exploracion_cruda/Tabla5_Pensiones.docx")
save_as_docx(ft_confianza, path = "outputs/exploracion_cruda/Tabla6_RankingConfianza.docx")
save_as_docx(ft_stats,     path = "outputs/exploracion_cruda/Tabla7_Stats_Continuas.docx")

# Gráficos
ggsave("outputs/exploracion_cruda/Grafico1_Edad.png", plot = plot_edad, width = 8, height = 5, bg="white")
ggsave("outputs/exploracion_cruda/Grafico2_Ingreso.png", plot = plot_ingreso, width = 8, height = 5, bg="white")
ggsave("outputs/exploracion_cruda/Grafico3_ConfianzaJNE.png", plot = plot_confianza, width = 8, height = 5, bg="white")

write_parquet(enaho_explorar, "datos/procesados/enaho_lista_para_clasificar.parquet")