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
# 3. EXPLORACIÓN UNIVARIADA: TABLAS DESCRIPTIVAS--------------------------------
# ==============================================================================
# Definimos una función para crear un formato Flextable estandarizado
formato_flextable <- function(tabla, titulo) {
  flextable(tabla) %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos expandidos a nivel poblacional.") %>%
    autofit() %>% 
    theme_vanilla() %>% 
    # TRUCO AQUÍ: Borramos las líneas horizontales internas del cuerpo de la tabla
    border_inner_h(part = "body", border = officer::fp_border(width = 0)) %>% 
    align(align = "center", part = "all") %>% 
    align(j = 1, align = "left", part = "body") %>% 
    bold(part = "header") %>%
    align(align = "left", part = "footer") %>% 
    fontsize(size = 9, part = "footer") %>% 
    # Aseguramos que la línea final del cuerpo y del pie de página sean correctas
    hline_bottom(part = "body", border = officer::fp_border(width = 1)) %>% 
    hline_bottom(part = "footer", border = officer::fp_border(width = 0))
}

# ------------------------------------------------------------------------------
# 3.1 Categoría Ocupacional-----------------------------------------------------
# ------------------------------------------------------------------------------
tabla_ocupacion <- enaho_diseno %>%
  filter(!is.na(categoria_ocupacional_etiqueta)) %>%
  group_by(categoria_ocupacional_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Categoría Ocupacional` = categoria_ocupacional_etiqueta, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_ocupacion <- formato_flextable(tabla_ocupacion, "Tabla 1. Perú: Distribución de la PEA ocupada según categoría ocupacional, 2025")
print(ft_ocupacion)

# ------------------------------------------------------------------------------
# 3.2 Formalidad del centro de trabajo------------------------------------------
# ------------------------------------------------------------------------------
tabla_ruc <- enaho_diseno %>%
  filter(!is.na(tiene_ruc_etiqueta)) %>%
  group_by(tiene_ruc_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Tipo de registro` = tiene_ruc_etiqueta, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_ruc <- formato_flextable(tabla_ruc, "Tabla 2. Perú: Distribución del centro de trabajo según tipo de registro, 2025")
print(ft_ruc)

# ------------------------------------------------------------------------------
# 3.3 Tipo de Contrato----------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_contrato <- enaho_diseno %>%
  filter(!is.na(tipo_contrato_etiqueta)) %>%
  group_by(tipo_contrato_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL), Porcentaje = survey_mean(vartype = NULL) * 100) %>%
  mutate(Poblacion = scales::comma(round(Poblacion, 0)), Porcentaje = paste0(round(Porcentaje, 1), "%")) %>%
  rename(`Tipo de Contrato` = tipo_contrato_etiqueta, `Total (N)` = Poblacion, `%` = Porcentaje)

ft_contrato <- formato_flextable(tabla_contrato, "Tabla 3. Perú: PEA ocupada según tipo de contrato laboral, 2025")
print(ft_contrato)

# ------------------------------------------------------------------------------
# 3.4 Bloque Salud (Múltiples variables combinadas)-----------------------------
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
    Poblacion = scales::comma(round(Poblacion, 0)), 
    Porcentaje = paste0(round(Porcentaje, 1), "%"),
    Seguro = str_to_title(str_remove(Seguro, "afiliado_")),
    # Corrección manual de siglas y nombres específicos
    Seguro = case_when(
      Seguro == "Sis" ~ "SIS",
      Seguro == "Eps" ~ "EPS",
      Seguro == "Ffaa_policiales" ~ "Sanidad de las FFAA o PNP",
      TRUE ~ Seguro
    )
  ) %>%
  arrange(desc(parse_number(str_remove(Poblacion, ",")))) %>%
  rename(`Sistema de Salud` = Seguro, `Afiliados (N)` = Poblacion, `% de la PEA` = Porcentaje)

ft_salud <- formato_flextable(tabla_salud, "Tabla 4. Perú: PEA ocupada según su afiliación a sistemas de salud, 2025")
print(ft_salud)

# ------------------------------------------------------------------------------
# 3.5 Bloque Pensiones (Múltiples variables combinadas)
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
      Sistema == "afiliado_SPP" ~ "Sistema Privado de Pensiones",
      Sistema == "afiliado_SNP_19990" ~ "Sistema Nacional de Pensiones (DL 19990)",
      Sistema == "afiliado_SNP_20530" ~ "Régimen DL 20530",
      Sistema == "afiliado_SNP_otro" ~ "Otro sistema público",
      Sistema == "no_afiliado_pensiones" ~ "Sin afiliación previsional"
    )
  ) %>%
  arrange(desc(parse_number(str_remove(Poblacion, ",")))) %>%
  rename(`Sistema de Pensiones` = Sistema, `Afiliados (N)` = Poblacion, `% de la PEA` = Porcentaje)

ft_pensiones <- formato_flextable(tabla_pensiones, "Tabla 5. Perú: PEA ocupada según afiliación a sistemas de pensiones")
print(ft_pensiones)

# ------------------------------------------------------------------------------
# 3.6 Bloque Confianza Institucional (Ranking de promedios ponderados)
# ------------------------------------------------------------------------------
tabla_confianza <- enaho_explorar %>%
  select(conglome, estrato, factorA07, starts_with("confianza_")) %>%
  pivot_longer(cols = starts_with("confianza_"), names_to = "Institucion", values_to = "Nivel") %>%
  filter(!is.na(Nivel)) %>%
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE) %>%
  group_by(Institucion) %>%
  summarise(Promedio = survey_mean(Nivel, vartype = NULL)) %>%
  mutate(
    # Primero limpiamos el prefijo y los guiones bajos
    Institucion = str_to_title(str_replace_all(str_remove(Institucion, "confianza_"), "_", " ")),
    
    # Luego aplicamos el diccionario exacto de nombres oficiales
    Institucion = case_when(
      Institucion == "Reniec" ~ "Registro Nacional de Identidad y Estado Civil - RENIEC",
      Institucion == "Minedu" ~ "Ministerio de Educación",
      Institucion == "Ffaa" ~ "Fuerzas Armadas",
      Institucion == "Defensoria" ~ "Defensoría del Pueblo",
      Institucion == "Radiotv" ~ "Radio y Televisión",
      Institucion == "Sunat" ~ "Superintendencia Nacional de Administración Tributaria - SUNAT",
      Institucion == "Onpe" ~ "Oficina Nacional de Procesos Electorales - ONPE",
      Institucion == "Muni Distrital" ~ "Municipalidad Distrital",
      Institucion == "Jne" ~ "Jurado Nacional de Elecciones - JNE",
      Institucion == "Muni Provincial" ~ "Municipalidad Provincial",
      Institucion == "Pnp" ~ "Policía Nacional del Perú - PNP",
      Institucion == "Pj" ~ "Poder Judicial",
      Institucion == "Fiscalia" ~ "Ministerio Público",
      Institucion == "Contraloria" ~ "Contraloría General de la Nación",
      Institucion == "Gore" ~ "Gobierno Regional",
      Institucion == "Procuraduria" ~ "Procuraduría Anticorrupción",
      Institucion == "Comision Anticorrupcion" ~ "Comisión Anticorrupción",
      Institucion == "Partidos" ~ "Partidos políticos",
      Institucion == "Congreso" ~ "Congreso de la República",
      TRUE ~ Institucion # Por si se nos escapa alguna 
    ),
    Promedio = round(Promedio, 2)
  ) %>%
  select(Institucion, Promedio) %>% 
  arrange(desc(Promedio)) %>%
  rename(`Institución Evaluada` = Institucion, `Nivel Promedio (1=Nada, 4=Bastante)` = Promedio)

ft_confianza <- formato_flextable(tabla_confianza, "Tabla 6. Perú: Confianza en instituciones entre la PEA ocupada, 2025")
print(ft_confianza)

# ------------------------------------------------------------------------------
# 3.7 Estadísticos de resumen: Edad (Variable Continua)
# ------------------------------------------------------------------------------
stats_edad <- enaho_diseno %>%
  filter(!is.na(edad)) %>%
  summarise(
    `Mínimo` = min(edad, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(edad, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(edad, vartype = NULL),
    `Media (Promedio)` = survey_mean(edad, vartype = NULL),
    `Desviación Estándar` = survey_sd(edad, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(edad, 0.75, vartype = NULL),
    `Máximo` = max(edad, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (Años)") %>%
  mutate(
    # Borramos los sufijos automáticos (_q25, _q75, etc.) que genera srvyr
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (Años)` = scales::comma(round(`Valor (Años)`, 1))
  )

ft_edad <- formato_flextable(stats_edad, "Tabla 7. Perú: Edad de la PEA Ocupada (estadísticos de resumen), 2025")
print(ft_edad)

# ------------------------------------------------------------------------------
# 3.8 Estadísticos de resumen: Ingreso Principal (Variable Continua)
# ------------------------------------------------------------------------------
stats_ingreso <- enaho_diseno %>%
  filter(!is.na(ingreso_prin_imputado)) %>%
  summarise(
    `Mínimo` = min(ingreso_prin_imputado, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(ingreso_prin_imputado, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(ingreso_prin_imputado, vartype = NULL),
    `Media (Promedio)` = survey_mean(ingreso_prin_imputado, vartype = NULL),
    `Desviación Estándar` = survey_sd(ingreso_prin_imputado, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(ingreso_prin_imputado, 0.75, vartype = NULL),
    `Percentil 90 (P90)` = survey_quantile(ingreso_prin_imputado, 0.90, vartype = NULL),
    `Percentil 99 (P99)` = survey_quantile(ingreso_prin_imputado, 0.99, vartype = NULL),
    `Máximo` = max(ingreso_prin_imputado, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor (Soles)") %>%
  mutate(
    Estadístico = str_remove(Estadístico, "_q[0-9]+"),
    `Valor (Soles)` = scales::comma(round(`Valor (Soles)`, 1))
  )

ft_ingreso <- formato_flextable(stats_ingreso, "Tabla 8. Perú: Ingreso proveniente de la ocupación principal de la PEA Ocupada (estadísticos de resumen), 2025")
print(ft_ingreso) #Importancia de imputar bien!

# ------------------------------------------------------------------------------
# 3.9 Bloque Discapacidad (Múltiples variables combinadas)
# ------------------------------------------------------------------------------
tabla_discapacidad <- enaho_explorar %>%
  select(conglome, estrato, factorA07, starts_with("discapacidad_")) %>%
  pivot_longer(cols = starts_with("discapacidad_"), names_to = "Tipo", values_to = "Tiene") %>%
  filter(Tiene == 1) %>% # Retenemos solo a quienes indicaron "1" (Sí tienen la dificultad)
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE) %>%
  group_by(Tipo) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  mutate(
    # Calculamos el % sobre el total de la PEA expandida
    Porcentaje = (Poblacion / sum(enaho_explorar$factorA07, na.rm = TRUE)) * 100,
    Poblacion = scales::comma(round(Poblacion, 0)), 
    Porcentaje = paste0(round(Porcentaje, 1), "%"),
    
    # Limpiamos los nombres para la tabla final
    Tipo = case_when(
      Tipo == "discapacidad_moverse" ~ "Dificultad para moverse o caminar",
      Tipo == "discapacidad_visual" ~ "Dificultad visual (aun usando lentes)",
      Tipo == "discapacidad_comunicarse" ~ "Dificultad para hablar o comunicarse",
      Tipo == "discapacidad_auditiva" ~ "Dificultad auditiva (aun usando audífonos)",
      Tipo == "discapacidad_cognitiva" ~ "Dificultad para entender o aprender",
      Tipo == "discapacidad_social" ~ "Dificultad para relacionarse con los demás",
      TRUE ~ str_to_title(str_replace_all(Tipo, "_", " "))
    )
  ) %>%
  arrange(desc(parse_number(str_remove(Poblacion, ",")))) %>%
  rename(`Tipo de Dificultad / Discapacidad` = Tipo, `Población (N)` = Poblacion, `% de la PEA` = Porcentaje)

ft_discapacidad <- formato_flextable(tabla_discapacidad, "Tabla 9. Perú: PEA ocupada según tipo de dificultad o discapacidad, 2025")
print(ft_discapacidad)

# ==============================================================================
# 4. EXPLORACIÓN UNIVARIADA: GRÁFICOS
# ==============================================================================

# 4.1 Histograma: Edad (Ponderado)
plot_edad <- ggplot(enaho_explorar %>% filter(!is.na(edad) & !is.na(factorA07)), 
                    aes(x = edad, weight = factorA07)) +
  geom_histogram(fill = "#4A7C59", color = "white", binwidth = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 1. Distribución de edad de la PEA ocupada", x = "Edad (años)", y = "Frecuencia Poblacional", caption = "Fuente: ENAHO 2025. Cálculos ajustados a nivel poblacional") + theme_minimal()
print(plot_edad)


# 4.2 Histograma: Ingreso Principal (Ponderado)
plot_ingreso <- ggplot(enaho_explorar %>% filter(!is.na(ingreso_prin_imputado) & !is.na(factorA07)), 
                       aes(x = ingreso_prin_imputado, weight = factorA07)) +
  geom_histogram(fill = "#2E5B88", color = "white", bins = 50) +
  coord_cartesian(xlim = c(0, 10000)) + scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 2. Distribución del ingreso principal", x = "Ingreso (Soles)", y = "Frecuencia Poblacional", caption = "Fuente: ENAHO 2025. Nota: Eje X truncado en S/10,000. Cálculos ajustados a nivel poblacional") + theme_minimal()
print(plot_ingreso)

# 4.3 Barras: Confianza en el JNE 
plot_confianza <- ggplot(enaho_explorar %>% filter(!is.na(confianza_jne) & !is.na(factorA07)), 
                         aes(x = as.factor(confianza_jne), weight = factorA07)) +
  geom_bar(fill = "#E69F00", alpha = 0.8) +
  scale_x_discrete(labels = c("1" = "1. Nada", "2" = "2. Poco", "3" = "3. Suficiente", "4" = "4. Bastante")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 3. Nivel de confianza en el Jurado Nacional de Elecciones (JNE)", x = "Nivel de Confianza", y = "Población", caption = "Fuente: ENAHO 2025. Nota: Excluye NS/NR.") + theme_minimal()
print(plot_confianza)

# ==============================================================================
# 5. EXPLORACIÓN BIVARIADA: RELACIONES ENTRE VARIABLES 
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Categórica vs. Categórica (Tablas de Contingencia)
# ------------------------------------------------------------------------------

# A. Registro del centro de trabajo según Sexo (Porcentajes por fila)
tabla_ruc_sexo_datos <- enaho_diseno %>%
  filter(!is.na(sexo_etiqueta) & !is.na(tiene_ruc_etiqueta)) %>%
  group_by(sexo_etiqueta, tiene_ruc_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(sexo_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(sexo_etiqueta, tiene_ruc_etiqueta, Celda) %>%
  pivot_wider(names_from = tiene_ruc_etiqueta, values_from = Celda) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_ruc_sexo <- formato_flextable(tabla_ruc_sexo_datos, "Tabla 10. Perú: Tipo de registro del centro de trabajo según sexo de la PEA Ocupada, 2025")
print(ft_ruc_sexo)

# B. Tipo de Contrato según Sexo (Porcentajes por fila)
tabla_contrato_sexo_datos <- enaho_diseno %>%
  filter(!is.na(sexo_etiqueta) & !is.na(tipo_contrato_etiqueta)) %>%
  group_by(sexo_etiqueta, tipo_contrato_etiqueta) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(sexo_etiqueta) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(sexo_etiqueta, tipo_contrato_etiqueta, Celda) %>%
  pivot_wider(names_from = tipo_contrato_etiqueta, values_from = Celda) %>%
  rename(`Sexo` = sexo_etiqueta)

ft_contrato_sexo <- formato_flextable(tabla_contrato_sexo_datos, "Tabla 11. Perú: Tipo de contrato laboral según sexo de la PEA Ocupada, 2025")
print(ft_contrato_sexo)

# ------------------------------------------------------------------------------
# 5.2 Categórica vs. Continua (Boxplots por grupos)
# ------------------------------------------------------------------------------
# Paquete necesario para estimación de cuantiles ponderados en gráficos
if(!require(quantreg)) install.packages("quantreg")
renv::snapshot()

# A. Ingreso principal según Tipo de Contrato
plot_ingreso_contrato <- ggplot(enaho_explorar %>% filter(!is.na(tipo_contrato_etiqueta) & !is.na(ingreso_prin_imputado)), 
                                aes(x = tipo_contrato_etiqueta, y = ingreso_prin_imputado, fill = tipo_contrato_etiqueta, weight = factorA07)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) + scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 4. Ingreso proveniente de la ocupación principal según tipo de contrato", x = "Tipo de Contrato", y = "Ingreso (Soles corrientes)") +
  theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_ingreso_contrato)

# B. Ingreso principal según Registro del Centro de Trabajo (RUC) y Sexo (Multivariado)
plot_ingreso_ruc_sexo <- ggplot(enaho_explorar %>% filter(!is.na(tiene_ruc_etiqueta) & !is.na(sexo_etiqueta) & !is.na(ingreso_prin_imputado)), 
                                aes(x = tiene_ruc_etiqueta, y = ingreso_prin_imputado, fill = sexo_etiqueta, weight = factorA07)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) + scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#2E5B88", "#E69F00")) +
  labs(title = "Gráfico 5. Perú: Ingreso proveniente de la ocupación según registro del centro de trabajo, por sexo, 2025", x = "Tipo de Registro (RUC)", y = "Ingreso (Soles corrientes)", fill = "Sexo:") +
  theme_minimal() + theme(legend.position = "bottom")
print(plot_ingreso_ruc_sexo)

# C. Confianza en el Congreso según Registro del Centro de Trabajo
plot_confianza_ruc <- ggplot(enaho_explorar %>% filter(!is.na(tiene_ruc_etiqueta) & !is.na(confianza_congreso)), 
                             aes(x = tiene_ruc_etiqueta, y = as.numeric(confianza_congreso), fill = tiene_ruc_etiqueta, weight = factorA07)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Gráfico 6. Perú: Nivel de confianza en el Congreso según registro del centro de trabajo", x = "Tipo de Registro (RUC)", y = "Confianza (1 = Nada, 4 = Bastante)") +
  theme_minimal() + theme(legend.position = "none")
print(plot_confianza_ruc)

# ------------------------------------------------------------------------------
# 5.3 Continua vs. Continua (Gráfico de Dispersión / Scatter)
# ------------------------------------------------------------------------------
plot_edad_ingreso <- ggplot(enaho_explorar %>% filter(!is.na(edad) & !is.na(ingreso_prin_imputado)), 
                            aes(x = edad, y = ingreso_prin_imputado)) +
  geom_jitter(alpha = 0.1, color = "#4A7C59", width = 0.5, height = 0) +
  geom_smooth(method = "gam", color = "red", se = FALSE) + 
  coord_cartesian(ylim = c(0, 15000)) + scale_y_continuous(labels = scales::comma) +
  labs(title = "Gráfico 7. Relación entre Edad e Ingreso Principal", subtitle = "Con línea de tendencia suavizada", x = "Edad (Años)", y = "Ingreso (Soles corrientes)") +
  theme_minimal()
print(plot_edad_ingreso)