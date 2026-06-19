# ==============================================================================
# Proyecto: Análisis de la informalidad laboral utilizando datos de la ENAHO
# Script: Exploración 
# Autor: Guillermo Coronado
# Fecha: 19-06-2026
# Objetivo: Análisis de Datos Exploratorio (EDA) de la base de datos procesda
# ==============================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS---------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(survey)      # Para declarar diseños muestrales complejos
library(srvyr)       # Para usar dplyr con encuestas complejas
library(gtsummary)   # Para tablas descriptivas con calidad de publicación
library(flextable)   # Para exportar tablas a Word
library(scales)      # Para formato de números (comas, porcentajes)
renv::snapshot()

# Cargamos la base de datos limpia
enaho_limpia <- read_parquet("datos/procesados/enaho_2025_19_06_25.parquet")

table(enaho_limpia$categoria_ocupacional, enaho_limpia$tipo_contrato)
table(enaho_limpia$categoria_ocupacional)

# ------------------------------------------------------------------------------
# 1. PREPARACIÓN DE VARIABLES ANALÍTICAS----------------------------------------
# ------------------------------------------------------------------------------
enaho_explorar <- enaho_limpia %>%
  mutate(
    # A. Matriz de Informalidad (Sector y Empleo)
    formalidad_sector = case_when(
      tiene_ruc %in% c(1, 2) ~ "Sector Formal",
      tiene_ruc == 3 ~ "Sector Informal",
      TRUE ~ NA_character_
    ),
    formalidad_empleo = case_when(
      # 1. Trabajador familiar no remunerado (Siempre informal por definición)
      categoria_ocupacional == 5 ~ "Empleo Informal",
      
      # 2. Independientes y Empleadores (Su empleo es formal solo si su unidad productiva tiene RUC o RUS)
      categoria_ocupacional %in% c(1, 2) & tiene_ruc == 3 ~ "Empleo Informal",
      categoria_ocupacional %in% c(1, 2) & tiene_ruc %in% c(1, 2) ~ "Empleo Formal",
      
      # 3. Dependientes: Empleados, Obreros, Trabajadoras del hogar, Otros (Dependen del contrato)
      categoria_ocupacional %in% c(3, 4, 6, 7) & tipo_contrato == 7 ~ "Empleo Informal",
      categoria_ocupacional %in% c(3, 4, 6, 7) & tipo_contrato %in% c(1, 2, 3, 4, 5, 6, 8) ~ "Empleo Formal",
      
      # Si hay NAs en las preguntas filtro y no entra en las categorías previas
      TRUE ~ NA_character_
    ),
    
    # B. Demográficas y Sociales
    sexo = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    grupo_edad = case_when(
      edad < 30 ~ "18 a 29 años",
      edad < 45 ~ "30 a 44 años",
      edad < 60 ~ "45 a 59 años",
      TRUE ~ "60 años a más"
    ),
    quintil_ingreso = ntile(ingreso_prin_imputado, 5),
    discapacidad = ifelse(
      discapacidad_moverse == 1 | discapacidad_visual == 1 | 
        discapacidad_comunicarse == 1 | discapacidad_auditiva == 1 | 
        discapacidad_cognitiva == 1 | discapacidad_social == 1, 
      "Con discapacidad", "Sin discapacidad"
    ),
    
    # C. Protección Social
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
  
  # D. Índice de Confianza Institucional (Normalizado 0-1)
  # 1. Convertimos los 5 (No sabe) y 9 (Missing value) a verdaderos NAs
  mutate(across(starts_with("confianza_"), ~na_if(., 5))) %>%
  mutate(across(starts_with("confianza_"), ~na_if(., 9))) %>%
  
  # 2. Calculamos el promedio de confianza del individuo 
  rowwise() %>%
  mutate(promedio_confianza = mean(c_across(starts_with("confianza_")), na.rm = TRUE)) %>%
  ungroup() %>%
  
  # 3. Normalizamos al rango [0, 1]
  # Fórmula: (Valor - Mínimo) / (Máximo - Mínimo) -> (x - 1) / (4 - 1)
  mutate(indice_confianza = (promedio_confianza - 1) / (4 - 1))

# ------------------------------------------------------------------------------
# 2. DISEÑO MUESTRAL: EL FACTOR DE EXPANSIÓN
# ------------------------------------------------------------------------------
# Le decimos a R que use el factorA07 (Factor Anual de Empleo ajustado)
enaho_diseno <- enaho_explorar %>%
  as_survey_design(
    ids = conglome,          
    strata = estrato,        
    weights = factorA07,     
    nest = TRUE              
  ) #Error que no detectanmos en acondicionamiento!


# ==============================================================================
# 3. EXPLORACIÓN UNIVARIADA
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Tabla Expandida (Frecuencias Absolutas Poblacionales) con Flextable
# ------------------------------------------------------------------------------
tabla_expandida_datos <- enaho_diseno %>%
  filter(!is.na(empleo_laboral)) %>%
  group_by(empleo_laboral) %>%
  summarise(
    Poblacion = survey_total(vartype = NULL), 
    Porcentaje = survey_mean(vartype = NULL) * 100
  ) %>%
  mutate(
    Poblacion = scales::comma(round(Poblacion, 0)),
    Porcentaje = paste0(round(Porcentaje, 1), "%")
  ) %>%
  rename(
    `Condición de Empleo` = empleo_laboral,
    `Total Expandido (N)` = Poblacion,
    `Proporción (%)` = Porcentaje
  )

tabla_uni_word <- flextable(tabla_expandida_datos) %>%
  add_header_lines(values = "Tabla 1. Población ocupada según condición de empleo") %>%
  add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual (factorA07).") %>%
  autofit() %>% theme_vanilla() %>% align(align = "center", part = "all") %>% bold(part = "header")

tabla_uni_word

# ------------------------------------------------------------------------------
# 3.2 Histograma: Ingreso Principal (Ponderado)
# ------------------------------------------------------------------------------
grafico_hist <- ggplot(enaho_explorar, aes(x = ingreso_prin_imputado, weight = factorA07)) +
  geom_histogram(fill = "#8C92AC", color = "white", bins = 50) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 10000)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Gráfico 1. Distribución poblacional del ingreso mensual (PEA ocupada)",
    subtitle = "Perú, 2025",
    x = "Ingreso principal (Soles corrientes)",
    y = "Número de personas (Expandido)",
    caption = "Fuente: ENAHO 2025.\nNota: Eje X truncado en S/10,000. Gráfico ajustado por factorA07."
  ) +
  theme_minimal()

print(grafico_hist)

# ==============================================================================
# 4. EXPLORAR BIVARIADA: RELACIONES Y ESTRUCTURA
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Categórica vs Categórica: El "Trabajo en Negro" (gtsummary ponderado)
# ------------------------------------------------------------------------------
# Usamos tbl_svysummary para que gtsummary lea el factor de expansión
tabla_trabajo_negro <- enaho_diseno %>%
  filter(!is.na(sector_laboral) & !is.na(empleo_laboral)) %>%
  tbl_svysummary(
    by = empleo_laboral,
    include = sector_laboral,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"), # Mostramos el 'n' muestral pero el '%' poblacional
  ) %>%
  add_overall() %>%
  modify_caption("**Tabla 2. Matriz de Sector y Empleo Laboral (Porcentajes Expandidos)**") %>%
  modify_footnote(all_stat_cols() ~ "Fuente: ENAHO 2025. | n = Casos en la muestra, % = Proporción poblacional") %>%
  bold_labels()

tabla_trabajo_negro

# Gráfico de Trabajo en Negro (Apilado al 100% y Ponderado)
grafico_negro <- ggplot(enaho_explorar %>% filter(!is.na(sector_laboral) & !is.na(empleo_laboral)), 
                        aes(x = sector_laboral, fill = empleo_laboral, weight = factorA07)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gráfico 2. Composición del empleo según sector de la empresa",
    subtitle = "Visibilizando el 'Trabajo en Negro' en el Sector Formal",
    x = "Sector de la empresa (Tenencia de RUC)",
    y = "Proporción de trabajadores (%)",
    fill = "Condición del empleo:",
    caption = "Fuente: ENAHO 2025.\nNota: Ponderado con factorA07. Sector formal = con RUC. Empleo formal = con contrato."
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

print(grafico_negro)

# ------------------------------------------------------------------------------
# 4.2 Tabla Bivariada Múltiple: Perfil de Vulnerabilidad
# ------------------------------------------------------------------------------
tabla_perfil_empleo <- enaho_diseno %>%
  filter(!is.na(empleo_laboral)) %>%
  tbl_svysummary(
    by = empleo_laboral, 
    include = c(sexo, discapacidad, afiliacion_salud, afiliacion_pensiones),
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  ) %>%
  add_overall() %>% 
  modify_caption("**Tabla 3. Perfil sociodemográfico y de protección social según empleo**") %>%
  modify_footnote(all_stat_cols() ~ "Fuente: ENAHO 2025. | % ajustados por factor de expansión (factorA07).") %>%
  bold_labels()

tabla_perfil_empleo

# ------------------------------------------------------------------------------
# 4.3 Continua vs Categórica: Brecha salarial por Sexo (Boxplot)
# ------------------------------------------------------------------------------
grafico_brecha_sexo <- ggplot(enaho_explorar %>% filter(!is.na(sexo)), 
                              aes(x = sexo, y = ingreso_prin_imputado, fill = sexo, weight = factorA07)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1, outlier.alpha = 0.3) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10000)) + 
  labs(
    title = "Gráfico 3. Distribución del ingreso principal según sexo",
    subtitle = "PEA Ocupada (18 a más años), Perú 2025",
    x = "Sexo del trabajador",
    y = "Ingreso mensual principal (Soles corrientes)",
    caption = "Fuente: ENAHO 2025.\nNota: Eje Y truncado a S/10,000."
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#2E5B88", "#E69F00")) + 
  theme(legend.position = "none")

print(grafico_brecha_sexo)

# ------------------------------------------------------------------------------
# 4.4 Categórica vs Categórica: Desprotección en la Vejez (Barras apiladas)
# ------------------------------------------------------------------------------
grafico_pensiones <- ggplot(enaho_explorar %>% filter(!is.na(empleo_laboral) & !is.na(afiliacion_pensiones)), 
                            aes(x = empleo_laboral, fill = afiliacion_pensiones, weight = factorA07)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gráfico 4. Afiliación a sistema de pensiones según condición de empleo",
    subtitle = "PEA Ocupada (18 a más años), Perú 2025",
    x = "Condición de empleo laboral",
    y = "Proporción de trabajadores (%)",
    fill = "Sistema de Pensiones:",
    caption = "Fuente: ENAHO 2025.\nNota: Ponderado por factorA07."
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

print(grafico_pensiones)

# ------------------------------------------------------------------------------
# 4.5 Continua vs Categórica: Índice de Confianza vs Empleo (Boxplot)
# ------------------------------------------------------------------------------
grafico_confianza <- ggplot(enaho_explorar %>% filter(!is.na(empleo_laboral)), 
                            aes(x = empleo_laboral, y = indice_confianza, fill = empleo_laboral, weight = factorA07)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1, outlier.alpha = 0.3) +
  labs(
    title = "Gráfico 5. Índice de Confianza Institucional según condición de empleo",
    subtitle = "PEA Ocupada (18 a más años), Perú 2025",
    x = "Condición de empleo laboral",
    y = "Índice de confianza (0 = Nula, 1 = Plena)",
    caption = "Fuente: ENAHO 2025 | Módulo 1B.\nNota: Índice normalizado basado en 21 instituciones."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(grafico_confianza)

# ==============================================================================
# 5. EXPORTACIÓN DE TABLAS Y GRÁFICOS
# ==============================================================================

# Tablas (Word)
save_as_docx(tabla_uni_word, path = "outputs/Tabla1_Empleo_Expandido.docx")

# Si quieres exportar las tablas de gtsummary a Word, usamos as_flex_table()
tabla_trabajo_negro %>% as_flex_table() %>% save_as_docx(path = "outputs/Tabla2_TrabajoNegro.docx")
tabla_perfil_empleo %>% as_flex_table() %>% save_as_docx(path = "outputs/Tabla3_PerfilSociodemografico.docx")

# Gráficos (PNG)
ggsave("outputs/Grafico1_Histograma_Ingresos.png", plot = grafico_hist, width = 8, height = 6, bg="white")
ggsave("outputs/Grafico2_TrabajoNegro.png", plot = grafico_negro, width = 8, height = 6, bg="white")
ggsave("outputs/Grafico3_Boxplot_Sexo_Ingresos.png", plot = grafico_brecha_sexo, width = 8, height = 6, bg="white")
ggsave("outputs/Grafico4_Barras_Pensiones.png", plot = grafico_pensiones, width = 8, height = 6, bg="white")
ggsave("outputs/Grafico5_Boxplot_Confianza.png", plot = grafico_confianza, width = 8, height = 6, bg="white")