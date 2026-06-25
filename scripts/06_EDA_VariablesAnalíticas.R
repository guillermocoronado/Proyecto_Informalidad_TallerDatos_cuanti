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
enaho_analitica <- read_parquet(here("datos", "procesados", "enaho_2025_24_06_26.parquet"))

# ------------------------------------------------------------------------------
# 1. DISEÑO MUESTRAL------------------------------------------------------------
# ------------------------------------------------------------------------------
enaho_diseno <- enaho_analitica %>%
  filter(!is.na(factorA07)) %>%
  as_survey_design(
    ids = conglome,          
    strata = estrato,        
    weights = factorA07,     
    nest = TRUE              
  ) #Para usar el factor de expansión

# ==============================================================================
# DEFINIMOS LA FUNCIÓN DE ESTILO PARA LAS TABLAS--------------------------------
# ==============================================================================
# Esta función aplica el formato limpio (sin renglones intermedios) a todas las tablas
library(officer)

estilo_reporte <- function(ft, titulo, fuente) {
  ft %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = fuente) %>%
    autofit() %>%
    border_remove() %>% # Elimina todos los bordes y renglones intermedios
    hline_top(border = fp_border(width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.5), part = "body") %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "body") %>% # 1ra columna a la izquierda
    bold(part = "header") %>%
    align(align = "left", part = "footer") %>%
    fontsize(size = 9, part = "footer")
}

# ==============================================================================
# 3. EXPLORACIÓN UNIVARIADA-----------------------------------------------------
# ==============================================================================
# ------------------------------------------------------------------------------
# 3.1 Tabla Expandida (Frecuencias Absolutas Poblacionales) con Flextable-------
# ------------------------------------------------------------------------------
tabla_expandida_datos <- enaho_diseno %>%
  filter(!is.na(formalidad_empleo)) %>%
  group_by(formalidad_empleo) %>%
  summarise(
    Poblacion = survey_total(vartype = NULL), 
    Porcentaje = survey_mean(vartype = NULL) * 100
  ) %>%
  mutate(
    Poblacion = scales::comma(round(Poblacion, 0)),
    Porcentaje = paste0(round(Porcentaje, 1), "%")
  ) %>%
  rename(
    `Condición de Empleo` = formalidad_empleo,
    `Total Expandido (N)` = Poblacion,
    `Proporción (%)` = Porcentaje
  )

tabla_uni_word <- flextable(tabla_expandida_datos) %>%
  estilo_reporte(
    titulo = "Tabla 1. Perú: Población ocupada según condición de formalidad, 2025",
    fuente = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual."
  )

tabla_uni_word

# ------------------------------------------------------------------------------
# 3.2 Histograma: Ingreso Principal (Ponderado)---------------------------------
# ------------------------------------------------------------------------------

enaho_analitica <- enaho_analitica %>%
  mutate(
    factorA07 = as.numeric(str_replace_all(factorA07, ",", "."))
  ) #Corregimos el formato del factor de expansión en nuestra base de datos principal

grafico_hist <- ggplot(enaho_analitica, aes(x = ingreso_prin_imputado, weight = factorA07)) +
  geom_histogram(fill = "#8C92AC", color = "white", bins = 50) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 10000)) + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Gráfico 1. Distribución del ingreso proveniente de la ocupación principal entre la PEA ocupada",
    subtitle = "Perú, 2025",
    x = "Ingreso principal (Soles corrientes)",
    y = "Número de personas (Expandido)",
    caption = "Fuente: ENAHO 2025.\nNota: Eje X truncado en S/10,000. Gráfico ajustado por factor de expansión."
  ) +
  theme_minimal()

print(grafico_hist)

# ------------------------------------------------------------------------------
# 3.3 Tabla de Estadísticos de Resumen: Ingreso Principal (Ponderado)
# ------------------------------------------------------------------------------
# 1. Calculamos los estadísticos usando el diseño muestral
estadisticos_ingreso <- enaho_diseno %>%
  filter(!is.na(ingreso_prin_imputado)) %>%
  summarise(
    `Mínimo` = min(ingreso_prin_imputado, na.rm = TRUE),
    `Percentil 25 (Q1)` = survey_quantile(ingreso_prin_imputado, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(ingreso_prin_imputado, vartype = NULL),
    `Media (Promedio)` = survey_mean(ingreso_prin_imputado, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(ingreso_prin_imputado, 0.75, vartype = NULL),
    `Máximo` = max(ingreso_prin_imputado, na.rm = TRUE)
  ) %>%
  # 2. Pasamos la tabla a formato vertical
  pivot_longer(
    cols = everything(), 
    names_to = "Estadístico", 
    values_to = "Valor"
  ) %>%
  # 3. Limpiamos los sufijos automáticos de srvyr (_q25, _q75)
  mutate(
    Estadístico = str_remove(Estadístico, "_q25|_q75")
  ) %>%
  # 4. Le damos formato numérico
  mutate(Valor = scales::comma(round(Valor, 1))) %>%
  rename(`Valor (Soles corrientes)` = Valor)

# 5. Renderizamos con el diseño estandarizado de Flextable
tabla_stats_word <- flextable(estadisticos_ingreso) %>%
  estilo_reporte(
    titulo = "Tabla 2. Estadísticos de resumen del ingreso mensual proveniente de la ocupación principal",
    fuente = "Fuente: ENAHO 2025. | Cálculos ponderados usando el factor de expansión anual."
  )

# Visualizar en el panel
tabla_stats_word

# ==============================================================================
# 4. EXPLORAR BIVARIADA: RELACIONES Y ESTRUCTURA--------------------------------
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.1 Categórica vs Categórica: Sector Informal vs. Empleo Informal-------------
# ------------------------------------------------------------------------------
#Calculamos N poblacional y % por fila usando srvyr
tabla_trabajo_negro_datos <- enaho_diseno %>%
  filter(!is.na(formalidad_sector) & !is.na(formalidad_empleo)) %>%
  group_by(formalidad_sector, formalidad_empleo) %>%
  summarise(
    Poblacion = survey_total(vartype = NULL)
  ) %>%
  # Agrupamos por el sector para que el porcentaje sume 100% en cada fila
  group_by(formalidad_sector) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    # Unimos el N y el % en una sola celda para que se vea ordenado
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  # Damos formato de matriz (filas = sector, columnas = empleo)
  select(formalidad_sector, formalidad_empleo, Celda) %>%
  pivot_wider(names_from = formalidad_empleo, values_from = Celda) %>%
  rename(`Sector Económico` = formalidad_sector)

# 2. Le damos el mismo diseño estético
tabla_negro_word <- flextable(tabla_trabajo_negro_datos) %>%
  estilo_reporte(
    titulo = "Tabla 3. Perú: Condición de formalidad del empleo según formalidad del sector",
    fuente = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual."
  )

tabla_negro_word

# Gráfico de Trabajo en Negro (Apilado al 100% y Ponderado)
grafico_negro <- ggplot(enaho_analitica %>% filter(!is.na(formalidad_sector) & !is.na(formalidad_empleo)), 
                        aes(x = formalidad_sector, fill = formalidad_empleo, weight = factorA07)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gráfico 2. Perú: Condición de formalidad del empleo según formalidad del sector, 2025",
    x = "Condición de formalidad de la empresa",
    y = "Proporción de trabajadores (%)",
    fill = "Condición de formalidad del empleo:",
    caption = "Fuente: ENAHO 2025.\nNota: Ponderado con factorA07. Sector formal = con RUC. Empleo formal = con contrato."
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

print(grafico_negro)

# ------------------------------------------------------------------------------
# 4.2 Tabla Bivariada Múltiple: Perfil de Vulnerabilidad (Categórica vs. Categórica)------------
# ------------------------------------------------------------------------------
datos_largos <- enaho_analitica %>%
  filter(!is.na(formalidad_empleo)) %>%
  select(conglome, estrato, factorA07, formalidad_empleo, sexo, discapacidad, afiliacion_salud, afiliacion_pensiones) %>%
  pivot_longer(
    cols = c(sexo, discapacidad, afiliacion_salud, afiliacion_pensiones),
    names_to = "Caracteristica",
    values_to = "Categoria"
  ) %>%
  filter(!is.na(Categoria))

diseno_largo <- datos_largos %>%
  as_survey_design(ids = conglome, strata = estrato, weights = factorA07, nest = TRUE)

totales_general <- diseno_largo %>%
  group_by(Caracteristica, Categoria) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(Caracteristica) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Total = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(Caracteristica, Categoria, Total)

totales_empleo <- diseno_largo %>%
  group_by(Caracteristica, Categoria, formalidad_empleo) %>%
  summarise(Poblacion = survey_total(vartype = NULL)) %>%
  group_by(Caracteristica, formalidad_empleo) %>%
  mutate(
    Porcentaje = (Poblacion / sum(Poblacion)) * 100,
    Celda = paste0(scales::comma(round(Poblacion, 0)), " (", round(Porcentaje, 1), "%)")
  ) %>%
  select(Caracteristica, Categoria, formalidad_empleo, Celda) %>%
  pivot_wider(names_from = formalidad_empleo, values_from = Celda)

tabla_perfil_datos <- totales_general %>%
  left_join(totales_empleo, by = c("Caracteristica", "Categoria")) %>%
  mutate(Caracteristica = case_when(
    Caracteristica == "sexo" ~ "Sexo",
    Caracteristica == "discapacidad" ~ "Condición de Discapacidad",
    Caracteristica == "afiliacion_salud" ~ "Afiliación a Salud",
    Caracteristica == "afiliacion_pensiones" ~ "Sistema de Pensiones"
  )) %>%
  arrange(Caracteristica)

tabla_perfil_word <- flextable(tabla_perfil_datos) %>%
  estilo_reporte(
    titulo = "Tabla 4. Perú: Perfil sociodemográfico y de protección social según condición de formalidad en el empleo",
    fuente = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual"
  ) %>%
  merge_v(j = "Caracteristica") # Mantenemos el merge vertical específico de esta tabla

tabla_perfil_word

# ------------------------------------------------------------------------------
# 4.3 Continua vs Categórica: Brecha salarial por Sexo según Condición de Formalidad (Boxplot)---------
# ------------------------------------------------------------------------------

datos_boxplot <- enaho_analitica %>% 
  filter(
    !is.na(sexo),
    !is.na(formalidad_empleo),
    !is.na(ingreso_prin_imputado),
    !is.na(factorA07)
  )

grafico_brecha_sexo <- ggplot(datos_boxplot, 
                              aes(x = formalidad_empleo, 
                                  y = as.numeric(ingreso_prin_imputado), 
                                  fill = sexo, 
                                  weight = factorA07)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1, outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Gráfico 4. Perú: Brecha salarial por sexo según condición de formalidad del empleo",
    subtitle = "PEA Ocupada (18 a más años)",
    x = "Condición de formalidad del empleo",
    y = "Ingreso mensual proveniente de la ocupación principal (Soles corrientes)",
    fill = "Sexo del trabajador:",
    caption = "Fuente: ENAHO 2025.\nNota: Eje Y truncado visualmente a S/10,000. Cajas ponderadas por factor de expansión."
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#2E5B88", "#E69F00")) + 
  theme(legend.position = "bottom")

print(grafico_brecha_sexo)

# ------------------------------------------------------------------------------
# 4.5 Continua vs Categórica: Índice de Confianza vs Empleo (Boxplot)-----------
# ------------------------------------------------------------------------------
grafico_confianza <- ggplot(enaho_analitica %>% filter(!is.na(formalidad_empleo)), 
                            aes(x = formalidad_empleo, y = indice_confianza_simple, fill = formalidad_empleo, weight = factorA07)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1, outlier.alpha = 0.3) +
  labs(
    title = "Gráfico 5. Perú: Índice de Confianza Institucional según condición de formalidad en el empleo, 2025",
    subtitle = "PEA Ocupada (18 a más años)",
    x = "Condición de  formalidad en el empleo",
    y = "Índice de confianza (0 = Nula, 1 = Plena)",
    caption = "Fuente: ENAHO 2025 | Módulo 1B.\nNota: Índice normalizado basado en 21 instituciones."
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(grafico_confianza)

# ------------------------------------------------------------------------------
# 4.6 Tabla Resumen - Tipología MECE, Edad (Z) e Índice de Confianza
# ------------------------------------------------------------------------------
tabla_nuevas_vars_datos <- enaho_diseno %>%
  filter(!is.na(tipologia_laboral)) %>%
  group_by(tipologia_laboral) %>%
  summarise(
    `Población (N)` = survey_total(vartype = NULL),
    `Edad Promedio (Z)` = survey_mean(edad_z, vartype = NULL, na.rm = TRUE),
    `Índice Confianza (Media Geométrica)` = survey_mean(indice_confianza_geom, vartype = NULL, na.rm = TRUE)
  ) %>%
  mutate(
    `Población (N)` = scales::comma(round(`Población (N)`, 0)),
    `Edad Promedio (Z)` = round(`Edad Promedio (Z)`, 2),
    `Índice Confianza (Media Geométrica)` = round(`Índice Confianza (Media Geométrica)`, 3)
  ) %>%
  rename(`Tipología Laboral (Sector x Empleo)` = tipologia_laboral)

tabla_nuevas_vars_word <- flextable(tabla_nuevas_vars_datos) %>%
  estilo_reporte(
    titulo = "Tabla 5. Perú: Perfil de edad y confianza institucional según tipología laboral",
    fuente = "Fuente: ENAHO 2025. Cálculos ponderados con factor de expansión anual."
  )

tabla_nuevas_vars_word

# ------------------------------------------------------------------------------
# 4.7 Ingreso según tipología laboral-------------------------------------------
# ------------------------------------------------------------------------------
datos_boxplot_tipologia <- enaho_analitica %>% 
  filter(
    !is.na(tipologia_laboral),
    !is.na(ingreso_prin_imputado),
    !is.na(factorA07)
  )

grafico_ingreso_tipologia <- ggplot(datos_boxplot_tipologia, 
                                    aes(x = tipologia_laboral, 
                                        y = as.numeric(ingreso_prin_imputado), 
                                        fill = tipologia_laboral, 
                                        weight = factorA07)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1, outlier.alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) + 
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Gráfico 7. Perú: Brecha salarial según tipología laboral multidimensional",
    subtitle = "PEA Ocupada (18 a más años)",
    x = "Tipología Laboral (Cruce Sector x Empleo)",
    y = "Ingreso mensual principal (Soles corrientes)",
    caption = "Fuente: ENAHO 2025.\nNota: Eje Y truncado visualmente a S/10,000. Cajas ponderadas por factor de expansión."
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1) # Inclinamos las etiquetas para mejor lectura
  )

print(grafico_ingreso_tipologia)

# ------------------------------------------------------------------------------
# 4.7 Gráfico de Tendencias - Edad (Z) vs. Índice de Confianza (Geom)
# ------------------------------------------------------------------------------
grafico_nuevas_vars <- ggplot(enaho_analitica %>% filter(!is.na(tipologia_laboral) & !is.na(edad_z) & !is.na(indice_confianza_geom)), 
                              aes(x = edad_z, y = indice_confianza_geom, color = tipologia_laboral, weight = factorA07)) +
  geom_smooth(method = "gam", se = FALSE, linewidth = 1.2) +
  labs(
    title = "Gráfico 6. Perú: Tendencia de la confianza institucional según edad estandarizada",
    subtitle = "Análisis por tipología laboral (Cruce MECE)",
    x = "Edad Estandarizada (Puntaje Z)",
    y = "Índice de Confianza (Media Geométrica 0-1)",
    color = "Tipología Laboral:",
    caption = "Fuente: ENAHO 2025. Nota: Líneas de tendencia suavizadas (GAM) ponderadas por factor de expansión."
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

print(grafico_nuevas_vars)

# ==============================================================================
# 5. EXPORTACIÓN DE TABLAS Y GRÁFICOS
# ==============================================================================

# ==============================================================================
# 5. EXPORTACIÓN DE TABLAS Y GRÁFICOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Exportación de Tablas (Word)
# ------------------------------------------------------------------------------
save_as_docx(tabla_uni_word,         path = here("outputs", "outputs_exploracion_analitica", "Tabla1_Condicion_Formalidad.docx"))
save_as_docx(tabla_stats_word,       path = here("outputs", "outputs_exploracion_analitica", "Tabla2_Estadisticos_Ingreso.docx"))
save_as_docx(tabla_negro_word,       path = here("outputs", "outputs_exploracion_analitica", "Tabla3_Matriz_Sector_Empleo.docx"))
save_as_docx(tabla_perfil_word,      path = here("outputs", "outputs_exploracion_analitica", "Tabla4_Perfil_Vulnerabilidad.docx"))
save_as_docx(tabla_nuevas_vars_word, path = here("outputs", "outputs_exploracion_analitica", "Tabla5_Nuevas_Variables.docx"))

# ------------------------------------------------------------------------------
# 5.2 Exportación de Gráficos (PNG Alta Resolución)
# ------------------------------------------------------------------------------
ggsave(here("outputs", "outputs_exploracion_analitica", "Grafico1_Histograma_Ingresos.png"),     plot = grafico_hist,              width = 10, height = 6, dpi = 300, bg = "white")
ggsave(here("outputs", "outputs_exploracion_analitica", "Grafico2_Matriz_Sector_Empleo.png"),    plot = grafico_negro,             width = 10, height = 6, dpi = 300, bg = "white")
ggsave(here("outputs", "outputs_exploracion_analitica", "Grafico4_Brecha_Salarial_Sexo.png"),    plot = grafico_brecha_sexo,       width = 11, height = 6, dpi = 300, bg = "white")
ggsave(here("outputs", "outputs_exploracion_analitica", "Grafico5_Confianza_Institucional.png"), plot = grafico_confianza,         width = 10, height = 6, dpi = 300, bg = "white")
ggsave(here("outputs", "outputs_exploracion_analitica", "Grafico6_Tendencia_Edad_Confianza.png"),plot = grafico_nuevas_vars,       width = 11, height = 6, dpi = 300, bg = "white")
ggsave(here("outputs", "outputs_exploracion_analitica", "Grafico7_Ingreso_Tipologia_MECE.png"),  plot = grafico_ingreso_tipologia, width = 11, height = 6, dpi = 300, bg = "white")