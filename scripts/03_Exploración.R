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
    ),
    # Limpieza previa de formatos numéricos
    factorA07 = as.numeric(str_replace_all(factorA07, ",", ".")),
    conglome  = as.numeric(conglome),
    estrato   = as.numeric(estrato),
    ingreso_prin_imputado = as.numeric(ingreso_prin_imputado)
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
# 2. DISEÑO MUESTRAL: EL FACTOR DE EXPANSIÓN------------------------------------
# ------------------------------------------------------------------------------
# Le decimos a R que use el factorA07 (Factor Anual de Empleo ajustado)
enaho_diseno <- enaho_explorar %>%
  as_survey_design(
    ids = conglome,          
    strata = estrato,        
    weights = factorA07,     
    nest = TRUE              
  ) %>% 
  #El paquete no admite NAs en el factor de expansión
  filter(!is.na(factorA07)) %>%
  #Declaramos el diseño muestral complejo
  as_survey_design(
    ids = conglome,          
    strata = estrato,        
    weights = factorA07,     
    nest = TRUE              
  )

# ==============================================================================
# 3. EXPLORACIÓN UNIVARIADA
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
  add_header_lines(values = "Tabla 1. Perú: Población ocupada según condición de formalidad, 2025") %>%
  add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual.") %>%
  autofit() %>% theme_vanilla() %>% align(align = "center", part = "all") %>% bold(part = "header") %>% 
  align(align = "left", part = "footer") %>%                     
  fontsize(size = 9, part = "footer") %>%                        
  hline_bottom(part = "footer", border = officer::fp_border(width = 0))

tabla_uni_word

# ------------------------------------------------------------------------------
# 3.2 Histograma: Ingreso Principal (Ponderado)---------------------------------
# ------------------------------------------------------------------------------

enaho_explorar <- enaho_explorar %>%
  mutate(
    factorA07 = as.numeric(str_replace_all(factorA07, ",", "."))
  ) #Corregimos el formato del factor de expansión en nuestra base de datos principal

grafico_hist <- ggplot(enaho_explorar, aes(x = ingreso_prin_imputado, weight = factorA07)) +
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
    `Percetil 25 (Q1)` = survey_quantile(ingreso_prin_imputado, 0.25, vartype = NULL),
    `Mediana (Q2)` = survey_median(ingreso_prin_imputado, vartype = NULL),
    `Media (Promedio)` = survey_mean(ingreso_prin_imputado, vartype = NULL),
    `Percentil 75 (Q3)` = survey_quantile(ingreso_prin_imputado, 0.75, vartype = NULL),
    `Máximo` = max(ingreso_prin_imputado, na.rm = TRUE)
  ) %>%
  # 2. Pasamos la tabla a formato vertical para que se vea más elegante
  pivot_longer(
    cols = everything(), 
    names_to = "Estadístico", 
    values_to = "Valor"
  ) %>%
  # 3. Le damos formato de moneda/número (con comas y un decimal)
  mutate(Valor = scales::comma(round(Valor, 1))) %>%
  rename(`Valor (Soles corrientes)` = Valor)

# 4. Renderizamos con el diseño estandarizado de Flextable
tabla_stats_word <- flextable(estadisticos_ingreso) %>%
  add_header_lines(values = "Tabla 2. Estadísticos de resumen del ingreso mensual proveniente de la ocupación principal") %>%
  add_footer_lines(values = "Fuente: ENAHO 2025. | Cálculos ponderados usando el factor de expansión anual.") %>%
  autofit() %>% 
  theme_vanilla() %>% 
  align(align = "center", part = "all") %>% 
  align(j = 1, align = "left", part = "body") %>% # El nombre del estadístico a la izquierda
  bold(part = "header") %>%
  
  # Ajustes de la nota al pie
  align(align = "left", part = "footer") %>% 
  fontsize(size = 9, part = "footer") %>% 
  hline_bottom(part = "footer", border = officer::fp_border(width = 0))

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

# 2. Le damos el mismo diseño estético de la Tabla 1
tabla_negro_word <- flextable(tabla_trabajo_negro_datos) %>%
  add_header_lines(values = "Tabla 3. Perú: Condición de formalidad del empleo según formalidad del sector") %>%
  add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual.") %>%
  autofit() %>%                     
  theme_vanilla() %>%               
  align(align = "center", part = "all") %>% 
  align(j = 1, align = "left", part = "body") %>% 
  bold(part = "header") %>%
  align(align = "left", part = "footer") %>%                     
  fontsize(size = 9, part = "footer") %>%                        
  hline_bottom(part = "footer", border = officer::fp_border(width = 0)) 

# Ver la tabla en el visor
tabla_negro_word


# Gráfico de Trabajo en Negro (Apilado al 100% y Ponderado)
grafico_negro <- ggplot(enaho_explorar %>% filter(!is.na(formalidad_sector) & !is.na(formalidad_empleo)), 
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
datos_largos <- enaho_explorar %>%
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
  add_header_lines(values = "Tabla 4. Perú: Perfil sociodemográfico y de protección social según condición de formalidad en el empleo") %>%
  add_footer_lines(values = "Fuente: ENAHO 2025. Cálculos usando el factor de expansión anual") %>%
  autofit() %>% theme_vanilla() %>% align(align = "center", part = "all") %>% 
  align(j = 1:2, align = "left", part = "body") %>% bold(part = "header") %>%
  merge_v(j = "Caracteristica") %>%
  align(align = "left", part = "footer") %>% fontsize(size = 9, part = "footer") %>% 
  hline_bottom(part = "footer", border = officer::fp_border(width = 0))

tabla_perfil_word

# ------------------------------------------------------------------------------
# 4.3 Continua vs Categórica: Brecha salarial por Sexo según Condición de Formalidad (Boxplot)---------
# ------------------------------------------------------------------------------
install.packages("quantreg") #Lo necesitamos para poder hacer un boxplot usando el factor de expansión
renv::snapshot()

datos_boxplot <- enaho_explorar %>% 
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
grafico_confianza <- ggplot(enaho_explorar %>% filter(!is.na(formalidad_empleo)), 
                            aes(x = formalidad_empleo, y = indice_confianza, fill = formalidad_empleo, weight = factorA07)) +
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

# ==============================================================================
# 5. EXPORTACIÓN DE TABLAS Y GRÁFICOS
# ==============================================================================
save_as_docx(tabla_uni_word,    path = "outputs/Tabla1_Condicion_Formalidad.docx")
save_as_docx(tabla_stats_word,  path = "outputs/Tabla2_Estadisticos_Ingreso.docx")
save_as_docx(tabla_negro_word,  path = "outputs/Tabla3_Matriz_Sector_Empleo.docx")
save_as_docx(tabla_perfil_word, path = "outputs/Tabla4_Perfil_Vulnerabilidad.docx")


ggsave("outputs/Grafico1_Histograma_Ingresos.png", plot = grafico_hist,          width = 10, height = 6, dpi = 300, bg = "white")
ggsave("outputs/Grafico2_Matriz_Sector_Empleo.png",plot = grafico_negro,         width = 10, height = 6, dpi = 300, bg = "white")
ggsave("outputs/Grafico3_Brecha_Salarial_Sexo.png",plot = grafico_brecha_sexo,    width = 11, height = 6, dpi = 300, bg = "white")
ggsave("outputs/Grafico4_Confianza_Institucional.png", plot = grafico_confianza, width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# 6. EXPORTACIÓN DE LA BASE DE DATOS FINAL
# ==============================================================================

write_parquet(enaho_explorar, "datos/procesados/enaho_2025_20_06_26.parquet")
