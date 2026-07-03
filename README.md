# README: Análisis de la informalidad laboral utilizando datos de la ENAHO

### Autor: Guillermo Coronado
### Curso: Taller de Procesamiento de Datos
### Encuesta: Encuesta Nacional de Hogares, Instituto Nacional de Estadística e Informática, 2025 (anual)
### Módulos utilizados: Módulo 300 (Educación), Módulo 400 (Salud), Módulo 500 (Empleo e ingresos) y Módulo 85 (Gobernabilidad, Democracia y Transparencia) parte 1 (Confianza en las instituciones)

## Descripción del proyecto
Este respositorio incluye el código y el flujo de trabajo completo del prpoyecto "Análisis de la informalidad laboral utilizando datos de la ENAHO", elaborado para el curso de Taller de Procesamiento de Datos 2026-1 de la PUCP. Se utilizan datos de la Encuesta Nacional de Hogares (ENAHO) de 2025 (versión  anual) trabajados integramente en **R** versión 4.5.3. La versión de todas las librerías se controla utilizando `renv`

El análisis explora la relación entre la formalidad y las siguientes dimensiones:
- **Demográficas**: Quintiles de ingreso, grupos de edad, nivel educativo, lengua materna y ubicación geográfica
- **Institucionales y de seguridad social**: Confianza en las instituciones, salud (aseguramiento, acceso, gasto de bolsillo) y pensiones

## Estructura del directorio
El directorio se organiza a través de la siguiente estructura de carpetas:
```text
├── Creacion_R_Project.R        # Script principal: Configuración del entorno, creación de carpetas y enlace a GitHub
├── datos/
│   ├── crudos/                                  # Módulos originales de la ENAHO en formato .csv (no aparecen en este repositorio debido a su peso)
│   └── procesados/                              # Bases procesadas en formato .parquet
│       ├── enaho_2025_02_07_27.parquet          # Base final con metadatos insertados y variables seleccionadas resultado de script 07
│       ├── enaho_2025_12_06_26.parquet          # Base procesada inicial resultado de unión entre módulos en script 01
│       ├── enaho_2025_19_06_26.parquet          # Base acondicionada resultado de script 02
│       ├── enaho_2025_20_06_26.parquet          # Base acondicionada con etiquetas resultado de script 03
│       └── enaho_2025_24_06_26.parquet          # Base con variables analíticas creadas resultado de script 05
├── scripts/
│   ├── 01_Carga_union_modulos.R     # Carga y cruce (joins) de los módulos extraídos de la ENAHO
│   ├── 02_Acondicionamiento.R           # Selección, renombrado, diagnóstico de NAs y tratamiento de valores perdidos; se exportó reporte de NAs
│   ├── 03_Exploracion.R        # Definición de etiquetas, uso de factores de expansión, EDA univariado y bivariado (tablas y gráficos exportados)
│   ├── 04_Informe_Exploracion_Inicial.Rmd        # Informe descriptivo en RMarkdown utilizando gráficos y tablas exportadas de script 03; se exportó en html
│   ├── 05_Clasificacion.R        # Creación de variables analíticas: tipologías, índices y recodificaciones; se exportó reporte de variables creadas
│   ├── 06_EDA_VariablesAnalíticas.R    # EDA univariado y bivariado utilizando variables creadas en script 05
│   └── 07_Documentacion # Definición de base de datos final y creación de CodeBook final
├── outputs/                                             # Outputs finales generados por los scripts del proyecto
│   ├── outputs_exploracion_analitica/                   # Gráficos y tablas exportadas del EDA de variables creadas (exportación del script 03)
│   ├── outputs_exploracion_inicial/                     # Gráficos y tablas exportadas del EDA inicial (exportación del script 06)
│   ├── ACONDICIONAMIENTO_Grafico_NAs_Informalidad.png   # Gráfico de diagnóstico de valores perdidos (exportación del script 02)
│   ├── ACONDICIONAMIENTO_Reporte_Datos_Perdidos_ENAHO.csv  # Diagnóstico tabular de valores perdidos (exportación del script 02)
│   ├── CLASIFICAR_Reporte_VariablesCreadas.html         # Tabla resumen (creada con ´gtsummary´) de las variables analíticas (exportación del script 05)
│   ├── CodeBook_codebook.html                           # Libro de códigos final creado con paquete ´codebook´ (exportación del script 07)
│   ├── CodeBook_codebook.Rmd                            # Archivo RMarkdown para la generación del libro de códigos con paquete ´codebook´
│   ├── CodeBook_dataMaid.html                           # Libro de códigos final creado con paquete ´dataMaid´ (exportación del script 07)
│   ├── CodeBook_dataMaid.Rmd                            # Archivo RMarkdown para la generación del libro de códigos con paquete ´dataMaid´
│   └── EXPLORACIÓN_Informe_EDA_Inicial.html             # Informe descriptivo inicial compilado en HTML (exportación del script 03)
├── renv/                       # Carpeta aislada del entorno local de paquetes
├── renv.lock                   # Registro exacto de las versiones de las librerías
├── .gitignore                  # Configuración de exclusión para evitar la subida de datos masivos al repositorio
└── [Nombre_del_Proyecto].Rproj # Archivo de inicialización del entorno R

