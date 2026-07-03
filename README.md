# README: Análisis de la informalidad laboral utilizando datos de la ENAHO

### Autor: Guillermo Coronado
### Curso: Taller de Procesamiento de Datos
### Encuesta: Encuesta Nacional de Hogares, Instituto Nacional de Estadística e Informática, 2025 (anual)
### Módulos utilizados: Módulo 300 (Educación), Módulo 400 (Salud), Módulo 500 (Empleo e ingresos) y Módulo 85 (Gobernabilidad, Democracia y Transparencia) parte 1 (Confianza en las instituciones)
### Unidad de análisis: Individuo (trabajador en PEA ocupada)

## Descripción del proyecto
Este respositorio incluye el código y el flujo de trabajo completo del prpoyecto "Análisis de la informalidad laboral utilizando datos de la ENAHO", elaborado para el curso de Taller de Procesamiento de Datos 2026-1 de la PUCP. Se utilizan datos de la Encuesta Nacional de Hogares (ENAHO) de 2025 (versión  anual) trabajados integramente en **R** versión 4.5.3. La versión de todas las librerías se controla utilizando `renv`

El análisis explora la relación entre la formalidad y las siguientes dimensiones:
- **Demográficas**: Quintiles de ingreso, grupos de edad, nivel educativo, lengua materna y ubicación geográfica
- **Institucionales y de seguridad social**: Confianza en las instituciones, salud (aseguramiento, acceso, gasto de bolsillo) y pensiones

## Estructura del directorio
El directorio se organiza a través de la siguiente estructura de carpetas:
```text
├── Creacion_R_Project.R        # Script principal: Configuración del entorno, creación de carpetas y enlace a GitHub
├── datos/                      # No se incluyen los datos en este repositorio debido a su peso
│   ├── crudos/                                  # Módulos originales de la ENAHO en formato .csv 
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
```
A continuación, se detalla las principales decisiones y acciones tomadas en cada paso del flujo de trabajo. Si se tienen dudas más específicas, por favor, referirse al script en concreto

## EXTRAER
Se descargó los módulos 300, 400, 500 y 85 de la Encuesta Nacional de Hogares 2025 en su formato anual. Se guardó las bases de datos (.csv) en la carpeta correspondiente, así como el diccionario y la ficha técnica

## GESTIONAR
Se creó un R.project con el título del trabajo, y se realizó la conexión con Git y GitHub desde Rstudio. Mediante este proceso, se creó este repositorio de Github, el cual es continuamente actualizado a través de commits desde Rstudio. En el proyecto, se creó la estructura de carpetas presentada en la sección anterior. Debe tenerse en cuenta que, en este repositorio, las carpetas de "datos" están vacías puesto que se evitó subir las bases de datos (tanto crudas como procesadas) para no sobrecargar el repositorio debido a su peso. Esto se realizó especificando en el archivo ".gigitnore" que Git ignore los commits asociados a dicha carpeta. No obstante, el presente README especifica los módulos de la ENAHO utilizados y cada script permite reproducir y generar como resultado las bases de datos procesadas. Finalmente, se utilizó el paquete ´renv´para gestionar las versiones de las librerías utilizadas.

## ACONDICIONAR
Se realizó las fusiones correspondientes de los módulos utilizados mediante joins, dando resultado a la primera base de datos procesada; el proceso detallado puede observarse correctamente documentado en el script 01. En el script 02, se seleccionó y renombró las variables de interés, se realizó una revisión rápida de la estructura de los datos y se realizó un diagnóstico de valores perdidos, el cual dio como resultado dos reportes (uno gráfico y otro tabular) que pueden encontrar en la carpeta "outputs". Finalmente, se aplicó una estrategia de tratamiento de valores perdidos a tres variables problemáticas, aplicando métodos de eliminación de NAs, imputación mediante mediana e imputación mediante el modelo MICE. Las variables imputadas y el procedimiento de imputación pueden ser observadas con detalle en el script 02. Como resultado, se exportó la segunda base de datos procesada, la cual: **solo incluye a mayores de 18 años, solo incluye a la PEA ocupada, y solo incluye a las personas que mencionaron su lengua materna** 

## EXPLORAR
En el script 03, se cargó la base procesada más reciente y, de manera previa a la creación de gráficos y tablas, se creó etiquetas para las opciones de respuesta de las variables de interés, guiandose del diccionario de datos de la ENAHO 2025. Posteriormente, se realizó un análisis exploratorio de datos (EDA) univariado y bivariado con las variables de interés, dando como resultado tablas y gráficos exportados a la subcarpeta "outputs_exploracion_inicial". Estos gráficos y tablas fueron utilizados en el script 04, en el que se redacta el informe descriptivo de los datos, y se exporta como html hacia la carpeta "outputs" ("EXPLORACIÓN_Informe_EDA_Inicial"). En todo el EDA, se utiliza los factores de expansión correspondientes. Como resultado del script 03, además de exportar los gráficos y tablas mencionados, se exportó una tercera base de datos procesada que incluye las etiquetas de las opciones de respuesta

## CLASIFICAR
En el script 05, se crean las siguientes variables analíticas: formalidad_sector, formalidad_empleo, tipologia_laboral, grupo_edad_teoria, quintil_ingreso, afiliacion_salud, afiliacion_pensiones, indice_confianza_simple. Para observar el proceso detallado de creación de las variables, por favor referirse al script. Para una definición más formal de cada una de ellas, por favor, referirse al *CodeBook* presentado en la carpeta "outputs". Como resultado del script 05, se exportó en html un reporte de las variables creadas, así como una cuarta base de datos procesada que incluye las nuevas variables. De manera adicional, se utilizó las variables analíticas creadas para hacer un nuevo EDA, que se puede encontrar en el script 06, donde se crean gráficos y tablas exportadas a la carpeta outputs_exploracion_analitica/ 

## DOCUMENTAR
En el script 07, se realiza la depuración final de la base de datos, quedándonos solo con las variables utilizadas en el EDA de variables analíticas. Asimismo, se incluye las etiquetas descriptivas y fuente de cada variable (su nombre en la ENAHO), así como los metadatos con información sobre las decisiones metodológicas tomadas y la descripción de la creación de las variables analíticas. Con esta información, se generan dos libros de códigos finales, exportados a la carpeta "outputs": uno con el paquete ´dataMaid´ (CodeBook_dataMaid) y otro con el paquete ´codebook´ (CodeBook_codebook). La versión tanto en html como en Rmd de ambos archivos puede encontrarse en la carpeta "outputs". En estos archivos, se describe detalladamente el significado de las variables, las opciones de respuesta, su distribución, sus NAs y estrategias de imputación, y se detalla la fuente original en la ENAHO. Como parte de la documentación, en la carpeta "docs" se puede encontrar el diccionario de datos de la ENAHO 2025, así como su ficha técnica. Las decisiones tomadas también están documentadas en commits en los propios scripts
