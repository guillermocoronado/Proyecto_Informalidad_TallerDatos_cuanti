#===========================================================================================
#Proyecto: Análisis de la informalidad laboral usando datos de la ENAHO
#Autor: Guillermo Coronado
#Objetivo de este script: Crear el sistema de carpetas y enlazar con GitHub
#Fecha: 12-06-2026
#==========================================================================================

#Creamos carpetas--------------------------

dir.create("datos")
dir.create("datos/crudos")
dir.create("datos/procesados")
dir.create("outputs")
dir.create("docs")


#Enlace con Git y GitHub
install.packages("usethis")
usethis::use_git()
usethis::use_github()
