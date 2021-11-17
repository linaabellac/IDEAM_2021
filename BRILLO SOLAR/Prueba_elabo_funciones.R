setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO")
rm(list = ls(all = TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                         Librerias y complementos                  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(IdeamMeteo)
library("readxl")
library("lubridate")
library("zoo")
library("dplyr")
library("writexl")
library("insol")

library(openxlsx)
library(tidyr)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#           Definicion de directorios y lectura de archivos         #
#                             de configuracion                      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Definición directorios 
inPathCatalogos <- file.path("Catalogos")
inPathDatos <- file.path("..", "..","Datos", "S_diarias", "BSGH_TT_D")
outPath  <- file.path("Output", "Brillo solar", "BSGH_TT_D")

for (i in 1:dim(catalogo)[1]){
  # Lectura Datos y Pruebas de validacion Genrales
  code_station <- catalogo$CODIGO[i]
  type_variable <- 'BSGH_TT_D'
  region <- catalogo$`No. Region`[i]
  
  
  data <- data.frame(read.table(file.path(inPathDatos, paste0('PTPM_CON@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                              day = day(data$Fecha), hour = hour(data$Fecha), min = minute(data$Fecha))
  tz(data$Fecha) <- "America/Bogota"
  data <- duplicated_verif_con(data)
}

#consistencia brillo y nubosidad
#1QATO = dato atipico

if (("nubosidad" == 8) && ("brillo"==0)){"1QATO"}
if (("nubosidad"==0) && ("brillo"==0)){"1QATO"}


#consistencia brillo y radiación 

if(("radiación">500) && ("brillo"==0)){"1QATO"}


#dia juliano 

dia_juliano <- JD(data$fecha, inverse=F)



# prueba para elaboraciín de funciones 
declinacion_solar <- function(dia_juliano){
  
  ds<-0.409*sin(2(pi)*dia_juliano-1.39)
  return(ds)
  
}
angulo_puesta <- function(latitud, ds){
  
  angle <-acos(-tan(latitud),tan(ds))
  return(angle)
}
num_horas_teorica <- function(angle){
  
  N <-24/pi * angle
  return(N)
}





