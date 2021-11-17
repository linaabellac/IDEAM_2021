setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO/")
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


declinacion_solar <- function(d_julianos){
  
  ds<-0.409*sin(2*pi/365*d_julianos - 1.39)
  return(ds)
  
}

angulo_puesta <- function(latitud, ds){
  
  angle <-acos(-tan(latitud)*tan(ds))
  return(angle)
}


num_horas_teorica <- function(angle){
  
  N <-24/pi * angle
  return(N)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#           Definicion de directorios y lectura de archivos         #
#                             de configuracion                      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Definición directorios 

#ruta_in<-"C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO/Datos/Brillo_diario_499/"
#catalogo<-"C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO/"
#ruta_out <-"C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO/Output/Brillo solar/"


Catalogos <- file.path("catalogo_reg")
Datos <- file.path("Datos", "DATOS_13SEP", 'REGION7')
outPath <- file.path("Output", "control_n_corr",'REGION7')

list_dir <- list.files(file.path(Datos), full.names = F)
catalogo <- read_excel(file.path(Catalogos, "region_7.xlsx"))
type_variable <- 'BSHG_TT_D'
control_n <- NULL

for (i in 1:dim(catalogo)[1]){
  # Lectura Datos y Pruebas de validacion Genrales
  #i=1
  code_station <- catalogo$Codigo[i]
  latitud <- catalogo$latitud_rad[i]
  
  data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
  #data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                              #day = day(data$Fecha))
  #tz(data$Fecha) <- "America/Bogota"
  data <- duplicated_verif_con(data)

  d_julianos<- strftime(data$Fecha, format = "%j")
  d_julianos<-as.integer(d_julianos)
  ds<-declinacion_solar(d_julianos)
  angle<-angulo_puesta(latitud, ds)
  teorica_N<-num_horas_teorica(angle)
  teorica_N<- round(teorica_N, 4)
  estado<- ifelse(data$Valor>teorica_N, '1QAT0', '1QC0')
  df<- data.frame(data$Fecha, data$Valor, d_julianos, teorica_N, estado)
  
  colnames(df)<- c('Fecha', 'Valor','Dia Juliano','Teorica N', 'Estado')
  
  write.table(df, file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  
  
  
  
  
}


  
