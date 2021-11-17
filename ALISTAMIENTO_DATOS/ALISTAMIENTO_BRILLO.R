setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/ALISTAMIENTO_DATOS/")
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


Datos <- file.path('Output_BS_20211011', 'Complementacion')
datos_variable<-file.path('SALIDAS_BRILLO_ALIST')
outPath <- file.path('BRILLO_SOLAR')

list_dir <- list.files(file.path(Datos), full.names = F)
list_dir_var<-list.files(file.path(datos_variable),full.names = F)

variable <- 'BSHG_TT_D'

for (file in list_dir){ 
  # Lectura Datos y Pruebas de validacion Genrales
  #file <-list_dir[4]
  
  code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  
  data <- data.frame(read.table(file.path(Datos, paste0(variable, '@', code_station, '.data')), sep = "|", header = T))
  #data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
  if (any(list_dir_var==file)) {
    #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
    data_v<- data.frame(read.table(file.path(datos_variable, file), sep = '|', header = T))}
  
  temp<-filter(data, data$status %in% c('1HOM0','1COM0'))
  
  colnames(data_v)<-c('event_time', 'event_value', 'status')
  data_v$station<-code_station
  data_v$sensor<-variable
  
  unido<-merge(temp, data_v, by=c('status', 'event_time', 'event_value','sensor','station'), all.x = T, all.y = T)
  unido<-arrange(unido, event_time)
  #completo<-full_join(temp, data_v, by='event_time')
  
  data_out<-data.frame(unido$station, unido$sensor, unido$status, unido$event_time, unido$event_value)
  colnames(data_out)<-c('station','sensor','status','event_time','event_value')
  
  data_out$event_value<-sprintf("%.2f",data_out$event_value) 
  
  #PENDIENTE DE DEFINIR
  if (any(data_out$status=='1COM0')){data_out$status[which(data_out$status == '1COM0')] <- '3C0'}
  if (any(data_out$status=='1HOM0')){data_out$status[which(data_out$status == '1HOM0')] <- '2H0'}
  
  data_out$event_time<-paste0(data_out$event_time, 'T', '07:00:00', '.000-0000')
  
  
  write.table(data_out, file.path(outPath, paste0(code_station,'-',variable, '.csv')), row.names = F, col.names = F, sep = ",")
  tx  <- readLines(file.path(outPath, paste0(code_station,'-',variable, '.csv')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0(code_station,'-',variable, '.csv')))
}
