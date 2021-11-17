setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

Datos <- file.path('DIRECCION_CONVENCIONALES')
box_series<-file.path('BOXPLOT_SERIES')
outPath <- file.path('SALIDAS_BOX')

list_dir <- list.files(Datos, full.names = F)
list_box <-list.files(box_series,full.names = F)


for (file in list_dir){
  #file <- list_dir[150]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  #data$dia<-as.Date(data$Fecha, format = "%Y-%m-%d")
  #data$horas<-format(as.POSIXct(data$Fecha), format = "%H:%M:%S")
  data$dia <- format(as.POSIXct(strptime(data$Fecha,"%Y-%m-%d %H:%M:%S")) ,format = "%Y-%m-%d")
  data$horas <- format(as.POSIXct(strptime(data$Fecha,"%Y-%m-%d %H:%M:%S")) ,format = "%H:%M:%S")
  
  if (any(list_box==file)) {
    #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
    datos_box<- data.frame(read.table(file.path(box_series, file), sep = '|', header = T))}
  
  colnames(datos_box)<-c('dia','seno','coseno','R','desv')
  datos_box$dia<-format(as.POSIXct(strptime(datos_box$dia,"%Y-%m-%d")) ,format = "%Y-%m-%d")
  unido<-full_join(data, datos_box, by='dia')
  
  data_final<-data.frame(unido$Fecha, unido$Valor, unido$desv);colnames(data_final)<-c('Fecha','Valor','desv')
  data_final$Valor_rad<-round(data_final$Valor*pi/180,4)
  
  data_out<-data.frame(data_final$Fecha, data_final$Valor, data_final$Valor_rad, data_final$desv)
  colnames(data_out)<-c('Fecha', 'Valor', 'Valor_rad', 'desv')
  
  write.table(data_out, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
}

  