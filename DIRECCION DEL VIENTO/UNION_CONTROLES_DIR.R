setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

Datos <- file.path('SALIDAS_PICOS_EXT')
control_12<-file.path('SALIDAS_QC1_QC2')
outPath <- file.path('CONTROLES_UNIDOS_CONV')

list_dir <- list.files(Datos, full.names = F)
list_control <-list.files(control_12,full.names = F)


for (file in list_dir){
  #file <- list_dir[12]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  data<-arrange(data,Fecha)
  
  if (any(is.na(data[,1]))) {
    data <- data[-which(is.na(data)),c(1,2,3,4)]
  }
  
  if (any(list_control==file)) {
    #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
    datos_control<- data.frame(read.table(file.path(control_12, file), sep = '|', header = T))}
  
  if (any(is.na(datos_control[,c(1,2,3,4,5)]))) {
    datos_control <- datos_control[-which(is.na(datos_control)),c(1,2,3,4,5)]
  }
  
  data_out<-data.frame(data$Fecha, data$Valor, data$Valor_rad, datos_control$QC1, datos_control$QC2, data$QC3)
  colnames(data_out)<-c('Fecha', 'Valor', 'Valor_rad', 'QC1', 'QC2','QC3')
  #data_out<-arrange(data_out, Fecha)
  
  write.table(data_out, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
}

