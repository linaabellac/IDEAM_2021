setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

Datos <- file.path('SALIDAS_BOX')
percentiles<-file.path('PERCENTILES_X_ESTACION')
outPath <- file.path('SALIDAS_QC1_QC2')

list_dir <- list.files(Datos, full.names = F)
list_per<-list.files(percentiles, full.names = F)

for (file in list_dir){
  #file <- list_dir[50]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  if (any(list_per==file)) {
    #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
    datos_per<- data.frame(read.table(file.path(percentiles, file), sep = '|', header = T))}

  lim_sup<-datos_per$X75.+ data$desv * (datos_per$X75.- datos_per$X25.)
  lim_inf<-datos_per$X25. - data$desv * (datos_per$X75.- datos_per$X25.)
  
  data$QC2<-ifelse(data$Valor_rad > lim_sup, '1QAT0',ifelse(data$Valor_rad < lim_inf, '1QAT0', '1QC0'))
  data$QC1<-ifelse(data$Valor_rad > 6.28318, '1QAT0',ifelse(data$Valor_rad < 0, '1QAT0', '1QC0'))
  
  data_out<-data.frame(data$Fecha, data$Valor, data$Valor_rad, data$QC1, data$QC2)
  colnames(data_out)<-c('Fecha', 'Valor', 'Valor_rad', 'QC1', 'QC2')
  
  write.table(data_out, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
}

