setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

Datos <- file.path('DEFINICION_ESTADO')
outPath <- file.path('SALIDAS_AGRE_ALIST')

variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[60]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    
    sin_na<-na.omit(data)    
    
    
    #if (any(sin_na$Estado=='1QAT0')) {
     # sin_na[which(sin_na$Estado=='1QAT0'), 2 ]<-NA 
    #}
    
    new_data<-ifelse(sin_na[,3]=='1QAT0',1,0)
    new_data<-data.frame(sin_na$Fecha, sin_na$Valor,new_data); colnames(new_data)<-c('Fecha', 'Valor', 'Estado')  
    
    
    write.table(new_data, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
}  
