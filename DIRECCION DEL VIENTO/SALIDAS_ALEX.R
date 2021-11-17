setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROLES/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

Datos <- file.path('DIRECCION_DIARIAS')
outPath <- file.path('SALIDAS_ALEX_DIR')

variables<-c('DV_10_VECT_MEDIA_D','DVAG_MEDIA_D')

for (etiqueta in variables) {
  #etiqueta<-variables[2]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[6]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
    
    data$valor_rad<-round(data$Valor*pi/180,4)
    
    estado_final<-ifelse(data$Estado >=9, '1QAT0', '1QC0')
    
    serie_nulos<-data.frame(data$Fecha, data$Valor, data$valor_rad,estado_final);colnames(serie_nulos)<-c('Fecha', 'Valor','valor_rad','Estado')
    
    serie_nulos$Valor<-ifelse(serie_nulos$Estado=='1QAT0'," ", serie_nulos$Valor)
    serie_nulos$valor_rad<-ifelse(serie_nulos$Estado=='1QAT0'," ", serie_nulos$valor_rad)
    
    
    write.table(serie_nulos, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
}  
