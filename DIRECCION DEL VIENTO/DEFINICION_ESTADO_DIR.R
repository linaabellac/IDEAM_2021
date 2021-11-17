setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROLES/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)

Datos <- file.path('CONTROLES_FINALES')

outPath <- file.path('DEFINICION_ESTADO')

variables<-c('DV_10_VECT_MEDIA_H','DVAG_CON')


for (etiqueta in variables) {
  #etiqueta<-variables[2]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[60]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    #data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
    #day = day(data$Fecha))
    #tz(data$Fecha) <- "America/Bogota"
    
    new_data<-ifelse(data[,c(3,4,5,6)]=='1QAT0',1,0)
    new_data<-data.frame(new_data)
    suma_test<-rowSums(new_data[,c(1,2,3,4)], na.rm=TRUE)
    
    data_out<-data.frame(new_data,suma_test)
    
    estado_final<-ifelse(data_out$suma_test==0,'1QC0','1QAT0')
    
    data$valor_rad<-round(data$Valor*pi/180,4) 
    
    serie<-data.frame(data$Fecha, data$Valor, data$valor_rad,estado_final)
    colnames(serie)<- c('Fecha', 'Valor','valor_rad','Estado')
    
    
    #if (any(is.na(serie$Valor))) {
     # serie[which(is.na(serie$Valor)), 3 ]<-NA 
    #}
    
    write.table(serie, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
  }
}
