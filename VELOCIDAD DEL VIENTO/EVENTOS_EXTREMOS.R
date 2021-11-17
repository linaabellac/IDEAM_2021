setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/EVENTOS_EXTREMOS/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)

Datos <- file.path('ESTACIONES_EVENTOS')

outPath <- file.path('SALIDAS')

variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables){
  #etiqueta<-variables[2]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[28]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    datos_eventos<-read.table(file.path('EVENTOS.csv'),sep = ',', header = T)
    datos_eventos$ï..FECHA<-paste0(datos_eventos$ï..FECHA, ' ' ,datos_eventos$HORA)
    datos_eventos$ï..FECHA<-as.POSIXct(datos_eventos$ï..FECHA,  format = '%m/%d/%Y %H:%M:%S')
    
    df_eventos<-data.frame(datos_eventos$ï..FECHA, datos_eventos$VELOCIDAD.DEL.VIENTO);colnames(df_eventos)<-c('Fecha', 'Valor_vel')
    
    #if (any(data$Fecha==df_eventos$Fecha)){
     # data$QC5<-ifelse(data$Valor< df_eventos$Valor_vel, '1QC0', '1QAT0')
    #}
    total<-merge(data, df_eventos, by='Fecha')
    total$estado<-ifelse(total$Valor < total$Valor_vel, '1QC0', '1QAT0')
    #todo<-full_join(data, total, by='Fecha', copy=T)
    todo<-full_join(data, total, by='Fecha')
    
    data_out<-data.frame(todo$Fecha, todo$Valor.x, todo$QC1.x, todo$QC2.x, todo$QC3.x, todo$QC4.x, todo$estado)
    colnames(data_out)<-c('Fecha', 'Valor', 'QC1', 'QC2', 'QC3', 'QC4', 'QC5')
    
    if(any(duplicated(data_out))){
      data_out<-data_out[-which(duplicated(data_out)),]
    }
    
   
    
    write.table(data_out, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
}
