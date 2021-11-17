rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/PICOS_EXT/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/PICOS_EXT/PERCENTILES_PICOS/"

library(lubridate)
library(dplyr)
#library(modeest)

#list_dir <- list.files(ruta_in, full.names = F)

#data_out<- NULL
variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(ruta_in, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[10]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(ruta_in, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    
    if (any(data$dif == 0)) {
      data <- data[-which(data$dif == 0), ]
    }
    
    if (any(data$corr == 0)) {
      data <- data[-which(data$corr == 0), ]
    }
    
    #colnames(df)<- c('Fecha', 'valor','mes')
    
    #data<- data[-which(data$Valor > 12), c(1,2,3)]
    percentil<-NULL
    
    for (i in 1:12) {
      #i=6
      temp<-filter(data,mes==i)
      percentil<-rbind(percentil,cbind(i,t(quantile(temp$dif,0.997)),t(quantile(temp$corr,0.997))))
      
    }
    
    
    data_out<-data.frame(percentil)
    data_out<-round(percentil, 4)
    colnames(data_out)<-c('mes','per_dif','per_corr')
    
    
    write.table(data_out, file.path(ruta_out, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
  
}
