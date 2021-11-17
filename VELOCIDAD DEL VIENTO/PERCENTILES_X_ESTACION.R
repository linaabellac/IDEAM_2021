rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/SERIES_VELOCIDAD/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/PERCENTILES_SERIES/"

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
    #file <-list_dir[6]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(ruta_in, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
    if (any(data$Valor > 12)) {
      data <- data[-which(data$Valor > 12), ]
    }
  
    if (any(data$Valor == 0)) {
      data <- data[-which(data$Valor == 0), ]
    }
  
  
    data$mes<-month(data$Fecha)
    data<-arrange(data, mes)
    #colnames(df)<- c('Fecha', 'valor','mes')
  
    #data<- data[-which(data$Valor > 12), c(1,2,3)]
    percentil<-NULL
  
    for (i in 1:12) {
      #i=1
      temp<-filter(data,mes==i)
      percentil<-rbind(percentil,cbind(i,t(quantile(temp$Valor,c(0.01,0.25,0.75,0.95,0.997,0.998)))))
    
    }
  
  data_out<-data.frame(percentil)
  data_out<-round(percentil, 3)
  colnames(data_out)<-c('mes','1%','25%','75%','95%','99.7%','99.8%')
  
  
  write.table(data_out, file.path(ruta_out, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
  
  
  
  }

}
