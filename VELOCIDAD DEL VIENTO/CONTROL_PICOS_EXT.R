setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)


picos_extremos_dif<-function(data, lim_max){
  data_s <- 0
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    ns <- NULL
    temp_data <- filter(data, mes==i)
    max <- lim_max[i]
    
    if (length(which(temp_data$dif > max)) > 0) {
      ns <- which(temp_data$dif > max)
    }
    if (length(ns) > 0){
      for (k in 1:length(ns)) {
        data_s <- c(data_s, which(data$Fecha == temp_data$Fecha[ns[k]]))
      }
    }
  }
  
  if (length(data_s[-1]) > 0) {
    return(data_s[-1])
  }
}

picos_extremos_corr<-function(data, lim_max){
  data_s <- 0
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    ns <- NULL
    temp_data <- filter(data, mes==i)
    max <- lim_max[i]
    
    if (length(which(temp_data$corr> max)) > 0) {
      ns <- which(temp_data$corr > max)
    }
    if (length(ns) > 0){
      for (k in 1:length(ns)) {
        data_s <- c(data_s, which(data$Fecha == temp_data$Fecha[ns[k]]))
      }
    }
  }
  
  if (length(data_s[-1]) > 0) {
    return(data_s[-1])
  }
}


Datos <- file.path('PICOS_EXT')
datos_perc<-file.path('PERCENTILES_PICOS')
outPath <- file.path('CONTROL_PICOS')

variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  list_dir_p<-list.files(file.path(datos_perc,etiqueta),full.names = F)
  
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[6]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    if (any(list_dir_p==file)) {
      #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
      data_per<- data.frame(read.table(file.path(datos_perc, etiqueta, file), sep = '|', header = T))}
    
    
    
    QC5_1<-picos_extremos_dif(data = data, lim_max = data_per$per_dif)
    data$QC5_1<-'1QC0';data$QC5_1[QC5_1]<-'1QAT0'
    QC5_2<-picos_extremos_corr(data = data, lim_max = data_per$per_corr)
    data$QC5_2<-'1QC0';data$QC5_2[QC5_2]<-'1QAT0'
    
    data$estado<-ifelse(data$QC5_1=='1QAT0'|data$QC5_2=='1QAT0','1QAT0',ifelse(data$QC5_1=='1QAT0'& data$QC5_2=='1QAT0', '1QAT0','1QC0'))   
    
    
    write.table(data, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
}
