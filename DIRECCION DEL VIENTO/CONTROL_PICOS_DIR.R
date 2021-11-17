setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

picos_extremos_dif<-function(data, lim_max){
  data_s <- 0
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    ns <- NULL
    temp_data <- filter(data, mes==i)
    max <- lim_max[i]
    
    if (length(which(temp_data$dif_cos > max)) > 0) {
      ns <- which(temp_data$dif_cos > max)
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


Datos <- file.path('PICOS EXTREMOS')
percentiles<-file.path('PERCENTILES_PICOS')
outPath <- file.path('SALIDAS_PICOS_EXT')

list_dir <- list.files(Datos, full.names = F)
list_per<-list.files(percentiles, full.names = F)

for (file in list_dir){
  #file <- list_dir[90]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  if (any(list_per==file)){
    #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
    datos_per<- data.frame(read.table(file.path(percentiles, file), sep = '|', header = T))}
  
  QC3_1<-picos_extremos_dif(data = data, lim_max = datos_per$per_dif)
  data$QC3_1<-'1QC0';data$QC3_1[QC3_1]<-'1QAT0'
  QC3_2<-picos_extremos_corr(data = data, lim_max = datos_per$per_corr)
  data$QC3_2<-'1QC0';data$QC3_2[QC3_2]<-'1QAT0'
  
  data$QC3<-ifelse(data$QC3_1=='1QAT0'|data$QC3_2=='1QAT0','1QAT0',ifelse(data$QC3_1=='1QAT0'& data$QC3_2=='1QAT0', '1QAT0','1QC0'))   
  data_out<-data.frame(data$Fecha, data$Valor, data$Valor_rad, data$QC3)
  colnames(data_out)<-c('Fecha', 'Valor', 'Valor_rad', 'QC3')
  
  write.table(data_out, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
  
}

  
  
  