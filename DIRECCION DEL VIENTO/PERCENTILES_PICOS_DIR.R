setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)



Datos <- file.path('PICOS EXTREMOS')
outPath <- file.path('PERCENTILES_PICOS')

list_dir <- list.files(Datos, full.names = F)

for (file in list_dir){
  #file <- list_dir[17]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  data<-na.omit(data)
  percentil<-NULL
  
 
  for (i in 1:12) {
    #i=12
    temp<-filter(data,mes==i)
    percentil<-rbind(percentil,cbind(i,t(quantile(temp$dif_cos,0.97)),t(quantile(temp$corr,0.97))))
    
  }
  
  
  data_out<-data.frame(percentil)
  data_out<-round(percentil, 4)
  colnames(data_out)<-c('mes','per_dif','per_corr')
  
  
  write.table(data_out, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
  
  
} 
