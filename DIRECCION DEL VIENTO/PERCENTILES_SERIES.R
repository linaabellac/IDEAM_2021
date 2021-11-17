setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)

Datos <- file.path('DIRECCION_CONVENCIONALES')
outPath <- file.path('PERCENTILES_X_ESTACION')

list_dir <- list.files(Datos, full.names = F)

for (file in list_dir){
  #file <- list_dir[50]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')

  data$valor_rad<-data$Valor*pi/180  
  
  percentil<-quantile(data$valor_rad, c(0.25,0.75))
  percentil<-round(percentil,4)
  
  data_out<-data.frame(t(percentil)); colnames(data_out)<-c('25%', '75%')
  
  
  write.table(data_out, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
  
}
