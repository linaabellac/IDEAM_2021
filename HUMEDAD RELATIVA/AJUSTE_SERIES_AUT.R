rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/AUTOMATICAS_REGIONES/REGION11/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/AUTOMA_CORREGI_SEP/REGION11/"

library(lubridate)
library(dplyr)
#library(modeest)

list_dir <- list.files(ruta_in, full.names = F)

for (file in list_dir){
  file <- list_dir[44]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = T))
  #data_out<-gsub(',', '|', data)
  #data<-data[-1,]
  data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
  #data$V1 <- as.POSIXct(data$V1,  format = '%Y-%m-%d')
  
 
  
  write.table(data, file.path(ruta_out, paste0('HR_CAL_MEDIA_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(ruta_out, paste0('HR_CAL_MEDIA_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(ruta_out, paste0('HR_CAL_MEDIA_D@', code_station, '.data')))
}

#write.table(df, file.path(ruta_out, paste0('REGION_7.data')), sep="|", row.names = FALSE, col.names = TRUE)
#tx  <- readLines(file.path(ruta_out, paste0('REGION_7.data')))
#tx2  <- gsub(pattern = '"', replace = '', x = tx)
#writeLines(tx2, con=file.path(ruta_out, paste0('REGION_7.data')))