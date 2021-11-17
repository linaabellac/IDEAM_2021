rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/TEST_UNIDOS/REGION7/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/TEST_UNIDOS/REPORTE/REGION7/"

library(lubridate)
library(dplyr)
#library(modeest)

list_dir <- list.files(ruta_in, full.names = F)

data_out<- NULL

for (file in list_dir){
  #file <- list_dir[1]
  #code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = F))
  data<-data[-1,]
  data$V2 <- as.POSIXct(data$V2,  format = '%Y-%m-%d')
  
  
  data_out<- rbind(data_out, data)
  #data_out<-data_out[-1,]
  #mes<-month(data_out$V1)
  #df<-data.frame(data_out, mes)
  #df<-arrange(df, mes)
  df<-data.frame(data_out)
  colnames(df)<- c('Codigo', 'Fecha', 'valor','QC1','QC2AT','QC2ER','QC3', 'QC4', 'QC5')
}

write.table(df, file.path(ruta_out, paste0('REGION_7.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_out, paste0('REGION_7.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_out, paste0('REGION_7.data')))


