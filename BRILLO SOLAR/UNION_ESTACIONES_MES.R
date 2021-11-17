rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/SALIDAS_SIN1QER/REGION7_SALIDA/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/SALIDAS_SIN1QER/SALIDAS POR MESES/REGION7/"

library(lubridate)
library(dplyr)
#library(modeest)

list_dir <- list.files(ruta_in, full.names = F)

data_out<- NULL

for (file in list_dir){
  #file <- list_dir[1]
  #code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = F))
  data$V1 <- as.POSIXct(data$V1,  format = '%Y-%m-%d')
  
  data_out<- rbind(data_out, data)
  data_out<-data_out[-1,]
  mes<-month(data_out$V1)
  df<-data.frame(data_out, mes)
  df<-arrange(df, mes)
  colnames(df)<- c('Fecha', 'valor','dia_juliano','N','estado','mes')
}

write.table(df, file.path(ruta_out, paste0('REGION_7.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_out, paste0('REGION_7.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_out, paste0('REGION_7.data')))


