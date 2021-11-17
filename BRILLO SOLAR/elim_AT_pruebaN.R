setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(modeest)

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/REGION7.1/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/REGION7_SALIDA/"


list_dir <- list.files(ruta_in, full.names = F)

for (file in list_dir){
  #file <- list_dir[2]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d')
  
  
  #if(any(day_groups[,2] < 3 )){
  #day_groups <- day_groups[-which(day_groups[,2] < 3), ] }  
  
  # meter la referencia a las columnas? 
  #if(any(data$estado == '1QATO' )){
    #data$estado <- data$estado[-which(data$estado =='1QATO'), ] }     
  
  data$estado<- data$estado[-which(data$estado =='1QER'), ]
  #corregir data frame de salida 
  
  data_out<- data.frame(data) 
  
  #colnames(df)<- c('Fecha', 'Valor','Dia Juliano','Teorica N', 'Estado')
  
  write.table(data_out, file.path(ruta_out, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(ruta_out, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(ruta_out, paste0('BSHG_TT_D@', code_station, '.data')))
  
  
  
  
  
}
