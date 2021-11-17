setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/")
rm(list = ls(all = TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                         Librerias y complementos                  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(IdeamMeteo)
library("readxl")
library("lubridate")
library("zoo")
library("dplyr")
library("writexl")
library("insol")

library(openxlsx)
library(tidyr)


Datos <- file.path("regiones_crudas", "SALIDAS_24SEP", "REGION7")
outPath <- file.path("regiones_crudas", "SALIDAS_24SEP",'SALIDAS_ALEX', 'REGION7')

list_dir <- list.files(file.path(Datos), full.names = F)
type_variable <- 'BSHG_TT_D'

for (file in list_dir){ 
  # Lectura Datos y Pruebas de validacion Genrales
  #file <-list_dir[2]
  
  code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  
  data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
  #data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                              #day = day(data$Fecha))
  #tz(data$Fecha) <- "America/Bogota"
  
  #volvemos los test 1 y 0 para sumarlos y hacer el condicional
  new_data<-ifelse(data[,c(4,5,6,7,8,9)]=='1QAT0',1,ifelse(data[,c(4,5,6,7,8,9)]=='1QER0',8,0))
  new_data<-data.frame(new_data)
  suma_test<-rowSums(new_data[,c(1,2,3,4,5,6)], na.rm=TRUE)
 
  data_out<-data.frame(new_data,suma_test)
  
  estado_final<-ifelse(data_out$suma_test==0,'1QC0',ifelse(data_out$suma_test<=5,'1QAT0','1QER0'))
    
  serie<-data.frame(data$Fecha, data$Valor, estado_final)
  #colnames(serie)<- c('Fecha', 'Valor', 'Estado')
  
  serie_nulos<-data.frame(data$Fecha, estado_final, data$Valor)
  
  serie_nulos$data.Valor<-ifelse(serie_nulos$estado_final=='1QAT0'," ", serie_nulos$data.Valor)
  serie_nulos$data.Valor<-ifelse(serie_nulos$estado_final=='1QER0'," ", serie_nulos$data.Valor)
  #serie_nulos$estado_final<-ifelse(serie_nulos$estado_final=='1QAT0', " ", serie_nulos$estado_final)
  
  serie_final<- data.frame(data$Fecha, serie_nulos$data.Valor, serie$estado_final)
  colnames(serie_final)<- c('Fecha', 'Valor', 'Estado')
  
  #en una corrida para resultados 
  #resultados_lina<- data.frame(data$Fecha, data$Valor, serie_nulos$data.Valor, serie$estado_final)
  
  
  write.table(serie_final, file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  
}
  