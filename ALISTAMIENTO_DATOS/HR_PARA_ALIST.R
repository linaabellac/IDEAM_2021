setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/ALISTAMIENTO_DATOS/")
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


Datos <- file.path("SALIDAS_24SEP_HR",'TODAS_SERIES_HR')
outPath <- file.path('SALIDAS_HR_ALIST')


variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[20]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    #data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
    data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
    #data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
    #day = day(data$Fecha))
    #tz(data$Fecha) <- "America/Bogota"
    
    
    #volvemos los test 1 y 0 para sumarlos y hacer el condicional
    new_data<-ifelse(data[,c(4,5,6,7,8,9,10)]=='1QAT0',1,ifelse(data[,c(4,5,6,7,8,9,10)]=='1QER0',10,0))
    new_data<-data.frame(new_data)
    suma_test<-rowSums(new_data[,c(1,2,3,4,5,6,7)], na.rm=TRUE)
    
    data_out<-data.frame(new_data,suma_test)
    
    estado_final<-ifelse(data_out$suma_test==0,'1QC0',ifelse(data_out$suma_test<=8,'1QAT0','1QER0'))
    
    serie<-data.frame(data$Fecha, data$Valor, estado_final)
    colnames(serie)<- c('Fecha', 'Valor', 'Estado')
    
    serie$Valor<-round(serie$Valor,2)
    
    write.table(serie, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
  }
}

