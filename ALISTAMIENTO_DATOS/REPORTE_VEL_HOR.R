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




Datos <- file.path('CONTEO_VEL_HORA')
outPath <- file.path('CONTEO_VEL_HORA')

variables<-c('VV_10_MEDIA_H','VVAG_CON')

data_out<-NULL

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[4]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = F))
    
    data<-data[-1,]
    data_out<- rbind(data_out, data)
    df<-data.frame(data_out)
    colnames(df)<-c('Codigo','variable','QC1','QC2','QC3','QC4','Final')
    
    
    
  }
}

write.table(df, file.path(outPath, paste0('REPORTE_HOR_VEL', '.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(outPath, paste0('REPORTE_HOR_VEL', '.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(outPath, paste0('REPORTE_HOR_VEL', '.data')))
