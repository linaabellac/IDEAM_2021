setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROLES/")
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




Datos <- file.path('CONTEO_ATIPICOS')
outPath <- file.path('CONTEO_ATIPICOS')

variables<-c('DV_10_VECT_MEDIA_H','DVAG_CON')

data_out<-NULL

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[8]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = F))
    
    data<-data[-1,]
    data_out<- rbind(data_out, data)
    df<-data.frame(data_out)
    colnames(df)<-c('Codigo','variable','QC1','QC2','QC3','Final')
    
    
    
  }
}

write.table(df, file.path(outPath, paste0('REPORTE_DIR', '.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(outPath, paste0('REPORTE_DIR', '.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(outPath, paste0('REPORTE_DIR', '.data')))
