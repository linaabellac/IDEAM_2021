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
datos_alist<-file.path('SALIDAS_HR_ALIST')
outPath <- file.path('UNION_HR_TODO')


variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  list_dir_var<-list.files(file.path(datos_alist, etiqueta),full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[6]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    #data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
    data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
    
    if (any(list_dir_var==file)) {
      #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
      data_v<- data.frame(read.table(file.path(datos_alist, etiqueta, file), sep = '|', header = T))}
    
    data_out<-data.frame(data, data_v[ ,3])
    colnames(data_out)<-c('Codigo', 'Fecha', 'Valor','QC1AT','QC1ER','QC2','QC3','QC4','QC5','QC6','Estado_final')
    
    write.table(data_out, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
  }
}
