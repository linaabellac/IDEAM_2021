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


Datos <- file.path('CONTEO_HR_ATIP')
outPath <- file.path('CONTEO_HR_ATIP')


variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')
data_out<-NULL

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[2]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = F))
    data<-data[-1,]
  
  
    data[,c(3,4,5,6,7,8,9,10,11)]<-as.numeric(data[,c(3,4,5,6,7,8,9,10,11)])
    data<-data %>% mutate_if(is.numeric, round, 4)
    data_out<- rbind(data_out, data)
    df<-data.frame(data_out)
    colnames(df)<-c('Codigo', 'Variable','QC1AT','QC1ER','QC2','QC3','QC4','QC5','QC6','final_atip','final_erroneos')
  }  
} 

write.table(df, file.path(outPath, paste0('REPORTE_HR.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(outPath, paste0('REPORTE_HR.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(outPath, paste0('REPORTE_HR.data')))
