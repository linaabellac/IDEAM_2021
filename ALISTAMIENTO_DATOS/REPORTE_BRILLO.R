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


Datos <- file.path('CONTEO_BRILLO_ATIP')
outPath <- file.path('CONTEO_BRILLO_ATIP')

list_dir <- list.files(file.path(Datos), full.names = F)

variable <- 'BSHG_TT_D'

data_out<-NULL

for (file in list_dir){ 
  # Lectura Datos y Pruebas de validacion Genrales
  #file <-list_dir[8]
  
  code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  
  data <- data.frame(read.table(file.path(Datos, paste0(variable, '@', code_station, '.data')), sep = "|", header = F))
  data<-data[-1,]
  
  
  data[,c(2,3,4,5,6,7,8,9)]<-as.numeric(data[,c(2,3,4,5,6,7,8,9)])
  data<-data %>% mutate_if(is.numeric, round, 4)
  data_out<- rbind(data_out, data)
  df<-data.frame(data_out)
  colnames(df)<-c('Codigo','QC1','QC2AT','QC2ER','QC3','QC4','QC5','final_atipicos','final_erroneos')

} 
  
write.table(df, file.path(outPath, paste0('REPORTE_BRILLO.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(outPath, paste0('REPORTE_BRILLO.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(outPath, paste0('REPORTE_BRILLO.data')))
