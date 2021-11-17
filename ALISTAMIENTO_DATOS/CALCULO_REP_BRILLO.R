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

library(openxlsx)
library(tidyr)


Datos <- file.path('UNION_CONTROLES_BRILLO')
outPath <- file.path('CONTEO_BRILLO_ATIP')

list_dir <- list.files(file.path(Datos), full.names = F)

variable <- 'BSHG_TT_D'

data_out<-NULL

for (file in list_dir){ 
  # Lectura Datos y Pruebas de validacion Genrales
  #file <-list_dir[8]
  
  code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  
  data <- data.frame(read.table(file.path(Datos, paste0(variable, '@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')

  new<-data.frame(data$Codigo)
  
  new$QC1<-length(data$QC1[data$QC1 == "1QAT0"])/length(data$QC1)*100
  new$QC2AT<-length(data$QC2AT[data$QC2AT == "1QAT0"])/length(data$QC2AT)*100
  new$QC2ER<-length(data$QC2ER[data$QC2ER == "1QER0"])/length(data$QC2ER)*100
  new$QC3<-length(data$QC3[data$QC3 == "1QAT0"])/length(data$QC3)*100
  new$QC4<-length(data$QC4[data$QC4 == "1QAT0"])/length(data$QC4)*100
  new$QC5<-length(data$QC5[data$QC5 == "1QAT0"])/length(data$QC5)*100
  new$final_atp<-length(data$Estado_final[data$Estado_final == "1QAT0"])/length(data$Estado_final)*100
  new$final_err<-length(data$Estado_final[data$Estado_final == "1QER0"])/length(data$Estado_final)*100
  new<-new[1,]

  write.table(new, file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  
}   
