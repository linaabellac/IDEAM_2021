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


Datos <- file.path('UNION_HR_TODO')
outPath <- file.path('CONTEO_HR_ATIP')


variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[50]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    
    new<-data.frame(data$Codigo)
    new$variable<-etiqueta
    new$QC1AT<-length(data$QC1AT[data$QC1AT == "1QAT0"])/length(data$QC1AT)*100
    new$QC1ER<-length(data$QC1ER[data$QC1ER == "1QAT0"])/length(data$QC1ER)*100
    new$QC2<-length(data$QC2[data$QC2 == "1QER0"])/length(data$QC2)*100
    new$QC3<-length(data$QC3[data$QC3 == "1QAT0"])/length(data$QC3)*100
    new$QC4<-length(data$QC4[data$QC4 == "1QAT0"])/length(data$QC4)*100
    new$QC5<-length(data$QC5[data$QC5 == "1QAT0"])/length(data$QC5)*100
    new$QC6<-length(data$QC6[data$QC6 == "1QAT0"])/length(data$QC6)*100
    new$final_atp<-length(data$Estado_final[data$Estado_final == "1QAT0"])/length(data$Estado_final)*100
    new$final_err<-length(data$Estado_final[data$Estado_final == "1QER0"])/length(data$Estado_final)*100
    new<-new[1,]
    
    write.table(new, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
  }
}
