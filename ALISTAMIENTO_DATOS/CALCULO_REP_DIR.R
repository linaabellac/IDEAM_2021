setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROLES")
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


Datos <- file.path('CONTROLES_FINALES')
datos_estado<-file.path('DEFINICION_ESTADO')
outPath <- file.path('CONTEO_ATIPICOS')

variables<-c('DV_10_VECT_MEDIA_H','DVAG_CON')


for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  list_dir_est<-list.files(file.path(datos_estado, etiqueta),full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[20]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    #sin_na<-na.omit(data)
    
    if (any(list_dir_est==file)) {
      #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
      data_es<- data.frame(read.table(file.path(datos_estado, etiqueta, file), sep = '|', header = T))}
    
    #omitir_na<-na.omit(data_es)
    
    new<-data.frame(code_station)
    new$variable<-etiqueta
    sin_na<-data.frame(data$QC2)
    sin_na<-na.omit(sin_na)
    new$QC1<-length(data$QC1[data$QC1 == "1QAT0"])/length(data$QC1)*100
    new$QC2<-length(sin_na$data.QC2[sin_na$data.QC2 == "1QAT0"])/length(sin_na$data.QC2)*100
    new$QC3<-length(data$QC3[data$QC3 == "1QAT0"])/length(data$QC3)*100
    new$final<-length(data_es$Estado[data_es$Estado == "1QAT0"])/length(data_es$Estado)*100
    new<-new %>% mutate_if(is.numeric, round, 4)
    
    #new$QC1<-length(data$QC1[data$QC1 == "1QAT0"])/length(data$QC1)*100
    #new$QC2<-length(data$QC2[data$QC2 == "1QAT0"])/length(data$QC2)*100
    #new$QC3<-length(data$QC3[data$QC3 == "1QAT0"])/length(data$QC3)*100
    #new$QC4<-length(data$QC4[data$QC4 == "1QAT0"])/length(data$QC4)*100
    #new<-new %>% mutate_if(is.numeric, round, 4)
    
    
    write.table(new, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
  }
}
