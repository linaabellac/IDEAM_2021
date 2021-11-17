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

catalogo<-file.path("regiones_crudas", "catalogo_regiones")
Datos <- file.path("Brillo_Solar_por_Regiones","REGION7_SALIDA")
datos_1<-file.path("regiones_crudas", "output_test","REGION7")
outPath <- file.path("regiones_crudas", "TEST_UNIDOS", "REGION7")


list_dir <- list.files(file.path(Datos), full.names = F)
list_dir_<-list.files(file.path(datos_1), full.names=F)
catalogo<-read_excel(file.path(catalogo,"REGION7.xlsx"))
type_variable <- 'BSHG_TT_D'
#control_n <- NULL

for (i in 1:dim(catalogo)[1]){ 
  # Lectura Datos y Pruebas de validacion Genrales
  i=1
  code_station <- catalogo$Codigo[i]
  
  data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
  data_1<-data.frame(read.table(file.path(datos_1, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d')
  data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                              day = day(data$Fecha))
  tz(data$Fecha) <- "America/Bogota"
  data_1$Fecha <- as.POSIXct(data_1$Fecha,  format = '%Y-%m-%d')
  data_1$Fecha <- make_datetime(year = year(data_1$Fecha), month = month(data_1$Fecha), 
                              day = day(data_1$Fecha))
  tz(data_1$Fecha) <- "America/Bogota"
  data <- duplicated_verif_con(data)
  data_1<-duplicated_verif_con(data_1)
  data<-na.omit(data)
  data_1<-na.omit(data_1)
  data$Estado[data$Estado=='1QER']<-'1QER0'
  data$Estado[data$Estado=='1AQTO']<-'1QAT0'
  data$Estado[data$Estado=='1QCO']<-'1QC0'
  
  data_out<-data.frame(data_1$Codigo, data_1$Fecha, data_1$Valor, data$Estado, data_1$QC2AT, data_1$QC2ER, data_1$QC3,
                      data_1$QC4, data_1$QC5 )
  colnames(data_out)<-c('Codigo','Fecha','Valor','QC1', 'QC2AT', 'QC2ER','QC3','QC4','QC5')
  #data_out<-data.frame(data_1, data$Estado)
  
  #write.xlsx(data_out,file.path(outPath, paste0('BSHG_TT_D@', code_station, '.xlsx')),row.names=FALSE, col.names=TRUE)
  
  write.table(data_out, file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  
  
  
}
  
  
  
  






