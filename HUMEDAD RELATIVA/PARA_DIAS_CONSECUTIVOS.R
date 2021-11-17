setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/")
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

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}



umbrales_mes <- file.path('SALIDAS POR MESES', 'REGION2_OUT')
umbrales_anuales<-file.path('SALIDAS ANUALES', 'REGION2_OUT')
umbrales_series<-file.path('SALIDAS SERIES COMPLETAS', 'REGION2_OUT')
max_min<-file.path('SALIDAS POR MESES', 'REGION2_OUT')
Datos <- file.path("REGION 2")
outPath <- file.path("OUTPUT_TEST",'REGION2')

max_min<-read.table(file.path(max_min,"REGION_2_VALORES.data"), sep = "|", header=T)
umbrales_mes <- read.table(file.path(umbrales_mes,"REGION_2_PERCENTIL.data"), sep = "|", header=T)
umbrales_anuales<-read.table(file.path(umbrales_anuales,"REGION2_PERCENTIL_PROM.data"), sep = "|", header=T)
umbrales_series<-read.table(file.path(umbrales_series,"REGION_2_PERCENTIL.data"), sep = "|", header=T)
#type_variable <- 'BSHG_TT_D'
k=1

variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')

for (etiqueta in variables){
  etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    file <-list_dir[2]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    #data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d')
    data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                                day = day(data$Fecha))
    tz(data$Fecha) <- "America/Bogota"
    
    
    #QC6<-Consecutive_equal_values(data = data, ts=2, variable = F)
    #salida<-data.frame(QC6)

    #n_occur <- data.frame(table(data$Valor))
    #n_occur[n_occur$Freq > 1,]
    #vocabulary[vocabulary$id %in% n_occur$Var1[n_occur$Freq > 1],]
    
    new<- data.frame(data$Fecha, data$Valor, data$Valor)
    colnames(new)<-c('fecha', 'valor', 'valor1')
    new$valor1<-shift(new$valor1,1)
    estado<- ifelse(new$valor==new$valor1, '1QAT0', '1QC0')
    
    final<-data.frame(new$valor1, estado)
    colnames(final)<-c('Valor','QC6')
    complet<-cbind(data[1,2],'1QC0')
    completar<-data.frame(complet);colnames(completar)<-c('Valor', 'QC6')
    data_out<-rbind(completar,final)
    data_out<-na.omit(data_out)
    data_out<-data.frame(data$Fecha,data_out)
  }
}