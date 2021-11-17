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

#funcion para todas las series
testbox<-function(data, perc_25, perc_75, R){
  lim_max<-perc_75 + R * (perc_75 - perc_25)
  lim_min<-perc_25 - R *(perc_75 - perc_25)
  
  estado<-ifelse(data > lim_max, '1QAT0', '1QC0')
  return(estado)
  
}

#QC7 con valores maximos y minimos mensuales 
max_and_min<-function(data, minimo, maximo){
  QC<-NULL
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    #ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- minimo[i]
    max <- maximo[i]
    
    QC<-ifelse(data$Valor > max, '1QAT0', ifelse(data$Valor < min, '1QAT0', '1QC0'))
  }
  return(QC)
}

#QC1 RANGO FIJO OJO VER SI ES PARA TODOS LOS DATOS O SOLO MENSUALES 

rango_fijo_total<-function(data, lim_max, lim_min){
  estado<- ifelse(data$Valor > lim_max, '1QAT0', ifelse(data$Valor < lim_min, '1QAT0', '1QC0'))
  return(estado)
}

#datos mes con percentiles para atipicos 
test_rangof<-function(data, lim_min, lim_max){
  QC<-NULL
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    #ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i]
    max <- lim_max[i]
    
    QC<-ifelse(data$Valor > max, '1QAT0', '1QC0')
  }
  return(QC)
}

#datos mes con percentiles para erroneos 
test_rangof_e<-function(data, lim_min, lim_max){
  QC<-NULL
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    #ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i]
    max <- lim_max[i]
    
    QC<-ifelse(data$Valor > max, '1QER0', '1QC0')
  }
  return(QC)
}
# datos mes con coeficiente R 
test_rangof_r<-function(data, lim_min, lim_max, R=1.5){
  QC<-NULL
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    #ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i]
    max <- lim_max[i]
    coefi<-R
    
    sup<-max + coefi * (max - min)
    inf<-min - coefi *(max - min)
    QC<-ifelse(data$Valor > sup, '1QAT0', '1QC0')
  }
  return(QC)
}


shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

#anual con valor R 
#test_rango_anual<-function(data, lim_min, lim_max, R){
 # QC<-NULL
  #data$año<-year(data$Fecha)
  
 # for (i in 1978:2021) {
 #   temp_data <- filter(data, año==i)
  #  min <- lim_min[i]
  # max <- lim_max[i]
   # coefi<-R[i]
    
  #  sup<-max + coefi * (max - min)
   # inf<-min - coefi *(max - min)
  #  QC<-ifelse(data$Valor > sup, '1QAT0', '1QC0')
  #}
  #return(QC)
#}

umbrales_mes <- file.path('SALIDAS POR MESES', 'REGION12_OUT')
umbrales_anuales<-file.path('SALIDAS ANUALES', 'REGION12_OUT')
umbrales_series<-file.path('SALIDAS SERIES COMPLETAS', 'REGION12_OUT')
max_min<-file.path('SALIDAS POR MESES', 'REGION12_OUT')
Datos <- file.path("REGION 12")
outPath <- file.path("OUTPUT_TEST",'REGION12')

max_min<-read.table(file.path(max_min,"REGION_12_VALORES.data"), sep = "|", header=T)
umbrales_mes <- read.table(file.path(umbrales_mes,"REGION_12_PERCENTIL.data"), sep = "|", header=T)
umbrales_anuales<-read.table(file.path(umbrales_anuales,"REGION12_PERCENTIL_PROM.data"), sep = "|", header=T)
umbrales_series<-read.table(file.path(umbrales_series,"REGION_12_PERCENTIL.data"), sep = "|", header=T)
k=1

variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')

for (etiqueta in variables) {
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[1]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    #data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d')
    data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                                day = day(data$Fecha))
    tz(data$Fecha) <- "America/Bogota"
    
    
    new<- data.frame(data$Fecha, data$Valor, data$Valor)
    colnames(new)<-c('fecha', 'valor', 'valor1')
    new$valor1<-shift(new$valor1,1)
    estado<- ifelse(new$valor==new$valor1, '1QAT0', '1QC0')
    
    final<-data.frame(data$Fecha, new$valor1, estado)
    colnames(final)<-c('Fecha','Valor','QC6')
    complet<-cbind(data[1,],'1QC0')
    completar<-data.frame(complet);colnames(completar)<-c('Fecha', 'Valor', 'QC6')
    data_out<-rbind(completar,final)
    data_out<-data.frame(data_out)
    
    test<-NULL
    #mensuales 
    QC1<-rango_fijo_total(data= data_out, lim_min = 0, lim_max = 100)
    QC2AT<-test_rangof(data = data_out, lim_min = umbrales_mes$X1., lim_max = umbrales_mes$X99.)
    QC2ER<-test_rangof_e(data = data_out, lim_min = umbrales_mes$X0.3., lim_max = umbrales_mes$X99.7.)
    QC3<-test_rangof_r(data= data_out, lim_min = umbrales_mes$X25., lim_max = umbrales_mes$X75., R=1.5)
    QC4<-testbox(data = data_out$Valor, perc_25 = umbrales_anuales$X25., perc_75 = umbrales_anuales$X75., R=1.5)
    QC5<-testbox(data = data_out$Valor, perc_25 = umbrales_series$X25., perc_75 = umbrales_series$X75., R= 1.5)
    QC7<-max_and_min(data = data_out, maximo = max_min$max, minimo = max_min$min )
    test<-data.frame(code_station, data_out$Fecha, data_out$Valor, QC1, QC2AT, QC2ER, QC3, QC4, QC5, data_out$QC6,QC7)
    colnames(test)<-c('Codigo', 'Fecha', 'Valor', 'QC1','QC2AT','QC2ER', 'QC3','QC4', 'QC5','QC6','QC7')
    
    
    
    write.table(test, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    k=k+1 
    print(k)
    
  }
  
  
  
}




