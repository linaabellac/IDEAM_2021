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


library(openxlsx)
library(tidyr)

#funcion para todas las series
testbox<-function(data, perc_25, perc_75, R){
  lim_max<-perc_75 + R * (perc_75 - perc_25)
  lim_min<-perc_25 - R *(perc_75 - perc_25)
  
  estado<-ifelse(data$Valor > lim_max, '1QAT0',ifelse(data$Valor < lim_min, '1QAT0', '1QC0'))
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
    
    QC<-ifelse(temp_data$Valor > max, '1QAT0', ifelse(temp_data$Valor < min, '1QAT0', '1QC0'))
  }
  return(QC)
}

#QC1 RANGO FIJO OJO VER SI ES PARA TODOS LOS DATOS O SOLO MENSUALES 

#rango_fijo_total<-function(data, lim_max, lim_min){
 # estado<- ifelse(data$Valor > lim_max, '1QAT0', ifelse(data$Valor < lim_min, '1QAT0', '1QC0'))
  #return(estado)
#}

#datos mes con percentiles para atipicos 
test_rangof<-function(data, lim_min, lim_max){
  QC<-NULL
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    #ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i]
    max <- lim_max[i]
    
    QC<-ifelse(temp_data$Valor > max, '1QAT0',ifelse(temp_data$Valor < min, '1QAT0', '1QC0'))
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
    
    QC<-ifelse(temp_data$Valor > max, '1QER0',ifelse(temp_data$Valor < min, '1QER0', '1QC0'))
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
    QC<-ifelse(temp_data$Valor > sup, '1QAT0',ifelse(temp_data$Valor < inf, '1QAT0', '1QC0'))
  }
  return(QC)
}



umbrales_mes <- file.path('SALIDAS POR MESES', 'REGION12_OUT')
umbrales_anuales<-file.path('SALIDAS ANUALES', 'REGION12_OUT')
umbrales_series<-file.path('SALIDAS SERIES COMPLETAS', 'REGION12_OUT')
max_min<-file.path('SALIDAS POR MESES', 'REGION12_OUT')
Datos <- file.path('HR_REGIONES_SEP',"REGION11")
outPath <- file.path("SALIDAS_22SEP",'REGION11')

max_min<-read.table(file.path(max_min,"REGION_12_VALORES.data"), sep = "|", header=T)
umbrales_mes <- read.table(file.path(umbrales_mes,"REGION_12_PERCENTIL.data"), sep = "|", header=T)
umbrales_anuales<-read.table(file.path(umbrales_anuales,"REGION12_PERCENTIL_PROM.data"), sep = "|", header=T)
umbrales_series<-read.table(file.path(umbrales_series,"REGION_12_PERCENTIL.data"), sep = "|", header=T)
k=1

variables<-c('HR_CAL_MEDIA_D','HRA2_MEDIA_D')

for (etiqueta in variables) {
  #etiqueta<-variables[2]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[12]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    #data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
    data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
    #data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                                #day = day(data$Fecha))
    #tz(data$Fecha) <- "America/Bogota"
    
    
    
    test<-NULL
    #mensuales 
    #QC1<-rango_fijo_total(data= data, lim_min = 0, lim_max = 100)
    QC1AT<-test_rangof(data = data, lim_min = umbrales_mes$X1., lim_max = umbrales_mes$X99.)
    QC1ER<-test_rangof_e(data = data, lim_min = umbrales_mes$X0.3., lim_max = umbrales_mes$X99.7.)
    QC2<-test_rangof_r(data= data, lim_min = umbrales_mes$X25., lim_max = umbrales_mes$X75., R=1.5)
    QC3<-testbox(data = data, perc_25 = umbrales_anuales$X25., perc_75 = umbrales_anuales$X75., R=1.5)
    QC4<-testbox(data = data, perc_25 = umbrales_series$X25., perc_75 = umbrales_series$X75., R= 1.5)
    QC5<-Consecutive_equal_values(data = data, ts=2, variable = F)
    data$estado<-'1QC0';data$estado[QC5]<-'1QAT0'
    QC6<-max_and_min(data = data, maximo = max_min$max, minimo = max_min$min )
    test<-data.frame(code_station, data$Fecha, data$Valor, QC1AT, QC1ER, QC2, QC3, QC4, data$estado,QC6)
    colnames(test)<-c('Codigo', 'Fecha', 'Valor','QC1AT','QC1ER', 'QC2','QC3', 'QC4','QC5','QC6')
    
    test$Fecha<-as.Date(test$Fecha, format = '%Y-%m-%d')
    
    
    write.table(test, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    k=k+1 
    print(k)
    
  }
  
  
  
}



