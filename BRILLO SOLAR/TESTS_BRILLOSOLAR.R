setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/")
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
  
  estado<-ifelse(data$Valor > lim_max, '1QAT0',ifelse(data$Valor < lim_min, '1QAT0', '1QC0'))
  return(estado)
  
}

#datos mes con percentiles para atipicos 
test_rangof<-function(data, lim_min, lim_max){
  QC<-NULL
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    #i=1
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
    
    QC<-ifelse(temp_data$Valor > max, '1QER0', ifelse(temp_data$Valor < min, '1QER0', '1QC0'))
  }
  return(QC)
}

# datos mes con coeficiente R 
test_rangof_r<-function(data, lim_min, lim_max, R){
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



umbrales_mes <- file.path('SALIDAS POR MESES_', 'REGION1')
umbrales_anuales<-file.path('SALIDAS YEAR_', 'REGION1')
umbrales_series<-file.path('SALIDAS_PERC_SERIES_', 'REGION1')
Datos <- file.path('DATOS_13SEP',"REGION1")
outPath <- file.path("SALIDAS_24SEP",'REGION1')


list_dir <- list.files(file.path(Datos), full.names = F)
umbrales_mes <- read.table(file.path(umbrales_mes,"REGION_1_PERCENTIL.data"), sep = "|", header=T)
umbrales_anuales<-read.table(file.path(umbrales_anuales,"REGION1_PERC_ANUAL.data"), sep = "|", header=T)
umbrales_series<-read.table(file.path(umbrales_series,"REGION_1_PERCENTIL.data"), sep = "|", header=T)
type_variable <- 'BSHG_TT_D'
k=1

for (file in list_dir){ 
  # Lectura Datos y Pruebas de validacion Genrales
  #file <-list_dir[1]
  
  code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  
  data <- data.frame(read.table(file.path(Datos, paste0(type_variable, '@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.Date(data$Fecha,  format = '%Y-%m-%d')
  #data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                             # day = day(data$Fecha))
  #tz(data$Fecha) <- "America/Bogota"
  
  test<-NULL
  #mensuales 
  QC2AT<-test_rangof(data = data, lim_min = umbrales_mes$X3., lim_max = umbrales_mes$X99.7.)
  QC2ER<-test_rangof_e(data = data, lim_min = umbrales_mes$X1., lim_max = umbrales_mes$X99.9.)
  QC3<-test_rangof_r(data= data, lim_min = umbrales_mes$X25., lim_max = umbrales_mes$X75., R=0.85)
  QC4<-testbox(data = data, perc_25 = umbrales_anuales$X25., perc_75 = umbrales_anuales$X75., R=1.4)
  QC5<-testbox(data = data, perc_25 = umbrales_series$X25., perc_75 = umbrales_series$X75., R=1.4)
  
  test<-data.frame(code_station, data$Fecha, data$Valor,data$Estado, QC2AT, QC2ER, QC3, QC4, QC5)
  colnames(test)<-c('Codigo', 'Fecha', 'Valor', 'QC1','QC2AT','QC2ER', 'QC3','QC4', 'QC5')
  
  
  
  write.table(test, file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  k=k+1 
  print(k)
  
}
  