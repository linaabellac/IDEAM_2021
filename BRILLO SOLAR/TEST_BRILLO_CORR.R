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

climatic_r_range<-function(data, lim_min, lim_max, R){
  data_s <- 0
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i] - R * (lim_max[i]- lim_min[i])
    max <- lim_max[i] + R * (lim_max[i]- lim_min[i])
    
    # Datos Erroneos - Eliminados
    if (length(which(temp_data$Valor < min)) > 0) {
      ns <- which(temp_data$Valor < min)
    }
    if (length(which(temp_data$Valor > max)) > 0) {
      ns <- which(temp_data$Valor > max)
    }
    if (length(ns) > 0){
      for (k in 1:length(ns)) {
        data_s <- c(data_s, which(data$Fecha == temp_data$Fecha[ns[k]]))
      }
    }
  }
  
  if (length(data_s[-1]) > 0) {
    return(data_s[-1])
  }
}


umbrales_mes <- file.path('SALIDAS POR MESES_', 'REGION7')
umbrales_anuales<-file.path('SALIDAS YEAR_', 'REGION7')
umbrales_series<-file.path('SALIDAS_PERC_SERIES_', 'REGION7')
Datos <- file.path('DATOS_13SEP',"REGION7")
outPath <- file.path("SALIDAS_24SEP",'REGION7')


list_dir <- list.files(file.path(Datos), full.names = F)
umbrales_mes <- read.table(file.path(umbrales_mes,"REGION_7_PERCENTIL.data"), sep = "|", header=T)
umbrales_anuales<-read.table(file.path(umbrales_anuales,"REGION7_PERC_ANUAL.data"), sep = "|", header=T)
umbrales_series<-read.table(file.path(umbrales_series,"REGION_7_PERCENTIL.data"), sep = "|", header=T)
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
  QC2AT<-climatic_range(data = data, lim_min = umbrales_mes$X3., lim_max = umbrales_mes$X99.7.)
  data$QC2AT<-'1QC0';data$QC2AT[QC2AT]<-'1QAT0'
  QC2ER<-climatic_range(data = data, lim_min = umbrales_mes$X1., lim_max = umbrales_mes$X99.9.)
  data$QC2ER<-'1QC0';data$QC2ER[QC2ER]<-'1QER0'
  QC3<-climatic_r_range(data= data, lim_min = umbrales_mes$X25., lim_max = umbrales_mes$X75., R=0.85)
  data$QC3<-'1QC0';data$QC3[QC3]<-'1QAT0'
  QC4<-testbox(data = data, perc_25 = umbrales_anuales$X25., perc_75 = umbrales_anuales$X75., R=1.4)
  QC5<-testbox(data = data, perc_25 = umbrales_series$X25., perc_75 = umbrales_series$X75., R=1.4)
  
  test<-data.frame(code_station, data$Fecha, data$Valor,data$Estado, data$QC2AT, data$QC2ER, data$QC3, QC4, QC5)
  colnames(test)<-c('Codigo', 'Fecha', 'Valor', 'QC1','QC2AT','QC2ER', 'QC3','QC4', 'QC5')
  
  
  
  write.table(test, file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('BSHG_TT_D@', code_station, '.data')))
  k=k+1 
  print(k)
  
}
