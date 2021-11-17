setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/SALIDAS POR MESES/REGION12_OUT/")

rm(list = ls(all = TRUE))

#ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/BrilloSolarDiario_Regiones_May19_2021/REGION_1/"
ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/SALIDAS POR MESES/REGION12_OUT/"


library(lubridate)
library(dplyr)
#library(modeest)


data <- read.table('REGION_12.data', sep = "|", header = T)
data<-data.frame(data)
data<-na.omit(data)
data$Fecha<-as.POSIXct(data$Fecha, format='%Y-%m-%d')
data$valor<-as.numeric(data$valor)
data$valor<-round(data$valor,2)
data<- data[-which(data$valor < 5), c(1,2,3)]
data<- data[-which(data$valor > 100), c(1,2,3)]


datos<-NULL

for (i in 1:12) {
  #i=1
  temp<-filter(data,mes==i)
  datos<-rbind(datos,cbind(i,t(max(temp$valor)),min(temp$valor)))
  
}

data_out<-data.frame(datos)
data_out<-round(datos, 2)
colnames(data_out)<-c('mes','max', 'min')

write.table(data_out, file.path(ruta_in, paste0('REGION_12_VALORES.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_in, paste0('REGION_12_VALORES.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_in, paste0('REGION_12_VALORES.data')))


