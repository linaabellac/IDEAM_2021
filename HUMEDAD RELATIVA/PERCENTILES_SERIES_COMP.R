setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/SALIDAS SERIES COMPLETAS/REGION12_OUT/")

rm(list = ls(all = TRUE))

#ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/BrilloSolarDiario_Regiones_May19_2021/REGION_1/"
ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/HUMEDAD RELATIVA/HUMEDAD_RELATIVA_REGIONES/SALIDAS SERIES COMPLETAS/REGION12_OUT/"


library(lubridate)
library(dplyr)
#library(modeest)


data <- read.table('REGION_12.data', sep = "|", header = T)
data<-data.frame(data)
data<-na.omit(data)
data$Fecha<-as.POSIXct(data$Fecha, format='%Y-%m-%d')
data<- data[-which(data$valor < 5), c(1,2)]
#data$valor<-as.numeric(data$valor)
percentil<-quantile(data$valor, c(0.01, 0.25,0.75, 0.99))
data_out<-data.frame(t(percentil))
data_out<-round(data_out, 2)
colnames(data_out)<-c('1%','25%','75%','99%')


write.table(data_out, file.path(ruta_in, paste0('REGION_12_PERCENTIL.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_in, paste0('REGION_12_PERCENTIL.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_in, paste0('REGION_12_PERCENTIL.data')))


