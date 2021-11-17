setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/SALIDAS_SIN1QER/SALIDAS_PERC_SERIES/REGION7/")

rm(list = ls(all = TRUE))

#ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/BrilloSolarDiario_Regiones_May19_2021/REGION_1/"
ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/SALIDAS_SIN1QER/SALIDAS_PERC_SERIES/REGION7/"


library(lubridate)
library(dplyr)
#library(modeest)


data <- read.table('REGION_7.data', sep = "|", header = T)
data<-data.frame(data)
data<-na.omit(data)
data$Fecha<-as.POSIXct(data$Fecha, format='%Y-%m-%d')
data$valor<-as.numeric(data$valor)
data<- data[-which(data$valor == 0), c(1,2,3,4,5)]
desv<-sd(data$valor)
promedio<-mean(data$valor)
r<-(desv/promedio)
percentil<-quantile(data$valor, c(0.25,0.75))
data_out<-data.frame(t(percentil), round(r,4))
colnames(data_out)<-c('25%','75%','R')


write.table(data_out, file.path(ruta_in, paste0('REGION_7_PERCENTIL.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_in, paste0('REGION_7_PERCENTIL.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_in, paste0('REGION_7_PERCENTIL.data')))


