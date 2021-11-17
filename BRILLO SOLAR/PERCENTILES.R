setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/SALIDAS POR MESES_/REGION7/")

rm(list = ls(all = TRUE))

#ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/BrilloSolarDiario_Regiones_May19_2021/REGION_1/"
ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/SALIDAS POR MESES_/REGION7/"


library(lubridate)
library(dplyr)
#library(modeest)


data <- read.table('REGION_7.data', sep = "|", header = T)
data<-data.frame(data)
data<-na.omit(data)
data$Fecha<-as.POSIXct(data$Fecha, format='%Y-%m-%d')
data$valor<-as.numeric(data$valor)
data<- data[-which(data$valor == 0), c(1,2,3,4,5,6)]

percentil<-NULL

for (i in 1:12) {
  #i=1
  temp<-filter(data,mes==i)
  desv<-sd(temp$valor)
  promedio<-mean(temp$valor)
  r<-(desv/promedio)
  percentil<-rbind(percentil,cbind(i,t(quantile(temp$valor,c(0.01,0.03,0.25,0.75,0.97,0.99,0.997,0.999))),round(r,4)))
  
}

data_out<-data.frame(percentil)
colnames(data_out)<-c('mes','1%','3%','25%', '75%', '97%','99%', '99.7%', '99.9%','R')

write.table(data_out, file.path(ruta_in, paste0('REGION_7_PERCENTIL.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_in, paste0('REGION_7_PERCENTIL.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_in, paste0('REGION_7_PERCENTIL.data')))


