setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/SALIDAS_SIN1QER/SALIDAS YEAR/REGION7/")

rm(list = ls(all = TRUE))

#ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/BrilloSolarDiario_Regiones_May19_2021/REGION_1/"
ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/Brillo_Solar_por_Regiones/SALIDAS_SIN1QER/SALIDAS YEAR/REGION7/"


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
anuales<-data$año
duplicados<-which(duplicated(anuales))
eliminar<-anuales[-duplicados]
anuales<-eliminar

for (i in 1:length(anuales)){
  #i=2
  temp<-filter(data,año==anuales[i])
  desv<-sd(temp$valor)
  promedio<-mean(temp$valor)
  r<-(desv/promedio)
  percentil<-rbind(percentil,cbind(anuales[i],t(quantile(temp$valor,c(0.25,0.75))),round(r,4)))
  
}


data_out<-data.frame(percentil)
#mediana<- apply(data_out, 2, median)
prom<-apply(data_out, 2, mean)
df<-data.frame(t(round(prom,4)))
colnames(df)<-c('año','25%', '75%', 'R')

write.table(df, file.path(ruta_in, paste0('REGION7_PERC_ANUAL.data')), sep="|", row.names = FALSE, col.names = TRUE)
tx  <- readLines(file.path(ruta_in, paste0('REGION7_PERC_ANUAL.data')))
tx2  <- gsub(pattern = '"', replace = '', x = tx)
writeLines(tx2, con=file.path(ruta_in, paste0('REGION7_PERC_ANUAL.data')))


