setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/TEST_UNIDOS/REPORTE/REGION1/")

rm(list = ls(all = TRUE))

#ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/BrilloSolarDiario_Regiones_May19_2021/REGION_1/"
ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/regiones_crudas/TEST_UNIDOS/REPORTE/REGION1/"


library(lubridate)
library(dplyr)
library(janitor)
#library(modeest)


data <- read.table('REGION_1.data', sep = "|", header = T)
data<-data.frame(data)
#data<-na.omit(data)
data$Fecha<-as.POSIXct(data$Fecha, format='%Y-%m-%d')
#data$valor<-as.numeric(data$valor)

reporte<-NULL

for (i in 1:dim(data)[1]) {
  i=1
  temp<-filter(data,Codigo==data$Codigo[i])
  new_data<-ifelse(temp[,c(4,5,6,7,8,9)]=='1QAT0','1QAT0',ifelse(temp[,c(4,5,6,7,8,9)]=='1QER0','1QER0'," "))
  new_data<-data.frame(new_data)
  data_out<-data.frame(temp$Codigo, temp$Fecha, temp$valor, new_data)
  remover<-new_data[!apply(new_data == " ", 1, all),]
  #data_out<- merge(data_out, remover, by=c('QC1', 'QC2AT', 'QC2ER', 'QC3', 'QC4', 'QC5'), all=T)
  #remover<-data_out[!apply(data_out == " ", 1, any),] #remueve las filas que estan vacias (" "), el 1 hace referencia a filas
  data_out<-data.frame(temp$Codigo, temp$Fecha, temp$valor, remover)
  
  #filtro<- filter(temp, (QC1=='1QAT0'|'1QER0')|(QC2AT=='1QAT0')|(QC2ER=='1QER0')|(QC3=='1QAT0'|'1QER0')
                  #|(QC4=='1QAT0'|'1QER0')|(QC5=='1QAT0'|'1QER0'))
  
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


