setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROL_CONV")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
library(modeest)


shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

Datos <- file.path('DIRECCION_CONVENCIONALES')
outPath <- file.path('PICOS EXTREMOS')

list_dir <- list.files(Datos, full.names = F)

for (file in list_dir){
  #file <- list_dir[1]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(file.path(Datos, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  data$valor_rad<-round(data$Valor*pi/180,4) 
  data$mes<-month(data$Fecha)
  data<-arrange(data, mes)

  dif<-data %>%
    mutate (diff = data$valor_rad - lag(data$valor_rad, default = first(data$valor_rad)))
  
  dif$cos<-round(1-cos(dif$diff),4)
  
  df<-data.frame(data$Fecha, data$Valor, data$valor_rad, data$mes,dif$cos); colnames(df)=c('Fecha', 'Valor', 'Valor_rad', 'mes','dif_cos')
  
  df$corr<-shift(df$dif_cos, 1)
  
  
  write.table(df, file.path(outPath, paste0('DVAG_CON@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
  tx  <- readLines(file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  tx2  <- gsub(pattern = '"', replace = '', x = tx)
  writeLines(tx2, con=file.path(outPath, paste0('DVAG_CON@', code_station, '.data')))
  
  
}
  
  
  