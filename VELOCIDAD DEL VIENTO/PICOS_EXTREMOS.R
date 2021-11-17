rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/SERIES_VELOCIDAD/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/PICOS_EXT/"

library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)
#library(modeest)

#list_dir <- list.files(ruta_in, full.names = F)
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}


#data_out<- NULL
variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables){
  etiqueta<-variables[2]
  list_dir <- list.files(file.path(ruta_in, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    file <-list_dir[4]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(ruta_in, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    
    faltantes<-time_na(data$Fecha); colnames(faltantes)='Fecha'
    data <- merge(data, faltantes, by = 'Fecha', all.x = TRUE, all.y = TRUE)
    
    data$mes<-month(data$Fecha)
    data<-arrange(data, mes)
    
    dif<-data %>%
      mutate(diff = data$Valor - lag(data$Valor, default = first(data$Valor)))
    
    dif$diff<-abs(dif$diff)
    
    df<-data.frame(data$Fecha, data$Valor, data$mes, dif$diff); colnames(df)=c('Fecha', 'Valor', 'mes', 'dif')
    
    df$corr<-shift(df$dif, 1)
    df$dif<-ifelse(is.na(df$dif),0,df$dif)
    df$corr<-ifelse(is.na(df$corr),0,df$corr)
    
    round(df$Valor,4) ;round(df$dif,4); round(df$corr,4)
    
    write.table(df, file.path(ruta_out, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
}
