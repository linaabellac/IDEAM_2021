rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/SERIES_VELOCIDAD/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/PERSISTENCIA/"

library(lubridate)
library(dplyr)

variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables){
  #etiqueta<-variables[1]
  list_dir <- list.files(file.path(ruta_in, etiqueta), full.names = F)
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[1]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(ruta_in, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    data$filter_day <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha))
    #tz(data$Fecha) <- "America/Bogota"
  
    day_groups<- summarise(group_by(data, filter_day), num = n())
  #if(any(day_groups[,2]< 1)){
  #  day_groups<-data$Valor}
  
    if(any(day_groups[,2] < 16 )){
      day_groups <- day_groups[-which(day_groups[,2] < 16), ] }     
  
    if (dim(day_groups)[1] > 16){
      day_groups$Fecha <- as.POSIXct(day_groups$filter_day, format = '%Y-%m-%d')
      day_groups <- arrange(day_groups, Fecha)
      data_out <- cbind(0, 0, 0)
      for (i in 1:dim(day_groups)[1]) {
        temp <- filter(data, filter_day == as.character(day_groups[i, 1]))
        data_out <- rbind(data_out, cbind(day_groups$filter_day[i], as.double(mean(temp$Valor)),sd(temp$Valor)))
     }
    
      data_out <- data_out[-1,]
      colnames(data_out) <- c("Fecha", "Valor","desviacion")
      data_out <- data.frame(data_out)
      data_out$Fecha <- as.POSIXct(data_out$Fecha,  format = '%Y-%m-%d')
      data_out$Valor<-as.numeric(data_out$Valor)
      data_out$Valor<-round(data_out$Valor, 4)
      data_out$desviacion<-as.numeric(data_out$desviacion);data_out$desviacion <-round(data_out$desviacion, 4)
      
    
      #write.table(data_out, paste0(ruta_out, file), sep="|", row.names = FALSE, col.names = TRUE)
      write.table(data_out, file.path(ruta_out, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
      tx  <- readLines(file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
      tx2  <- gsub(pattern = '"', replace = '', x = tx)
      writeLines(tx2, con=file.path(ruta_out, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
    }
  
  }
}
