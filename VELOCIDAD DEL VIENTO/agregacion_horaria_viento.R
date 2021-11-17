rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/VIENTO FALTANTES/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/VIENTO AGREGADAS FALTANTES/"

library(lubridate)
library(dplyr)


list_dir <- list.files(ruta_in, full.names = F)

for (file in list_dir){
  file <- list_dir[1]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')

  #data$Fecha <- data$Fecha - as.difftime(7, format = "%X", units = "hours")
  data$filter_hour <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha), "-", hour(data$Fecha))
  #data$filter_day <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha))
  
  hour_groups <- summarise(group_by(data, filter_hour), num = n())
  hour_groups <- hour_groups[-which(hour_groups[,2] < 4), ]
  hour_groups$Fecha <- as.POSIXct(hour_groups$filter_hour, format = '%Y-%m-%d-%H')#'%Y-%m-%d-%H'
  #hour_groups$filter_day <- paste0(year(hour_groups$Fecha), "-", month(hour_groups$Fecha), "-", day(hour_groups$Fecha))
  
  if (dim(hour_groups)[1] > 4){
    hour_groups$Fecha <- as.POSIXct(paste0(hour_groups$filter_hour, format = '%Y-%m-%d-%H'))
    hour_groups<-arrange(hour_groups, Fecha)
    data_out <- cbind(0, 0)
    for (i in 1:dim(hour_groups)[1]) {
      temp <- filter(data, filter_hour == as.character(hour_groups[i, 1]))
      data_out <- rbind(data_out, cbind(hour_groups$filter_hour[i], as.double(mean(temp$Valor))))
    }
  
 
    data_out <- data_out[-1,]
    colnames(data_out) <- c("Fecha", "Valor")
    data_out <- data.frame(data_out)
    data_out$Fecha <- as.POSIXct(data_out$Fecha,  format = '%Y-%m-%d-%H')
    data_out <- arrange(data_out, Fecha)
    #data_out<-data_out[order(data_out$Fecha), ]
    
    
    #write.table(data_out, paste0(ruta_out, file), sep="|", row.names = FALSE, col.names = TRUE)
    write.table(data_out, file.path(ruta_out, paste0('VV_10_MEDIA_H@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(ruta_out, paste0('VV_10_MEDIA_H@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(ruta_out, paste0('VV_10_MEDIA_H@', code_station, '.data')))
    
    
    #tx  <- readLines(paste0(ruta_out, file))
    #tx <- (file.path(ruta_out , paste0('BSHG_TT_D@', code_station, '.data')))
    #tx2  <- gsub(pattern = '"', replace = '', x = tx)
    #writeLines(tx2, con=(file.path(ruta_out , paste0('BSHG_TT_D@', code_station, '.data'))))
    #writeLines(tx2, con=paste0(ruta_out, file))
  }
  
}
