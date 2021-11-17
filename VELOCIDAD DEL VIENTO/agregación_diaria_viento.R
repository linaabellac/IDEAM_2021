rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/Velocidad_AGO_10_2021/viento_convencionales/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/Velocidad_AGO_10_2021/convencionales_agregadas/"

library(lubridate)
library(dplyr)


list_dir <- list.files(ruta_in, full.names = F)

for (file in list_dir){
  #file <- list_dir[31]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  #data$Fecha <- data$Fecha - as.difftime(7, format = "%X", units = "hours")
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
    data_out <- cbind(0, 0)
    for (i in 1:dim(day_groups)[1]) {
      temp <- filter(data, filter_day == as.character(day_groups[i, 1]))
      data_out <- rbind(data_out, cbind(day_groups$filter_day[i], as.double(mean(temp$Valor))))
    }
    
    data_out <- data_out[-1,]
    colnames(data_out) <- c("Fecha", "Valor")
    data_out <- data.frame(data_out)
    data_out$Fecha <- as.POSIXct(data_out$Fecha,  format = '%Y-%m-%d')
    data_out$Valor<-as.numeric(data_out$Valor)
    data_out$Valor<-round(data_out$Valor, 4)
    
    #write.table(data_out, paste0(ruta_out, file), sep="|", row.names = FALSE, col.names = TRUE)
    write.table(data_out, file.path(ruta_out, paste0('VVAG_MEDIA_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(ruta_out, paste0('VVAG_MEDIA_D@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(ruta_out, paste0('VVAG_MEDIA_D@', code_station, '.data')))
    
    
    #tx  <- readLines(paste0(ruta_out, file))
    #tx <- (file.path(ruta_out , paste0('BSHG_TT_D@', code_station, '.data')))
    #tx2  <- gsub(pattern = '"', replace = '', x = tx)
    #writeLines(tx2, con=(file.path(ruta_out , paste0('BSHG_TT_D@', code_station, '.data'))))
    #writeLines(tx2, con=paste0(ruta_out, file))
  }
  
}
