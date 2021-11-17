rm(list = ls(all = TRUE))

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROLES/DVAG_CON/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/DIRECCION_CONTROLES/DIRECCION_DIARIAS/"

library(lubridate)
library(dplyr)


list_dir <- list.files(ruta_in, full.names = F)

for (file in list_dir){
  #file <- list_dir[60]
  code_station <- substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
  data <- data.frame(read.table(paste0(ruta_in, file), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  
  
  #data$seno<-sin(2*pi*(data$Valor/360))
  #data$coseno<-cos(2*pi*(data$Valor/360))
  #data_1<-data.frame(data$Fecha, data$Valor, seno, coseno)
  #colnames(data_1)<-c('Fecha', 'Valor', 'seno','coseno' )
  data$seno<-sin(round(data$Valor,5))
  data$coseno<-cos(round(data$Valor,5))
  
  data$filter_day <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha))
  #tz(data$Fecha) <- "America/Bogota"
  
  day_groups<- summarise(group_by(data, filter_day), num = n())
  
  if(any(day_groups[,2] < 16 )){
    day_groups <- day_groups[-which(day_groups[,2] < 16), ] }     
  
  if (dim(day_groups)[1] > 16){
    day_groups$Fecha <- as.Date(day_groups$filter_day, format = '%Y-%m-%d')
    day_groups <- arrange(day_groups, Fecha)
    data_out <- cbind(0, 0, 0, 0)
    for (i in 1:dim(day_groups)[1]) {
      temp <- filter(data, filter_day == as.character(day_groups[i, 1]))
      data_out <- rbind(data_out, cbind(t(day_groups$filter_day[i]), as.double(mean(temp$seno,na.rm = T)),as.double(mean(temp$coseno, na.rm = T)),sum(temp$Estado)))
    }
                                            
    
    
    data_out <- data_out[-1,]
    colnames(data_out) <- c("Fecha", "seno", 'coseno','Estado')
    data_out <- data.frame(data_out)
    data_out$Fecha <- as.Date(data_out$Fecha,  format = '%Y-%m-%d')
    data_out <- arrange(data_out, Fecha)
    data_out$seno<-as.numeric(data_out$seno)
    data_out$coseno<-as.numeric(data_out$coseno)
    
    data_out$prom_uv<-(180/pi*atan2(data_out$coseno,data_out$seno))
    data_out$prom_uv_c<-ifelse(data_out$prom_uv < 0, data_out$prom_uv + 360, data_out$prom_uv)
    
    data_final<-data.frame(data_out$Fecha, round(data_out$prom_uv_c,3), data_out$Estado)
    colnames(data_final)<-c('Fecha', 'Valor','Estado')
    
    
    
    #write.table(data_out, paste0(ruta_out, file), sep="|", row.names = FALSE, col.names = TRUE)
    write.table(data_final, file.path(ruta_out, paste0('DVAG_MEDIA_D@', code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(ruta_out, paste0('DVAG_MEDIA_D@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(ruta_out, paste0('DVAG_MEDIA_D@', code_station, '.data')))
    
    
  }
  
}
