setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/VELOCIDAD Y DIRECCION VIENTO/CONTROLES/")
rm(list = ls(all = TRUE))


library(lubridate)
library(dplyr)
library(IdeamMeteo)
library(tidyr)

climatic_range_corr <- function(data, lim_min, lim_max){
  data_s <- 0
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i]
    max <- lim_max[i]
    
    # Datos Erroneos - Eliminados
    if (length(which(temp_data$Valor < min)) > 0) {
      ns <- which(temp_data$Valor < min)
    }
    if (length(which(temp_data$Valor > max)) > 0) {
      ns <- c(ns,which(temp_data$Valor > max))
    }
    if (length(ns) > 0){
      for (k in 1:length(ns)) {
        data_s <- c(data_s, which(data$Fecha == temp_data$Fecha[ns[k]]))
      }
    }
  }
  
  if (length(data_s[-1]) > 0) {
    return(data_s[-1])
  }
}


sigma_velocidad<-function(data, f, variable) {
  if (variable == 1){
    data$Fecha <- data$Fecha - as.difftime(7, format = "%X", units = "hours")
    data$filter_hour <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha), "-", hour(data$Fecha))
    data$filter_day <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha))
    
    hour_groups <- summarise(group_by(data, filter_hour), num = n())
    hour_groups <- hour_groups[-which(hour_groups[,2] < 4), ]
    hour_groups$Fecha <- as.POSIXct(hour_groups$filter_hour, format = '%Y-%m-%d-%H')
    hour_groups$filter_day <- paste0(year(hour_groups$Fecha), "-", month(hour_groups$Fecha), "-", day(hour_groups$Fecha))
    
    day_groups <- summarise(group_by(hour_groups, filter_day), num = n())
    day_groups <- day_groups[-which(day_groups[,2] < 17), ]
    day_groups <- arrange(day_groups, filter_day)
    
    data_out <- cbind(0, 0, 0)
    for (i in 1:dim(day_groups)[1]) {
      temp <- filter(data, filter_day == as.character(day_groups[i, 1]))
      data_out <- rbind(data_out, cbind(temp$filter_day[1], as.double(max(temp$Valor)), as.double(min(temp$Valor))))
    }
    colnames(data_out) <- c("Fecha", "Max", "Min")
    data_out <- data.frame(data_out[-1, ])
    data_out$Fecha <- paste0(as.POSIXct(data_out$Fecha,  format = '%Y-%m-%d'),  " 07:00:00")
    data_out <- arrange(data_out, Fecha)
    
  } else if (variable == 2){
    data$filter_day <- paste0(year(data$Fecha), "-", month(data$Fecha), "-", day(data$Fecha))
    
    day_groups <- summarise(group_by(data, filter_day), num = n())
    day_groups <- day_groups[-which(day_groups[,2] < 16), ]
    day_groups <- arrange(day_groups, filter_day)
    
    data_out <- cbind(0, 0, 0)
    for (i in 1:dim(day_groups)[1]) {
      temp <- filter(data, filter_day == as.character(day_groups[i, 1]))
      data_out <- rbind(data_out, cbind(temp$filter_day[1], as.double(max(temp$Valor)), as.double(min(temp$Valor))))
    }
    colnames(data_out) <- c("Fecha", "Max", "Min")
    data_out <- data.frame(data_out[-1, ])
    data_out$Fecha <- as.POSIXct(data_out$Fecha,  format = '%Y-%m-%d')
    data_out <- arrange(data_out, Fecha)
  }
  
  df <- data_out
  if (any(which(is.na(df$Max)))) {
    df <- df[-which(is.na(df$Max)), ]
  }
  
  umax <- mean(as.double(df$Max))
  umin <- mean(as.double(df$Min))
  smax <- sd(as.double(df$Max))
  smin <- sd(as.double(df$Min))
  
  lim_min <- umin - f * smin
  lim_max <- umax + f * smax
  
  if (any(c(which(data$Valor < lim_min), which(data$Valor > lim_max)))) {
    data_out1 <- c(which(data$Valor < lim_min), which(data$Valor > lim_max))
    return(data_out1)
  }
}


climatic_r_range<-function(data, lim_min, lim_max, R){
  data_s <- 0
  data$mes <- month(data$Fecha)
  
  for (i in 1:12){
    ns <- NULL
    temp_data <- filter(data, mes==i)
    min <- lim_min[i] - R * (lim_max[i]- lim_min[i])
    max <- lim_max[i] + R * (lim_max[i]- lim_min[i])
    
    # Datos Erroneos - Eliminados
    if (length(which(temp_data$Valor < min)) > 0) {
      ns <- which(temp_data$Valor < min)
    }
    if (length(which(temp_data$Valor > max)) > 0) {
      ns <- c(ns,which(temp_data$Valor > max))
    }
    if (length(ns) > 0){
      for (k in 1:length(ns)) {
        data_s <- c(data_s, which(data$Fecha == temp_data$Fecha[ns[k]]))
      }
    }
  }
  
  if (length(data_s[-1]) > 0) {
    return(data_s[-1])
  }
}


Datos <- file.path('CONTROL_PICOS')
datos_perc<-file.path('PERCENTILES_SERIES')
outPath <- file.path('CONTROLES_UNIDOS')

variables<-c('VV_10_MEDIA_H','VVAG_CON')

for (etiqueta in variables) {
  #etiqueta<-variables[2]
  list_dir <- list.files(file.path(Datos, etiqueta), full.names = F)
  list_dir_p<-list.files(file.path(datos_perc,etiqueta),full.names = F)
  
  for (file in list_dir){ 
    # Lectura Datos y Pruebas de validacion Genrales
    #file <-list_dir[10]
    code_station <-  substr(file, which(strsplit(file, "")[[1]]=="@") + 1, which(strsplit(file, "")[[1]]==".") - 1)
    data<- data.frame(read.table(file.path(Datos, etiqueta, file), sep = '|', header = T))
    data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
    
    
    if (any(list_dir_p==file)) {
      #data_c <- data.frame(read.table(file.path(inPathDatos, 'Complementacion', etiqueta, file), sep = '|', header = T))
      data_per<- data.frame(read.table(file.path(datos_perc, etiqueta, file), sep = '|', header = T))}
    
    QC1<-climatic_range_corr(data = data, lim_min = data_per$X1., lim_max = data_per$X99.8.)
    data$QC1<-'1QC0';data$QC1[QC1]<-'1QAT0'
    QC2<-sigma_velocidad(data = data, f=1.5, variable = 2)
    data$QC2<-'1QC0';data$QC2[QC2]<-'1QAT0'
    QC3<-climatic_r_range(data = data, lim_min = data_per$X25., lim_max = data_per$X75., R= 1.5)
    data$QC3<-'1QC0';data$QC3[QC3]<-'1QAT0'

    data_final<- data.frame(data$Fecha, data$Valor, data$QC1, data$QC2, data$QC3, data$estado) 
    colnames(data_final)<-c('Fecha', 'Valor', 'QC1', 'QC2','QC3','QC4')
    
    if (any(is.na(data_final$Valor))) {
      data_final[which(is.na(data_final$Valor)),c(3,4,5,6) ]<-NA 
    }
    
    data_final<- arrange(data_final, Fecha)
    
    write.table(data_final, file.path(outPath, paste0(etiqueta,'@',code_station, '.data')), sep="|", row.names = FALSE, col.names = TRUE)
    tx  <- readLines(file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    tx2  <- gsub(pattern = '"', replace = '', x = tx)
    writeLines(tx2, con=file.path(outPath, paste0(etiqueta,'@', code_station, '.data')))
    
    
    
  }
}

   
    