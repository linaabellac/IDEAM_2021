setwd("C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/QC_BRILLO/QC_BRILLO")
rm(list = ls(all = TRUE))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                         Librerias y complementos                  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(IdeamMeteo)
library("readxl")
library("lubridate")
library("zoo")
library("dplyr")
library("writexl")
library("insol")
library(timeDate)
library(openxlsx)
library(tidyr)

ruta_in <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/Nubosidad_Jose_Mar24_2021/Nubosidad_Jose_Mar24_2021/Nubosidad_agregada/"
ruta_out <- "C:/Users/Lina Maria Abella C/Documents/TRABAJO IDEAM/Nubosidad_Jose_Mar24_2021/Nubosidad_Jose_Mar24_2021/prueba_julianos/"


prueba<-read.table(file.path(ruta_in, "NB_MEDIA_D@11035010.data"), sep = '|', header=T)
prueba$Fecha <-as.POSIXct(prueba$Fecha,  format = '%Y-%m-%d')
tz(prueba$Fecha) <- "America/Bogota"
dia_jul<- JD(prueba$Fecha, inverse = F)
#dia_jul_ <-JDymd(prueba$Fecha)

time<-dayOfYear(prueba$Fecha)





dia_julian<-julian.POSIXt(prueba$Fecha, origin = as.POSIXct(prueba$Fecha[1], format = '%Y-%m-%d', tz="America/Bogota"))

#SIRVE PRUEBA_2 PARA JULIANOS
prueba_2<- strftime(prueba$Fecha, format = "%j")
prueba_2<-as.integer(prueba_2)
df<- data.frame(prueba$Fecha, prueba_2)
 