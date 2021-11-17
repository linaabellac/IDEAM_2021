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

library(openxlsx)
library(tidyr)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#           Definicion de directorios y lectura de archivos         #
#                             de configuracion                      #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Definición directorios 
inPathCatalogos <- file.path("Catalogos")
inPathDatos <- file.path("..", "..","Datos", "S_diarias", "PTPM_CON")
outPath  <- file.path("Output", "Precipitacion", "PTPM_CON")



# Catalogo 
catalogo <- read_excel(file.path(inPathCatalogos, "Estaciones_Prec_Convencionales.xlsx"))

# Umbrales
Umbrales_P <- read.table(file.path(inPathClima, 'Reporte_Umbrales_PTPM_CON.txt'), sep = ';', header = T)
Umbrales_O <- read.table(file.path(inPathClima, 'Umbrales_Ceros_PTPM_CON.txt'), sep = ';', header = T)

# # ONI
# oni <- read_excel(file.path(inPathComplementos, "ONI.xlsx"))
# oni$filtro <- paste0(oni$AÑO, '-', oni$MES)
series_malas <- NULL 
reporte_s <- NULL
reporte_e <- NULL

for (i in 1:dim(catalogo)[1]){
  # Lectura Datos y Pruebas de validacion Genrales
  code_station <- catalogo$CODIGO[i]
  type_variable <- 'PTPM_CON'
  region <- catalogo$`No. Region`[i]
  
  
  data <- data.frame(read.table(file.path(inPathDatos, paste0('PTPM_CON@', code_station, '.data')), sep = "|", header = T))
  data$Fecha <- as.POSIXct(data$Fecha,  format = '%Y-%m-%d %H:%M:%S')
  data$Fecha <- make_datetime(year = year(data$Fecha), month = month(data$Fecha), 
                              day = day(data$Fecha), hour = hour(data$Fecha), min = minute(data$Fecha))
  tz(data$Fecha) <- "America/Bogota"
  data <- duplicated_verif_con(data)
  
  if (dim(data)[1] < 365) {series_malas <- c(series_malas, i) ; next}
  # Definicion de Umbrales
  umbral_p <- filter(Umbrales_P, Region == region)
  umbral_0 <- filter(Umbrales_O, Region == region)
  
  ### Aplicacion de los controles de calidad
  ne <- NULL
  ns <- NULL
  
  # test de pruebas de rango fijo 
  ns_1 <- sensor_range(data = data, lim_min = quantile(data, 0.01), lim_max = quantile(data,0.99))
  ne_1 <- sensor_range(data = data, lim_min = quantile(data, 0.3), lim_max = quantile(data,0.997))
  if (is.null(ns_1) == F) { ns <- c(ns, ns_1) } 
  if (is.null(ne_1) == F) { ne <- c(ne, ne_1) }
  
  # test de boxplot 
  ns_2 <- boxplot_test(data)
  
  
  # Test de limites climatologicos
  ns_1 <- climatic_range(data = data, lim_min = umbral_p$P0.3, lim_max = umbral_p$P99)
  ne_1 <- climatic_range(data = data, lim_min = umbral_p$P0.3, lim_max = umbral_p$P99.7)
  if (is.null(ns_1) == F) { ns <- c(ns, ns_1) } 
  if (is.null(ne_1) == F) { ne <- c(ne, ne_1) }
  
  # Test de outliers 
  # Distribucion Mensual y Anuel
  ns_2 <- test.outliers(data, type_variable)
  if (is.null(ns_2) == F) {
    ns <- c(ns, ns_2)
  }
  
  # Outliers de la serie
  Rs <- c(6, 4, 4, 4, 4, 4, 4) ; names(Rs) <- c("PTPM_CON", "TMN_CON", "TMX_CON", "TSSM_MEDIA_D", "TMN_MEDIA_M", "TMX_MEDIA_M", "TSSM_MEDIA_M")
  ns_3 <- outliers(data$Valor, Rs[type_variable])
  if (length(ns_3) > 0) {
    ns <- c(ns, ns_3)
  } 
  
  # Valores Consecutivos Iguales
  columns <-  c("PTPM_CON", "TMN_CON", "TMN_MEDIA_M", "TMX_CON", "TMX_MEDIA_M", "TSSM_MEDIA_D", "TSSM_MEDIA_M")
  dt_s <- c(2, 3, 3, 3, 3, 3, 3) ; dt_e <- c(3, 4, 4, 4, 4, 4, 4)
  names(dt_s) <- columns ; names(dt_e) <- columns
  ns_4 <- Consecutive_equal_values(data, dt_s[type_variable])
  ne_2 <- Consecutive_equal_values(data, dt_e[type_variable])
  if (length(ns_4) > 0) {ns <- c(ns, ns_4)} 
  if (is.null(ne_2) == F) { ne <- c(ne, ne_2) }
  
  # Persistencia extrema (definida climatoligicamente) de dias sin precipitacion  
  ns_5 <- days_without_precipitation(data, umbral_0)
  if (length(ns_5) > 0) {
    ns <- c(ns, ns_5)
  } 
  
  ###################
  ns <- sort(ns)
  if (any(duplicated(ns))) {ns <- ns[-which(duplicated(ns))]}
  ne <- sort(ne)
  if (any(duplicated(ne))) {ne <- ne[-which(duplicated(ne))]}
  
  reporte_s <- rbind(reporte_s, c(code_station, dim(data)[1], length(ns_1), length(ns_2), length(ns_3), length(ns_4), length(ns_5), length(ns))) 
  reporte_e <- rbind(reporte_e, c(code_station, dim(data)[1], length(ne_1), length(ne_2), length(ne)))
  
  # # Reporte Serie Completa 
  # df <- cbind(code_station, type_variable, '1QC0', data) ; colnames(df) <- c('station', 'sensor', 'status',	'event_time',	'event_value')
  # # if (length(ne) > 0) {df$status[ne] <- '1QER0'}
  # if (length(ns) > 0) {df$status[ns] <- '1QER0'}
  # 
  # # Guardado de la serie y el reporte 
  # write.table(df, file.path(outPath,'Series_QC', paste0('PTPM_CON@', code_station, '.data')),
  #             row.names = F, col.names = T, sep = '|')
  # 
  # # Reporte Areas Operativas 
  # if (length(ns) > 0){
  #   df1 <- cbind(data[ns,], NA) ; colnames(df1) <- c('Fecha', 'Valor', 'Valor_Corregido')
  #   write.csv(df1, file.path(outPath,'Reportes_AO', area_op, paste0('PTPM_CON@', code_station, '.csv')),
  #             row.names = F)
  print(dim(catalogo)[1] - i)
  # }
}

colnames(reporte_s) <- c('codigo', 'longitud', 'control1', 'control2', 'control3', 'control4', 'control5', 'ns')
colnames(reporte_e) <- c('codigo', 'longitud', 'control1', 'control2', 'ne')

write.csv(reporte_s, file.path(outPath,'Reporte_PTPM_CON_S.csv'), row.names = F)
write.csv(reporte_e, file.path(outPath,'Reporte_PTPM_CON_E.csv'), row.names = F)