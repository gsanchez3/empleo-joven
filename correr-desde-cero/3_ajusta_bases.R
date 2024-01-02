rm(list = ls())

library(data.table)     # manipular grandes conjuntos de datos
library(lubridate)      # manipular fechas
library(openxlsx)       # leer archivos .xlsx  
library(googlesheets4)
library(plyr)
library(tidyverse)
library(fst)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(Mectritas)
options(scipen=999) 


# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Empleo_joven/")
setwd(ruta)

# Seteamos ruta de bases
ruta_bases <- "C:/Users/Usuario/Documents/Bases CEP XXI/"

# Seteamos la ruta de las mectras
ruta_mectras <- "C:/Users/Usuario/Documents/Bases CEP XXI/MECTRA/Mectra FST"



### Ahora quiero calcular los puestos totales, por provincia y por letra. Por otro lado, los salarios promedios y medianos, y por
# otro el share de mujer

## Llamo los data tables para ir agregando
data_grupo <- setDT(read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='grupo', detectDates=T))
data_letra <- setDT(read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='letra', detectDates=T))
data_provincia <- setDT(read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='provincia', detectDates=T))
data_provincia_letra <- setDT(read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='provincia_letra', detectDates=T))
data_letra_clae2 <- setDT(read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='letra_clae2', detectDates=T))




#### por no haber datos o tener datos cero en algunas divisiones para los casos imputados quedaron un par de NAs en la base, voy a corregirlo un poco


ajusta_bases <- function(data){
  
  data <- data[, mujeres := fifelse(is.na(mujeres), 0, mujeres)] # Le pongo a todas las mujeres que tengan NA cero
  data <- data[, mujeres := fifelse(mujeres == 'Inf', 0, mujeres)]
  # luego elimino los casos donde puestos sea NA
  data <- data[!is.na(puestos)]
  # Con esto estaria todo
  
  return(data)
}

# Esto deberia hacerlo con una funcion pero no la encontre, luego buscarla. Listo mis bases

bases <- list()
bases[[1]] <- data_grupo
bases[[2]] <- data_letra
bases[[3]] <- data_provincia
bases[[4]] <- data_provincia_letra
bases[[5]] <- data_letra_clae2
#bases[[6]] <- data_prov_letra_clae2


# Usando for-loop:
for ( f in 1:length(bases)) {
  
  bases[[f]]  <-  ajusta_bases(bases[[f]])
}


# Con un assign paso estas bases ya limpias al environment

nombres <- c('grupo', 'letra', 'provincia', 'provincia_letra', 'letra_clae2')

for (f in 1:length(nombres)) {
  
  assign( paste0('data_', nombres[f]) , bases[[f]] )
  
}



# Guardamos un excel con lo trabajado
wb <- createWorkbook()
addWorksheet(wb, "grupo")
addWorksheet(wb, "provincia")
addWorksheet(wb, "letra")
addWorksheet(wb, "letra_clae2")
addWorksheet(wb, "provincia_letra")

# Le escribimos lo solicitado en cada una de las pestañas
writeDataTable(wb,
               sheet = "grupo",
               data_grupo)
writeDataTable(wb,
               sheet = "provincia",
               data_provincia)
writeDataTable(wb,
               sheet = "letra",
               data_letra)
writeDataTable(wb,
               sheet = "letra_clae2",
               data_letra_clae2)
writeDataTable(wb,
               sheet = "provincia_letra",
               data_provincia_letra)

# Exportamos
saveWorkbook(wb,
             paste0(ruta,"serie_completa_v5.xlsx"), # lo voy a llamar v5 para diferenciarlo de la 4 al contar con mas informacion
             overwrite = TRUE)


