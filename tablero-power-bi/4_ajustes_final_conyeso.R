rm(list = ls())

library(data.table)     # manipular grandes conjuntos de datos
library(lubridate)      # manipular fechas
library(openxlsx)       # leer archivos .xlsx  
library(plyr)
library(tidyverse)
library(fst)
library(ggplot2)
library(Mectritas)
library(DatosAbiertosCEP)
options(scipen=999) 

# Vamos a generar las series de datos para datos abiertos

# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Empleo_joven/")
setwd(ruta)

# Seteamos ruta de bases
ruta_bases <- "C:/Users/Usuario/Documents/Bases CEP XXI/"


# Necesito simplificar la base de clae2_prov_universos, simplificarla y mergearla con las series desestacionalizadas


data_prov_letra_clae2 <- setDT(fread(paste0(ruta, "clae2_prov_universos.csv")))
data_prov_letra_clae2 <- data_prov_letra_clae2[,mes:=as.Date(mes)]

bases_desest <- setDT(fread(paste0(ruta, 'Publicacion DA/provincia_letra_totales.csv')))
bases_desest <- bases_desest[,mes:=as.Date(mes)]


# Cortemos al minimo las columnas y eliminemos las letras T, U y los NA

data_prov_letra_clae2 <- data_prov_letra_clae2[,. (Grupo, provincia, letra, clae2, mes, puestos, mujeres, Remu_media, puestos_totales)]

# Paso las filas con NA, T o U en data_prov_letra_clae2 a un clae de Otros. Ademas para el caso de los privados paso la letra O
# tambien a otros

data_prov_letra_clae2 <- data_prov_letra_clae2 %>%
  mutate(letra = case_when((is.na(letra) | letra == 'T' | letra == 'U' | 
                              (letra == 'O' & (Grupo == 'Privado' | Grupo == 'Total empresas' ))) ~ "Z",
                           TRUE ~ letra))



data_prov_letra_clae2 <- data_prov_letra_clae2 %>%
  mutate(clae2 = case_when((is.na(clae2) | clae2 == '97' | clae2 == '99' |
                              (clae2 == '84' & (Grupo == 'Privado' | Grupo == 'Total empresas' ))) ~ 999,
                           TRUE ~ clae2))


# Tengo que colapsar para que por grupo, provincia y clae2 tenga solo una observacion

data_prov_letra_clae2 <- data_prov_letra_clae2[,. (puestos = sum(puestos, na.rm=T),
                                                   mujeres = sum(mujeres, na.rm=T),
                                                   puestos_totales = sum(puestos_totales, na.rm=T),
                                                   Remu_media = round(weighted.mean(Remu_media, w=puestos))), by = c('Grupo', 'provincia',
                                                                    'letra', 'clae2', 'mes')]

### Elimino de bases_desest las totales

bases_desest <- bases_desest[!(provincia == 'TOTAL' | provincia == 'TOTAL PROVINCIAS' | letra== 'TOTAL'| letra== 'TOTAL SECTORES')]

# Me quedo con las columnas que suman info

setnames(bases_desest, 'Remu_media', 'Remu_media_desest')
bases_desest <- bases_desest[, Remu_media_desest := round(Remu_media_desest)]
bases_desest <- bases_desest[,. (Grupo, provincia, letra, mes, Remu_media_desest)]
bases_desest <- bases_desest[!(Grupo== 'Privado' & letra== 'O')] # Saco 

# Ahora si mergeo 

data_final <- merge(data_prov_letra_clae2, bases_desest, by=c('Grupo', 'provincia', 'letra', 'mes'), all.x=T)
#data_final <- data_final %>% select(-c('letra')) # vemos si sacar o no letra

# lo exportamos
write.csv(data_final, 'fact_empleojoven.csv', row.names=F) 
