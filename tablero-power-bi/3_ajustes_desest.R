rm(list = ls())

library(data.table)     # manipular grandes conjuntos de datos
library(lubridate)      # manipular fechas
library(openxlsx)       # leer archivos .xlsx  
library(googlesheets4)
library(plyr)
library(tidyverse)
library(fst)
library(hrbrthemes)
library(viridis)
library(Mectritas)
library(ggplot2)
library(RJDemetra)
library(zoo)

# Anoto la fecha a la que voy a deflactar. Recordar que es siempre el ultimo mes QUE SE PUBLICA
fecha <- '2023-08-01'

# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Empleo_joven/")
setwd(ruta)

# Seteamos ruta de bases
ruta_bases <- "C:/Users/Usuario/Documents/Bases CEP XXI/"

# Seteamos la ruta de las mectras
ruta_mectras <- "C:/Users/Usuario/Documents/Bases CEP XXI/MECTRA/Mectra FST"

# Voy a abrir las bases que quiero desestacionalizar


#variables <- c('puestos', 'empleadoras' , 'share_mujer', 'Remu_mediana', 'Remu_media')

data_grupo <- read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='grupo', cols= c(1:3, 8:11), detectDates=T)
setDT(data_grupo)
#Deflacto a precios del ultimo mes disponible
data_grupo <- data_grupo %>% deflactanator(fecha, variables_deflactar = c('Remu_media', 'Remu_mediana'), variable_mes = 'mes', pisar_datos = T)

data_provincia<- read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='provincia', cols= c(1:4, 9:12), detectDates=T)
setDT(data_provincia)
#Deflacto a precios del ultimo mes disponible
data_provincia <- data_provincia %>% deflactanator(fecha, variables_deflactar = c('Remu_media', 'Remu_mediana'), variable_mes = 'mes', pisar_datos = T)

data_letra<- read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='letra', cols= c(1:5, 10:13), detectDates=T)
setDT(data_letra)
#Deflacto a precios del ultimo mes disponible
data_letra <- data_letra %>% deflactanator(fecha, variables_deflactar = c('Remu_media', 'Remu_mediana'), variable_mes = 'mes', pisar_datos = T)

data_provincia_letra<- read.xlsx(paste0(ruta, "serie_completa_v5.xlsx"), sheet='provincia_letra', cols= c(1:6, 11:14), detectDates=T)
setDT(data_provincia_letra)
#Deflacto a precios del ultimo mes disponible
data_provincia_letra <- data_provincia_letra %>% deflactanator(fecha, variables_deflactar = c('Remu_media', 'Remu_mediana'), variable_mes = 'mes', pisar_datos = T)


# Vuelvo a llamar las variables por su nombre normal, sin aclaraciones de cons,
# para que pueda correr el codigo mas facilmente

#setnames(data_grupo, 'Remu_media_cons', 'Remu_media')
#setnames(data_grupo, 'Remu_mediana_cons', 'Remu_mediana')

#setnames(data_provincia, 'Remu_media_cons', 'Remu_media')
#setnames(data_provincia, 'Remu_mediana_cons', 'Remu_mediana')

#setnames(data_letra, 'Remu_media_cons', 'Remu_media')
#setnames(data_letra, 'Remu_mediana_cons', 'Remu_mediana')

#setnames(data_provincia_letra, 'Remu_media_cons', 'Remu_media')
#setnames(data_provincia_letra, 'Remu_mediana_cons', 'Remu_mediana')

# Elimino las filas con NA en data_letra y data_provincia_letra
data_letra <- data_letra[letra != 'T' & letra != 'U']
data_provincia_letra <- data_provincia_letra[letra != 'T' & letra != 'U']


# Veamos como debe quedar la base para poder usarla bien

#objetivo <- fread(r'(C:\Users\Usuario\Documents\Cep Pedidos\Empleo_joven\ajustes_powerbi\provincia_letra_TOTALES_sinNA.csv)')
#objetivo$V1 <- NULL
#objetivo <- objetivo[mes=='2007-01-01']
# Faltaria agregar las filas por total general del grupo

# Ahora reemplazo todas las series con sus versiones desestacionalizadas. Llamo primero algunas listas

variables <- c('Remu_mediana', 'Remu_media')
universos <- c('Total general', 'Total empresas', 'Privado')
letras <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
            'P', 'Q', 'R', 'S')
provs <- unique(data_provincia$provincia)

#1. Desestacionalizo primero para el grupo

data_grupo_full <- data.table()

for(u in 1:length(universos)){
  
  # Lo primero es alimentar la lista con las bases enteras
  
  tmp <- data_grupo %>% filter(Grupo == universos[u])
  setDT(tmp)
  tmp_sa <- tmp %>% select( -c(Remu_media, Remu_mediana))
  
  for(i in 1:2){
    # Pasamos a time series. Usamos zoo para poder identificar los meses que no tienen datos y ajustarlos
    ts_sa <- read.zoo(select(tmp, c(mes, variables[i])), FUN = as.yearmon, aggregate = function(x) tail(x, 1))
    ts_sa <- as.ts(ts_sa)
    #ts_sa <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
    #Descomponemos con RJDemetra
    ts_sa <- RJDemetra::x13(ts_sa)
    # Llevamos el resultado a dataframe
    ts_sa <- data.frame(.preformat.ts(ts_sa$final$series), stringsAsFactors = FALSE)
    # Pasar el nombre de las rows (fecha) a variable
    ts_sa <- tibble::rownames_to_column(ts_sa, var = 'mes')
    # Damos formato a la fecha
    ts_sa$mes <- lubridate::my(ts_sa$mes)
    # Nos quedamos sólo con la tendencia de la serie
    setDT(ts_sa)
    ts_sa <- ts_sa[,.(mes, sa)]
    # Mergeamos con la serie nueva que solo va a incluir las tendencias
    tmp_sa <- merge(tmp_sa, ts_sa, by='mes')
    ### Nos quedamos solo con la tendencia y la renombramos como la comun
    setnames(tmp_sa, "sa", variables[i])
    rm(ts_sa)
  }
  
  data_grupo_full <- rbind(data_grupo_full, tmp_sa)
  rm(tmp, tmp_sa)
}

# con respecto a data_grupo se pierden 9 observaciones (dic-2022, ene-2023 y feb-2023 x 3)
# Le agrego tres columnas que voy a llamar igual para luego poder hacer el bind
data_grupo_full <- data_grupo_full[, provincia:= 'TOTAL']
data_grupo_full <- data_grupo_full[, letra:= 'TOTAL']
data_grupo_full <- data_grupo_full[, letra_desc:= 'TOTAL']


# 2. Vamos ahora con la desestacionalizacion de la info por letra
data_letra_full <- data.table()


for(u in 1:length(universos)){
  for (l in 1:length(letras)){
    
    # Lo primero es alimentar la lista con las bases enteras
    
    tmp <- data_letra %>% filter(Grupo == universos[u] & letra == letras[l])
    setDT(tmp)
    tmp_sa <- tmp %>% select( -c(Remu_media, Remu_mediana))
    
    for(i in 1:2){
      # Pasamos a time series. Usamos zoo para poder identificar los meses que no tienen datos y ajustarlos
      ts_sa <- read.zoo(select(tmp, c(mes, variables[i])), FUN = as.yearmon, aggregate = function(x) tail(x, 1))
      ts_sa <- as.ts(ts_sa)
      #ts_sa <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
      #Descomponemos con RJDemetra
      ts_sa <- RJDemetra::x13(ts_sa)
      # Llevamos el resultado a dataframe
      ts_sa <- data.frame(.preformat.ts(ts_sa$final$series), stringsAsFactors = FALSE)
      # Pasar el nombre de las rows (fecha) a variable
      ts_sa <- tibble::rownames_to_column(ts_sa, var = 'mes')
      # Damos formato a la fecha
      ts_sa$mes <- lubridate::my(ts_sa$mes)
      # Nos quedamos sólo con la tendencia de la serie
      setDT(ts_sa)
      ts_sa <- ts_sa[,.(mes, sa)]
      # Mergeamos con la serie nueva que solo va a incluir las tendencias
      tmp_sa <- merge(tmp_sa, ts_sa, by='mes')
      ### Nos quedamos solo con la tendencia y la renombramos como la comun
      setnames(tmp_sa, "sa", variables[i])
      rm(ts_sa)
    }
    
    data_letra_full <- rbind(data_letra_full, tmp_sa)
    rm(tmp, tmp_sa)
  }
}

# con respecto a data_grupo se pierden 171 observaciones (dic-2022, ene-2023 y feb-2023 x 3 x 19)
# Le agrego una columna que voy a llamar igual para luego poder hacer el bind
data_letra_full <- data_letra_full[, provincia:= 'TOTAL PROVINCIAS']



# 3. Vamos ahora con la desestacionalizacion de la info por provincia
data_provincia_full <- data.table()


for(u in 1:length(universos)){
  for (p in 1:length(provs)){
    
    # Lo primero es alimentar la lista con las bases enteras
    
    tmp <- data_provincia %>% filter(Grupo == universos[u] & provincia == provs[p])
    setDT(tmp)
    tmp_sa <- tmp %>% select( -c(Remu_media, Remu_mediana))
    
    for(i in 1:2){
      # Pasamos a time series. Usamos zoo para poder identificar los meses que no tienen datos y ajustarlos
      ts_sa <- read.zoo(select(tmp, c(mes, variables[i])), FUN = as.yearmon, aggregate = function(x) tail(x, 1))
      ts_sa <- as.ts(ts_sa)
      #ts_sa <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
      #Descomponemos con RJDemetra
      ts_sa <- RJDemetra::x13(ts_sa)
      # Llevamos el resultado a dataframe
      ts_sa <- data.frame(.preformat.ts(ts_sa$final$series), stringsAsFactors = FALSE)
      # Pasar el nombre de las rows (fecha) a variable
      ts_sa <- tibble::rownames_to_column(ts_sa, var = 'mes')
      # Damos formato a la fecha
      ts_sa$mes <- lubridate::my(ts_sa$mes)
      # Nos quedamos sólo con la tendencia de la serie
      setDT(ts_sa)
      ts_sa <- ts_sa[,.(mes, sa)]
      # Mergeamos con la serie nueva que solo va a incluir las tendencias
      tmp_sa <- merge(tmp_sa, ts_sa, by='mes')
      ### Nos quedamos solo con la tendencia y la renombramos como la comun
      setnames(tmp_sa, "sa", variables[i])
      rm(ts_sa)
    }
    
    data_provincia_full <- rbind(data_provincia_full, tmp_sa)
    rm(tmp, tmp_sa)
  }
}

# con respecto a data_grupo se pierden 216 observaciones (dic-2022, ene-2023 y feb-2023 x 3 x 24)
# Le agrego dos columnas que voy a llamar igual para luego poder hacer el bind
data_provincia_full <- data_provincia_full[, letra:= 'TOTAL SECTORES']
data_provincia_full <- data_provincia_full[, letra_desc:= 'TOTAL SECTORES']



#4. Ahora falta hacer la desestacionalizacion por provincia y letra

data_provincia_letra_full <- data.table()


for(u in 1:length(universos)){
  for (p in 1:length(provs)){
    for (l in 1:length(letras)){
      
      # Lo primero es alimentar la lista con las bases enteras
      
      tmp <- data_provincia_letra %>% filter(Grupo == universos[u] & provincia == provs[p] & letra == letras[l])
      setDT(tmp)
      tmp_sa <- tmp %>% select( -c(Remu_media, Remu_mediana))
      
      for(i in 1:2){
        # Pasamos a time series. Usamos zoo para poder identificar los meses que no tienen datos y ajustarlos
        ts_sa <- read.zoo(select(tmp, c(mes, variables[i])), FUN = as.yearmon, aggregate = function(x) tail(x, 1))
        ts_sa <- as.ts(ts_sa)
        #ts_sa <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
        #Descomponemos con RJDemetra
        ts_sa <- RJDemetra::x13(ts_sa)
        # Llevamos el resultado a dataframe
        ts_sa <- data.frame(.preformat.ts(ts_sa$final$series), stringsAsFactors = FALSE)
        # Pasar el nombre de las rows (fecha) a variable
        ts_sa <- tibble::rownames_to_column(ts_sa, var = 'mes')
        # Damos formato a la fecha
        ts_sa$mes <- lubridate::my(ts_sa$mes)
        # Nos quedamos sólo con la tendencia de la serie
        setDT(ts_sa)
        ts_sa <- ts_sa[,.(mes, sa)]
        # Mergeamos con la serie nueva que solo va a incluir las tendencias
        tmp_sa <- merge(tmp_sa, ts_sa, by='mes')
        ### Nos quedamos solo con la tendencia y la renombramos como la comun
        setnames(tmp_sa, "sa", variables[i])
        rm(ts_sa)
      }
      
      data_provincia_letra_full <- rbind(data_provincia_letra_full, tmp_sa)
      rm(tmp, tmp_sa)
    }
  }
}

# Ahora junto todo en un solo archivo
data_total <- rbind(data_grupo_full, data_letra_full, data_provincia_full, data_provincia_letra_full)

write.csv(data_total, 'Publicacion DA/provincia_letra_totales.csv', row.names=F)

