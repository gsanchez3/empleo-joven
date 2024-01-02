############ SCRIPT CON GRAFICOS

############################## SEGUNDA PARTE. QUE FLOREZCAN MIL GRAFICOS #########################################

############## Borramos todo y volvemos a levantar la base para trabajar

rm(list = ls())

library(viridis)
library(hrbrthemes)
library(ggplot2)
library(openxlsx)
library(tidyverse)

# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Empleo_joven/")
ruta_v4 <- paste0(ruta, "/v4/")
setwd(ruta)

##### Para graficar solo vamos a graficar las variables deflactadas,
##### por tanto de ahora en mas, eliminamos las columnas que estan a valores 
##### nominales y las reemplamos por su version deflactada. Pero en la base
##### original se mantiene toda la información

variables <- c('puestos', 'empleadoras' , 'share_mujer', 'Remu_mediana', 'Remu_media')

data_grupo<- read.xlsx(paste0(ruta, "serie_completa_v4.xlsx"), sheet='grupo', cols= c(1:6, 10:12), detectDates=T)
setDT(data_grupo)
data_provincia<- read.xlsx(paste0(ruta, "serie_completa_v4.xlsx"), sheet='provincia', cols= c(1:7, 11:13), detectDates=T)
setDT(data_provincia)
data_letra<- read.xlsx(paste0(ruta, "serie_completa_v4.xlsx"), sheet='letra', cols= c(1:8, 12:14), detectDates=T)
setDT(data_letra)
data_provincia_letra<- read.xlsx(paste0(ruta, "serie_completa_v4.xlsx"), sheet='provincia_letra', cols= c(1:9, 13:15), detectDates=T)
setDT(data_provincia_letra)

# Vuelvo a llamar las variables por su nombre normal, sin aclaraciones de cons,
# para que pueda correr el codigo mas facilmente

setnames(data_grupo, 'Remu_media_cons', 'Remu_media')
setnames(data_grupo, 'Remu_mediana_cons', 'Remu_mediana')

setnames(data_provincia, 'Remu_media_cons', 'Remu_media')
setnames(data_provincia, 'Remu_mediana_cons', 'Remu_mediana')

setnames(data_letra, 'Remu_media_cons', 'Remu_media')
setnames(data_letra, 'Remu_mediana_cons', 'Remu_mediana')

setnames(data_provincia_letra, 'Remu_media_cons', 'Remu_media')
setnames(data_provincia_letra, 'Remu_mediana_cons', 'Remu_mediana')

### Desestacionalizar con RJdemetra los salarios medios y medianos

library(RJDemetra)
library(data.table)

####################################### ESTRATEGIA: REPETIR 4 VECES PARA TODOS LOS GRAFICOS POR DISTINTAS DESAGREGACIONES
# Primero para el grupo

# Me genero un data table q voy a llenar con las tendencias
data_grupo_t <- data_grupo %>% select( -c(Remu_media, Remu_mediana))

for(i in 4:5){
  # Pasamos a time series
  ts_tend <- ts(select(data_grupo, c(variables[i])), start = c(2007), frequency = 12)
  #Descomponemos con RJDemetra
  ts_tend <- RJDemetra::x13(ts_tend)
  # Llevamos el resultado a dataframe
  ts_tend <- data.frame(.preformat.ts(ts_tend$final$series), stringsAsFactors = FALSE)
  # Pasar el nombre de las rows (fecha) a variable
  ts_tend <- tibble::rownames_to_column(ts_tend, var = 'mes')
  # Damos formato a la fecha
  ts_tend$mes <- lubridate::my(ts_tend$mes)
  # Nos quedamos sólo con la tendencia de la serie
  setDT(ts_tend)
  ts_tend <- ts_tend[,.(mes, t)]
  # Mergeamos con la serie nueva que solo va a incluir las tendencias
  data_grupo_t <- merge(data_grupo_t, ts_tend, by='mes')
  ### Nos quedamos solo con la tendencia y la renombramos como la comun
  setnames(data_grupo_t, "t", variables[i])
  rm(ts_tend)
}

# Loop con 4 graficos

for( i in 1:length(variables)){
  
  ggplot(data_grupo_t) +
    aes(x = mes, y = !! sym((variables[[i]]))) +
    geom_line(color="red", linewidth=0.3) +
    scale_color_viridis(discrete = TRUE) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 #limits = c("2007-01-01", "2022-04-01")
                 expand = c(0, 250)) +
    ggtitle(variables[i], subtitle="Jovenes entre 18 y 25 años") +
    ylab("")+
    xlab("") +
    theme_bw()+
    labs(caption = "Elaboración CEP XXI en base a registros SIPA.")
  
  # Guardamos en png
  ggsave(paste0(variables[i], "_general_v4.png"),
         plot = last_plot())
}


# Segundo para la letra
# Me genero un data table q voy a llenar con las tendencias
#data_letra_t <- data_letra %>% select( -c(Remu_media, Remu_mediana))

letras <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
            'P', 'Q', 'R', 'S', 'T', 'U')

# Para poder desestacionalizar y tambien graficar, debo armar data tables con series 
# por letra. 

bases_letra_full <- c() # Estrategia armar un objeto q contenga todas las bases ya listas 
# y luego ir llamandolo para graficar

for(p in 1:length(letras)){
  
  # Lo primero es alimentar la lista con las bases enteras
  
  tmp <- data_letra %>% filter(letra == letras[p])
  setDT(tmp)
  tmp_t <- tmp %>% select( -c(Remu_media, Remu_mediana))
  
  for(i in 4:5){
    # Pasamos a time series
    ts_tend <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
    #Descomponemos con RJDemetra
    ts_tend <- RJDemetra::x13(ts_tend)
    # Llevamos el resultado a dataframe
    ts_tend <- data.frame(.preformat.ts(ts_tend$final$series), stringsAsFactors = FALSE)
    # Pasar el nombre de las rows (fecha) a variable
    ts_tend <- tibble::rownames_to_column(ts_tend, var = 'mes')
    # Damos formato a la fecha
    ts_tend$mes <- lubridate::my(ts_tend$mes)
    # Nos quedamos sólo con la tendencia de la serie
    setDT(ts_tend)
    ts_tend <- ts_tend[,.(mes, t)]
    # Mergeamos con la serie nueva que solo va a incluir las tendencias
    tmp_t <- merge(tmp_t, ts_tend, by='mes')
    ### Nos quedamos solo con la tendencia y la renombramos como la comun
    setnames(tmp_t, "t", variables[i])
    rm(ts_tend)
  }
  
  bases_letra_full[[p]] <- tmp_t
  rm(tmp, tmp_t)
}  

# Ahora tengo guardadas un data.table por letra ya con la tendencia de las monetarias

#prueba <- bases_letra_full[[15]]

# Loop con graficos
for(p in 1:length(letras)){
  
  for( i in 1:length(variables)){
    
    ggplot(bases_letra_full[[p]]) +
      aes(x = mes, y = !! sym((variables[[i]]))) +
      geom_line(color="red", linewidth=0.3) +
      scale_color_viridis(discrete = TRUE) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   #limits = c("2007-01-01", "2022-04-01")
                   expand = c(0, 250)) +
      ggtitle(variables[i], subtitle="Jovenes entre 18 y 25 años") +
      ylab("")+
      xlab("") +
      theme_bw()+
      labs(caption = "Elaboración CEP XXI en base a registros SIPA.")
    
    # Guardamos en png
    ggsave(paste0(ruta_v4, "graphs_letra/", variables[i], "_letra", letras[p], "_v4", ".png"),
           plot = last_plot())
  }
}




######## Ahora graficos por provincia 

provs <- unique(data_provincia$provincia)

# Para poder desestacionalizar y tambien graficar, debo armar data tables con series 
# por letra. 

bases_prov_full <- c() # Estrategia armar un objeto q contenga todas las bases ya listas 
# y luego ir llamandolo para graficar

for(p in 1:length(provs)){
  
  # Lo primero es alimentar la lista con las bases enteras
  
  tmp <- data_provincia %>% filter(provincia == provs[p])
  setDT(tmp)
  tmp_t <- tmp %>% select( -c(Remu_media, Remu_mediana))
  
  for(i in 4:5){
    # Pasamos a time series
    ts_tend <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
    #Descomponemos con RJDemetra
    ts_tend <- RJDemetra::x13(ts_tend)
    # Llevamos el resultado a dataframe
    ts_tend <- data.frame(.preformat.ts(ts_tend$final$series), stringsAsFactors = FALSE)
    # Pasar el nombre de las rows (fecha) a variable
    ts_tend <- tibble::rownames_to_column(ts_tend, var = 'mes')
    # Damos formato a la fecha
    ts_tend$mes <- lubridate::my(ts_tend$mes)
    # Nos quedamos sólo con la tendencia de la serie
    setDT(ts_tend)
    ts_tend <- ts_tend[,.(mes, t)]
    # Mergeamos con la serie nueva que solo va a incluir las tendencias
    tmp_t <- merge(tmp_t, ts_tend, by='mes')
    ### Nos quedamos solo con la tendencia y la renombramos como la comun
    setnames(tmp_t, "t", variables[i])
    rm(ts_tend)
  }
  
  bases_prov_full[[p]] <- tmp_t
  rm(tmp, tmp_t)
}  

# Ahora tengo guardadas un data.table por letra ya con la tendencia de las monetarias

#prueba <- bases_prov_full[[15]]

# Loop con graficos
for(p in 1:length(provs)){
  
  for( i in 1:length(variables)){
    
    ggplot(bases_prov_full[[p]]) +
      aes(x = mes, y = !! sym((variables[[i]]))) +
      geom_line(color="red", linewidth=0.3) +
      scale_color_viridis(discrete = TRUE) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                   #limits = c("2007-01-01", "2022-04-01")
                   expand = c(0, 250)) +
      ggtitle(variables[i], subtitle="Jovenes entre 18 y 25 años") +
      ylab("")+
      xlab("") +
      theme_bw()+
      labs(caption = "Elaboración CEP XXI en base a registros SIPA.")
    
    # Guardamos en png
    ggsave(paste0(ruta_v4, "graphs_provincia/", variables[i], "_provincia", provs[p], "_v4", ".png"),
           plot = last_plot())
  }
}




############# Por ultimo graficos por letra y provincia
######## 


# Para poder desestacionalizar y tambien graficar, debo armar data tables con series 
# por letra y provincia. Una por fecha

letras <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
            'P', 'Q', 'R', 'S') # Quito ultimas dos letras que son marginales

# Estrategia armar una matriz q contenga todas las bases ya listas 
# y luego ir llamandolo para graficar
#bases_letra_prov_full <- data.frame()
bases_letra_prov_full <- tibble()


for(p in 1:length(provs)){
  for (l in 1:length(letras)){
    
    # Lo primero es alimentar la lista con las bases enteras
    
    tmp <- data_provincia_letra %>% filter(provincia == provs[p] & letra == letras[l])
    setDT(tmp)
    tmp_t <- tmp %>% select( -c(Remu_media, Remu_mediana))
    
    for(i in 4:5){
      # Pasamos a time series
      ts_tend <- ts(select(tmp, c(variables[i])), start = c(2007), frequency = 12)
      #Descomponemos con RJDemetra
      ts_tend <- RJDemetra::x13(ts_tend)
      # Llevamos el resultado a dataframe
      ts_tend <- data.frame(.preformat.ts(ts_tend$final$series), stringsAsFactors = FALSE)
      # Pasar el nombre de las rows (fecha) a variable
      ts_tend <- tibble::rownames_to_column(ts_tend, var = 'mes')
      # Damos formato a la fecha
      ts_tend$mes <- lubridate::my(ts_tend$mes)
      # Nos quedamos sólo con la tendencia de la serie
      setDT(ts_tend)
      ts_tend <- ts_tend[,.(mes, t)]
      # Mergeamos con la serie nueva que solo va a incluir las tendencias
      tmp_t <- merge(tmp_t, ts_tend, by='mes')
      ### Nos quedamos solo con la tendencia y la renombramos como la comun
      setnames(tmp_t, "t", variables[i])
      rm(ts_tend)
    }
    # Necesito llenar 456 elementos de bases_letra_prov_full
    bases_letra_prov_full[p,l] <- tmp_t %>%
      nest()
    rm(tmp, tmp_t)
  }
}



# Ahora tengo guardadas un data.table por letra ya con la tendencia de las monetarias

#prueba <- bases_letra_prov_full[1,5] %>% unnest()

# Loop con graficos. Este loop tira 1824 graficos!! 19 letras x 24 provincias x 4 variables...

for(p in 1:length(provs)){
  for (l in 1:length(letras)){
    
    for( i in 1:length(variables)){
      
      ggplot(bases_letra_prov_full[p,l] %>% unnest()) +
        aes(x = mes, y = !! sym((variables[[i]]))) +
        geom_line(color="red", linewidth=0.3) +
        scale_color_viridis(discrete = TRUE) +
        scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                     #limits = c("2007-01-01", "2022-04-01")
                     expand = c(0, 250)) +
        ggtitle(variables[i], subtitle="Jovenes entre 18 y 25 años") +
        ylab("")+
        xlab("") +
        theme_bw()+
        labs(caption = "Elaboración CEP XXI en base a registros SIPA.")
      
      # Guardamos en png
      ggsave(paste0(ruta_v4, "graphs_letra_prov/", variables[i], "_provincia", provs[p],
                    "_letra", letras[l], "_v4", ".png"),
             plot = last_plot())
    }
  }    
}









