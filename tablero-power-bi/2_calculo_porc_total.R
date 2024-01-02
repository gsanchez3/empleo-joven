######## Creamos las series de % de participacion

rm(list = ls())

library(data.table)     # manipular grandes conjuntos de datos
library(lubridate)      # manipular fechas
library(openxlsx)       # leer archivos .xlsx  
library(plyr)
library(tidyverse)
library(fst)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(Mectritas)
options(scipen=999) 


# Tengo q setear el limite a actualizar de las mectras. Formato: AAAA-MM-DD
# Esta fecha limite es la mectra mas vieja que se esta actualizando. De esta forma,
# luego nos quedamos solo con lo sea estrictamente menor a esa fecha

ymd_limite <- '2023-03-01' # en este ejemplo me quedo con lo menor a nov 2022
### ATENCION CAMBIAR TAMBIEN EL LOOP DE INICIO EN LAS MECTRAS
s <- 03
#### ATENCION CAMBIAR TAMBIEN LA BASE DE CUIT CON CLAE PONDERADO


# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Empleo_joven/")
setwd(ruta)

# Seteamos ruta de bases
ruta_bases <- "C:/Users/Usuario/Documents/Bases CEP XXI/"

# Seteamos la ruta de las mectras
ruta_mectras <- "C:/Users/Usuario/Documents/Bases CEP XXI/MECTRA/Mectra FST"

## Le tengo que agregar a clae2_prov_universos la dimension de participacion

# Ok voy a hacer lo mismo pero para toda la mectra

### La pesada base cuit con clae ponderado
clae_ponderado <- fread(r'(C:\Users\Usuario\Documents\Bases CEP XXI\CUITs con CLAE ponderado\CUITs con CLAE ponderado V2 2023-11-28 - Actualizado.csv)')
clae_ponderado <- clae_ponderado[,.(cuit, clae6_pond, mes)]
clae_ponderado <- clae_ponderado[,cuit:=as.double(cuit)]
clae_ponderado <- clae_ponderado[,clae6_pond:=as.double(clae6_pond)]
clae_ponderado <- clae_ponderado[,mes:=as.double(mes)]
setnames(clae_ponderado, "clae6_pond", "clae6")

# Listamos las mectras
mectras <- list.files(ruta_mectras,
                      pattern='*.fst',
                      full.names = T,
                      recursive = T)

mectras <- mectras[str_detect(mectras, 'm2023')==T] # me quedo solo con ultimas mectras


data_prov_letra_clae2 <- setDT(fread(paste0(ruta, 'serie_puestos_totales.csv')))[mes < ymd_limite]
data_prov_letra_clae2 <- data_prov_letra_clae2[, mes:= as.Date(mes)]

## Llamo los data tables para ir agregando

#data_prov_letra_clae2 <- data.table()

universos <- c('Total general', 'Total empresas', 'Privado')

for(i in s:(length(mectras)-1)){
  tmp <- read.fst(mectras[i], columns = c("cuit", "cuil", "zona", "modalidad", "remuneracion", "mes"), as.data.table = TRUE)
  print(i)
  tmp <- tmp %>% mectra_numerica()
  # Filtramos las cajas y falopa
  tmp <- tmp %>% limpia_cuits(indica_publico=F)
  # Creo una variable para indicar si tuvo mas de un empleo
  tmp <- tmp[, pluriempleo := .N, by=cuil]
  # Creo una variable que va a servir de contador si el tipo esta en agencia. Vale 0 en todos los casos menos para los de agencia
  tmp <- tmp[, agencia := 0]
  
  #if (i >= 90) { # Los casos de modalidad 102 empiezan a aparecer en septiembre del 2014, que es la mectra 90
    # Vamos a explorar los casos q son de agencia
    casos <- tmp %>% filter(pluriempleo>1 & remuneracion==0 & modalidad==102)
    setorder(casos, -pluriempleo, cuil)
    casos <- casos[,conteo:=.N, by='cuil']
    casos_dif <- casos[conteo >1]
    casos_dif <- unique(casos_dif$cuil) # para oct 2016 tengo 808 casos donde tendre q randomizar a q empresa le mando el sueldo
    
    casos_cuiles <- unique(casos$cuil) # cantidad de personas distintas que aparecen con modalidad 102 y remuneracion 0
    agencieros <- tmp %>% filter(cuil %in% casos_cuiles) 
    #setorder(agencieros, -pluriempleo, cuil)
    
    # supongamos que queremos sumar todas las remuneraciones y dejar al trabajador en una NO agencia. Es decir lo dejo en un caso donde tenga remu 0 y 
    # modalidad 102. En la mayoria solo tienen 1 observacion asi por cuil, pero hay casos donde tienen muchas. En ese caso dejar al azar
    
    setorder(agencieros, cuil, remuneracion) # ordeno de forma q me quede primero la observacion donde no tienen remuneracion
    agencieros <- agencieros[,index:=1:.N, by=cuil]
    prueba_aux <- agencieros[index ==1]# En esta base tengo a una observacion por caso de agencia con una remuneracion igual a 0.
    prueba_aux <- prueba_aux[!cuil %in% casos_dif] # Quito los casos dificiles que los voy a tratar aparte
    prueba_aux$remuneracion <- NULL 
    
    # Trabajo aparte a los 800 casos donde hay varias remuneraciones 0 y entre estas hay q randomizar 
    # De agencieros deberia separar a los cuiles dificiles, quedarme con sus remuneraciones 0 y elegir una al azar
    
    agencieros_dif <- agencieros[cuil %in% casos_dif] # me quedo con los cuiles dificiles
    agencieros_dif <- agencieros_dif[remuneracion == 0] # sigo teniendo los mismos cuiles unicos, pero los casos donde tienen remu cero
    # Ahora de estos casos me tengo que quedar con una observacion por cuil al azar
    set.seed(55)
    pruebaza <- ddply(agencieros_dif,.(cuil),function(x) x[sample(nrow(x),1),])
    pruebaza$remuneracion <- NULL
    
    # deberia joinear pruebaza que tiene los casos dificiles con prueba_aux que tiene los normales
    tmp1 <- rbind(prueba_aux, pruebaza)
    
    #Sumo las remuneraciones por cuil para todos los agencieros
    agencieros_sal <- agencieros[,.(remuneracion = sum(remuneracion,na.rm=T)), by=c("cuil")] # colapso una obs por cuil con la suma de sus remus
    
    #Al mergear tengo finalmente una observacion por cuil, con la suma de sus remuneraciones ubicada en la empresa donde esta y no en la agencia
    tmp1 <- merge(tmp1,agencieros_sal, by='cuil')
    
    # Ahora si, con una observacion por cuil de estos casos los sumo a la anterior
    # elimino estos casos de tmp y los reemplazo por estas observaciones ya trabajadas
    tmp <- tmp[!cuil %in% casos_cuiles]
    tmp1$index <- NULL
    # Antes de joinear con la base general voy a modificar el valor de la variable agencia para que sea 1
    tmp1 <-  tmp1[, agencia := 1]
    tmp <- rbind(tmp, tmp1)
    
    # listo me quedo una sola observacion por cuil en los casos de las agencias, no asi en  el resto de casos de pluriempleo...
    # Eliminemos todo lo sobrante que usamos
    rm(agencieros, agencieros_dif, agencieros_sal , casos, prueba_aux, pruebaza, tmp1, casos_cuiles, casos_dif )
  #}
  
  #agrego actividad principal
  tmp <- tmp[,mes:=as.double(mes)] #me pide pasar a doble mes para mergear
  tmp <- merge(tmp, clae_ponderado, by=c("cuit", 'mes'), all.x = TRUE)
  #Asignaci?n de descripcion de los claes
  tmp <- tmp %>% add_claes(agregacion_deseada = c('letra', 'clae2'))
  #zona
  tmp <- merge(tmp, zona_loc, by="zona", all.x = TRUE)
  setnames(tmp, 'zona_prov', 'provincia')
  
  ## Paso a formato fecha la variable mes
  tmp <- tmp[,mes1 := paste0(tmp$mes, "01")]
  tmp <- tmp[,mes := ymd(tmp$mes1)]
  tmp$mes1 <- NULL
  
  for (p in 1:length(universos)){
    
    if (p == 2) {
      tmp <- tmp %>% limpia_cuits(elimina_pub_no_prod = T, indica_publico = F)
    }
    
    if (p == 3) {
      tmp <- tmp %>% limpia_cuits(elimina_publico = T, indica_publico = F)
    }
    
    tmp$Grupo <- universos[p]
    
    ### Por provincia, letra y clae2
    tmp_prov_letra_clae2 <- tmp[,.(puestos_totales = .N),
    by=c('Grupo', 'provincia', 'letra', 'letra_desc', 'clae2', 'clae2_desc', 'mes')]
    setorder(tmp_prov_letra_clae2, provincia, letra, clae2)
    data_prov_letra_clae2 <- rbind(data_prov_letra_clae2, tmp_prov_letra_clae2)
    
    rm(tmp_prov_letra_clae2)
  }  
  #Vemos por donde vamos, borramos temporales y sigue el loop
  print('Cargado')
  rm(tmp)
  gc()
}

# Guardo la base para volver a usar en el proximo mes. Esta base va a tener mas filas que 
# el que solo tiene universo joven, pero luego al mergear conservando solo x estas se van.

write.csv(data_prov_letra_clae2, r"(C:\Users\Usuario\Documents\Cep Pedidos\Empleo_joven\serie_puestos_totales.csv)",row.names = F)

# Ahora mergeo los resultados y calculo el % de jovenes
data_total <- setDT(fread(paste0(ruta, 'clae2_prov_universos_beta.csv')))
#data_total <- data_total[,. (Grupo, provincia, letra, letra_desc, clae2,
#                             clae2_desc, anio, mes, puestos)]
data_total <- data_total[, mes:= as.Date(mes)]

data_total <- merge(data_total, data_prov_letra_clae2, by=c('Grupo',
                                                            'provincia', 'letra',
                                                            'letra_desc', 'clae2',
                                                            'clae2_desc', 'mes'), all.x = T)
setorder(data_total, mes, provincia, letra, clae2)
#data_total <- data_total[, share:= round((puestos/puestos_totales)*100, digits=2)]

#### Guardo el nuevo archivo con los datos del total de puestos general

write.csv(data_total, r"(C:\Users\Usuario\Documents\Cep Pedidos\Empleo_joven\clae2_prov_universos.csv)",row.names = F)

