
# Limpiar consola, entorno, fijar directorio y cargar paquetes
cat("\f")
rm(list=ls())
directory <- "~/Dropbox/Carpetas Compartidas/counties-districts"
setwd(directory)
packages <- c("tidyverse","rgdal","rgeos","raster")
sapply(packages,require,character.only=T)
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

# Importar los shape de distritos
load("datos/originales/districts108.Rdata")
str(distritos@data)
distritos@data <- lapply(distritos@data,function(x) as.character(x)) %>%
                  as.data.frame(stringsAsFactors=F)

# Importar los shape de los condados
load("datos/originales/tl_2016_us_county.Rdata")
counties@data <- lapply(counties@data,function(x) as.character(x)) %>%
                  as.data.frame(stringsAsFactors=F)

# Reproyectando las capas
UTM <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
counties <- spTransform(counties, CRSobj = UTM)
distritos <- spTransform(distritos, CRSobj = UTM)

# Funcion que corta los condados deabjo de un distrito
n_counties <- function(i,condados,district){
  
              "Dejamos la parte de un condado que esta debajo del distriro i"
              distrito_i <- district[i,] %>% gBuffer(spgeom = .,byid = T,width = 0)
              counties_i <- crop(gBuffer(spgeom = condados,byid = T,width = 0),distrito_i)
              
              "Primero verificamos que existan condados debajo del distrito"
              if (length(counties_i)>0){
                  df_area <- data.frame(ID_condado = rep(NA,1), 
                                        name_condado = rep(NA,1) , 
                                        state_countie = rep(NA,1) , 
                                        area = rep(NA,1))
                  for (j in 1:length(counties_i)){
                            df_area[j,1] <- counties_i[j,]$COUNTYNS
                            df_area[j,2] <- counties_i[j,]$NAME
                            df_area[j,3] <- counties_i[j,]$STATEFP
                            df_area[j,4] <- gArea(counties_i[j,])
                  }
                  df_area$ID_distrito <- distrito_i$ID 
                  df_area$state_distric <- distrito_i$STATENAME 
                  return(df_area)
              }
              
              "Sino tiene elementos entonces"
              if (length(counties_i)==0){
                  df_area <- data.frame(ID_condado = rep(NA,1), 
                                        name_condado = rep(NA,1) , 
                                        state_countie = rep(NA,1) , 
                                        area = rep(NA,1),
                                        ID_distrito = distrito_i$ID, 
                                        state_distric= distrito_i$STATENAME )
                  return(df_area)
              }
}

# Aplicando funcion
condados_distritos <- lapply(1:length(distritos), 
                             function(x) n_counties(i = x,condados = counties,district = distritos)) %>%
                      data.table::rbindlist(use.names = T,fill = T) %>% 
                      as.data.frame(stringsAsFactors=F)

# Calculando area de distritos
condados_distritos <- condados_distritos %>% group_by(ID_condado) %>% 
                      mutate(area_condado = sum(area) , p_area = area/area_condado)





