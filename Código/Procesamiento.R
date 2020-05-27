
# Limpiar consola, entorno, fijar directorio y cargar paquetes
cat("\f")
rm(list=ls())
directory <- "~/Dropbox/Carpetas Compartidas/Limites condados USA"
setwd(directory)
packages <- c("tidyverse","rgdal","rgeos","raster")
sapply(packages,require,character.only=T)
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

# Cargar shapefiles
condado <- readOGR(dsn="Datos/Originales/Counties",layer = "US_HistCounties_Gen001")
condado$FIPS <- as.character(condado$FIPS)
condado <- spTransform(condado, CRSobj = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# Definir y exportar shapefiles
condado_1860 <- condado[condado$START_N <= 18590101 & condado$END_N >= 18590101,] 
writeOGR(condado_1860, dsn = "Datos/Procesados" , layer = "Counties_1860" , driver = "ESRI Shapefile",overwrite_layer = T)

condado_2000 <- condado[condado$START_N <= 20001231 & condado$END_N >= 20001231,] 
writeOGR(condado_2000, dsn = "Datos/Procesados" , layer = "Counties_2000" , driver = "ESRI Shapefile",overwrite_layer = T)

#------------------------------#
# Haciendo ejemplo con un caso #
#------------------------------#
"Hagamos un ejemplo ID_NUM 1498"

# Filtramos el condado i
condado_2000_i <- condado_2000[condado_2000$FIPS == "04005",]

# Cortamos los condado de 1860 que estan debajo
condado_1860_i <- crop(condado_1860,condado_2000_i)

# Calcular el area de cada feature de conado_1860_i
length(condado_1860_i)
df_area <- data.frame(ID_NUM_2000 = rep(NA,1), ID_NUM_1860 = rep(NA,1) , area = rep(NA,1))
for (j in 1:length(condado_1860_i)){
    x <- condado_1860_i[j,]
    df_area[j,2] <- x$ID_NUM 
    df_area[j,3] <- gArea(x)
}
df_area[,1] <- condado_2000_i$ID_NUM 

#---------------#
# Haciendo loop #
#---------------#

"Hagamos un ejemplo ID_NUM 1498"
crop_area <- function(counti_i){
             print(counti_i)
             # Filtramos el condado i
             condado_2000_i <- condado_2000[counti_i,] %>% gBuffer(spgeom = .,byid = T,width = 0)
            
             # Cortamos los condado de 1860 que estan debajo
             condado_1860_i <- crop(gBuffer(spgeom = condado_1860,byid = T,width = 0),condado_2000_i)
            
             # Calcular el area de cada feature de conado_1860_i
             if (length(condado_1860_i) > 0){
                 df_area <- data.frame(ID_NUM_2000 = rep(NA,1), ID_NUM_1860 = rep(NA,1) , area = rep(NA,1))
                 for (j in 1:length(condado_1860_i)){
                         x <- condado_1860_i[j,]
                         df_area[j,2] <- x$ID_NUM 
                         df_area[j,3] <- gArea(spgeom = x,byid = T)
                 }
                 df_area[,1] <- condado_2000_i$ID_NUM  
            return(df_area)
            } 
            # Cuando no hay elementos en conado_1860_i
            if (length(condado_1860_i) == 0){
                df_area <- data.frame(ID_NUM_2000 = rep(NA,1), ID_NUM_1860 = rep(NA,1) , area = rep(NA,1))
                df_area[1,1] <- condado_2000_i$ID_NUM
                return(df_area)
            } 
}

# Creamos vector a iterar
vector <- seq(1,length(condado_2000),1)
lista_areas <- list()
lista_areas <- lapply(vector,crop_area)
areas_1860_2000 <- data.table::rbindlist(lista_areas)            

# Calculando areas de condados 1860
condado_1860$AREA <- gArea(spgeom = condado_1860,byid = T)
df_condado_1860 <- condado_1860@data %>% dplyr::select(.,FIPS,ID_NUM,AREA)
colnames(df_condado_1860) <- c("FIPS_1860","ID_NUM_1860","AREA_1860")
condado_2000$AREA <- gArea(spgeom = condado_2000,byid = T)
df_condado_2000 <- condado_2000@data %>% dplyr::select(.,FIPS,ID_NUM,AREA)
colnames(df_condado_2000) <- c("FIPS_2000","ID_NUM_2000","AREA_2000")

# Uniendo los dos dataframes
df_1860_2000 <- merge(areas_1860_2000,df_condado_1860,by="ID_NUM_1860",all.x=T) %>%
                merge(.,df_condado_2000,by="ID_NUM_2000",all.x=T)
df_1860_2000 <- mutate(df_1860_2000,peso=(area/AREA_1860)*100)
df_1860_2000 <- dplyr::select(df_1860_2000,ID_NUM_2000,ID_NUM_1860,peso,FIPS_1860,FIPS_2000)
collapse <- doBy::summaryBy(peso~ID_NUM_1860,FUN = (sum),data = df_1860_2000)









  