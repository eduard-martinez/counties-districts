# Limpiar consola, entorno, fijar directorio y cargar paquetes
cat("\f")
rm(list=ls())
directory <- "~/Desktop/OneDrive - Universidad de Los Andes/Doctorado en Economia-Uniandes/Tesis/Lobbying and Ideology/Data/Original/Congressional Districts by County"
setwd(directory)
packages <- c("tidyverse","rgdal","rgeos","raster")
sapply(packages,require,character.only=T)
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+

# Cargar shapefiles
cd <- readOGR(dsn="Datos/Originales/CD_2004",layer = "districts108")
str(cd@data)
cd$ID <- as.character(cd$ID)
cd <- spTransform(cd, CRSobj = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

counties <- readOGR(dsn = "Datos/Originales/Counties", layer = "tl_2016_us_county")
counties$STATEFP <- as.character(counties$STATEFP)
counties$COUNTY <- as.character(counties$COUNTY)
counties <- spTransform(counties, CRSobj = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

cd_florida<- cd[cd@data$STATE=="Florida",]
counties_florida <- crop(counties,cd_florida)
length(counties_florida)

df_area <- data.frame(NAME = rep(NA,1), DISTRICT = rep(NA,1) , area = rep(NA,1))
for (j in 1:length(counties_florida)){
  x <- counties_florida[j,]
  df_area[j,2] <- x$NAME
  df_area[j,3] <- gArea(x)
}
counties_table <- table(df_area$NAME)


for ( i in c(counties,cd,counties_florida)){
  print(proj4string(i))
}

crop_area <- function(counti_i){
  print(counti_i)
  # Filtramos el condado i
  cd_state_i <- cd_state[state_i,] %>% gBuffer(spgeom = .,byid = T,width = 0)
  
  # Cortamos los condado de 1860 que estan debajo
  counties_state_i <- crop(gBuffer(spgeom = counties_state,byid = T,width = 0),cd_state_i)
  
  # Calcular el area de cada feature de conado_1860_i
  if (length(counties_state_i) > 0){
    df_area <- data.frame(ID_NUM_2000 = rep(NA,1), ID_NUM_1860 = rep(NA,1) , area = rep(NA,1))
    for (j in 1:length(counties_state_i)){
      x <- counties_state_i[j,]
      df_area[j,2] <- x$NAME 
      df_area[j,3] <- gArea(spgeom = x,byid = T)
    }
    df_area[,1] <- cd_state_i$DISTRICT  
    return(df_area)
  } 
  # Cuando no hay elementos en conado_1860_i
  if (length(counties_state_i) == 0){
    df_area <- data.frame(ID_NUM_2000 = rep(NA,1), ID_NUM_1860 = rep(NA,1) , area = rep(NA,1))
    df_area[1,1] <- cd_state_i$DISTRICT
    return(df_area)
  } 
}

# Creamos vector a iterar
vector <- seq(1,length(cd_state),1)
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