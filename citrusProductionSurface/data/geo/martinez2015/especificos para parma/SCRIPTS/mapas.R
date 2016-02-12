###########################################################
#################### GENERAMOS MAPAS CON LOW PREVALENCE ###
###########################################################

library(ggmap)
library(rgeos)
library(rgdal)
library(raster)
setwd("I:/IVIA/CBS/CBS_SDM/RASTERS/SCRIPS_R/DEFINITIVOS/EJPP/ESPECIFICOS PARA PARMA/DIRECTORIO DE TRABAJO")


################################
######## DATOS #################
################################
datos_n<-read.table("I:/IVIA/CBS/CBS_SDM/RASTERS/ANÁLISIS_ESTADÍSTICO/ANALISIS_INLA/ANALISIS_1945_1950_2014/wager2014_nuevos/BANCOS DE DATOS/datos2014.txt", sep="")



#################################
########## Cargamos boundaries ##
#################################
boundZA<-getData('GADM',country="ZAF",level=0)
boundZA1<-getData('GADM',country="ZAF",level=1)
boundZA2<-getData('GADM',country="ZAF",level=2)

boundZA<-spTransform(boundZA,CRS=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")) #assign CRS/projection
boundZA1<-spTransform(boundZA1,CRS=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")) #assign CRS/projection
boundZA2<-spTransform(boundZA2,CRS=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")) #assign CRS/projection

#We expand the map. In this way, the island won't be showed
CP<-as(extent(15,35,-35,-20),"SpatialPolygons")
proj4string(CP)<-CRS(proj4string(boundZA1))

## Clip the map
sudafrica0<-gIntersection(boundZA, CP, byid=TRUE)
sudafrica1<-gIntersection(boundZA1,CP,byid=TRUE)



########################################
######### Descargamos del google maps ##
########################################
#Seleccionamos la zona a extraer (SUDÁFRICA)
bbox2 <- ggmap::make_bbox(c(15,35), c(-35, -20), f = 0.05)
map_loc2 <- get_map(location = bbox2, source = 'google', maptype = 'satellite', zoom=5)
map2 <- ggmap(map_loc2, extent = 'device', maprange=FALSE, darken = c(0, "white"))
#mapa
map2


########################################
######### BOUNDARIES SUDA ##############
########################################
suda_ggmap <- fortify(sudafrica0)

colores<-c("green4", "red", "orange")

png("cbs_low.png", width=1500, height=1500)
  map2 +
  geom_polygon(aes(x = long, y = lat, group = group), data = suda_ggmap,  colour = 'white', fill="black",
               alpha =0.1, size = 1) +
  geom_point(aes(x = lon, y = lat), data=datos_n, alpha = 1,size=3, col=colores[as.factor(datos_n$cbs_low)])+ 
  geom_polygon(aes(x = long, y = lat, group = group), data = suda_ggmap,  colour = 'white', fill=NA,
                 alpha = 1, size = 1) 
    
dev.off()

)
