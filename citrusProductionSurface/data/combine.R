library(pacman)
p_load(raster)

p_load(cartography)
p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)
p_load(readr)
p_load(memoise)
p_load(maptools)
p_load(tmap)
p_load_gh("eblondel/cleangeo")
                                        #p_unload(raster)
p_load(Hmisc)
p_load(dplyr)
p_load(readxl)

source("replace.R")
source("mapTools.R")
source("utils.R")

source("spain/program/read.R")
source("france/program/read.R")
source("italy/program/read.R")
source("portugal/program/read.R")
source("cyprus/program/read.R")
source("croatia/program/read.R")
source("greece/program/read.R")
source("malta/program/read.R")

crs <- CRS("+proj=longlat +ellps=WGS84")
extent=raster::extent(-10,34,34,48)

postProcessMap <- function(map) {
    map <- spTransform(map,crs)
    map <- raster::crop(map,extent)
    map
}




resolution <- "01M"  # 60M


extent <- raster::extent(-10,34,34,48) #small


EU_NUTS <- readOGR(dsn = sprintf("./geo/NUTS_2013_%s_SH/data",resolution),
                  layer = sprintf("NUTS_RG_%s_2013",resolution))
EU_NUTS <- EU_NUTS[!grepl("TR.*|MT.*|MK.*|ME.*|CH.*",EU_NUTS@data$NUTS_ID),]

EU_NUTS.0 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==0,] %>%
    postProcessMap()
EU_NUTS.3 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==3,] %>%
    postProcessMap()


world.eu <- readOGR(dsn = sprintf("./geo/CNTR_%s_2013_SH/data",resolution),
                   layer = sprintf("CNTR_RG_%s_2013",resolution))

world.eu <- postProcessMap(world.eu[!world.eu@data$CNTR_ID %in%  as.character(EU_NUTS.0@data$NUTS_ID),])




warnIfUnkownIds <- function(europe) {
    diff <- setdiff(europe$NUTS.Code,EU_NUTS.3@data$NUTS_ID)
    missingNutsIds <- europe %>%
        filter(NUTS.Code %in% diff) %>%
        select(NUTS.Code,NUTS3.name) %>% distinct()
    if(nrow(missingNutsIds)>0) {
        warning("The following nuts3 ids exist in the data but not in the map. So there values will not be shown: \n",missingNutsIds)
    }
}



extractCitrusData <- function() {

    sizeEurope_sq_ha <- 1018000000
    sumAreaShapeEurope <-  sum(EU_NUTS.3@data$Shape_Area)
    conversion_sa_ha <- sizeEurope_sq_ha / sumAreaShapeEurope

    nutsLevels <- readNutsLevels() %>%
        filter(Level==3) %>%
        filter(!NUTS.Code %in% c("CY00","MT00","PT17", "PT15", "PT20", "PT30", "PT113", "PT114", "PT115", "PT116", "PT117","ES13"))
    europe <- bind_rows(
        readCitrusHectar_spain(),
        readCitrusHectar_france(),
        readCitrusHectar_italy(),
        readCitrusHectar_portugal(),
        readCitrusHectar_cyprus(),
        readCitrusHectar_croatia(),
        readCitrusHectar_greece(),
        read_CitrusHectarMalta()) %>%
        tbl_df() %>%
        addNewData("nutsReplacements.csv") %>%
        left_join(nutsLevels,by = c('name'='Description')) %>%
        rename(NUTS3.name = name) %>%
        mutate(NUTS.Code = ifelse(is.na(NUTS.Code.Country),NUTS.Code,NUTS.Code.Country)) %>%
        select(country,year,NUTS3.name,NUTS.Code,ha,comment,source,link,date,sourceFile) %>%
        left_join(EU_NUTS.3@data,by=c("NUTS.Code"="NUTS_ID")) %>%
        mutate(Shape_Area_ha = Shape_Area * conversion_sa_ha,citrus_density=ha/Shape_Area_ha) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area,-Shape_Area_ha)    

   
    write.csv(europe,"output/citrusProduction.csv")

   
    europe
}

latestData <- function() {
    
    data <- extractCitrusData() %>%
        group_by(NUTS.Code) %>%
        mutate(max_year=max(year)) %>%
        filter(year==max_year) %>%
        ungroup() %>%
        select(-max_year)
    write.csv(data,"output/citrusProduction_latest.csv")
    data
}



citrusSurface_layer <- function() {

    data <- latestData()
    warnIfUnkownIds(data)
    breaks=c(1,500,2500,5000,10000,25000,Inf)
    pal <- carto.pal(pal1 = "red.pal",n1 = length(breaks))
    
    EU_NUTS.3.ha <- EU_NUTS.3
    newData <- EU_NUTS.3@data %>% left_join(data,by=c("NUTS_ID"="NUTS.Code"))
    
    EU_NUTS.3.ha@data <- newData

    tm_shape(world.eu) +
        tm_fill(col = "#E3DEBF") +
        tm_shape(EU_NUTS.3.ha) +
        tm_fill("ha",palette = pal,breaks = breaks,textNA = NA,colorNA = "white") +
        tm_borders(lwd=0.1,col="grey10") +
        tm_shape(EU_NUTS.0) +
        tm_borders(lwd=0.5,col="grey20") #+
                                        #tm_format_Europe()
}

magarey_layer <- function() {

    europe <- latestData()
    mag2015Data <- read_csv("./mag2015_table1.csv",skip=2) %>%
        mutate(Lat=gsub("−","-",Lat,fixed=T),
               Lon=gsub("−","-",Lon,fixed=T),
               Lat=as.numeric(Lat),
               Lon=as.numeric(Lon))
    pts <- mag2015Data %>% select(Lon,Lat) %>% data.frame() %>% SpatialPoints(crs)
    
    
    match <- (over(pts,EU_NUTS.3))
    mag2015Data <- bind_cols(mag2015Data,match)  %>%
        filter(!is.na(NUTS_ID)) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area) %>%
        left_join((europe %>% select(NUTS.Code,NUTS3.name,ha)),by=c("NUTS_ID"="NUTS.Code"))  
    write.csv(mag2015Data,"mag2015Nuts3.csv")

    lonLat <- mag2015Data %>% select(Lon,Lat) %>% data.frame()
    print(qtm(world.eu)) # to make pointLabel happy
    xy <- pointLabel(lonLat$Lon,lonLat$Lat,labels = paste0(seq_along(lonLat$Lon)),doPlot = F,cex=2)
    
    spts <- SpatialPointsDataFrame(coords=lonLat,
                                  data=data.frame(index=seq_along(mag2015Data$Lon)),
                                  proj4string = crs)
                         
    text_sp <- SpatialPointsDataFrame(coords=xy,data=data.frame(index=seq_along(mag2015Data$Lon)),
                                     proj4string = crs)
    tm_shape(spts) +
        tm_dots(col="blue",size=0.5) +

    tm_shape(text_sp) +
    tm_bubbles(size=1.1,col="white",alpha=0.5)+
    tm_text("index",col="blue")

}


aschmann_layer <- function() {
    aschmann <- raster::raster("./geo/martinez2015/rasters/mediterranean/ASCHMANN/Aschmann_med.grd")
    raster::values(aschmann) <- ifelse(is.na(raster::values(aschmann)),0,1)
    aschmann <- raster::crop(aschmann,extent)
    tm_shape(aschmann) +
        tm_raster(alpha = 0.2,legend.show = F)


}

##              x        y
##[1,] 33.602809 34.83600
##[2,]  9.485809 42.52425

##
##
citrusSurface_layer() +
    magarey_layer() +
    aschmann_layer()
