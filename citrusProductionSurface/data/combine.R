library(pacman)
p_load(cartography)
p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)
p_load(dplyr)
p_load(readr)
p_load(memoise)
p_load(maptools)
p_load(tmap)

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


postProcessMap <- function(map,extent=raster::extent(-10,34,34,48)) {
    map <- spTransform(map,CRS("+proj=longlat +ellps=WGS84"))
    map <- raster::crop(map,extent)
    map
}


par(cex=1)

resolution <- "01M"  # 60M


extent <- raster::extent(-10,34,34,48) #small
legend.pos = "right"

EU_NUTS <- readOGR(dsn = sprintf("./geo/NUTS_2013_%s_SH/data",resolution),
                  layer = sprintf("NUTS_RG_%s_2013",resolution))
EU_NUTS <- EU_NUTS[!grepl("TR.*|MT.*|MK.*|ME.*|CH.*",EU_NUTS@data$NUTS_ID),]

EU_NUTS.0 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==0,]
EU_NUTS.3 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==3,]
EU_NUTS.0.tr <- postProcessMap(EU_NUTS.0,extent)
EU_NUTS.3.tr <- postProcessMap(EU_NUTS.3,extent)


world.eu <- readOGR(dsn = sprintf("./geo/CNTR_%s_2013_SH/data",resolution),
                   layer = sprintf("CNTR_RG_%s_2013",resolution))

world.eu <- world.eu[!world.eu@data$CNTR_ID %in%  as.character(EU_NUTS.0.tr@data$NUTS_ID),] %>%
    spTransform(CRS("+proj=longlat +ellps=WGS84")) %>%
    raster::crop(extent)






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
    extractCitrusData() %>%
        group_by(NUTS.Code) %>%
        mutate(max_year=max(year)) %>%
        filter(year==max_year) %>%
        ungroup() %>%
        select(-max_year)
}

plotCitrusData <- function() {

    data <-  latestData()
    
    write.csv(data,"output/citrusProduction_latest.csv")

    breaks=c(0,500,2500,5000,10000,25000,Inf)
    plotCitrusMap(data,breaks)
    capture.output(sessionInfo(),file="sessionInfo.txt")
    width <- 1366
    height <- 768
    png("citrusMapHa.png",width = width,height = height)
    plotCitrusMap(data,breaks)
    dev.off()
     
}


plotCitrusMap_svg <- function(data) {
    svg(width=12,height=6)
    plotCitrusData()
}


plotOverlay <- function(inFile=F) {
    EU_NUTS.3.tr <- postProcessMap(EU_NUTS.3)
    europe <- latestData()
    
    mag2015Data <- read_csv("./mag2015_table1.csv",skip=2) %>%
        mutate(Lat=gsub("−","-",Lat,fixed=T),
               Lon=gsub("−","-",Lon,fixed=T),
               Lat=as.numeric(Lat),
               Lon=as.numeric(Lon)
                              
               )
    pts <- mag2015Data %>% select(Lon,Lat) %>% data.frame() %>% SpatialPoints(CRS("+proj=longlat +ellps=WGS84"))
    
    
    match <- (over(pts,EU_NUTS.3.tr))
    mag2015Data <- bind_cols(mag2015Data,match)  %>%
        filter(!is.na(NUTS_ID)) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area) %>%
        left_join((europe %>% select(NUTS.Code,NUTS3.name,ha)),by=c("NUTS_ID"="NUTS.Code"))  
    write.csv(mag2015Data,"mag2015Nuts3.csv")
    
    plotCitrusData()
    if (inFile) {
        width <- 1366
        height <- 768
        par(mar=c(0,0,0,0))
        png("mag2015Nuts3.png",width = width,height = height)

    }
    
    plot(EU_NUTS.3.tr[EU_NUTS.3.tr@data$NUTS_ID %in% mag2015Data$NUTS_ID,],lwd=0.5,add=T,border="blue")
    points(mag2015Data$Lon,mag2015Data$Lat,col="blue",cex=1.5,pch=19)
    xy <- pointLabel(mag2015Data$Lon,mag2015Data$Lat,labels = paste0(seq_along(mag2015Data$Lon)),col="blue",cex=2,doPlot = F)
    points(xy$x,xy$y,col="white",cex=2.5,pch=19)
    text(xy$x,xy$y,col="blue")
    if (inFile) {
        dev.off()
    }
}



plotTmap <- function() {

    data <- latestData()
    breaks=c(1,500,2500,5000,10000,25000,Inf)
    pal <- carto.pal(pal1 = "red.pal",n1 = length(breaks)) 
    newData <- EU_NUTS.3.tr@data %>% left_join(data,by=c("NUTS_ID"="NUTS.Code"))
    EU_NUTS.3.tr@data <- newData

    tm_shape(world.eu) +
        tm_fill(col = "#E3DEBF") +
        tm_shape(EU_NUTS.3.tr) +
        tm_fill("ha",palette = pal,breaks = breaks,textNA = NA,colorNA = "white") +
        tm_borders(lwd=0.1,col="grey10") +
        tm_shape(EU_NUTS.0.tr) +
        tm_borders(lwd=0.5,col="grey20") +
        tm_format_Europe()
}

plotOverlayTmap <- function() {

    europe <- latestData()
    crs <- CRS("+proj=longlat +ellps=WGS84")
    mag2015Data <- read_csv("./mag2015_table1.csv",skip=2) %>%
        mutate(Lat=gsub("−","-",Lat,fixed=T),
               Lon=gsub("−","-",Lon,fixed=T),
               Lat=as.numeric(Lat),
               Lon=as.numeric(Lon)
               
               )
    pts <- mag2015Data %>% select(Lon,Lat) %>% data.frame() %>% SpatialPoints(crs)
    
    
    match <- (over(pts,EU_NUTS.3.tr))
    mag2015Data <- bind_cols(mag2015Data,match)  %>%
        filter(!is.na(NUTS_ID)) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area) %>%
        left_join((europe %>% select(NUTS.Code,NUTS3.name,ha)),by=c("NUTS_ID"="NUTS.Code"))  
    write.csv(mag2015Data,"mag2015Nuts3.csv")

    lonLat <- mag2015Data %>% select(Lon,Lat) %>% data.frame()
    print(plotTmap) # to make pointLabel happy
    xy <- pointLabel(lonLat$Lon,lonLat$Lat,labels = paste0(seq_along(lonLat$Lon)),doPlot = F,trace = T,cex=2)
    
    spts <- SpatialPointsDataFrame(coords=lonLat,
                                  data=data.frame(index=seq_along(mag2015Data$Lon)),
                                  proj4string = crs)
                         
    text_sp <- SpatialPointsDataFrame(coords=xy,data=data.frame(index=seq_along(mag2015Data$Lon)),
                                     proj4string = crs)

    
    
    plotTmap() +
        tm_shape(spts) +
        tm_dots(col="blue",size=0.5) +

                                        tm_shape(text_sp) +
    tm_bubbles(size=1.1,col="white",alpha=0.5)+
    tm_text("index",col="blue") 
    

}

                                        #plotOverlay()


    plotOverlayTmap()

    ##              x        y
    ##[1,] 33.602809 34.83600
    ##[2,]  9.485809 42.52425

    ##
    ##
