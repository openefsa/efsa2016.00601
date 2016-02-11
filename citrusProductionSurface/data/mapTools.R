library(pacman)
p_load(cartography)
p_load_gh("eblondel/cleangeo")
p_unload(raster)
p_load(rgdal)
p_load(Hmisc)

getNuts3Areas <-  function() {
    nuts.data <- data_frame(NUTS.Code=nuts3.spdf@data$id,id=row.names(nuts3.spdf@data))
    polygon_data <- data_frame(id=sapply(slot(nuts3.spdf, "polygons"), slot, "ID"),
                              Shape_Area= sapply(slot(nuts3.spdf, "polygons"), slot, "area"))
    nuts.area <- left_join(nuts.data,polygon_data)
    nuts.area
}

postProcessMap <- function(map,extent=raster::extent(-10,34,34,48)) {
    map <- spTransform(map,CRS("+proj=longlat +ellps=WGS84"))
    map <- raster::crop(map,extent)
    map
}

warnIfUnkownIds <- function(europe) {
    diff <- setdiff(europe$NUTS.Code,EU_NUTS.3@data$NUTS_ID)
    missingNutsIds <- europe %>%
        filter(NUTS.Code %in% diff) %>%
        select(NUTS.Code,NUTS3.name) %>% distinct()
    if(nrow(missingNutsIds)>0) {
        warning("The following nuts3 ids exist in the data but not in the map. So there values will not be shown: \n",missingNutsIds)
    }
}

plotCitrusMap <- function(europe,breaks) {
                               
    
    warnIfUnkownIds(europe)
    
                                        #         48
                                        #   -10         32
                                        #         35

                                        # osm
    extent <- raster::extent(-10,34,34,48) #small
    legend.pos = "right"
        
        

    EU_NUTS.0.tr <- postProcessMap(EU_NUTS.0,extent)
    EU_NUTS.3.tr <- postProcessMap(EU_NUTS.3,extent)
    
    

    
    world.eu <- world.eu[!world.eu@data$CNTR_ID %in%  as.character(EU_NUTS.0.tr@data$NUTS_ID),] %>%
        spTransform(CRS("+proj=longlat +ellps=WGS84")) %>%
        raster::crop(extent)
    
  
    cols <- carto.pal(pal1 = "red.pal",
                     n1 = length(breaks)) 
   
    europe <- data.frame(europe) %>%
        mutate(t_ha=ha/1000)

    layoutLayer(col = NA, coltitle = "black",
                sources = "", author = "",
                frame = T,
                title="",
                scale=NULL,
                extent=EU_NUTS.0.tr)


    plot(world.eu,col  = "#E3DEBF", border= NA, ,add=T)
    plot(EU_NUTS.0.tr,border = "grey20", lwd=0.5, add=TRUE)
    choroLayer(spdf = EU_NUTS.3.tr,
               df = europe,
               dfid="NUTS.Code",
               var="ha",                      
               breaks=breaks,
               col = cols,
               border = "grey10", # color of the polygons borders
               lwd = 0.1,, #0.05, # width of the borders
               legend.pos = legend.pos, # position of the legend
               legend.title.txt = "Citrus production \nsurface in ha", # title of the legend
               legend.values.rnd = 0, # number of decimal in the legend values
               add = T) # add the layer to the current plot
                                     
    
    plot(EU_NUTS.0.tr,border = "grey20", lwd=0.5, add=TRUE)

}
