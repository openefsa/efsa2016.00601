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



warnIfUnkownIds <- function(europe) {
    diff <- setdiff(europe$NUTS.Code,EU_NUTS.3@data$NUTS_ID)
    missingNutsIds <- europe %>%
        filter(NUTS.Code %in% diff) %>%
        select(NUTS.Code,NUTS3.name) %>% distinct()
    if(nrow(missingNutsIds)>0) {
        warning("The following nuts3 ids exist in the data but not in the map. So there values will not be shown: \n",missingNutsIds)
    }
}

plotCitrusMap <- function(europe,large=F,breaks,var) {
    EU_NUTS.3.tr <- spTransform(EU_NUTS.3,CRS("+proj=longlat +ellps=WGS84"))
    EU_NUTS.0.tr <- spTransform(EU_NUTS.0,CRS("+proj=longlat +ellps=WGS84"))
    
    warnIfUnkownIds(europe)
    
                                        #         48
                                        #   -10         32
                                        #         35
                                        # osm
    if(large) {
        extent <- raster::extent(-26,53,34,72) #large
        legend.pos = "left"
    } else {
        extent <- raster::extent(-10,34,34,48) #small
        legend.pos = "right"
    }
                                     
    EU_NUTS.3.tr <- raster::crop(EU_NUTS.3.tr,extent)
    EU_NUTS.0.tr <- raster::crop(EU_NUTS.0.tr,extent)

    
    world.eu <- readOGR(dsn = "./geo/CNTR_01M_2013_SH/data", layer = "CNTR_RG_01M_2013")
    world.eu <- world.eu[!world.eu@data$CNTR_ID %in%  as.character(EU_NUTS.0.tr@data$NUTS_ID),] %>%
        spTransform(CRS("+proj=longlat +ellps=WGS84")) %>%
        raster::crop(extent)
    
  
    cols <- carto.pal(pal1 = "red.pal",
                     n1 = 10) 
   
    europe <- data.frame(europe) %>%
        mutate(t_ha=ha/1000)

    if(large) {
        layoutLayer(title = "Citrus production surface per NUTS3 area", 
                    scale = NULL,
                    coltitle = "white", # color of the title
                    frame = F,  # no frame around the map
                    bg = "#A6CAE0",
                    author = "Author: EFSA",
                    sources = "Sources: EU member states official statistics",
                    extent=EU_NUTS.0.tr
                    ) 
    } else {
        layoutLayer(col = NA, coltitle = "black",
                    sources = "", author = "",
                    frame = FALSE,
                    title="",
                    scale=NULL,
                    extent=EU_NUTS.0.tr)
    }

    plot(world.eu,col  = "#E3DEBF", border= NA, ,add=T)
    plot(EU_NUTS.0.tr,border = "grey20", lwd=0.5, add=TRUE)
    choroLayer(spdf = EU_NUTS.3.tr,
               df = europe,
               dfid="NUTS.Code",
               var=var,                      
               breaks=breaks,
               col = cols,
               border = "grey10", # color of the polygons borders
               lwd = 0.1,, #0.05, # width of the borders
               legend.pos = legend.pos, # position of the legend
               legend.title.txt = "Citrus production surface \nin thousand ha", # title of the legend
               legend.values.rnd = 5, # number of decimal in the legend values
               add = T) # add the layer to the current plot
                                     
    
    plot(EU_NUTS.0.tr,border = "grey20", lwd=0.5, add=TRUE)

    if (large) {
        totals <- europe %>%
            group_by(country) %>%
            summarize(total=paste0(as.character(round(sum(t_ha)),0))) %>%
            rename(id=country) %>%
            data.frame()
        
        labelLayer(spdf = EU_NUTS.0.tr, # SpatialPolygonsDataFrame used to plot he labels
                   df = totals, # data frame containing the lables
                   txt = "total", # label field in df
                   col = "black",  
                   cex = 1, # size of the labels
                   font = 2) # label font
    }
}
