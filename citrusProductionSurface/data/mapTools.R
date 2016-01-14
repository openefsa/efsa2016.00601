library(pacman)
p_load(cartography)
p_load_gh("eblondel/cleangeo")
p_unload(raster)

getNuts3Areas <-  function() {
    nuts.data <- data_frame(NUTS.Code=nuts3.spdf@data$id,id=row.names(nuts3.spdf@data))
    polygon_data <- data_frame(id=sapply(slot(nuts3.spdf, "polygons"), slot, "ID"),
                               Shape_Area= sapply(slot(nuts3.spdf, "polygons"), slot, "area"))
    nuts.area <- left_join(nuts.data,polygon_data)
    nuts.area
}


EU_NUTS <- readOGR(dsn = "./geo/NUTS_2013_60M_SH/data", layer = "NUTS_RG_60M_2013")
EU_NUTS.0 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==0,]
EU_NUTS.3 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==3,]


warnIfUnkownIds <- function(europe) {
    diff <- setdiff(europe$NUTS.Code,EU_NUTS.3@data$NUTS_ID)
    missingNutsIds <- europe %>%
        filter(NUTS.Code %in% diff) %>%
        select(NUTS.Code,NUTS3.name) %>% distinct()  
    warning("The following nuts3 ids exist in the data but not in the map. So there values will not be shown.",missingNutsIds)
}

plotCitrusMap <- function(europe,large=F) {
    EU_NUTS.3.tr <- spTransform(EU_NUTS.3,CRS("+proj=longlat +ellps=WGS84"))

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

                                       

    EU_NUTS.0.tr <- spTransform(EU_NUTS.0,CRS("+proj=longlat +ellps=WGS84"))
    EU_NUTS.0.tr <- raster::crop(EU_NUTS.0.tr,extent)


    world.eu <- readOGR(dsn = "./geo/CNTR_60M_2013_SH/data", layer = "CNTR_RG_60M_2013") %>%
        spTransform(CRS("+proj=longlat +ellps=WGS84")) %>%
        raster::crop(extent)
    world.eu <- world.eu[!world.eu@data$CNTR_ID %in%  as.character(EU_NUTS.0.tr@data$NUTS_ID),]
    
  
    cols <- carto.pal(pal1 = "red.pal", # first color gradient
                      n1 = 8) #, # number of colors in the first gradiant
                                        #pal2 = "red.pal", # second color gradient
                                        #n2 = 4) # number of colors in the second gradiant


    
    europe <- data.frame(europe) %>%
        mutate(t_ha=ha/1000)
                                     

    opar <- par(mar = c(0,0,0,0))
    if(large) {
        layoutLayer(title = "Citrus production surface per NUTS3 area", # title of the map
                    scale = NULL,
                    coltitle = "white", # color of the title
                    frame = F,  # no frame around the map
                    bg = "#A6CAE0",
                    author = "Author: EFSA",
                    sources = "Sources: EU member states official statistics",
                    extent=world.spdf.tr.clean
                    )
    }
    
    plot(world.eu,col  = "#E3DEBF", border= NA, ,add=large)
    
    choroLayer(spdf = EU_NUTS.3.tr, # SpatialPolygonsDataFrame of the regions
               df = europe, # data frame with compound annual growth rate
               dfid="NUTS.Code",
               var = "t_ha", # compound annual growth rate field in df
               breaks = c(0,.1,1,2,5,10,20,30,40), # list of breaks
               col = cols,
               border = "grey10", # color of the polygons borders
               lwd = 0.1,, #0.05, # width of the borders
               legend.pos = legend.pos, # position of the legend
               legend.title.txt = "Citrus production surface \nin thousand ha", # title of the legend
               legend.values.rnd = 2, # number of decimal in the legend values
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
