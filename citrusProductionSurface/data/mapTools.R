library(pacman)
p_load(cartography)
p_load_gh("eblondel/cleangeo")
p_unload(raster)

nuts3.spdf.tr <- spTransform(nuts3.spdf,CRS("+proj=longlat +ellps=WGS84"))


                                        #         48
                                        #   -10         32
                                        #         35
                                        # osm
                                        #extent <- raster::extent(-10,34,34,48) #small
extent <- raster::extent(-26,53,34,72) #big

nuts3.spdf.tr <- raster::crop(nuts3.spdf.tr,extent)

nuts3.spdf@data <- nuts3.spdf@data

nuts0.spdf.tr <- spTransform(nuts0.spdf,CRS("+proj=longlat +ellps=WGS84"))
nuts0.spdf.tr <- raster::crop(nuts0.spdf.tr,extent)
world.spdf.tr <- spTransform(world.spdf.tr,CRS("+proj=longlat +ellps=WGS84"))
world.spdf.tr.clean <- clgeo_Clean(world.spdf.tr , print.log = TRUE)
world.spdf.tr.clean <- raster::crop(world.spdf.tr.clean,extent)

cols <- carto.pal(pal1 = "red.pal", # first color gradient
                  n1 = 8) #, # number of colors in the first gradiant
                                        #pal2 = "red.pal", # second color gradient
                                        #n2 = 4) # number of colors in the second gradiant

plotCitrusMap <- function(europe) {
    europe <- data.frame(europe) %>%
        mutate(t_ha=ha/1000)
                                        #opar <- par(mar = c(0,0,1.2,0))

    opar <- par(mar = c(0,0,0,0))
    layoutLayer(title = "Citrus production surface per NUTS3 area", # title of the map
                scale = NULL,
                coltitle = "white", # color of the title
                frame = F,  # no frame around the map
                bg = "#A6CAE0",
                author = "Author: EFSA",
                sources = "Sources: EU member states official statistics",
                extent=world.spdf.tr.clean
                )

    

    plot(world.spdf.tr.clean,col  = "#E3DEBF", border=NA,add=T)
    
    choroLayer(spdf = nuts3.spdf.tr, # SpatialPolygonsDataFrame of the regions
               df = europe, # data frame with compound annual growth rate
               dfid="NUTS.Code",
               var = "t_ha", # compound annual growth rate field in df
               breaks = c(0,.1,1,2,5,10,20,30,40), # list of breaks
               col = cols,
               border = NA, #"grey10", # color of the polygons borders
               lwd = 0.05,, #0.05, # width of the borders
               legend.pos = "left", # position of the legend
               legend.title.txt = "Citrus production surface \nin thousand ha", # title of the legend
               legend.values.rnd = 2, # number of decimal in the legend values
               add = T) # add the layer to the current plot
    
    plot(nuts0.spdf.tr,border = "grey20", lwd=0.5, add=TRUE)

    totals <- europe %>%
        group_by(country) %>%
        summarize(total=paste0(as.character(round(sum(t_ha)),0))) %>%
        rename(id=country) %>%
        data.frame()
    
    labelLayer(spdf = nuts0.spdf.tr, # SpatialPolygonsDataFrame used to plot he labels
               df = totals, # data frame containing the lables
               txt = "total", # label field in df
               col = "black",  
               cex = 1, # size of the labels
               font = 2) # label font
    
}
