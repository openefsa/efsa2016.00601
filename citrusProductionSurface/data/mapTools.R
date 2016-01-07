p_load(cartography)

nuts3.spdf.tr <- spTransform(nuts3.spdf,CRS("+proj=longlat +ellps=WGS84"))
nutsr.spdf <- nuts3.spdf@data
nuts0.spdf.tr <- spTransform(nuts0.spdf,CRS("+proj=longlat +ellps=WGS84"))
world.spdf.tr <- spTransform(world.spdf,CRS("+proj=longlat +ellps=WGS84"))

cols <- carto.pal(pal1 = "green.pal", # first color gradient
                  n1 = 2, # number of colors in the first gradiant
                  pal2 = "red.pal", # second color gradient
                  n2 = 4) # number of colors in the second gradiant

plotCitrusMap <- function(europe) {
    europe <- data.frame(europe)
    opar <- par(mar = c(0,0,1.2,0))


    layoutLayer(title = "Citrus production", # title of the map
                scale = NULL,
                coltitle = "white", # color of the title
                frame = FALSE,  # no frame around the map
                bg = "#A6CAE0",
                author = "Author: EFSA",
                sources = "Sources: EU member states official statistics",
                extent=nuts3.spdf.tr
                )
    plot(world.spdf.tr,col  = "#E3DEBF", border=NA, add=TRUE)

    choroLayer(spdf = nuts3.spdf.tr, # SpatialPolygonsDataFrame of the regions
               df = europe, # data frame with compound annual growth rate
               dfid="NUTS.Code",
               var = "ha", # compound annual growth rate field in df
               breaks = c(0,100,1000,2000,5000,10000,20000), # list of breaks
               col = cols,
               border = "grey40", # color of the polygons borders
               lwd = .2, # width of the borders
               legend.pos = "right", # position of the legend
               legend.title.txt = "Citrus production in ha", # title of the legend
               legend.values.rnd = 2, # number of decimal in the legend values
               add = T) # add the layer to the current plot
    plot(nuts0.spdf.tr,border = "grey20", lwd=0.75, add=TRUE)


    totals <- europe %>%
        group_by(country) %>%
        summarize(total=paste0(as.character(round(sum(ha) / 1000),0),"t ha")) %>%
        rename(id=country) %>%
        data.frame()
    
    labelLayer(spdf = nuts0.spdf.tr, # SpatialPolygonsDataFrame used to plot he labels
               df = totals, # data frame containing the lables
               txt = "total", # label field in df
               col = "#690409", # color of the labels
               cex = 0.9, # size of the labels
               font = 2) # label font
}
