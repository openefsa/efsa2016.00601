
clipToEurope <- function(spdf) {
    spdf.ps <- SpatialPolygons2PolySet(spdf)
                                        #spdf.ps.clipped <- clipPolys(spdf.ps, xlim = c(-20, 40), ylim = c(30, 72))
    spdf.ps.clipped <- clipPolys(spdf.ps, xlim = c(-25, 45), ylim = c(41, 70))
    PolySet2SpatialPolygons(spdf.ps.clipped, close_polys=TRUE)

}

nuts2013 <- readOGR("geo/NUTS_2013_60M_SH/data/","NUTS_RG_60M_2013")
countryMap <- nuts2013[which(nuts2013@data$STAT_LEVL_ == 0),]
nuts2013Map <- clipToEurope(nuts2013)




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
                extent=nuts2013Map
                )



    choroLayer(spdf = nuts2013, # SpatialPolygonsDataFrame of the regions
                                        #spdfid="NUTS_BN_ID",
               df = europe, # data frame with compound annual growth rate
               dfid="NUTS.Code",
               var = "ha", # compound annual growth rate field in df
               breaks = c(0,100,100,2000,5000,10000,20000), # list of breaks
                                        #col = cols, # colors
               
               col = cols,
               border = "grey40", # color of the polygons borders
               lwd = .2, # width of the borders
               legend.pos = "right", # position of the legend
               legend.title.txt = "Citrus production in ha", # title of the legend
               legend.values.rnd = 2, # number of decimal in the legend values
               add = T) # add the layer to the current plot
    plot(countryMap,border = "grey20", lwd=1, add=TRUE)

}
