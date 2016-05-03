
#'@importFrom dplyr distinct
warnIfUnkownIds <- function(europe) {
    diff <- setdiff(europe$NUTS.Code,euNuts3()@data$NUTS_ID)
    missingNutsIds <- europe %>%
        filter(NUTS.Code %in% diff) %>%
        select(NUTS.Code,NUTS3.name) %>% distinct()
    if(nrow(missingNutsIds)>0) {
        warning("The following nuts3 ids exist in the data but not in the map. So there values will not be shown: \n",missingNutsIds)
    }
}


#' @export
euNuts <- function() {
    NUTS_01M_2013 %>%
        efsagis::retainEU28()

}

#' @export
euExtent <- function() {
    raster::extent(-10,34,34,48) #small
}

#' @export
euNuts0 <- function() {
    euNuts()[euNuts()@data$STAT_LEVL_==0,] %>%
        efsagis::postProcessMap(wgs84(),euExtent())
}

#' @export
euNuts3 <- function() {

    euNuts()[euNuts()@data$STAT_LEVL_==3,] %>%
        efsagis::postProcessMap(wgs84(),euExtent())

}

#' @export
worldEu <- function() {
    world.eu <- efsagis::postProcessMap(countries_01M_2013[!countries_01M_2013@data$CNTR_ID %in%  as.character(euNuts0()@data$NUTS_ID),],crs=wgs84(),extent = euExtent())
}

#' @export
wgs84 <- function() {
    sp::CRS("+proj=longlat +ellps=WGS84")
}


#' @export
latestData <- function() {

    data <- euCitrusSurface %>%
        group_by(NUTS.Code) %>%
        mutate(max_year=max(year)) %>%
        filter(year==max_year) %>%
        ungroup() %>%
        select(-max_year)
    data
}


#' @export
base_layer <- function() {
    tmap::tm_shape(euNuts0()) +
        tmap::tm_borders(lwd = 0)
}

#' @export
nuts0_layer <- function(alpha=1) {
    tmap::tm_shape(euNuts0()) +
        tmap::tm_borders(lwd=0.5,col="grey20",alpha = alpha)
}
#' @export
nuts3_layer <- function(alpha=1) {
    tmap::tm_shape(euNuts3()) +
        tmap::tm_borders(lwd=0.1,col="grey10",alpha=alpha)

}

#' @export
prepare_citrus_layer <- function() {
    data <- latestData()
    warnIfUnkownIds(data)

    EU_NUTS.3.ha <- euNuts3()
    newData <- euNuts3()@data %>% left_join(data,by=c("NUTS_ID"="NUTS.Code"))

    EU_NUTS.3.ha@data <- newData
    EU_NUTS.3.ha <- EU_NUTS.3.ha[!is.na(EU_NUTS.3.ha@data$ha),]
    EU_NUTS.3.ha
}

#' @export
plot_citrus_density <- function(add=F) {
    spdf <- prepare_citrus_layer()
    brks <- quantile(spdf@data$citrus_density)
    dens <- (2:length(brks))*6
    plot(spdf,density = dens[findInterval(spdf@data$citrus_density,brks,all.inside = T)],add=add)

}

#' @export
plot_citrus_koppen_schraffiert <-  function() {

    koppenCombined <- combined_koppen(whichToShow=c(5,6))

    plot(koppenCombined,
         col= c("#F6A200","#FDDA62","#FCFE04","#CECC08"),
         breaks=c(4,5,6),
         axis.args = list(at=c(5,6), labels=c("BSh","BSk")),
         axes=F,
         frame.plot = F
         )
    plot_citrus_density(add = T)
    plot(euNuts0(),add=T)

}

#' @export
citrusSurface_outline_layer <- function() {
    require(geos)
    require(maptools)
    citrusMap <- prepare_citrus_layer()
    coords <- coordinates(citrusMap)
    ID <- cut(coords[,1], range(coords[,1]), include.lowest=TRUE)
    outline <- maptools::unionSpatialPolygons(citrusMap, ID)
    tmap::tm_shape(outline) +
        tmap::tm_borders(lwd=1,col="darkmagenta")
}

#' @export
citrusSurface_dots_layer <- function() {
    citrusMap <- prepare_citrus_layer()

    total <- sum(citrusMap@data$ha)

    shp <- tmap::sample_dots(citrusMap,
                            shp.id = "NUTS_ID",
                            w=100,
                            vars="ha",
                            units="ha",
                            units.size=1,
                            convert2density = F,nrow=400,ncol=1300,
                            npop=total,
                            total.area=sum(citrusMap@data$Shape_Area_ha))


    tmap::tm_shape(shp)+
        tmap::tm_dots(size = 0.01,col="red")

}



#' @export
citrusSurface_layer <- function(column,alpha=1) {
    if (column=="citrus_density") {
        title=paste0("Citrus production area\ndensity(ha/km^2)")
        breaks=NULL
    } else  if (column=="ha") {
        title="Citrus production area \n (ha)"
        breaks=c(1,500,2500,5000,10000,25000,Inf)
    }
    tmap::tm_shape(prepare_citrus_layer()) +
        tmap::tm_fill(column,
                      palette = "Reds",
                      breaks = breaks,
                      textNA = NA,
                      alpha = alpha,
                      legend.format = list(scientific=T,format="f"),
                      contrast=c(0.2,1),
                      title=title)

}

magarey_shp <- function() {

    europe <- latestData()
    mag2015table1 <- readMagTable1()

    pts <- mag2015table1 %>% select(Lon,Lat) %>% data.frame() %>% sp::SpatialPoints(wgs84())


    match <- (sp::over(pts,euNuts3()))
    mag2015Data <- bind_cols(mag2015table1,match)  %>%
        filter(!is.na(NUTS_ID)) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area) %>%
        left_join((europe %>% select(NUTS.Code,NUTS3.name,ha)),by=c("NUTS_ID"="NUTS.Code"))

    mag2015table2 <- readr::read_csv(system.file("extdata/mag2015_table2.csv",package = "efsa2016.00601"),skip=5) %>%
        setNames(c("Location","Country","prevalence","asco_pat_average","asco_days_average","asco_days_stddev","asco_suit_years","pyc_average","pyc_stddev","pyc_suit_years")) %>%
                                        #select(Location,asco_days_average) %>%
        mutate(Location=gsub("<comma>",".",Location)) %>%
        mutate(Location=gsub("-","/",Location,fixed=T),
               Location=gsub("Larnaca Cyprus","Larnaca",Location),
               Location=gsub("^Palermo/Punta$","Palermo/Punta Raisi",Location))


    mag2015Data <- mag2015Data %>% left_join(mag2015table2,by=c("Location"))






                                        #write.csv(mag2015Data,"mag2015Nuts3.csv")

    lonLat <- mag2015Data %>% select(Lon,Lat) %>% data.frame()
    invisible(sp::plot(euNuts0(),col = "white",type="n",lwd=0.001)) # to make pointLabel happy
    xy <- maptools::pointLabel(lonLat$Lon,lonLat$Lat,
                               labels = paste0(seq_along(lonLat$Lon)),
                               doPlot = F,
                               offset=0,
                               cex=1)


    spts <- sp::SpatialPointsDataFrame(coords=lonLat,
                                      data=mag2015Data %>% data.frame(),
                                      proj4string = wgs84())

    text_sp <- sp::SpatialPointsDataFrame(coords=xy,data=mag2015Data %>% data.frame(),
                                         proj4string = wgs84())

    list(spts=spts,text_sp=text_sp)
}

#' @importFrom dplyr bind_cols
#' @export
magarey_layer <- function(column_size,title_size,
                          column_text,title_text,
                          scale,style=NULL,alpha=1) {

    shps <- magarey_shp()
    spts <- shps$spts
    text_sp <- shps$text_sp

    tmap::tm_shape(spts) +
        tmap::tm_bubbles(size=column_size,
                         border.col = "blue",
                         alpha=alpha,
                         col=c("white"),
                         title.size = title_size,
                         scale=scale,
                         #style = style,
                         legend.format = list(scientific=T,format="f"),
                         sizes.legend=c(0,10,20,30,40,50,60),


                         ) +
    tmap::tm_shape(spts) +
    tmap::tm_dots(size=0.05,col="blue") +
    tmap::tm_shape(text_sp) +
    tmap::tm_text(column_text,col=column_text,palette = c("blue"),breaks=c(0,1),
                  title.col = "Magarey2015 score",
                  labels=c(title_text),
                  labels.text = "xx.x")
}


#' @export
aschmann_layer <- function(alpha=1) {
    aschmann <- raster::raster(system.file("extdata/martinez2015/rasters/mediterranean/ASCHMANN/Aschmann_med.grd",
                                          package = "efsa2016.00601")) %>%
        raster::crop(euExtent())
    tmap::tm_shape(aschmann) +
        tmap::tm_raster(alpha = alpha,legend.show = T,style="cat",
                        palette=c("grey"),
                        textNA = NA,
                        labels=c("Aschmann's mediteranea type"))
}
## type: med,Bsk_Bsh,Csa_Csb
#' @export
koppen_layer <- function(type,alpha=0.2,palette=NULL) {
    koppen <- raster::raster(system.file(sprintf("extdata/martinez2015/rasters/mediterranean/KOPPEN/koppen_%s.grd",type),package = "efsa2016.00601")) %>%
        raster::crop(euExtent())

    tmap::tm_shape(koppen) +
        tmap::tm_raster(alpha = alpha,legend.show = T,palette = palette,
                        textNA=NA)

}

#' @export
combined_koppen <- function(whichToShow=c(5,6,8,9)) {
    koppen1 <- raster::raster(system.file(sprintf("extdata/martinez2015/rasters/mediterranean/KOPPEN/koppen_%s.grd","Bsk_Bsh"),package = "efsa2016.00601")) %>%
        raster::crop(euExtent())

    koppen2 <- raster::raster(system.file(sprintf("extdata/martinez2015/rasters/mediterranean/KOPPEN/koppen_%s.grd","Csa_Csb"),package = "efsa2016.00601")) %>%
        raster::crop(euExtent())
    koppen1Data <- raster::getValues(koppen1)
    koppen2Data <- raster::getValues(koppen2)
    koppenCombinedData <- ifelse(is.na(koppen1Data),0,koppen1Data) + ifelse(is.na(koppen2Data),0,koppen2Data)
    koppenCombinedData <- ifelse(koppenCombinedData==0,NA,koppenCombinedData)
    koppenCombinedData <- ifelse(koppenCombinedData %in% whichToShow,koppenCombinedData,NA)
    koppenCombined <- koppen1
    koppenCombined <- raster::setValues(koppenCombined,koppenCombinedData)
    koppenCombined
}
##
##  labels=c("BSh","BSk","CSa","CSb"),
#' @export
combined_koppen_layer <- function(alpha=1,whichToShow=c(5,6,8,9)) {
    koppenCombined <- combined_koppen(whichToShow)
    tmap::tm_shape(koppenCombined) +
        tmap::tm_raster(
            palette= c("#F6A200","#FDDA62","#FCFE04","#CECC08"),
            style = "cat",
                                        #colorNA = "#FFFFFF00",
            alpha = alpha,
            labels=c("BSh","BSk","CSa","CSb"),
            title="Köppen–Geiger classification",
            textNA = NA,
            legend.show = T)

}


#' @export
prepare_infection_sp <- function(fileName,column) {
    data <- readxl::read_excel(system.file(fileName,package = "efsa2016.00601"))
                                        #data <- left_join(cgms25grid@data,asco,by=c("Grid_Code"="GRID_NO"))
    data <- left_join(cgms25grid@data,data,by=c("Grid_Code"="GRID_NO")) %>%
        data.frame()
                                        #    column <- paste0("X",column)
    dataSpdf <- cgms25grid
    dataSpdf@data <- data
    dataSpdf <- dataSpdf[!is.na(dataSpdf[[column]]),]
    dataSpdf

}


#' @export
infection_layer <- function(fileName,column,title,alpha=1) {
    tmap::tm_shape(prepare_infection_sp(fileName,column)) +
        tmap::tm_polygons(column,border.col = NULL,alpha=alpha,border.alpha = alpha,
                          palette=paste0(gplots::col2hex(c("white","lightblue","green","yellow","orange","red")),"FF"),
                          breaks=c(-Inf,0.01,0.5,1,5,10,Inf),
                          title=title
                          )

}

#' @export
plot_infection <- function(fileName,column){
    dataSpdf <-  prepare_infection_sp(fileName,column) %>%
        postProcessMap(wgs84(),euExtent())
    intervals <- classInt::classIntervals(dataSpdf[[column]],style="fixed",
                                         fixedBreaks=c(-Inf,0.01,0.5,1,5,10,Inf))
    palette <- gplots::col2hex(c("beige","lightblue","green","yellow","orange","red"))

    colors <- classInt::findColours(intervals,palette)
    plot(dataSpdf,col=colors,border="transparent")


}

