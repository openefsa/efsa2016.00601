testLayer1 <- function() {
    cl <- prepare_citrus_layer()
    mag <- magarey_shp()
    
    leaflet(cl) %>%
        addTiles() %>%
        addPolygons(color=~colorQuantile("Reds",cl$ha)(ha),
                    fillOpacity = 0.8) %>%
        addCircleMarkers(data=mag$spts@data,
                         radius = mag$spts$asco_days_average,
                         stroke = F,
                         fillOpacity = 1,
                         popup = ~paste0("Asco PAT:     ",asco_pat_average,"<br/>",
                                         "Asco % years: ",asco_suit_years,"<br/>",
                                         "Pyc avg:      ",pyc_average,"<br/>",
                                         "Pyc % years:  ",pyc_suit_years,"<br/>"))
    
    
}
testLayer2 <- function() {
    leaflet(combined_koppen()) %>%
        addTiles() %>%
        addRasterImage(koppen,opacity = 0.5)

}

testLayer3 <- function() {
    cl <- prepare_citrus_layer()
    koppen <- combined_koppen()
    mag <- magarey_shp()
    leaflet(cl) %>%
        addPolygons(color=~colorQuantile("Reds",cl$ha)(ha)) %>%
        addCircleMarkers(data=mag$spts,radius = mag$spts$asco_days_average) %>%
        addRasterImage(koppen,opacity = 0.5)

}

testLayer4 <- function() {
    palette <- gplots::col2hex(c("beige","lightblue","green","yellow","orange","red"))
    fixedBreaks=c(-Inf,0.01,0.5,1,5,10,Inf)
    color <- leaflet::colorBin(palette,inf@data$Jun,fixedBreaks)(inf@data$Jun)
    leaflet::leaflet(inf) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(color=color,fill = T) %>%
        leaflet::addPolygons(data=euNuts0(),fill=F,weight = 1,color = "black") %>%
        leaflet::addPolygons(data=euNuts3(),fill=F,weight = 1,color = "#333333") 
        

}

testLayer5 <- function() {
    cl <- prepare_citrus_layer()
    mag <- magarey_shp()
    koppen <- combined_koppen()
    inf <- prepare_infection_sp("extdata/spores2/Asco_3_15_Model_AVG.xlsx","Sep") %>%
        efsagis::postProcessMap(wgs84(),euExtent())
    palette <- gplots::col2hex(c("beige","lightblue","green","yellow","orange","red"))
    fixedBreaks=c(-Inf,0.01,0.5,1,5,10,Inf)
    infColor <- leaflet::colorBin(palette,inf@data$Jun,fixedBreaks)(inf@data$Jun)

    leaflet::leaflet(cl) %>%
        leaflet::addTiles(group="Topo") %>%
        leaflet::addPolygons(data=euNuts0(),fill=F,weight = 3,color = "black",group="Nuts") %>%
        leaflet::addPolygons(data=euNuts3(),fill=F,weight = 1,color = "#333333",group="Nuts") %>%
    leaflet::addPolygons(color=~leaflet::colorQuantile("Reds",cl$ha)(ha),fillOpacity = 0.8,group="Citrus surface") %>%
        leaflet::addPolygons(data=inf,color=infColor,fill = T,group="EFSA 2014 infection events",
                             fillOpacity = 0.5,stroke = F,

                             ) %>%

    leaflet::addCircleMarkers(data=mag$spts@data,
                              radius = mag$spts$asco_days_average / 2,
                              color = leaflet::colorNumeric("Blues",mag$spts$asco_suit_years)(mag$spts$asco_suit_years),
                              stroke = F,
                              fillOpacity = 1,
                              popup = ~paste0("Asco PAT:     ",asco_pat_average,"<br/>",
                                              "Asco % years: ",asco_suit_years,"<br/>",
                                              "Pyc avg:      ",pyc_average,"<br/>",
                                              "Pyc % years:  ",pyc_suit_years,"<br/>"),
                              group="Magarey 2015 - ascospores"
                              ) %>%

    leaflet::addCircleMarkers(data=mag$spts@data,
                              radius = mag$spts$pyc_average / 3,
                              color = leaflet::colorNumeric("Blues",mag$spts$pyc_suit_years)(mag$spts$pyc_suit_years),
                              stroke = F,
                              fillOpacity = 1,
                              group="Magarey 2015 - pycnidiospores"
                              ) %>%

    leaflet::addRasterImage(koppen,opacity = 0.5,group="Koppen climate zones") %>%
    leaflet::addLayersControl(
        baseGroups = c("Topo", "Nuts"),
        overlayGroups = c("Citrus surface",
                          "Magarey 2015 - ascospores",
                          "Magarey 2015 - pycnidiospores",
                          "Koppen climate zones",
                          "EFSA 2014 infection events"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
    )

    
    
}
