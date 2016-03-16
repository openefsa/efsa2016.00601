
#' @export
getAsco_3_15 <- function(sqlContext) {
    asco_df <- SparkR::read.parquet(sqlContext,system.file("extdata/asco_3_15.paquet",package = "efsa2016.00601"))
    dummy <- SparkR::cache(asco_df)
    asco_df
    

}

#' @export
sum_by_rain <- function(sqlContext) {
    asco_df <- getAsco_3_15(sqlContext)
    
    asco_df %>%
        SparkR::groupBy(asco_df$ind_rain) %>%
        SparkR::summarize(sum(asco_df$INFECTION_EVENTS)) %>%
        SparkR::collect() %>%
        tbl_df()
}

#' @export
sum_by_rain_gridno <- function(sqlContext) {
    asco_df <- getAsco_3_15(sqlContext)

    asco_df %>%
        SparkR::groupBy(asco_df$GRID_NO,asco_df$ind_rain)  %>%
        SparkR::summarize(sum(asco_df$INFECTION_EVENTS)) %>%
        SparkR::collect() %>%
        tbl_df()
}

#' @export
sum_by_gridno_rain_month <- function(sqlContext) {
    asco_df <- getAsco_3_15(sqlContext)
    asco_df %>%
        SparkR::groupBy(asco_df$GRID_NO,asco_df$ind_rain,asco_df$month) %>%
        SparkR::summarize(sum(asco_df$INFECTION_EVENTS)) %>%
        SparkR::collect() %>%
        tbl_df()

}



                                        #write.df(df, "./output/", "com.databricks.spark.csv", "overwrite")
#' @export
joinEfsaGridMagereyPts <- function(gridedValues,dataColumn) {
    crs <- sp::CRS("+proj=longlat +ellps=WGS84")

    mag2015table1 <- readMagTable1()
    coords <- mag2015table1 %>% dplyr::select(Lon,Lat) %>% data.frame()
    coordsData <- mag2015table1 %>% dplyr::select(Country,Location) %>% data.frame()

    mag2015pts <- sp::SpatialPointsDataFrame(coords,coordsData,proj4string = crs)

    crsGrid <- raster::crs(cgms25grid)
    mag2015pts <- sp::spTransform(mag2015pts,crsGrid)
    match <- sp::over(mag2015pts,cgms25grid)
    mag2015pts.eu <- 
        dplyr::bind_cols(mag2015table1,match) %>%
        dplyr::filter(!is.na(Grid_Code)) %>%
        dplyr::left_join(gridedValues,by=c("Grid_Code"="GRID_NO")) %>%
        dplyr::select_("Country","Location","Prevalence","Lat","Lon","ind_rain",dataColumn)
    mag2015pts.eu

}
                                      
#' @export
plotMag2015byRain <- function(mag2015pts.eu) {
    mag2015pts.eu$Location <- reorder(mag2015pts.eu$Location,mag2015pts.eu$infection_events)
    mag2015pts.eu = mag2015pts.eu[with(mag2015pts.eu, order(ind_rain)), ] %>%
        dplyr::mutate(`rain indicator`=ifelse(ind_rain==0,
                                              "infections on days without rain",
                                              "infections on days with rain"))

                                       
    ggplot2::ggplot(mag2015pts.eu,ggplot2::aes(x=Location,y=infection_events,fill=`rain indicator`)) +
        ggplot2::geom_bar(stat="identity") +
        ggplot2::coord_flip() +
        ggplot2::theme(legend.position="bottom")
                                      
}

gridDataToSpdf <- function(data,col) {
                                        
    dataSpdf <-  tmap::append_data(cgms25grid,data,key.data = "GRID_NO",key.shp = "Grid_Code")
    dataSpdf <- dataSpdf[!is.na(dataSpdf[[col]]),]    
    dataSpdf
}

#' @export
plotInfections <- function(data,ind_rain_,col) {

                                        #withRain <- sum_by_rain_gridno %>% dplyr::filter(ind_rain==1)
    filtered <- data %>%
        dplyr::filter(ind_rain==ind_rain_)


    filteredSpdf <- gridDataToSpdf(filtered,col)
    tmap::tm_shape(filteredSpdf) +
        tmap::tm_fill(col=col,

                      breaks=seq(0,5000,500),
                      contrast=c(0.3,1),
                      legend.hist = T,

                      ) +
        tmap::tm_borders() +
    tmap::tm_format_Europe()
    

}
