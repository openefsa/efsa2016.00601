library(pacman)
if (!p_loaded(SparkR)) {
    .libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
    p_load(SparkR)
}
p_load(magrittr)
p_load(efsagis)
p_load(sp)
p_load(dplyr)
p_load(ggplot2)
p_load(tmap)
source("./readMagareyTable.R")
data(cgms25grid)

if (!exists("asco_df")) {
    sc <- SparkR::sparkR.init(master="local",sparkPackages = "com.databricks:spark-csv_2.10:1.3.0")
    sqlContext <- SparkR::sparkRSQL.init(sc)

    ##asco_df <- SparkR::read.df(sqlContext, "./infections/asco_3_15.csv",
    ##    source = "com.databricks.spark.csv",
    ##  header="true",
    ##inferSchema = "true")
    asco_df <- SparkR::read.parquet(sqlContext,"./infections/asco_3_15.paquet")
    SparkR::cache(asco_df)


    sum_by_rain <- asco_df %>%
        SparkR::groupBy(asco_df$ind_rain) %>%
        SparkR::summarize(sum(asco_df$INFECTION_EVENTS)) %>%
        SparkR::collect() %>%
        tbl_df()

    sum_by_rain_gridno <- asco_df %>%
        SparkR::groupBy(asco_df$GRID_NO,asco_df$ind_rain)  %>%
        SparkR::summarize(sum(asco_df$INFECTION_EVENTS)) %>%
        SparkR::collect() %>%
        tbl_df()

    sum_by_gridno_rain_month <- asco_df %>%
        SparkR::groupBy(asco_df$GRID_NO,asco_df$ind_rain,asco_df$month) %>%
        SparkR::summarize(sum(asco_df$INFECTION_EVENTS)) %>%
        SparkR::collect() %>%
        tbl_df()

}



                                        #write.df(df, "./output/", "com.databricks.spark.csv", "overwrite")
joinEfsaGridMagereyPts <- function(gridedValues,dataColumn) {
    crs <- sp::CRS("+proj=longlat +ellps=WGS84")

    mag2015table1 <- readMagTable1()
    coords <- mag2015table1 %>% dplyr::select(Lon,Lat) %>% data.frame()
    coordsData <- mag2015table1 %>% dplyr::select(Country,Location) %>% data.frame()

    mag2015pts <- SpatialPointsDataFrame(coords,coordsData,proj4string = crs)

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
                                        #write.csv(mag2015pts.eu,"./output/mag2015Tab1_rain.csv")
plotMag2015byRain <- function(mag2015pts.eu) {
    mag2015pts.eu$Location <- reorder(mag2015pts.eu$Location,mag2015pts.eu$`sum(INFECTION_EVENTS)`)
    mag2015pts.eu = mag2015pts.eu[with(mag2015pts.eu, order(ind_rain)), ] %>%
        dplyr::mutate(`rain indicator`=ifelse(ind_rain==0,"no rain","rain"))

                                        #png("./output/mag2015Tab1_rain.png")
    ggplot(mag2015pts.eu,aes(x=Location,y=`sum(INFECTION_EVENTS)`,fill=`rain indicator`)) +
        geom_bar(stat="identity") +
        coord_flip()
                                        #dev.off()
}

gridDataToSpdf <- function(data,col) {

                                        #data <- left_join(cgms25grid@data,data,by=c("Grid_Code"="GRID_NO")) %>%
                                        #    as.data.frame()
                                        #    column <- paste0("X",column)
                                        
    dataSpdf <-  append_data(cgms25grid,data,key.data = "GRID_NO",key.shp = "Grid_Code")
    dataSpdf <- dataSpdf[!is.na(dataSpdf[[col]]),]    
    dataSpdf
}

plotInfections <- function(data,ind_rain_,col) {

                                        #withRain <- sum_by_rain_gridno %>% dplyr::filter(ind_rain==1)
    filtered <- data %>%
        dplyr::filter(ind_rain==ind_rain_)


    filteredSpdf <- gridDataToSpdf(filtered,col)
    tm_shape(filteredSpdf) +
        tm_fill(col=col,

                breaks=seq(0,5000,500),
                contrast=c(0.2,1)

                ) +
    tm_borders()
    

}
