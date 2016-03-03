library(pacman)
p_load(RPostgreSQL)
p_load(efsagis)
p_load(sp)
p_load(dplyr)
p_load(raster)
p_load(ggplot2)
source("./readMagareyTable.R")


crs <- CRS("+proj=longlat +ellps=WGS84")

db <- src_postgres(dbname = 'ascodb', user = 'postgres', password = 'postgres',host='localhost')
limit <- "ALL"
asco_3_15 <- tbl(db, sql(sprintf('SELECT * FROM asco3_15 limit %s',limit)))

sum_by_rain <-  asco_3_15 %>%
    group_by(ind_rain) %>%
    summarize(sum(infection_events))


sum_by_rain_gridno <-  asco_3_15 %>%
    group_by(ind_rain,grid_no) %>%
    summarize(sum(infection_events)) %>%
    collect()

mag2015table1 <- readMagTable1()
coords <- mag2015table1 %>% dplyr::select(Lon,Lat) %>% data.frame()
coordsData <- mag2015table1 %>% dplyr::select(Country,Location) %>% data.frame()

mag2015pts <- SpatialPointsDataFrame(coords,coordsData,proj4string = crs)

crsGrid <- crs(cgms25grid)
mag2015pts <- sp::spTransform(mag2015pts,crsGrid)
match <- over(mag2015pts,cgms25grid)
mag2015pts.eu <- 
    bind_cols(mag2015table1,match) %>%
    filter(!is.na(Grid_Code)) %>%
    left_join(sum_by_rain_gridno,by=c("Grid_Code"="grid_no")) %>%
    dplyr::select(Country,Location,Prevalence,Lat,Lon,ind_rain,`sum(infection_events)`) 

write.csv(mag2015pts.eu,"mag2015Tab1_rain.csv")

mag2015pts.eu$Location <- reorder(mag2015pts.eu$Location,mag2015pts.eu$`sum(infection_events)`)
mag2015pts.eu = mag2015pts.eu[with(mag2015pts.eu, order(ind_rain)), ] %>%
    mutate(`rain indicator`=ifelse(ind_rain==0,"no rain","rain"))

png("mag2015Tab1_rain.png")
ggplot(mag2015pts.eu,aes(x=Location,y=`sum(infection_events)`,fill=`rain indicator`)) +
    geom_bar(stat="identity") +
    coord_flip()
dev.off()
