library(pacman)
p_load(raster)

p_load(cartography)
p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)
p_load(readr)
p_load(memoise)
p_load(maptools)
p_load(tmap)
p_load_gh("eblondel/cleangeo")
                                        #p_unload(raster)
p_load(Hmisc)
p_load(dplyr)
p_load(readxl)
p_load(efsagis)
p_load(gplots)

source("replace.R")
source("utils.R")

source("spain/program/read.R")
source("france/program/read.R")
source("italy/program/read.R")
source("portugal/program/read.R")
source("cyprus/program/read.R")
source("croatia/program/read.R")
source("greece/program/read.R")
source("malta/program/read.R")

crs <- CRS("+proj=longlat +ellps=WGS84")


resolution <- "01M"
extent <- raster::extent(-10,34,34,48) #small
data(NUTS_01M_2013,countries_01M_2013)

EU_NUTS <- NUTS_01M_2013 %>%
    retainEU28()


EU_NUTS.0 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==0,] %>%
    postProcessMap(crs,extent)
EU_NUTS.3 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==3,] %>%
    postProcessMap(crs,extent)


world.eu <- postProcessMap(countries_01M_2013[!countries_01M_2013@data$CNTR_ID %in%  as.character(EU_NUTS.0@data$NUTS_ID),],crs=crs,extent = extent)




warnIfUnkownIds <- function(europe) {
    diff <- setdiff(europe$NUTS.Code,EU_NUTS.3@data$NUTS_ID)
    missingNutsIds <- europe %>%
        filter(NUTS.Code %in% diff) %>%
        select(NUTS.Code,NUTS3.name) %>% distinct()
    if(nrow(missingNutsIds)>0) {
        warning("The following nuts3 ids exist in the data but not in the map. So there values will not be shown: \n",missingNutsIds)
    }
}



extractCitrusData <- function() {

    conversion_sa_ha <- 904436

    nutsLevels <- nutsLevels() %>%
        filter(Level==3) %>%
        filter(!NUTS.Code %in% c("CY00","MT00","PT17", "PT15", "PT20", "PT30", "PT113", "PT114", "PT115", "PT116", "PT117","ES13"))
    
    europe <- bind_rows(
        readCitrusHectar_spain(),
        readCitrusHectar_france(),
        readCitrusHectar_italy(),
        readCitrusHectar_portugal(),
        readCitrusHectar_cyprus(),
        readCitrusHectar_croatia(),
        readCitrusHectar_greece(),
        read_CitrusHectarMalta()) %>%
        tbl_df() %>%
        addNewData("nutsReplacements.csv") %>%
        left_join(nutsLevels,by = c('name'='Description')) %>%
        rename(NUTS3.name = name) %>%
        mutate(NUTS.Code = ifelse(is.na(NUTS.Code.Country),NUTS.Code,NUTS.Code.Country)) %>%
        select(country,year,NUTS3.name,NUTS.Code,ha,comment,source,link,date,sourceFile) %>%
        left_join(EU_NUTS.3@data,by=c("NUTS.Code"="NUTS_ID")) %>%
        mutate(Shape_Area_ha = Shape_Area * conversion_sa_ha,citrus_density=ha/Shape_Area_ha*100)

                                        #%>%
        
                                        #select(-STAT_LEVL_,-Shape_Leng,-Shape_Area,-Shape_Area_ha)    

   
    write.csv(europe,"output/citrusProduction.csv")

   
    europe
}

latestData <- function() {
    
    data <- extractCitrusData() %>%
        group_by(NUTS.Code) %>%
        mutate(max_year=max(year)) %>%
        filter(year==max_year) %>%
        ungroup() %>%
        select(-max_year)
    write.csv(data,"output/citrusProduction_latest.csv")
    data
}

base_layer <- function() {
    tm_shape(EU_NUTS.0) +
        tm_borders(lwd = 0)
}

nuts0_layer <- function(alpha=1) {
    tm_shape(EU_NUTS.0) +
        tm_borders(lwd=0.5,col="grey20",alpha = alpha) 
}
nuts3_layer <- function(alpha=1) {
    tm_shape(EU_NUTS.3) +
        tm_borders(lwd=0.1,col="grey10",alpha=alpha) 
    
}


prepare_citrus_layer <- function() {
    data <- latestData()
    warnIfUnkownIds(data)
    
    EU_NUTS.3.ha <- EU_NUTS.3
    newData <- EU_NUTS.3@data %>% left_join(data,by=c("NUTS_ID"="NUTS.Code"))
    
    EU_NUTS.3.ha@data <- newData
    EU_NUTS.3.ha <- EU_NUTS.3.ha[!is.na(EU_NUTS.3.ha@data$ha),]
    EU_NUTS.3.ha
}
citrusSurface_schraffiert_layer <-  function() {

    spdf <- prepare_citrus_layer()
    brks <- quantile(spdf@data$citrus_density)
    dens <- (2:length(brks))*3
    plot(spfd,density = dens[findInterval(spfd@data$citrus_density,brks,all.inside = T)])
    
}

citrusSurface_outline_layer <- function() {
    citrusMap <- prepare_citrus_layer()
    coords <- coordinates(citrusMap)
    ID <- cut(coords[,1], range(coords[,1]), include.lowest=TRUE)
    outline <- unionSpatialPolygons(citrusMap, ID)
    tm_shape(outline) +
        tm_borders(lwd=2,col="red") 
}

citrusSurface_dots_layer <- function() {
    citrusMap <- prepare_citrus_layer()

    total <- sum(citrusMap@data$ha)

    shp <- sample_dots(citrusMap,
                      shp.id = "NUTS_ID",
                      w=100,
                      vars="ha",
                      units="ha",
                      units.size=1,
                      convert2density = F,nrow=400,ncol=1300,
                      npop=total,
                      total.area=sum(citrusMap@data$Shape_Area_ha))
                      
    
    tm_shape(shp)+
        tm_dots(size = 0.01,col="red") 

}



citrusSurface_layer <- function(column,alpha=1) {
    if (column=="citrus_density") {
        title=paste0("Citrus production area density \n (ha/km^2)")
        breaks=NULL
    } else  if (column=="ha") {
        title="Citrus production area \n (ha)"
        breaks=c(1,500,2500,5000,10000,25000,Inf)
    }
    tm_shape(prepare_citrus_layer()) +
        tm_fill(column,
                palette = "Reds",
                breaks = breaks,
                textNA = NA,
                colorNA = "white",
                alpha = alpha,
                legend.format = list(scientific=T,format="f"),
                contras=c(0.2,1),
                title=title) 
}
#' columname: Any of:
#'  "prevalence"
#' "asco_pat_average"     
#' "asco_days_average"
#' "asco_days_stddev"
#' "asco_days_years"
#' "pyc_infection_average"
#' "pyc_infection_stddev"
#' "pyc_infection_years"  

magarey_layer <- function(column_size,column_col=NA,title.size=NA,title.col=NA,style=NULL) {

    europe <- latestData()
    mag2015table1 <- read_csv("./mag2015_table1.csv",skip=2) %>%
        mutate(Lat=gsub("−","-",Lat,fixed=T),
               Lon=gsub("−","-",Lon,fixed=T),
               Lat=as.numeric(Lat),
               Lon=as.numeric(Lon)) %>%
        rename(Location=`Location<comma> Stateb`) %>%
        mutate(Location=gsub("<comma>",".",Location))
    pts <- mag2015table1 %>% select(Lon,Lat) %>% data.frame() %>% SpatialPoints(crs)
    
    
    match <- (over(pts,EU_NUTS.3))
    mag2015Data <- bind_cols(mag2015table1,match)  %>%
        filter(!is.na(NUTS_ID)) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area) %>%
        left_join((europe %>% select(NUTS.Code,NUTS3.name,ha)),by=c("NUTS_ID"="NUTS.Code"))
    
    mag2015table2 <- read_csv("./mag2015_table2.csv",skip=5) %>%
        setNames(c("Location","Country","prevalence","asco_pat_average","asco_days_average","asco_days_stddev","asco_suit_years","pyc_average","pyc_stddev","pyc_suit_years")) %>%
                                        #select(Location,asco_days_average) %>%
            mutate(Location=gsub("<comma>",".",Location)) %>%
            mutate(Location=gsub("-","/",Location,fixed=T),
                   Location=gsub("Larnaca Cyprus","Larnaca",Location),
                   Location=gsub("^Palermo/Punta$","Palermo/Punta Raisi",Location)) 
        
     
        mag2015Data <- mag2015Data %>% left_join(mag2015table2,by=c("Location")) 
    




        write.csv(mag2015Data,"mag2015Nuts3.csv")

        lonLat <- mag2015Data %>% select(Lon,Lat) %>% data.frame()
        plot(EU_NUTS.0,col = "white") # to make pointLabel happy
        xy <- pointLabel(lonLat$Lon,lonLat$Lat,labels = paste0(seq_along(lonLat$Lon)),doPlot = F,cex=2) 


        spts <- SpatialPointsDataFrame(coords=lonLat,
                                      data=mag2015Data %>% data.frame(),
                                      proj4string = crs)
                         
        text_sp <- SpatialPointsDataFrame(coords=xy,data=mag2015Data %>% data.frame(),
                                         proj4string = crs)
                                     
        tm_shape(spts) +
            tm_bubbles(size=column_size,col=column_col,border.col = "blue",alpha=1,
                       title.col = title.col,title.size = title.size,
                       palette = "Blues",
                       scale=1.2,
                       style = style,
                       legend.format = list(scientific=T,format="f"),

                       ) +

    tm_shape(text_sp) +
                                        #tm_bubbles(size=1.1,col=c("white"),alpha=0.5)+
    tm_text(column_size,col="blue")

}


aschmann_layer <- function(alpha=1) {
    aschmann <- raster::raster("./geo/martinez2015/rasters/mediterranean/ASCHMANN/Aschmann_med.grd") %>%
        raster::crop(extent)
                                        #raster::values(aschmann) <- ifelse(is.na(raster::values(aschmann)),0,1)
        
        tm_shape(aschmann) +
            tm_raster(alpha = alpha,legend.show = T,style="cat",
                      palette=c("grey"),
                      colorNA="white",
                      textNA = NA,
                      title="",
                      labels=c("Aschmann's mediteranea type"))
    }
    ## type: med,Bsk_Bsh,Csa_Csb
    koppen_layer <- function(type,alpha=0.2,palette=NULL) {
        koppen <- raster::raster(sprintf("./geo/martinez2015/rasters/mediterranean/KOPPEN/koppen_%s.grd",type)) %>%
            
            raster::crop(extent) 
        
        tm_shape(koppen) +
            tm_raster(alpha = alpha,legend.show = T,palette = palette) 
        
            
        
    }

    combined_koppen_layer <- function(alpha=1) {
        koppen1 <- raster::raster(sprintf("./geo/martinez2015/rasters/mediterranean/KOPPEN/koppen_%s.grd","Bsk_Bsh")) %>%
            raster::crop(extent)
        koppen2 <- raster::raster(sprintf("./geo/martinez2015/rasters/mediterranean/KOPPEN/koppen_%s.grd","Csa_Csb")) %>%
            raster::crop(extent)
        koppen1Data <- getValues(koppen1)
        koppen2Data <- getValues(koppen2)
        koppenCombinedData <- ifelse(is.na(koppen1Data),0,koppen1Data) + ifelse(is.na(koppen2Data),0,koppen2Data)
        koppenCombinedData <- ifelse(koppenCombinedData==0,NA,koppenCombinedData)
        koppenCombined <- koppen1
        koppenCombined <- setValues(koppenCombined,koppenCombinedData)
        tm_shape(koppenCombined) +
            tm_raster(
                palette= c("#F6A200","#FDDA62","#FCFE04","#CECC08"),
                style = "cat",colorNA = "#FFFFFF00",alpha = alpha,
                labels=c("BSh","BSk","CSa","CSb"),
                title="Köppen–Geiger classification",
                textNA = NA)
    
    }


    infection_layer <- function(fileName,column,title,alpha=1) {
        data <- read_excel(fileName)
                                        #data <- left_join(cgms25grid@data,asco,by=c("Grid_Code"="GRID_NO"))
        data <- left_join(cgms25grid@data,data,by=c("Grid_Code"="GRID_NO")) %>%
            data.frame()
                                        #    column <- paste0("X",column)
        dataSpdf <- cgms25grid
        dataSpdf@data <- data
        dataSpdf <- dataSpdf[!is.na(dataSpdf[[column]]),]    
        tm_shape(dataSpdf) +
            tm_polygons(column,border.col = "grey10",alpha=alpha,border.alpha = alpha,
                        palette=paste0(col2hex(c("white","lightblue","green","yellow","orange","red")),"FF"),
                        breaks=c(-Inf,0.01,0.5,1,5,10,Inf),
                        title=title
                        )    
    
    }


    if(!is.memoised(prepare_citrus_layer)) {
        prepare_citrus_layer <- memoise(prepare_citrus_layer)
    }

    if(!is.memoised(sample_dots)) {
        sample_dots <- memoise(tmap::sample_dots)
    }
