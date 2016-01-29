library(pacman)
p_load(cartography)
p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)
p_load(dplyr)
p_load(readr)
p_load(memoise)

source("replace.R")
source("mapTools.R")
source("utils.R")

source("spain/program/read.R")
source("france/program/read.R")
source("italy/program/read.R")
source("portugal/program/read.R")
source("cyprus/program/read.R")
source("croatia/program/read.R")
source("greece/program/read.R")
source("malta/program/read.R")

if (!exists("readOGR_mem"))
    readOGR_mem <- memoise(readOGR)

resolution <- "01M"  #60M

EU_NUTS <- readOGR_mem(dsn = sprintf("./geo/NUTS_2013_%s_SH/data",resolution), layer = sprintf("NUTS_RG_%s_2013",resolution))
EU_NUTS.0 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==0,]
EU_NUTS.3 <- EU_NUTS[EU_NUTS@data$STAT_LEVL_==3,]

world.eu <- readOGR_mem(dsn = sprintf("./geo/CNTR_%s_2013_SH/data",resolution), layer = sprintf("CNTR_RG_%s_2013",resolution))



extractCitrusData <- function() {

    sizeEurope_sq_ha <- 1018000000
    sumAreaShapeEurope <-  sum(EU_NUTS.3@data$Shape_Area)
    conversion_sa_ha <- sizeEurope_sq_ha / sumAreaShapeEurope

    nutsLevels <- readNutsLevels() %>%
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
        mutate(Shape_Area_ha = Shape_Area * conversion_sa_ha,citrus_density=ha/Shape_Area_ha) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area,-Shape_Area_ha)
    

   
    write.csv(europe,"output/citrusProduction.csv")

    mostRecentData <-  europe %>%
        group_by(NUTS.Code) %>%
        mutate(max_year=max(year)) %>%
        filter(year==max_year) %>%
        ungroup()
    
    write.csv(mostRecentData,"output/citrusProduction_latest.csv")

    mostRecentData
}

plotCitrusData <- function(data) {
                                        #png()
        par(bg = "white")           # default is likely to be transparent
        split.screen(c(2, 1))
        screen(1)
        breaks=cut2(data$citrus_density,onlycuts=T,g=10)
        plotCitrusMap(data,large=F,breaks,"citrus_density")
        screen(2)
        breaks=cut2(data$ha,onlycuts=T,g=10)
        plotCitrusMap(data,large=F,breaks,"ha")
        close.screen(all = TRUE)
                                        #dev.off()
        capture.output(sessionInfo(),file="sessionInfo.txt")
        width <- 1366
        height <- 768
        png("citrusMapHa.png",width = width,height = height)
        plotCitrusMap(data,large=F,breaks,"ha")
        dev.off()
  
    }


    plotCitrusMap_svg <- function(data) {
        svg(width=12,height=6)
        breaks=cut2(data$ha,onlycuts=T,g=10)
        plotCitrusMap(data,large=F,breaks,"ha")    
    }


