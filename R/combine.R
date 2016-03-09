## library(pacman)
## p_load(raster)

## p_load(cartography)
## p_load(rgdal)
## p_load(PBSmapping)
## p_load(maptools)
## p_load(readr)
## p_load(memoise)
## p_load(maptools)
## p_load(tmap)
## p_load_gh("eblondel/cleangeo")
## p_unload(raster)
## p_load(Hmisc)
## p_load(dplyr)
## p_load(readxl)
## p_load(efsagis)
## p_load(gplots)
## p_load(classInt)

## source("replace.R")
## source("utils.R")
## source("./readMagareyTable.R")

## source("spain/program/read.R")
## source("france/program/read.R")
## source("italy/program/read.R")
## source("portugal/program/read.R")
## source("cyprus/program/read.R")
## source("croatia/program/read.R")
## source("greece/program/read.R")
## source("malta/program/read.R")


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
extractCitrusData <- function() {


  
    


    conversion_sa_ha <- 904436

    nutsLevels <- efsagis::nutsLevels() %>%
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
        addNewData(system.file("extdata/nutsReplacements.csv",package="efsa2016.00601")) %>%
        left_join(nutsLevels,by = c('name'='Description')) %>%
        rename(NUTS3.name = name) %>%
        mutate(NUTS.Code = ifelse(is.na(NUTS.Code.Country),NUTS.Code,NUTS.Code.Country)) %>%
        select(country,year,NUTS3.name,NUTS.Code,ha,comment,source,link,date,sourceFile) %>%
        left_join(euNuts3()@data,by=c("NUTS.Code"="NUTS_ID")) %>%
        mutate(Shape_Area_ha = Shape_Area * conversion_sa_ha,citrus_density=ha/Shape_Area_ha*100) %>%
        select(-STAT_LEVL_,-Shape_Leng,-Shape_Area,-Shape_Area_ha)    

    europe
}

