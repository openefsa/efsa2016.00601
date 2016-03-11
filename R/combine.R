#' Combines the citrus production data of all countries into a single data frame.
#' The result of this is the data object 'euCitrusData'
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

