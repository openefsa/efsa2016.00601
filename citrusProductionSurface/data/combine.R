library(pacman)
p_load(cartography)
p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)
p_load(dplyr)
p_load(readr)

source("replace.R")
source("mapTools.R")
source("utils.R")

source("spain/program/read.R")
source("france/program/read.R")
source("italy/program/read.R")
source("portugal/program/read.R")
source("cyprus/program/read.R")


run <- function() {

    nutsLevels <- readNutsLevels()


    europe <- bind_rows(
        readCitrusHectar_spain(),
        readCitrusHectar_france(),
        readCitrusHectar_italy(),
        readCitrusHectar_portugal(),
        readCitrusHectar_cyprus()) %>%
        addNewData("nutsReplacements.csv") %>%
        left_join(nutsLevels,by = c('name'='Description')) %>%
        rename(NUTS3.name = name) %>%
        select(country,year,NUTS3.name,NUTS.Code,ha)
                                        #,comment,source,link,date)

    write.csv(europe,"output/citrusProduction.csv")


                                        #pdf()
    plotCitrusMap(europe %>% filter(year==2013))
                                        #dev.off()
    capture.output(sessionInfo(),file="sessionInfo.txt")
    
}
