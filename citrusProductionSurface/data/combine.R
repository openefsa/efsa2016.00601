library(pacman)
p_load(cartography)
p_load(dplyr)

p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)

source("replace.R")

source("spain/program/read.R")
source("france/program/read.R")
source("mapTools.R")

nuts <- read.csv("NUTS_2013L.csv",stringsAsFactors=F) %>%
    tbl_df() %>%
    select(Level,NUTS.Code,Description)



europe <- readCitrusHectar_spain() %>%
    rbind(readCitrusHectar_france()) %>%
    addNewData("nutsReplacements.csv") %>%
    left_join(nuts,by = c('name'='Description')) %>%
    rename(NUTS3.name = name) %>%
    select(country,year,NUTS3.name,NUTS.Code,ha)
                                        #,comment,source,link,date)

write.csv(europe,"output/citrusProduction.csv")



                                        #pdf()
plotCitrusMap(europe)
                                        #dev.off()
