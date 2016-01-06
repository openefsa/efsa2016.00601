library(pacman)
p_load(cartography)
p_load(dplyr)

p_load(rgdal)
p_load(PBSmapping)
p_load(maptools)

source("replace.R")

source("spain/program/read.R")
source("mapTools.R")



spain <- readCitrusHectar_spain() %>%
    addNewData("nutsReplacements.csv")

europe <- left_join(spain,nuts,by = c('name'='Description'))

                                        #pdf()
plotCitrusMap(europe)
                                        #dev.off()
