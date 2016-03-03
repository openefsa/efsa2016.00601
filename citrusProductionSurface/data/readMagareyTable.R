library(pacman)
p_load(readr)

readMagTable1 <- function() {
    read_csv("./mag2015_table1.csv",skip=2) %>%
        dplyr::mutate(Lat=gsub("−","-",Lat,fixed=T),
                      Lon=gsub("−","-",Lon,fixed=T),
                      Lat=as.numeric(Lat),
                      Lon=as.numeric(Lon)) %>%
        dplyr::rename(Location=`Location<comma> Stateb`) %>%
        dplyr::mutate(Location=gsub("<comma>",".",Location))

}
