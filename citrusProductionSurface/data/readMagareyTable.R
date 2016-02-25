library(pacman)
p_load(readr)

readMagTable1 <- function() {
    read_csv("./mag2015_table1.csv",skip=2) %>%
        mutate(Lat=gsub("−","-",Lat,fixed=T),
               Lon=gsub("−","-",Lon,fixed=T),
               Lat=as.numeric(Lat),
               Lon=as.numeric(Lon)) %>%
        rename(Location=`Location<comma> Stateb`) %>%
        mutate(Location=gsub("<comma>",".",Location))

}
