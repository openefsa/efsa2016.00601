readMagTable1 <- function() {
    readr::read_csv(system.file("extdata/mag2015_table1.csv",package = "efsa2016.00601"),skip=2) %>%
        dplyr::mutate(Lat=gsub("−","-",Lat,fixed=T),
                      Lon=gsub("−","-",Lon,fixed=T),
                      Lat=as.numeric(Lat),
                      Lon=as.numeric(Lon)) %>%
        dplyr::rename(Location=`Location<comma> Stateb`) %>%
        dplyr::mutate(Location=gsub("<comma>",".",Location))

}
