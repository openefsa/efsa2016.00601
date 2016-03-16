#' @export 
readMagTable1 <- function() {
    readr::read_csv(system.file("extdata/mag2015_table1.csv",package = "efsa2016.00601"),skip=2) %>%
        dplyr::mutate(Lat=gsub("−","-",Lat,fixed=T),
                      Lon=gsub("−","-",Lon,fixed=T),
                      Lat=as.numeric(Lat),
                      Lon=as.numeric(Lon)) %>%
        dplyr::rename(Location=`Location<comma> Stateb`) %>%
        dplyr::mutate(Location=gsub("<comma>",".",Location))

}
#' @export 
readMagTable2 <- function() {

    readr::read_csv(system.file("extdata/mag2015_table2.csv",package = "efsa2016.00601"),skip=5) %>%
        setNames(c("Location","Country","prevalence","asco_pat_average","asco_days_average","asco_days_stddev","asco_suit_years","pyc_average","pyc_stddev","pyc_suit_years")) %>%
                                        #select(Location,asco_days_average) %>%
        mutate(Location=gsub("<comma>",".",Location)) %>%
        mutate(Location=gsub("-","/",Location,fixed=T),
               Location=gsub("Larnaca Cyprus","Larnaca",Location),
               Location=gsub("^Palermo/Punta$","Palermo/Punta Raisi",Location)) 
}    
