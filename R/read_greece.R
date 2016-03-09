
#' @importFrom dplyr bind_rows

readCitrusHectar_sheet <- function(year) {

   
    nonNuts3 <- c("Greece Total",
                 "Region of Eastern Macedonia and Thrace",
                 "Region of Central Macedonia",
                 "Region of Epirus",
                 "Region of Thessally",
                 "Region of Central Greece",
                 "Region of Ionian Islands",
                 "Region of Western Greece",
                 "Region of Peloponnese",
                 "Region of Attica",
                 "Region of Northern Aegean",
                 "Region of Southern Aegean",
                 "Region of Crete")

    filename <- sprintf("5α.Δενδρ.καλ.,εκτ.συνεχ.κανον.δενδρ.,Περιφέρεια, Π.Ε.,%d.xls",year)
    data <- readxl::read_excel(system.file(paste0("extdata/greece/Κουφάκης/",filename)
                                         ,package="efsa2016.00601"),skip=6,
                              col_types=rep("text",24),
                              col_names= letters[1:24]) %>%
        select(a,c,u) %>%
        slice(1:101) %>%
        rename(name=u,
               stremma=c,
               greekName=a) %>%
        mutate(stremma=ifelse(stremma %in% c("\xe2\x80\x94","\xe2\x80\x95"),NA,stremma),
               name=str_trim(name),
               greekName=str_trim(greekName)) %>%
        filter(!is.na(name)) %>%
        filter(!name %in% nonNuts3) 
    corr2 <-  readxl::read_excel(system.file("extdata/greece/correspondance2.xlsx",package="efsa2016.00601")) %>%
        mutate(Stats_greek=str_trim(Stats_greek),
               stats_latin=str_trim(stats_latin))
    
    

    data <- data %>%
        left_join(corr2,by=c("greekName"="Stats_greek")) %>%
        left_join(corr2,by=c("nuts3Code"="nutsCode 2013")) %>%
        select(stremma,nuts3Code,`nutsName 2013.y`) %>%
        rename(name=`nutsName 2013.y`,
               NUTS.Code.Country=nuts3Code) %>%
        mutate(stremma=as.numeric(stremma),
               ha=stremma/10) %>%
        select(-stremma) %>% 
        group_by(name) %>%
        mutate(ha_sum=sum(ha,na.rm=T)) %>%
        filter(row_number()==1) %>%
        mutate(ha=ha_sum) %>%
        select(-ha_sum) %>%
        filter(ha>0) %>%
        mutate(year=year,
               country="EL",
               date="18/01/2016",
               comment="",
               source="Via email on 15/01/2016 from ELSTAT",
               sourceFile=filename)

    data
}
#' @export

readCitrusHectar_greece <- function() {
    bind_rows(readCitrusHectar_sheet(2011),
              readCitrusHectar_sheet(2012)
              )
}
