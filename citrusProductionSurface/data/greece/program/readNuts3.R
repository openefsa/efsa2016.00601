p_load(dplyr)
p_load(stringr)
p_load(readxl)
p_load(tidyr)
source("utils.R")
read_greekNuts3 <- function() {

   
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

  
    
    data <- read_excel(paste0(getwd(),"/greece/original/Κουφάκης/5α.Δενδρ.καλ.,εκτ.συνεχ.κανον.δενδρ.,Περιφέρεια, Π.Ε.,2012.xls"),skip=6,
                      col_types=rep("text",24),
                      col_names= letters[1:24]) %>%
        select(a,c,u) %>%
        slice(1:101) %>%
        rename(name=u,
               ha=c,
               greekName=a) %>%
        mutate(ha=ifelse(ha=="—",NA,ha),
               name=str_trim(name),
               greekName=str_trim(greekName)) %>%
        filter(!is.na(name)) %>%
        filter(!name %in% nonNuts3)

    corr2 <-  read_excel(paste0(getwd(),"/greece/original/correspondance2.xlsx")) %>%
        mutate(Stats_greek=str_trim(Stats_greek),
               stats_latin=str_trim(stats_latin))
        
    

    data <- data %>%
        left_join(corr2,by=c("greekName"="Stats_greek")) %>%
        left_join(corr2,by=c("nuts3Code"="nutsCode 2013")) %>%
        select(ha,nuts3Code,`nutsName 2013.y`) %>%
        rename(name=`nutsName 2013.y`,
               NUTS.Code.Country=nuts3Code) %>%
        mutate(ha=as.numeric(ha)) %>%
        group_by(name) %>%
        mutate(ha_sum=sum(ha,na.rm=T)) %>%
        filter(row_number()==1) %>%
        mutate(ha=ha_sum) %>%
        select(-ha_sum) %>%
        filter(ha>0) %>%
        mutate(year=2012,
               country="EL",
               date="18/01/2016",
               comment="",
               source="Via email on 15/01/2016 from ELSTAT",
               sourceFile="Κουφάκης/5α.Δενδρ.καλ.,εκτ.συνεχ.κανον.δενδρ.,Περιφέρεια, Π.Ε.,2012.xls")

    data
}
