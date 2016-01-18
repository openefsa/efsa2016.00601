library(pacman)
p_load(dplyr)
p_load(readr)
p_load(readxl)
p_load(tidyr)
source("greece/program/readNuts3.R")

convertNutsIds <- function(data) {
    nuts3Change2010 <- read_excel(paste0(getwd(),"/geo/nutsChanges2006-2010.xls"),1,skip=1)
   
    
    names(nuts3Change2010)[2:3]<-c("Code_2006","Code_2010")
    nuts3Change2010 <-  nuts3Change2010 %>% select(Code_2006,Code_2010)
    data <- data  %>%  left_join(nuts3Change2010,by=c("NUTS.Code.Country"="Code_2006")) %>%
                                 
        mutate(NUTS.Code.Country=ifelse(is.na(Code_2010),NUTS.Code.Country,Code_2010)) %>%
        select(-Code_2010)
    
    nuts3Change2013 <- read_nuts3Change2013()

    data  %>%  left_join(nuts3Change2013,by=c("NUTS.Code.Country"="Code_2010")) %>%
        mutate(NUTS.Code.Country=ifelse(is.na(Code_2013),NUTS.Code.Country,Code_2013)) %>%
        select(-Code_2013,-change2010_2013_comment)
    
  
}



readCitrusHectar_greece <- function() {
    data <- read_delim(paste0(getwd(),"/greece/original/efsaOpinion2014.txt"),delim=" ") %>%
        rename(country=Country,
               name=Name,
               NUTS.Code.Country=NUTS_ID) %>%
        mutate(year=2007) %>%
        convertNutsIds()

    EL30x <-  EU_NUTS@data %>%
        filter(grepl("EL30.+",NUTS_ID)) %>%
        mutate(EL30_total=sum(Shape_Area),
               perc=Shape_Area/EL30_total,
               NUTS.Code.Country="EL300") %>%
        
        select(NUTS_ID, perc,NUTS.Code.Country) %>%
        left_join(data) %>%
        mutate(ha=perc * ha) %>%
        select(country,
               NUTS.Code.Country=NUTS_ID,
               name,
               ha,year) %>%
        mutate(name=paste0(name,"_",row_number()),
               comment="EL300 was distributed into E30x according to size")
                           
    

    data %>%
        mutate(source="10.2903/j.efsa.2014.3557 appendix f",
               link="http://www.efsa.europa.eu/en/efsajournal/pub/3557",
               date="14/01/2016") %>%
        bind_rows(EL30x) %>%
        filter(!NUTS.Code.Country=="EL300") %>%
        bind_rows(read_greekNuts3())
    
}
