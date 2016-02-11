p_load(zoo)
p_load(dplyr)
p_load(tidyr)
p_load(stringr)


source('utils.R')
fixCitrinos <-  function(df) {
    df %>%
        filter(!Citrinos %in% c("o","...","-")) %>%
        mutate(Citrinos=gsub("[ |\\.|,]","",Citrinos)) %>%
        mutate(Citrinos=as.numeric(Citrinos))
} 

readPortugalCsv <- function(name,fileNumber,skip,sliceStart,sliceEnd,link) {
    data <- read_csv(paste0(getwd(),"/portugal/original/",name,"/",fileNumber,".csv"),
                    locale=iso88591Locale(),skip=skip)
    names(data)[1:3] <- c("nuts3","concelho","unit")
    data <- data[,1:8]
    data  %>% select(nuts3,Citrinos) %>%
        slice(sliceStart:(n()+sliceEnd))  %>% 
        fixCitrinos() %>%
        mutate(link=link) %>%
        filter(!grepl("^Fonte:",nuts3))
}


readCitrus2009Census <- function() {

    names <- c("212_RGA Açores",
              "213_RGA EDM", #done
              "214_RGA Madeira",
              "216_RGA Beira Litoral", #done
              "217_RGA Trás Montes", #done
              "218_RGA Alentejo", #done
              "219_RGA Algarve",
              "220_RGA ROeste", #done
              "RGA-BI_1999")

    
    for (name in names) {
        unzip(paste0(getwd(),"/portugal/original/",name,".zip"),
              exdir=paste0(getwd(),"/portugal/original/",name))
    }

    
    data <- bind_rows(readPortugalCsv("218_RGA Alentejo","020",2,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=62669139&att_display=n&att_download=y"),
                     readPortugalCsv("218_RGA Alentejo","021",2,1,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=62669139&att_display=n&att_download=y"),
                     readPortugalCsv("213_RGA EDM","016",2,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=5605078&att_display=n&att_download=y"),
                     readPortugalCsv("216_RGA Beira Litoral","019",2,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=62668225&att_display=n&att_download=y"),
                     readPortugalCsv("217_RGA Trás Montes","010",2,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=62668853&att_display=n&att_download=y"),
                     readPortugalCsv("220_RGA ROeste","017",skip=3,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=374399&att_display=n&att_download=y"),
                     readPortugalCsv("220_RGA ROeste","018",3,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=374399&att_display=n&att_download=y"),
                     readPortugalCsv("RGA-BI_1999","013",2,3,-1,"http://ra09.ine.pt/ngt_server/attachfileu.jsp?look_parentBoui=374403&att_display=n&att_download=y")) %>%
        mutate(ha=lead(Citrinos)) %>%
        filter(!is.na(nuts3)) %>%
        rename(name=nuts3) %>%
        select(-Citrinos) %>%
        mutate(country="PT", 
               year=2009,
               comment="",
               source="http://ra09.ine.pt",
               nutsVersion="1995PT",
               date="11/01/2016")
    data

    
}
    
readCitrus2009Census_singleFile <- function() {
    nuts2010 <- readNutsLevels(2010) %>%
        filter(grepl("^PT",NUTS.Code),Level==3) %>%
        filter(!NUTS.Code %in% c("PT1",   "PT2" ,  "PT3",   "PTZ" ,  "PT11",  "PT15",  "PT16" , "PT17",  "PT18",  "PT20",  "PT30"))
    
    nuts2010Projected2013 <- read_csv(paste0(getwd(),"/portugal/original/nuts2010projected2013.csv"))
    
    data <- read_csv2(paste0(getwd(),"/portugal/original/nuts3DataCensus2009.csv"),skip=11, locale=iso88591Locale())
    names(data) <- c("name","ha","nothing")

    data <- data %>% select(name,ha) %>%
        separate(name,into = c("code","name"),sep=":") %>%
        select(-code) %>%
        mutate(name=str_trim(name)) %>%
        filter(!is.na(ha),!is.na(name)) %>%
        mutate(ha=as.numeric(ha),
               country="PT", 
               year=2009,
               comment="The data was approximately mapped from the 2010 NUTS 3 regions to the 2013 NUTS 3 regions following the NUTS change description from EC",
               source="http://ra09.ine.pt",
               file="nuts3DataCensus2009.csv",
               date="11/01/2016")  %>%
        left_join(nuts2010,by=c("name"="Description")) %>%
        rename(NUTS.Code.Country=NUTS.Code) %>%
        left_join(nuts2010Projected2013,by=c("NUTS.Code.Country"="nutsCode2010")) %>%
        mutate(NUTS.Code.Country=nuts2010projected2013) %>%
        select(-nuts2010projected2013)  %>%
        group_by(NUTS.Code.Country) %>%
        mutate(ha=sum(ha)) %>%
        slice(1)
    data 
    
}
