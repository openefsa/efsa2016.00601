p_load(zoo)

fixCitrinos <-  function(df) {
    df %>%
        filter(!Citrinos %in% c("o","...","-")) %>%
        mutate(Citrinos=gsub("[ |\\.|,]","",Citrinos)) %>%
        mutate(Citrinos=as.numeric(Citrinos))
} 

readPortugalCsv <- function(name,fileNumber,skip,sliceStart,sliceEnd) {
    data <- read_csv(paste0(getwd(),"/portugal/original/",name,"/",fileNumber,".csv"),
                     locale=iso88591Locale(),skip=skip)
    names(data)[2:4] <- c("nuts3","concelho","unit")
    data <- data[,1:8]
    data  %>% select(nuts3,Citrinos) %>%
        slice(sliceStart:(n()+sliceEnd))  %>% 
        fixCitrinos()
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

    
    data <- bind_rows(readPortugalCsv("218_RGA Alentejo","020",2,3,-1),
                      readPortugalCsv("218_RGA Alentejo","021",2,1,-1),
                      readPortugalCsv("213_RGA EDM","016",2,3,-1),
                      readPortugalCsv("216_RGA Beira Litoral","019",2,3,-1),
                      readPortugalCsv("217_RGA Trás Montes","010",2,3,-1),
                      readPortugalCsv("220_RGA ROeste","017",skip=3,3,-1),
                      readPortugalCsv("220_RGA ROeste","018",3,3,-1),
                      readPortugalCsv("RGA-BI_1999","013",2,3,-1)) %>%
        mutate(ha=lead(Citrinos)) %>%
        filter(!is.na(nuts3)) %>%
        rename(name=nuts3) %>%
        select(-Citrinos) %>%
        mutate(country="PT", 
               year=2009,
               comment="",
               source="http://ra09.ine.pt",
               link="",
               nutsVersion="1995PT",
               date="11/01/2016")
    
}

