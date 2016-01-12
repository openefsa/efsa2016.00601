p_load(zoo)

fixCitrino <-  function(df) {
    df %>%
        filter(Citrinos!="o") %>%
        filter(Citrinos!="...") %>%
        filter(Citrinos!="-")  %>%
        mutate(Citrinos=gsub(" ","",Citrinos)) %>%
        mutate(Citrinos=gsub("\\.","",Citrinos)) %>%
        mutate(Citrinos=gsub(",","",Citrinos)) %>%
        mutate(Citrinos=as.numeric(Citrinos))
} 

readPortugalCsv <- function(name,fileNumber,skip=2) {
    read_csv(paste0(getwd(),"/portugal/original/",name,"/",fileNumber,".csv"),locale=iso88591Locale(),skip=skip) 
}


readSheet <- function(name,fileNumber,sliceStart,sliceEnd) {
    dataSheet <- readPortugalCsv(name,fileNumber)

    names(dataSheet)[1] <- "nuts3"
    names(dataSheet)[2] <- "concelho"
    names(dataSheet)[3] <- "unit"
    dataSheet <- dataSheet[,1:6]
    dataSheet  %>% slice(sliceStart:(n()+sliceEnd)) %>%  makeColNamesUserFriendly() %>%
        select(nuts3,Citrinos) %>%
        fixCitrino()
   
}


read_edm016 <- function() {
    data_edm016 <- readPortugalCsv("213_RGA EDM","016")
    names(data_edm016)[2] <- "nuts3"
    names(data_edm016)[3] <- "concelho"
    names(data_edm016)[4] <- "unit"
    data_edm016<- data_edm016[,1:8]
    data_edm016 %>%
        makeColNamesUserFriendly() %>%
        slice(3:(n()-1)) %>%
        select(nuts3,Citrinos) %>%
        fixCitrino()


}
read_beira019 <- function() {
    data_beira019 <-  readPortugalCsv("216_RGA Beira Litoral","019")
    names(data_beira019)[2:4] <- c("nuts3","concelho","unit")
    data_beira019 %>%
        select(nuts3,Citrinos) %>%
        slice(3:(n()-1)) %>%
        fixCitrino()


}

read_montes010 <- function() {
    data_montes010 <- readPortugalCsv("217_RGA Trás Montes","010")
    names(data_montes010)[2:4] <- c("nuts3","concelho","unit")

    data_montes010 %>%
        select(nuts3,Citrinos) %>%
        slice(3:(n()-1)) %>%
        fixCitrino()
    
    
}
read_roeste017 <- function() {
    data_roeste017 <- readPortugalCsv("220_RGA ROeste","017",skip=3)
    names(data_roeste017)[2:4] <- c("nuts3","concelho","unit")
    data_roeste017 %>%
        select(nuts3,Citrinos) %>%
        slice(3:(n()-1)) %>%
        fixCitrino()

}

read_roeste018 <- function() {
    data_roeste018 <- readPortugalCsv("220_RGA ROeste","018",skip=3)
    names(data_roeste018)[2:4] <- c("nuts3","concelho","unit")
    data_roeste018 <- data_roeste018 %>%
        select(nuts3,Citrinos) %>%
        slice(3:(n()-1)) %>%
        fixCitrino()
}


readCitrus2009Census <- function() {

    names <- c("212_RGA Açores",
               "213_RGA EDM", #done
               "214_RGA Madeira",
               "216_RGA Beira Litoral", #done
               "217_RGA Trás Montes",
               "218_RGA Alentejo", #done
               "219_RGA Algarve",
               "220_RGA ROeste")

    
    for (name in names) {
        unzip(paste0(getwd(),"/portugal/original/",name,".zip"),
              exdir=paste0(getwd(),"/portugal/original/",name))
    }

    data_edm016 <-read_edm016()
    data_beira019 <-  read_beira019()

    data_montes010 <- read_montes010() 

    
    data_alentejo020<- readSheet("218_RGA Alentejo","020",3,-1)
    data_alentejo021<- readSheet("218_RGA Alentejo","021",1,-1)

    data_roeste017 <- read_roeste017()

    data_roeste018 <- read_roeste018()
    
    data <- bind_rows(data_alentejo020,data_alentejo021,
                      data_edm016, data_beira019,data_montes010,
                      data_roeste017,data_roeste018
                      )
    
    
    
    data %>%
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

