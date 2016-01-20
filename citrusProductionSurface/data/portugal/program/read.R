library(pacman)
p_load(dplyr)
p_load(reshape2)
p_load(stringr)
p_load(readr)
source("portugal/program/readNuts3.R")

readCitrusHectar_portugal <- function() {

    nutsLevels <- readNutsLevels()
                                        #https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_indicadores&indOcorrCod=0000020&contexto=bd&selTab=tab2
   
    dataNuts <- read_csv2(paste0(getwd(),"/portugal/original/eSFjCZvkxI27xPkYFHeKHPoy_55629.csv"),
                          
                          skip=12,
                                        #fileEncoding="ISO-8859-1",
                          col_names=F,
                          n_max=11,
                          locale=iso88591Locale()) %>% tbl_df() %>% select(-X31)
    colnames(dataNuts) <- c("name",paste0("y",seq(2014,1986,-1)))


    dataNuts <- tbl_df(dataNuts)   %>%
        slice(1:11) %>%
        mutate(y2014=as.numeric(y2014),
               name=sub(".+:","",name),
               name=str_trim(name)) %>%
        filter(!row_number() %in% c(8,10)) %>%
        mutate(name=str_replace_all(name,c("Portugal"="PORTUGAL",
                                           "Continente"="CONTINENTE",
                                           "Centro"="Centro (PT)"))) %>%
        left_join(nutsLevels,by=c("name"="Description")) %>%
        filter(!row_number() %in% c(1,2),
               !Level==2)  %>%
                                        #correct mistakes in nuts levels (reference: https://en.wikipedia.org/wiki/NUTS_of_Portugal)
        mutate(Level=ifelse(name=="CONTINENTE",1,Level),
               Level=ifelse(name=="Norte",2,Level),
               Level=ifelse(name=="Centro (PT)",2,Level),
               Level=ifelse(name=="Alentejo",2,Level),
               Level=ifelse(name=="Área Metropolitana de Lisboa" & NUTS.Code=="PT17",2,Level),
               Level=ifelse(name=="Alentejo",2,Level),
               Level=ifelse(name=="Algarve" & NUTS.Code=="PT15",2,Level),
               Level=ifelse(name=="Região Autónoma dos Açores" & NUTS.Code=="PT20" ,2,Level),
               Level=ifelse(name=="Região Autónoma da Madeira" & NUTS.Code=="PT30" ,2,Level)
               ) %>%
                                      
        melt(c("name","Level","NUTS.Code"),variable.name="year",value.name="ha") %>%
        tbl_df() %>%
        mutate(year=sub("y","",year),
               year=as.numeric(year)) %>%
        select(name,year,ha,Level) %>%
        mutate(country="PT",
               comment="",
               source="https://www.ine.pt",
               link="",
               date="11/01/2016")

    dataNuts3 <- dataNuts %>% filter(Level==3)

                                       
    dataNuts3  %>%  select(-Level) %>%
        bind_rows(readCitrus2009Census())
    
}
                                      
