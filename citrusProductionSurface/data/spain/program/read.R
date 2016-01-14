library(pacman)
p_load(openxlsx)
p_load(dplyr)
p_load(stringr)


distributeToNuts3 <- function(df,nuts2,name) {
    nutsLevels <- readNutsLevels()
    nutsAreas <- getNuts3Areas()
    
    nutsLevels %>%
        filter(grepl(paste0(nuts2,".+"),NUTS.Code)) %>%
        left_join(nutsAreas) %>%
        mutate(total=sum(Shape_Area),
               perc=Shape_Area/total,name=name) %>%
        left_join(df) %>%
        mutate(ha=ha*perc) %>%
        select(-Level,-id,-Shape_Area,-total,-perc,-name) %>%
        mutate(NUTS.Code.Country=NUTS.Code) %>%
        rename(name=Description) %>%
        mutate(comment="ES53 was distributed to ES531,ES532,ES533 according to area size") %>%
        select(-NUTS.Code)
    

}

readCitrusHectar_spain_sheet<- function(sheet) {

   

    
    
    df <- read.xlsx(paste0(getwd(),"/spain/original/AE_2014_13.xlsx"),sheet="13.8.2.2",startRow = 9,rowNames =T) %>%
      
        mutate(name=str_trim(row.names(.))) %>%
        rename(ha=X3) %>%
        tbl_df() 
    
    row.names(df)<-NULL
                                        # balleares=data_frame(country=c("ES"),year=c(2013),
                                        #NUTS.Code.Country=c("ES531","ES532","ES533"),
                                        #ha=c(40000,40000,40000),
                                        #name=c("1","2","3"))
   
    df <- df %>% select(name,ha) %>%
        filter(!name %in% c(
                              "GALICIA",
                              "PAÍS VASCO",
                              "CATALUÑA",
                                        # "BALEARES", to keep
                              "CASTILLA Y LEÓN",
                              "C. VALENCIANA",
                                        # "R. DE MURCIA",
                              "EXTREMADURA",
                              "ANDALUCÍA",
                              "CANARIAS",
                              "ESPAÑA")) %>%
        filter(!ha=="-") %>%
        mutate(ha=as.numeric(ha)) %>%
        filter(!is.na(ha)) %>%
        mutate(country="ES",
               year=2013,
               comment="",
               source="http://www.magrama.gob.es/es/estadistica/temas/publicaciones/anuario-de-estadistica/2014/default.aspx?parte=3&capitulo=13",
               link="http://www.magrama.gob.es/estadistica/pags/anuario/2014/AE_2014_13.xlsx",
               date="06/01/2015")# 
   
        
    baleares <- distributeToNuts3(df,"ES53","BALEARES")
    murcias <- distributeToNuts3(df,"ES62","R. DE MURCIA")
    df %>%
        bind_rows(baleares,murcias) %>%
        filter(!name %in% c("BALEARES","R. DE MURCIA")) 
        
}


readCitrusHectar_spain <- function() {
    readCitrusHectar_spain_sheet("13.8.2.2")
}
                                        #13.8.2.7 - 244
                                        #13.8.3.2 - 246
                                        #13.8.4.2 - 249
                                        #13.8.5.2 - 252
                                        #13.8.6.2 - 254
