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
        mutate(comment=paste0(nuts2,"(nuts2) was distributed to nuts3 regions according to area size")) %>%
        select(-NUTS.Code) %>%
        filter(!is.na(ha))
    

}

readCitrusHectar_spain_sheet<- function(sheet,startRow=9) {

    
    df <- read.xlsx(paste0(getwd(),"/spain/original/AE_2014_13.xlsx"),sheet=sheet,startRow = startRow,rowNames =T) %>%
      
        mutate(name=str_trim(row.names(.))) %>%
        rename(ha=X3) %>%
        tbl_df() 
    
    row.names(df)<-NULL
  
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
        filter(!ha=="–") %>%
        mutate(ha=as.numeric(ha)) %>%
        filter(!is.na(ha)) %>%
        mutate(country="ES",
               year=2013,
               source="http://www.magrama.gob.es/es/estadistica/temas/publicaciones/anuario-de-estadistica/2014/default.aspx?parte=3&capitulo=13",
               link="http://www.magrama.gob.es/estadistica/pags/anuario/2014/AE_2014_13.xlsx",
               date="06/01/2015",
               file="AE_2014_13.xlsx")
   
        
    baleares <- distributeToNuts3(df,"ES53","BALEARES")
    murcias <- distributeToNuts3(df,"ES62","R. DE MURCIA")
    df %>%
        bind_rows(baleares,murcias) %>%
        filter(!name %in% c("BALEARES","R. DE MURCIA")) 
        
}


readCitrusHectar_spain <- function() {
    sheets <- c("13.8.2.2","13.8.2.7","13.8.3.2","13.8.4.2","13.8.5.2","13.8.6.2")
    startRows <- c(9,9,9,9,8,8) 


    data <- list()
    for (i in seq_along(sheets)) {
        data[[i]] <- readCitrusHectar_spain_sheet(sheets[i],startRows[i])
         
    }
    data <- bind_rows(data) %>%
        group_by(name) %>%
        mutate(ha=sum(ha)) %>%
        slice(1) %>%
        ungroup()
    
    
}
 
