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

readCitrusHectar_spain_sheet<- function(filename,year,sheet,skip=8) {

    df <- read_excel(paste0(getwd(),"/spain/original/",filename),sheet=sheet,skip = skip)
    names(df) <- c("name","X2","ha",paste0("X",seq(4,9)))
    df <- df %>%
        mutate(name=str_trim(name)) %>%
        tbl_df() %>%
        select(name,ha) %>%
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
               year=year,
               source=sprintf("http://www.magrama.gob.es/es/estadistica/temas/publicaciones/anuario-de-estadistica/%d/default.aspx?parte=3&capitulo=13",year),
               link=sprintf("http://www.magrama.gob.es/estadistica/pags/anuario/%d/%s",year,filename),
               date="02/02/2016",
               file=filename)
   
        
    baleares <- distributeToNuts3(df,"ES53","BALEARES")
    murcias <- distributeToNuts3(df,"ES62","R. DE MURCIA")
    df %>%
        bind_rows(baleares,murcias) %>%
        filter(!name %in% c("BALEARES","R. DE MURCIA")) 
        
}


readCitrusHectar_spain <- function() {
    files <- c("AE_2014_13.xlsx","AE_2013_13.xls")
    years <- c(2013,2012)
    sheets <- c("13.8.2.2","13.8.2.7","13.8.3.2","13.8.4.2","13.8.5.2","13.8.6.2")
    startRows <- c(8,8,8,8,7,7) 


    data <- list()
    index <- 1
    for (i in seq_along(years)) {
        for (j in seq_along(sheets)) {
            data[[index]] <- readCitrusHectar_spain_sheet(files[i],years[i],sheets[j],startRows[j])
            index <- index + 1
        }
        
    }
    data <- bind_rows(data) %>%
        group_by(year,name) %>%
        mutate(ha=sum(ha)) %>%
        slice(1) %>%
        ungroup()
    
    
}
 
