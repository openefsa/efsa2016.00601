library(pacman)
p_load(openxlsx)
p_load(dplyr)
p_load(stringr)




readCitrusHectar_spain <- function() {
    df <- read.xlsx(paste0(getwd(),"/spain/original/AE_2014_13.xlsx"),sheet=239,startRow = 9,rowNames =T) %>%
        mutate(name=str_trim(row.names(.))) %>%
        rename(ha=X3)
    
    row.names(df)<-NULL
    
    df %>% select(name,ha) %>%
        filter(!name %in% c("GALICIA",
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
   
       mutate(ha=as.numeric(ha)) %>%
       filter(!is.na(ha)) 
       
                                        
}
