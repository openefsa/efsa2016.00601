library(pacman)
p_load(openxlsx)
p_load(dplyr)
p_load(stringr)




readCitrusHectar_spain <- function() {
    df <- read.xlsx(paste0(getwd(),"/spain/original/AE_2014_13.xlsx"),sheet=239,startRow = 9,rowNames =T) %>%
      
        mutate(name=str_trim(row.names(.))) %>%
        rename(ha=X3) %>%
        tbl_df() 
    
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
       filter(!is.na(ha)) %>%
       mutate(country="Spain",
              year=2013,
              comment="",
              source="http://www.magrama.gob.es/es/estadistica/temas/publicaciones/anuario-de-estadistica/2014/default.aspx?parte=3&capitulo=13",
              link="http://www.magrama.gob.es/estadistica/pags/anuario/2014/AE_2014_13.xlsx",
              date="06/01/2015")
        
       
                                        
}
