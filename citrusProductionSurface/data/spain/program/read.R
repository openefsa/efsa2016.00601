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
                                        # balleares=data_frame(country=c("ES"),year=c(2013),
                                        #NUTS.Code.Country=c("ES531","ES532","ES533"),
                                        #ha=c(40000,40000,40000),
                                        #name=c("1","2","3"))
    
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
       mutate(country="ES",
              year=2013,
              comment="",
              source="http://www.magrama.gob.es/es/estadistica/temas/publicaciones/anuario-de-estadistica/2014/default.aspx?parte=3&capitulo=13",
              link="http://www.magrama.gob.es/estadistica/pags/anuario/2014/AE_2014_13.xlsx",
              date="06/01/2015")# %>%
                                        #bind_rows(balleares) %>%
                                        #filter(!name=="BALEARES")
    
   
    
}
