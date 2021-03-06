#' @importFrom dplyr mutate_each funs


getDataFromSheet <- function(file,sheet,year,comment) {
    downloadLink <- paste0("http://agri.istat.it/sag_is_pdwout/excel/",file)
    
    fullFileName <- system.file(paste0("extdata/italy/",file),package="efsa2016.00601")
    data <- readxl::read_excel(fullFileName,sheet,F,skip=3) %>%
        tbl_df() %>%
        select(X1,X3,X8,X13,X18) %>%
        rename(name=X1) %>% mutate(name=str_trim(name)) %>%
        filter(!name %in% c("Piemonte",
                            "Valle d'Aosta/Vallée d'Aoste",
                            "Lombardia",
                            "Liguria",
                            "Trentino-Alto Adige",
                            "Veneto",                                   
                            "Friuli-Venezia Giulia",
                            "Emilia-Romagna",
                            "Toscana",
                            "Umbria",
                            "Marche",
                            "Lazio",
                            "Abruzzo",
                            "Molise",
                            "Campania",
                            "Puglia",
                            "Basilicata",
                            "Calabria",
                            "Sicilia",
                            "Sardegna",
                            "ITALIA"
                            )) %>%
   
       mutate_each(funs(ifelse(. == "-",0,.))) %>%
       mutate_each(funs(as.numeric(sub(".","",.,fixed=T))),X3,X8,X13,X18) %>%
       group_by(name) %>% filter(sum(c(X3,X8,X13,X18))>0) %>%
       mutate(ha=sum(X3,X8,X13,X18)) %>% ungroup() %>%
       select(name,ha) %>%
       mutate(year=year,
              country="IT",
              date="06/01/2015",
              comment=comment,
              link=downloadLink,
              source="http://agri.istat.it/sag_is_pdwout/jsp/NewDownload.jsp?id=97A|15A|18A|21A|31A")
             
    data
}

readCitrusHectar_italy <- function() {
    italy <- bind_rows(
        getDataFromSheet("Dw312011.xls",1,2011,""),
        getDataFromSheet("Dw312011.xls",2,2011,""),
        getDataFromSheet("Dw312012.xls",1,2012,""),
        getDataFromSheet("Dw312012.xls",2,2012,""),
        getDataFromSheet("Dw312013.xls",1,2013,""), 
        getDataFromSheet("Dw312013.xls",2,2013,""),
        getDataFromSheet("Dw312014.xls",1,2014,""), 
        getDataFromSheet("Dw312014.xls",2,2014,"")) %>%
        
                                        #, 
                                        #getDataFromSheet("Dw312015.xls",1,2015,"Data for pompelmo, bergamotto, cedro, chinotto is missing for 2015")) %>%
            group_by(name,year,country) %>%
            mutate(ha=sum(ha))  %>%
            filter(row_number()==1) 
        italy
    
    }
