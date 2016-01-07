library(pacman)
p_load(readxl)
p_load(dplyr)
p_load(stringr)




getDataFromSheet <- function(file,sheet,year,comment) {
    fullFileName <- paste0(getwd(),"/italy/original/",file)
    downloadLink <- paste0("http://agri.istat.it/sag_is_pdwout/excel/",file)
    download.file(downloadLink,fullFileName)
    data <- read_excel(fullFileName,sheet,F,skip=3) %>%
        tbl_df() %>%
        select(X1,X3,X8,X13,X18) %>%
        rename(name=X1) %>% mutate(name=str_trim(name)) %>%
        filter(!name %in% c("Piemonte", "Valle d'Aosta/Vallée d'Aoste",
                            "Lombardia", "Liguria",
                            "Trentino-Alto Adige", "Veneto",                                   
                            "Friuli-Venezia Giulia",
                            "Emilia-Romagna",
                            "Toscana",
                            "Umbria",
                            "Marche",
                            "Lazio",
                            "Abruzzo",
                            "Molise","Campania","Puglia","Basilicata","Calabria","Sicilia","Sardegna","ITALIA"
                            )) %>%
   
       mutate_each(funs(ifelse(. == "-",0,.))) %>%
       mutate_each(funs(as.numeric(sub(".","",.,fixed=T))),X3,X8,X13,X18) %>%
       group_by(name) %>% filter(sum(c(X3,X8,X13,X18))>0) %>%
       mutate(ha=sum(X3,X8,X13,X18)) %>% ungroup() %>%
       select(name,ha) %>%
       mutate(year=year,
              country="Italy",
              date="06/01/2015",
              comment=comment,
              link=downloadLink,
              source="http://agri.istat.it/sag_is_pdwout/jsp/NewDownload.jsp?id=97A|15A|18A|21A|31A"
              )
    data
}
readCitrusHectar_italy <- function() {
    getDataFromSheet("Dw312013.xls",1,2013,"") %>%
        rbind(getDataFromSheet("Dw312013.xls",2,2013,"")) %>%
        rbind(getDataFromSheet("Dw312014.xls",1,2014,"")) %>%
        rbind(getDataFromSheet("Dw312014.xls",2,2014,"")) %>%
        rbind(getDataFromSheet("Dw312015.xls",1,2015,"Data for pompelmo, bergamotto, cedro, chinotto is missing for 2015"))
}
