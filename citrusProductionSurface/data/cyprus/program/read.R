library(pacman)
p_load(dplyr)
p_load(readxl)
p_load(reshape2)

condDownload("http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/E82BC8FA48353FE9C2257E5E0031C997/$file/AGRI-PRODUCTS-A2009_13-EN-080615.xls?OpenElement",paste0(getwd(),"/cyprus/originals/AGRI-PRODUCTS-A2009_13-EN-080615.xls"))


readCitrusHectar_cyprus <- function() {
    read_excel(paste0(getwd(),"/cyprus/originals/AGRI-PRODUCTS-A2009_13-EN-080615.xls"),skip=3) %>% tbl_df() %>%
        melt("PRODUCT",variable.name="year",value.name="ha",rm.na=T) %>%
        tbl_df() %>%
        filter(!is.na(ha)) %>%
        filter(!ha=="..") %>%
        mutate(year=as.numeric(as.character(year)),
               ha=as.numeric(ha)) %>%
        filter(PRODUCT=="Citrus") %>%
        select(-PRODUCT) %>%
        mutate(name="Κύπρος (Kypros)",
               country="CY",
               comment="",
               source="http://www.cystat.gov.cy/mof/cystat/statistics.nsf/index_gr/index_gr?OpenDocument",
               link="http://www.cystat.gov.cy/mof/cystat/statistics.nsf/All/E82BC8FA48353FE9C2257E5E0031C997/$file/AGRI-PRODUCTS-A2009_13-EN-080615.xls?OpenElement",
               date="07/01/2015")

}
