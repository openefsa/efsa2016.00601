
#' @importFrom dplyr %>% tbl_df group_by ungroup rename mutate slice select filter left_join
#' @export

readCitrusHectar_cyprus <- function() {
    readxl::read_excel(system.file("extdata/cyprus/AGRI-PRODUCTS-A2009_13-EN-080615.xls",package="efsa2016.00601"),skip=3) %>% tbl_df() %>%
        reshape2::melt("PRODUCT",variable.name="year",value.name="ha",rm.na=T) %>%
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
