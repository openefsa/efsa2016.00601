#' @importFrom dplyr %>% group_by ungroup rename mutate slice select filter left_join
#' @importFrom tidyr gather separate
#' @importFrom stringr str_trim


#' @export

readCitrusHectar_croatia <- function() {
    nutsLevels <- efsagis::nutsLevels()
    nuts.area <-  efsagis::getNuts3Areas() 

    nuts2 <- readr::read_csv(system.file("extdata/croatia/CP421_ENG.csv",package="efsa2016.00601"),skip=2) %>%
        
        group_by(Category) %>%
        gather(key=year_region,value=ha,-Category) %>%
        ungroup() %>%
        rename(species=Category) %>%
        separate(year_region,into=c("year","name"),extra="merge",remove=T) %>%
        
        mutate(species=gsub("\\d*","",species)) %>%
        mutate(species=str_trim(species)) %>%
        
        group_by(year,name) %>%
        mutate(ha_citrus=sum(ha)) %>%
        slice(1) %>% ungroup() %>%
        select(year,name,ha_citrus) %>%
        rename(ha=ha_citrus) %>%
        mutate(year=as.numeric(year))
    

    adriatic_shapes_perc <- nutsLevels %>%
        filter(grepl("HR03.+",NUTS.Code)) %>%
        left_join(nuts.area) %>%
        mutate(adriatic_area=sum(Shape_Area),perc=Shape_Area / adriatic_area) %>%
        select(perc,Description) %>%
        mutate(name="Adriatic Croatia")


    interpolated_by_area <- left_join(adriatic_shapes_perc,nuts2) %>%
        mutate(ha_area=ha*perc) %>%
        select(Description,year,ha_area)%>%
        rename(ha=ha_area) %>%
        mutate(country="HR",
               comment="Total area of citrus production in Adriatic Croatia distributed to regions of level 3 proportional to the area of the regions",
               link="http://www.dzs.hr/App/PXWeb/PXWebEng/Selection.aspx?px_tableid=T421_Eng.px&px_path=Agriculture%2c+Hunting%2c+Forestry+and+Fishing__Crop+production&px_language=en&px_db=Agriculture%2c+Hunting%2c+Forestry+and+Fishing&rxid=04654694-2080-4769-8ff9-1140ef84ab42") %>%
        
        rename(name=Description)
    interpolated_by_area
    
}
                                          
